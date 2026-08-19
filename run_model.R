#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_model.R -- fetch inputs, backtest, and predict the 2026-27 table
# ---------------------------------------------------------------------------
#   Rscript run_model.R              use cached inputs where present
#   Rscript run_model.R --refresh    re-fetch everything

source("R/config.R"); source("R/teams.R")
source("R/model_config.R"); source("R/model_data.R")
source("R/manager_tenure.R"); source("R/model.R")
suppressPackageStartupMessages({ library(dplyr); library(readr) })

refresh <- "--refresh" %in% commandArgs(TRUE)
TEAMS <- load_teams()
CLUBS <- TEAMS$team

## Resolve a source's club names onto canonical ones, keeping the raw name for
## clubs outside the current twenty (needed by the backtest).
resolve_keep <- function(x) {
  r <- resolve_team(x, strict = FALSE)
  ifelse(is.na(r), trimws(x), r)
}

message("== inputs ==")
history <- fetch_history(refresh = refresh) |> mutate(team = resolve_keep(raw_name))
xg      <- fetch_xg(refresh = refresh)      |> mutate(team = resolve_keep(raw_name))
values  <- fetch_squad_values(refresh = refresh) |> mutate(team = resolve_keep(raw_name))
adjust  <- if (file.exists("data/model/adjustments.csv"))
             read_csv("data/model/adjustments.csv", show_col_types = FALSE) else NULL
managers <- if (file.exists("data/model/managers.csv"))
             read_csv("data/model/managers.csv", show_col_types = FALSE) else NULL
injuries <- if (file.exists("data/model/injuries.csv"))
             read_csv("data/model/injuries.csv", show_col_types = FALSE) else NULL
chronic  <- if (file.exists("data/model/injuries_chronic.csv"))
             read_csv("data/model/injuries_chronic.csv", show_col_types = FALSE) else NULL

if (!is.null(managers)) {
  missing_mgr <- managers$team[is.na(managers$appointed)]
  message(sprintf("  managers: %d of %d with an appointment date",
                  sum(!is.na(managers$appointed)), nrow(managers)))
  if (length(missing_mgr)) {
    message("    NO DATE, scored neutral on the manager term: ",
            paste(missing_mgr, collapse = ", "))
  }
}
if (!is.null(injuries)) {
  message(sprintf("  injuries: %d clubs, %.1f absences on average (range %d-%d)",
                  nrow(injuries), mean(injuries$injured),
                  min(injuries$injured), max(injuries$injured)))
}

message(sprintf("  history: %d league-seasons, %d rows",
                nrow(distinct(history, league, season)), nrow(history)))
message(sprintf("  xG: %d club-seasons (coverage %s)", nrow(xg),
                paste(sprintf("%d:%.0f%%", xg$season_end,
                              100 * pmin(1, xg$matches / 38))[!duplicated(xg$season_end)],
                      collapse = " ")))
message(sprintf("  squad values: %d clubs matched", sum(values$team %in% CLUBS)))
if (!is.null(adjust) && any(adjust$manager_stability != 0 | adjust$injury_burden != 0)) {
  message("  manual adjustments: ", sum(adjust$manager_stability != 0 |
                                        adjust$injury_burden != 0), " clubs set")
} else message("  manual adjustments: none set (all neutral)")

## --- backtest --------------------------------------------------------------
## Predict seasons the model has not been shown, using only what was knowable
## beforehand -- previous seasons plus the squad values on record at the start
## of the season being predicted -- and compare against a baseline of simply
## reusing last season's finishing order.

message("\n== backtest ==")
hist_fetch <- function(seasons) {
  fetch_history(seasons = seasons, refresh = refresh) |>
    mutate(team = resolve_keep(raw_name))
}
vals_fetch <- function(season) {
  fetch_squad_values(season = season, refresh = refresh) |>
    mutate(team = resolve_keep(raw_name))
}
bt <- backtest_model(targets = MODEL$backtest_targets, fetch_hist = hist_fetch,
                     xg = xg, fetch_values = vals_fetch)

if (is.null(bt)) {
  message("  backtest unavailable")
} else {
  for (i in seq_len(nrow(bt$per_season))) {
    r <- bt$per_season[i, ]
    message(sprintf("  %d-%02d  model %.2f  last-table %.2f  (exact %d vs %d)",
                    r$season, (r$season + 1) %% 100, r$model_mae, r$prev_mae,
                    r$model_exact, r$prev_exact))
  }
  message(sprintf("  ---- over %d club-seasons ----", bt$n))
  message(sprintf("  model mean |error|      : %.2f places", bt$model_mae))
  message(sprintf("  last-table baseline     : %.2f places", bt$prev_mae))
  message(sprintf("  random ordering         : %.2f places", (20 + 1) / 3))
  message(sprintf("  model advantage         : %+.2f places (95%% CI %+.2f to %+.2f, p = %.3f)",
                  bt$diff, bt$ci[1], bt$ci[2], bt$p))
  readr::write_csv(bt$rows, "data/model/backtest.csv")
}

## --- the prediction --------------------------------------------------------

message("\n== 2026-27 prediction ==")
pred <- predict_table(CLUBS, history, xg = xg, values = values, adjust = adjust,
                      managers = managers, injuries = injuries, chronic = chronic)

w <- attr(pred, "weights_used")
message("  weights used: ",
        paste(sprintf("%s %.0f%%", names(w), 100 * w), collapse = ", "))

out <- pred |>
  transmute(
    Pos = position, Club = team,
    Index = round(index, 3),
    PPG = round(domestic_ppg, 2),
    xGD = ifelse(is.na(xgd_pg), NA, round(xgd_pg, 2)),
    `Val€m` = round(squad_value / 1e6),
    Eur = round(europe, 2),
    MgrY = ifelse(is.na(tenure_years), NA, round(tenure_years, 1)),
    Inj = injured,
    Adj = round(adjust_raw, 2)
  )
print(as.data.frame(out), row.names = FALSE)

## How much is the adjustment component actually worth? Re-rank without it
## and compare, so the answer is measured rather than assumed.
no_adj <- predict_table(CLUBS, history, xg = xg, values = values,
                        adjust = NULL, managers = NULL, injuries = NULL,
                        chronic = NULL) |>
  select(team, pos_without = position)
moves <- pred |> select(team, pos_with = position) |>
  left_join(no_adj, by = "team") |>
  mutate(shift = pos_without - pos_with) |>
  filter(shift != 0) |> arrange(desc(abs(shift)))

message(sprintf("\nmanager + injury adjustment moved %d of %d clubs; largest shift %d place(s)",
                nrow(moves), length(CLUBS),
                if (nrow(moves)) max(abs(moves$shift)) else 0L))
if (nrow(moves)) {
  for (i in seq_len(min(6, nrow(moves)))) {
    message(sprintf("   %-24s %2d -> %2d  (%+d)", moves$team[i],
                    moves$pos_without[i], moves$pos_with[i], moves$shift[i]))
  }
}

write_csv(pred, "data/model/prediction_full.csv")
write_csv(out,  "data/model/prediction.csv")

## A pick code in the site's own format, so the prediction can be entered
## as an entry or compared against one.
abb <- setNames(TEAMS$abbrev, TEAMS$team)
code <- paste0("Model:", paste(abb[pred$team], collapse = ","))
writeLines(code, "data/model/prediction_code.txt")
message("\npick code -> data/model/prediction_code.txt")
message(code)
