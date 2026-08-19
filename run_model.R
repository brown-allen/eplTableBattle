#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_model.R -- fetch inputs, backtest, and predict the 2026-27 table
# ---------------------------------------------------------------------------
#   Rscript run_model.R              use cached inputs where present
#   Rscript run_model.R --refresh    re-fetch everything

source("R/config.R"); source("R/teams.R")
source("R/model_config.R"); source("R/model_data.R"); source("R/model.R")
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
pred <- predict_table(CLUBS, history, xg = xg, values = values, adjust = adjust)

w <- attr(pred, "weights_used")
message("  weights used: ",
        paste(sprintf("%s %.0f%%", names(w), 100 * w), collapse = ", "))

out <- pred |>
  transmute(
    Pos = position, Club = team,
    Index = round(index, 3),
    PPG = round(domestic_ppg, 2),
    xGD = ifelse(is.na(xgd_pg), NA, round(xgd_pg, 2)),
    `Value €m` = round(squad_value / 1e6),
    Europe = round(europe, 2)
  )
print(as.data.frame(out), row.names = FALSE)

write_csv(pred, "data/model/prediction_full.csv")
write_csv(out,  "data/model/prediction.csv")

## A pick code in the site's own format, so the prediction can be entered
## as an entry or compared against one.
abb <- setNames(TEAMS$abbrev, TEAMS$team)
code <- paste0("Model:", paste(abb[pred$team], collapse = ","))
writeLines(code, "data/model/prediction_code.txt")
message("\npick code -> data/model/prediction_code.txt")
message(code)
