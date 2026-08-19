#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# run_simulation.R -- simulate the season many times, present three of them
# ---------------------------------------------------------------------------
#   Rscript run_simulation.R [n_sims]

source("R/config.R"); source("R/teams.R")
source("R/model_config.R"); source("R/model_data.R")
source("R/manager_tenure.R"); source("R/model.R"); source("R/simulate.R")
suppressPackageStartupMessages({ library(dplyr); library(readr) })

args <- commandArgs(TRUE)
N_SIMS <- if (length(args) && !is.na(suppressWarnings(as.integer(args[1]))))
            as.integer(args[1]) else MODEL$n_sims

pred <- read_csv("data/model/prediction_full.csv", show_col_types = FALSE)
if (!nrow(pred)) stop("Run run_model.R first.", call. = FALSE)
pred <- pred |> arrange(position)
TEAMS <- load_teams()

## --- calibration -----------------------------------------------------------
cal <- calibrate_noise(MODEL$backtest_mae, nrow(pred))

message("== noise calibration ==")
message(sprintf("  model's backtested error : %.2f places", MODEL$backtest_mae))
message(sprintf("  latent correlation       : %.3f", cal$r))
message(sprintf("  noise added              : %.2f SD of the strength index", cal$noise_sd))
message(sprintf("  simulated error (check)  : %.2f places", cal$implied_mae))
message("  For reference, a model that knew every club's true strength would")
message("  still miss by 2.34 places: a season is only ~76% signal.")

message(sprintf("\n== %s simulations ==", format(N_SIMS, big.mark = ",")))
message(sprintf("  Monte Carlo error on a probability: +/- %.2f pp worst case",
                100 * mc_error(N_SIMS)))

sims <- simulate_seasons(pred$index, pred$team, n_sims = N_SIMS, r = cal$r)
summ <- summarise_sims(sims)

## --- outcome probabilities -------------------------------------------------
message("\n== outcome probabilities ==")
out <- summ |> transmute(
  Club = team,
  Pred = pred$position[match(team, pred$team)],
  Mean = sprintf("%.1f", mean_pos),
  `90% range` = sprintf("%d-%d", as.integer(p05), as.integer(p95)),
  Title = sprintf("%4.1f%%", 100 * p_title),
  `Top 4` = sprintf("%4.1f%%", 100 * p_top4),
  Releg = sprintf("%4.1f%%", 100 * p_releg)
)
print(as.data.frame(out), row.names = FALSE)

## Every simulated season is a valid permutation, so these columns must sum to
## exactly one champion, four Champions League places and three relegations.
## If they do not, the simulation is producing impossible seasons.
chk <- c(title = sum(summ$p_title), top4 = sum(summ$p_top4), releg = sum(summ$p_releg))
ok <- all(abs(chk - c(1, 4, 3)) < 1e-9)
message(sprintf("\n  consistency: champions %.3f, top-4 places %.3f, relegations %.3f -- %s",
                chk[["title"]], chk[["top4"]], chk[["releg"]],
                if (ok) "exact" else "BROKEN"))
if (!ok) stop("Simulation produced impossible seasons.", call. = FALSE)

## --- three representative seasons ------------------------------------------
base_pos <- pred$position
rep <- pick_representative(sims, base_pos, probs = MODEL$sim_percentiles)

labels <- c("FORM HOLDS", "A TYPICAL SEASON", "A CHAOTIC SEASON")
blurb <- c(
  "quiet by this model's standards - 15% of seasons stray less",
  "the median: half of simulated seasons stray more, half less",
  "turbulent - only 10% of seasons stray further"
)

tabs <- lapply(seq_along(rep$index), function(k) {
  pos <- sims[, rep$index[k]]
  tibble(position = as.integer(pos), team = rownames(sims)) |> arrange(position)
})

message("\n== three possible tables ==")
message(sprintf("  picked at the %s percentiles of how far a season strays from",
                paste0(100 * MODEL$sim_percentiles, collapse = "/")))
message("  the point prediction. Each is one real simulated season, not a blend.\n")

hdr <- sprintf("%-4s %-24s %-24s %-24s", "#", labels[1], labels[2], labels[3])
cat(hdr, "\n")
cat(strrep("-", nchar(hdr)), "\n")
for (i in seq_len(nrow(pred))) {
  cat(sprintf("%-4d %-24s %-24s %-24s\n", i,
              tabs[[1]]$team[i], tabs[[2]]$team[i], tabs[[3]]$team[i]))
}
cat("\n")
for (k in seq_along(labels)) {
  message(sprintf("  %-18s %s", labels[k], blurb[k]))
  message(sprintf("  %-18s avg %.2f places from the prediction; champions %s",
                  "", rep$deviation[k], tabs[[k]]$team[1]))
}

## --- outputs ---------------------------------------------------------------
dir.create("data/model", showWarnings = FALSE, recursive = TRUE)
write_csv(summ, "data/model/simulation_summary.csv")
abb <- setNames(TEAMS$abbrev, TEAMS$team)
codes <- vapply(seq_along(tabs), function(k) {
  sprintf("%s:%s", gsub(" ", "-", labels[k]), paste(abb[tabs[[k]]$team], collapse = ","))
}, character(1))
writeLines(codes, "data/model/simulated_tables_codes.txt")
bind_rows(lapply(seq_along(tabs), function(k)
  tabs[[k]] |> mutate(scenario = labels[k]))) |>
  write_csv("data/model/simulated_tables.csv")

message("\nwritten: simulation_summary.csv, simulated_tables.csv, simulated_tables_codes.txt")
