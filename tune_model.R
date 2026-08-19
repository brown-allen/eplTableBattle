#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# tune_model.R -- search component weights, honestly
# ---------------------------------------------------------------------------
# A grid search scored by leave-one-season-out cross-validation: each candidate
# is judged on seasons that were not used to choose it. With only four seasons
# this is thin evidence, but it is the difference between weights that were
# tested and weights that were asserted.

source("R/config.R"); source("R/teams.R")
source("R/model_config.R"); source("R/model_data.R"); source("R/model.R")
suppressPackageStartupMessages({ library(dplyr); library(readr) })

resolve_keep <- function(x) { r <- resolve_team(x, strict = FALSE); ifelse(is.na(r), trimws(x), r) }
targets <- MODEL$backtest_targets

message("caching inputs for ", length(targets), " seasons ...")
xg <- fetch_xg() |> mutate(team = resolve_keep(raw_name))

FOLDS <- lapply(targets, function(t) {
  hist <- fetch_history(seasons = (t - 3):(t - 1)) |> mutate(team = resolve_keep(raw_name))
  act  <- fetch_history(seasons = t) |> mutate(team = resolve_keep(raw_name)) |>
          filter(league == "eng.1", season == t) |> transmute(team, actual = rank)
  prev <- fetch_history(seasons = t - 1) |> mutate(team = resolve_keep(raw_name)) |>
          filter(league == "eng.1", season == t - 1) |> transmute(team, prev_rank = rank)
  list(target = t, hist = hist, actual = act,
       values = fetch_squad_values(season = t) |> mutate(team = resolve_keep(raw_name)),
       prev = prev)
})
names(FOLDS) <- targets

score_fold <- function(fold, w) {
  p <- predict_table(fold$actual$team, fold$hist, xg = xg, values = fold$values,
                     adjust = NULL, seasons = (fold$target - 3):(fold$target - 1),
                     weights = w) |> select(team, predicted = position)
  mean(abs(left_join(fold$actual, p, by = "team") |>
             mutate(e = predicted - actual) |> pull(e)))
}

grid <- expand.grid(
  domestic = c(0.20, 0.30, 0.40, 0.50),
  xg       = c(0.00, 0.10, 0.20, 0.30),
  value    = c(0.10, 0.20, 0.30, 0.40),
  europe   = c(0.00, 0.05, 0.10)
)
grid <- grid[rowSums(grid) > 0, ]
message("grid: ", nrow(grid), " weight combinations x ", length(FOLDS), " seasons")

mae <- matrix(NA_real_, nrow(grid), length(FOLDS))
for (i in seq_len(nrow(grid))) {
  w <- unlist(grid[i, ]); w <- c(w, adjust = 0); w <- w / sum(w)
  for (j in seq_along(FOLDS)) mae[i, j] <- score_fold(FOLDS[[j]], w)
  if (i %% 40 == 0) message("  ", i, "/", nrow(grid))
}

## Leave-one-season-out: pick the winner on the other three seasons, then
## record how it did on the held-out one. That held-out number is the estimate
## that has not been contaminated by the search.
loso <- vapply(seq_along(FOLDS), function(j) {
  train <- rowMeans(mae[, -j, drop = FALSE])
  mae[which.min(train), j]
}, numeric(1))

baseline <- vapply(FOLDS, function(f) {
  d <- f$actual |> left_join(f$prev, by = "team") |>
    mutate(prev_rank = ifelse(is.na(prev_rank), 18, prev_rank),
           prev_rank = rank(prev_rank, ties.method = "first"))
  mean(abs(d$prev_rank - d$actual))
}, numeric(1))

overall <- rowMeans(mae)
best <- unlist(grid[which.min(overall), ]); best <- c(best, adjust = 0); best <- best / sum(best)

message("\n=== results ===")
for (j in seq_along(FOLDS)) {
  message(sprintf("  %d-%02d  held-out model %.2f   last-table %.2f",
                  targets[j], (targets[j] + 1) %% 100, loso[j], baseline[j]))
}
message(sprintf("\n  cross-validated model : %.2f places", mean(loso)))
message(sprintf("  last-table baseline   : %.2f places", mean(baseline)))
message(sprintf("  advantage             : %+.2f places", mean(baseline) - mean(loso)))
message(sprintf("\n  in-sample best weights: %s",
                paste(sprintf("%s %.0f%%", names(best), 100 * best), collapse = ", ")))
message(sprintf("  in-sample MAE         : %.2f places", min(overall)))
message("\n  (in-sample is what the grid was optimised on, so treat the")
message("   cross-validated figure as the honest one.)")

saveRDS(list(grid = grid, mae = mae, loso = loso, baseline = baseline, best = best),
        "data/model/tuning.rds")
