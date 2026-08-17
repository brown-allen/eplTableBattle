# ---------------------------------------------------------------------------
# scoring.R -- pluggable scoring rules
# ---------------------------------------------------------------------------
# The competition's points method is not final yet. Everything downstream
# (leaderboard, per-player tables, charts) only calls `score_all()` and reads
# the columns it returns, so finalising the method means either flipping
# CONFIG$scoring_method or adding one function to SCORERS below.
#
# A scorer takes a data frame with one row per club and the columns
#   team, predicted (integer 1-20), actual (integer 1-20)
# and returns a numeric vector of per-club scores, same length/order.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

## --- individual rules ------------------------------------------------------

score_abs_diff <- function(df) abs(df$predicted - df$actual)

score_squared_diff <- function(df) (df$predicted - df$actual)^2

score_banded <- function(df) {
  off <- abs(df$predicted - df$actual)
  bands <- CONFIG$banded_points
  band_keys <- as.integer(names(bands))
  vapply(off, function(d) {
    hit <- which(band_keys == d)
    if (length(hit)) unname(bands[hit[1]]) else CONFIG$banded_floor
  }, numeric(1))
}

SCORERS <- list(
  abs_diff     = score_abs_diff,
  squared_diff = score_squared_diff,
  banded       = score_banded
)

## Bonuses apply only to "higher is better" rules; for difference-based rules
## they would perversely add to a player's (bad) total, so they are skipped.
apply_bonuses <- function(df, method = CONFIG$scoring_method) {
  if (score_direction(method) != "higher") return(0)

  bonus <- 0
  champ_ok <- with(df, any(predicted == 1 & actual == 1))
  if (isTRUE(champ_ok)) bonus <- bonus + CONFIG$bonus_champion

  n <- nrow(df)
  bottom_pred   <- df$team[df$predicted > n - 3]
  bottom_actual <- df$team[df$actual    > n - 3]
  if (setequal(bottom_pred, bottom_actual)) bonus <- bonus + CONFIG$bonus_relegation

  bonus
}

## --- driver ----------------------------------------------------------------

#' Join one player's prediction to the live table and score it.
#' Returns a tibble with one row per club, ordered by predicted position.
score_player <- function(prediction, live, method = CONFIG$scoring_method) {
  stopifnot(method %in% names(SCORERS))

  df <- prediction |>
    select(team, predicted = position) |>
    left_join(live |> select(team, actual = position), by = "team") |>
    arrange(predicted)

  if (anyNA(df$actual)) {
    stop("Clubs in the prediction are absent from the live table: ",
         paste(df$team[is.na(df$actual)], collapse = ", "), call. = FALSE)
  }

  df <- df |>
    mutate(
      diff  = actual - predicted,          # + = club finished lower than picked
      off   = abs(diff),
      exact = off == 0
    )
  df$score <- SCORERS[[method]](df)
  df
}

#' Score every player. Returns a list with:
#'   $detail      long tibble, one row per player x club
#'   $leaderboard one row per player, ranked
score_all <- function(predictions, live, players = NULL,
                      method = CONFIG$scoring_method) {

  who <- unique(predictions$player)

  detail <- lapply(who, function(p) {
    score_player(filter(predictions, player == p), live, method) |>
      mutate(player = p, .before = 1)
  }) |> bind_rows()

  dir <- score_direction(method)

  leaderboard <- detail |>
    group_by(player) |>
    summarise(
      total_score  = sum(score),
      exact_hits   = sum(exact),
      within_one   = sum(off <= 1),
      within_three = sum(off <= 3),
      worst_miss   = max(off),
      .groups = "drop"
    )

  ## bonuses (no-ops for difference-based rules)
  bonuses <- vapply(who, function(p) {
    apply_bonuses(filter(detail, player == p), method)
  }, numeric(1))
  leaderboard$bonus <- unname(bonuses[leaderboard$player])
  leaderboard$total_score <- leaderboard$total_score + leaderboard$bonus

  ## Rank. Tiebreak: more exact hits wins, then fewer big misses.
  leaderboard <- leaderboard |>
    arrange(
      if (dir == "lower") total_score else -total_score,
      -exact_hits, worst_miss, player
    ) |>
    mutate(rank = rank(
      if (dir == "lower") total_score else -total_score,
      ties.method = "min"
    ), .before = 1)

  if (!is.null(players)) {
    leaderboard <- left_join(leaderboard, players, by = "player") |>
      mutate(display_name = coalesce(display_name, player))
  } else {
    leaderboard$display_name <- leaderboard$player
  }

  list(detail = detail, leaderboard = leaderboard, method = method,
       direction = dir)
}

#' Human-readable one-liner describing the active rule, for the site.
describe_method <- function(method = CONFIG$scoring_method) {
  switch(
    method,
    abs_diff = paste0(
      "Each club scores the number of places between where you put it and ",
      "where it actually sits. Your total is the sum across all 20 clubs. ",
      "**Lowest total wins.** A perfect table scores 0."),
    squared_diff = paste0(
      "Each club scores the *square* of the number of places between your ",
      "pick and reality, so single large misses hurt far more than several ",
      "small ones. **Lowest total wins.**"),
    banded = paste0(
      "Each club earns points for how close you were: ",
      paste(sprintf("%s place%s out = %s pt%s",
                    names(CONFIG$banded_points),
                    ifelse(names(CONFIG$banded_points) == "1", "", "s"),
                    CONFIG$banded_points,
                    ifelse(CONFIG$banded_points == 1, "", "s")),
            collapse = "; "),
      "; anything further out = ", CONFIG$banded_floor, ". ",
      "Bonuses: +", CONFIG$bonus_champion, " for the champion, +",
      CONFIG$bonus_relegation, " for the relegated three. ",
      "**Highest total wins.**"),
    paste0("Custom rule: ", method)
  )
}
