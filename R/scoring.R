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

## The competition's rule. Each club is worth 5 points for an exact call,
## one less for every place the guess was out, and never less than 0:
##
##   off by  0  1  2  3  4  5+
##   points  5  4  3  2  1  0
##
## A perfect table scores 5 * 20 = 100.
score_proximity5 <- function(df) {
  pmax(0, 5 - abs(df$predicted - df$actual))
}

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
  proximity5   = score_proximity5,
  abs_diff     = score_abs_diff,
  squared_diff = score_squared_diff,
  banded       = score_banded
)

## The maximum a single club can be worth, used to show "x of y" totals.
max_club_points <- function(method = CONFIG$scoring_method) {
  if (score_direction(method) != "higher") return(NA_integer_)
  probe <- data.frame(predicted = 1L, actual = 1L)
  as.integer(SCORERS[[method]](probe))
}

## --- bonuses ---------------------------------------------------------------
## Four separate awards, all added on top of the per-club points and only in
## the final week of the season:
##
##   champion         +5  predicted the title winner exactly
##   relegation_set   +5  named the relegated three, in any order
##   ucl_spots        +1  per club placed inside the top 4 that finishes there
##   relegation_spots +1  per club placed inside the bottom 3 that finishes there
##
## They deliberately overlap, as specified: a correct champion also earns a
## top-4 spot point, and naming all three relegated clubs earns both the set
## bonus and three spot points.
##
## Bonuses apply only to "higher is better" rules; on a difference-based rule
## they would perversely add to a player's (bad) total, so they are skipped.
BONUS_COMPONENTS <- c("champion", "relegation_set", "ucl_spots", "relegation_spots")

bonus_breakdown <- function(df, method = CONFIG$scoring_method,
                            now = Sys.time()) {
  z <- setNames(numeric(length(BONUS_COMPONENTS)), BONUS_COMPONENTS)
  if (score_direction(method) != "higher") return(z)
  if (!bonuses_active(now)) return(z)

  n     <- nrow(df)
  top_n <- CONFIG$ucl_places
  bot_n <- CONFIG$relegation_places

  ## 1. the champion
  if (any(df$predicted == 1 & df$actual == 1)) {
    z[["champion"]] <- CONFIG$bonus_champion
  }

  ## 2. the relegated three as a set, order irrelevant
  bottom_pred   <- df$team[df$predicted > n - bot_n]
  bottom_actual <- df$team[df$actual    > n - bot_n]
  if (setequal(bottom_pred, bottom_actual)) {
    z[["relegation_set"]] <- CONFIG$bonus_relegation_set
  }

  ## 3. one per club correctly placed inside the top 4 (max +4)
  z[["ucl_spots"]] <- sum(df$predicted <= top_n & df$actual <= top_n) *
    CONFIG$bonus_ucl_each

  ## 4. one per club correctly placed inside the bottom 3 (max +3)
  z[["relegation_spots"]] <- sum(df$predicted > n - bot_n & df$actual > n - bot_n) *
    CONFIG$bonus_relegation_each

  z
}

## --- driver ----------------------------------------------------------------

#' Join one player's prediction to the live table and score it.
#' Returns a tibble with one row per club, ordered by predicted position.
score_player <- function(prediction, live, method = CONFIG$scoring_method,
                         now = Sys.time()) {
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
  ## `earned` is what the prediction is worth on the current table; `score` is
  ## what actually counts, which is nothing until scoring opens. Keeping both
  ## lets the site show the shape of a table without awarding points early.
  df$earned <- SCORERS[[method]](df)
  df$score  <- if (scoring_active(now)) df$earned else 0
  df
}

#' Score every player. Returns a list with:
#'   $detail      long tibble, one row per player x club
#'   $leaderboard one row per player, ranked
score_all <- function(predictions, live, players = NULL,
                      method = CONFIG$scoring_method,
                      now = Sys.time()) {

  who <- unique(predictions$player)

  detail <- lapply(who, function(p) {
    score_player(filter(predictions, player == p), live, method, now) |>
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

  ## bonuses: zero until the final week, and always zero on a "lower wins" rule
  bmat <- vapply(who, function(p) {
    bonus_breakdown(filter(detail, player == p), method, now)
  }, numeric(length(BONUS_COMPONENTS)))
  bonuses <- as.data.frame(t(bmat))
  bonuses$player <- colnames(bmat)

  leaderboard <- left_join(leaderboard, bonuses, by = "player")
  leaderboard$bonus <- rowSums(leaderboard[, BONUS_COMPONENTS, drop = FALSE])
  leaderboard$base_score <- leaderboard$total_score
  leaderboard$total_score <- leaderboard$base_score + leaderboard$bonus

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
       direction = dir,
       scoring_active = scoring_active(now),
       bonuses_active = bonuses_active(now))
}

#' Human-readable one-liner describing the active rule, for the site.
describe_method <- function(method = CONFIG$scoring_method) {
  switch(
    method,
    proximity5 = paste0(
      "Every club is scored on its own. Put a club in exactly the right place ",
      "and it is worth **5 points**; each place you were out costs one point, ",
      "down to a floor of 0. So a club four places off still earns 1, and ",
      "anything five or more places off earns nothing. Your total is the sum ",
      "across all 20 clubs. **Highest total wins**, and a perfect table ",
      "scores 100."),
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
