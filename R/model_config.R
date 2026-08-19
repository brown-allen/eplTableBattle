# ---------------------------------------------------------------------------
# model_config.R -- every dial the prediction model has
# ---------------------------------------------------------------------------

MODEL <- list(

  ## Seasons to learn from, by starting year. 2025 = the 2025-26 season.
  seasons = c(2023, 2024, 2025),

  ## Past seasons to backtest against, by starting year. Each is predicted
  ## from the three seasons before it only.
  backtest_targets = c(2022, 2023, 2024, 2025),

  ## Recency weights, newest first. Normalised internally, so relative size is
  ## all that matters.
  recency = c(0.50, 0.30, 0.20),

  ## How much each component contributes. Renormalised over whatever is
  ## actually available, so a missing component does not silently shrink the
  ## spread of the final index.
  weights = c(
    domestic = 0.38,   # league results, recency-weighted
    xg       = 0.22,   # xG difference per game, recency-weighted
    value    = 0.25,   # current squad market value
    europe   = 0.10,   # Champions/Europa League performance
    adjust   = 0.05    # manager stability + injury burden
  ),

  ## A Championship season is worth this fraction of a Premier League season
  ## when judging a promoted club. Promoted sides average roughly 1.0 PPG in
  ## the top flight against roughly 1.9 PPG in the division they came up from,
  ## so a little over a half.
  championship_discount = 0.55,

  ## Clubs with no record at all in a given season (neither division) are
  ## treated as this many points per game -- deliberately pessimistic, since
  ## an absent club is usually a lower-league one.
  absent_ppg = 0.9,

  ## European competition credit, before recency weighting. The league phase
  ## rank is turned into a 0-1 score and multiplied by these.
  europe_weight = c(uefa.champions = 1.0, uefa.europa = 0.55),

  ## How far the manual adjustments can move a club, in standard deviations
  ## of the final index. Deliberately small: these are nudges, not opinions.
  adjust_scale = 0.35,

  user_agent = paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                     "AppleWebKit/537.36 (KHTML, like Gecko)",
                     "Chrome/126.0.0.0 Safari/537.36")
)
