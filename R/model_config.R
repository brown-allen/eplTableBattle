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

  ## Manager tenure curve. See R/manager_tenure.R for the shape.
  ##   new_boost  size of the new-appointment bounce at day one
  ##   tau_new    how fast that bounce decays, in years
  ##   long_max   what fully-established tenure is eventually worth
  ##   tau_long   how fast stability accumulates, in years
  ##   baseline   pulls the middle of the curve below zero
  tenure = list(
    new_boost = 0.40,
    tau_new   = 0.28,
    long_max  = 0.70,
    tau_long  = 2.00,
    baseline  = 0.28
  ),

  ## How the two adjustment inputs are mixed before z-scoring.
  adjust_mix = c(manager = 0.5, injury = 0.5),

  ## Within the injury term: how much is the club's chronic record versus who
  ## is actually unavailable right now.
  injury_mix = c(chronic = 0.5, current = 0.5),

  ## The adjustment component is z-scored like every other component, so the
  ## 5% weight in `weights` means what it appears to mean. It is a ranking of
  ## clubs against each other, not an absolute scale.
  adjust_scale = 1.0,

  ## --- simulation ---------------------------------------------------------
  ## The model's own backtested mean absolute error, from run_model.R. Noise is
  ## calibrated to this, so simulated seasons scatter as widely as the model
  ## actually misses. Update it if the backtest number changes.
  backtest_mae = 3.40,

  ## 10,000 keeps Monte Carlo error on any probability under +/- 0.5 points,
  ## which is finer than the model's real accuracy deserves.
  n_sims = 10000,

  ## Percentiles of "distance from the point prediction" at which the three
  ## presented tables are drawn.
  sim_percentiles = c(0.15, 0.50, 0.90),

  user_agent = paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                     "AppleWebKit/537.36 (KHTML, like Gecko)",
                     "Chrome/126.0.0.0 Safari/537.36")
)
