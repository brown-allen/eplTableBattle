# ---------------------------------------------------------------------------
# model.R -- turn the assembled inputs into a predicted 2026-27 table
# ---------------------------------------------------------------------------
# This is a weighted composite index, not a fitted statistical model. Each
# component is converted to a z-score across the 20 clubs, the z-scores are
# combined using MODEL$weights, and the clubs are ranked. That keeps every
# component on a comparable scale regardless of its natural units, and makes
# the weights mean what they appear to mean.
#
# backtest_model() in this file checks the approach against a season it did
# not see, which is the only honest way to say whether any of it works.

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr) })

.z <- function(x) {
  if (all(is.na(x))) return(rep(0, length(x)))
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

.recency <- function(seasons, newest_first = MODEL$recency) {
  w <- rev(newest_first[seq_along(seasons)])   # seasons ascending
  setNames(w / sum(w), sort(seasons))
}

## --- components ------------------------------------------------------------

#' Domestic strength: recency-weighted points per game, with Championship
#' seasons discounted and missing seasons given a pessimistic default.
component_domestic <- function(clubs, history, seasons = MODEL$seasons) {
  rw <- .recency(seasons)

  dom <- history |>
    filter(league %in% c("eng.1", "eng.2"), season %in% seasons) |>
    mutate(adj_ppg = ifelse(league == "eng.2",
                            ppg * MODEL$championship_discount, ppg)) |>
    ## a club can only appear once per season across the two divisions
    group_by(team, season) |>
    summarise(adj_ppg = max(adj_ppg, na.rm = TRUE), .groups = "drop")

  grid <- expand.grid(team = clubs, season = seasons,
                      stringsAsFactors = FALSE) |> as_tibble()

  grid |>
    left_join(dom, by = c("team", "season")) |>
    mutate(
      present = !is.na(adj_ppg),
      adj_ppg = ifelse(present, adj_ppg, MODEL$absent_ppg),
      w = rw[as.character(season)]
    ) |>
    group_by(team) |>
    summarise(
      domestic_ppg = sum(adj_ppg * w),
      seasons_seen = sum(present),
      .groups = "drop"
    )
}

#' xG strength: recency-weighted xG difference per game. Each season is also
#' weighted by how much of it the data actually covers, so a season with five
#' matches on file counts for about a seventh of one with all thirty-eight.
component_xg <- function(clubs, xg, seasons = MODEL$seasons) {
  if (!nrow(xg)) {
    return(tibble(team = clubs, xgd_pg = NA_real_, xg_coverage = 0))
  }
  rw <- .recency(seasons)

  xg |>
    mutate(season = season_end - 1L) |>
    filter(season %in% seasons, team %in% clubs) |>
    mutate(
      xgd_pg   = (xg - xga) / matches,
      coverage = pmin(1, matches / 38),
      w        = rw[as.character(season)] * coverage
    ) |>
    group_by(team) |>
    summarise(
      xgd_pg      = if (sum(w) > 0) sum(xgd_pg * w) / sum(w) else NA_real_,
      xg_coverage = sum(w),
      .groups = "drop"
    ) |>
    right_join(tibble(team = clubs), by = "team") |>
    mutate(xg_coverage = coalesce(xg_coverage, 0))
}

#' European strength: league-phase finish in the Champions and Europa League,
#' scored 0-1 within each competition and scaled by competition prestige.
component_europe <- function(clubs, history, seasons = MODEL$seasons) {
  rw <- .recency(seasons)

  ## Field size must come from the whole competition, not just the English
  ## clubs in it: a side ranked 30th of 36 is 30th of 36, not 30th of 4.
  eu <- history |>
    filter(league %in% names(MODEL$europe_weight), season %in% seasons,
           !is.na(rank)) |>
    group_by(league, season) |>
    mutate(field = n(),
           placing = 1 - (rank - 1) / pmax(field - 1, 1)) |>   # 1 = top of phase
    ungroup() |>
    filter(team %in% clubs) |>
    mutate(credit = placing * MODEL$europe_weight[league],
           w = rw[as.character(season)]) |>
    filter(!is.na(credit)) |>
    group_by(team, season) |>
    summarise(credit = max(credit), w = first(w), .groups = "drop") |>
    group_by(team) |>
    summarise(europe = sum(credit * w), .groups = "drop")

  tibble(team = clubs) |>
    left_join(eu, by = "team") |>
    mutate(europe = coalesce(europe, 0))
}

#' Manager stability and injury burden, combined into one small adjustment.
#'
#' The two inputs live on wildly different scales -- a tenure score spans about
#' half a point, an injury headcount spans two to eleven -- so each is z-scored
#' on its own before they are mixed. Otherwise the injury count would swamp the
#' manager term simply by having bigger numbers.
#'
#' Anything set by hand in `manual` overrides the computed value for that club,
#' on the assumption that if you bothered to type it you know something the
#' sources do not.
component_adjust <- function(clubs, managers = NULL, injuries = NULL,
                             chronic = NULL, manual = NULL,
                             ref_date = Sys.Date(),
                             mix = MODEL$adjust_mix,
                             inj_mix = MODEL$injury_mix) {

  d <- tibble(team = clubs)

  ## --- manager tenure ---
  d <- d |> left_join(
    if (is.null(managers)) tibble(team = character(), appointed = as.Date(character()))
    else managers |> select(team, appointed),
    by = "team") |>
    mutate(appointed = as.Date(appointed),
           tenure_years = as.numeric(as.Date(ref_date) - appointed) / 365.25,
           tenure_raw = tenure_score(appointed, ref_date))

  ## --- injuries: a current snapshot plus two measures of chronic proneness ---
  d <- d |> left_join(
    if (is.null(injuries)) tibble(team = character(), injured = integer())
    else injuries |> select(team, injured), by = "team")

  ch <- if (is.null(chronic)) {
    tibble(team = character(), rate_per1000min_2021_24 = double(),
           days_lost_2025_26 = double())
  } else {
    chronic |> select(any_of(c("team", "rate_per1000min_2021_24",
                               "days_lost_2025_26")))
  }
  d <- d |> left_join(ch, by = "team")
  for (k in c("rate_per1000min_2021_24", "days_lost_2025_26")) {
    if (!k %in% names(d)) d[[k]] <- NA_real_
  }

  ## Each measure is z-scored on its own -- a rate near 8, a day count near 400
  ## and a headcount near 5 cannot be averaged raw. The chronic score is the
  ## mean of whichever chronic measures a club has; a club with neither (a
  ## promoted side) gets NA and is carried on the snapshot alone.
  z_rate <- .z(d$rate_per1000min_2021_24)
  z_days <- .z(d$days_lost_2025_26)
  d$z_chronic <- rowMeans(cbind(z_rate, z_days), na.rm = TRUE)
  d$z_chronic[is.nan(d$z_chronic)] <- NA_real_
  d$z_current <- .z(as.numeric(d$injured))

  ## Blend chronic and current, renormalising per club over what exists.
  wc <- ifelse(is.na(d$z_chronic), 0, inj_mix[["chronic"]])
  wn <- ifelse(is.na(d$z_current), 0, inj_mix[["current"]])
  den <- wc + wn
  d$injury_z <- ifelse(den > 0,
                       (coalesce(d$z_chronic, 0) * wc +
                        coalesce(d$z_current, 0) * wn) / den, 0)

  ## --- manual overrides win ---
  man <- if (is.null(manual)) {
    tibble(team = character(), manager_stability = double(), injury_burden = double())
  } else {
    manual |> select(any_of(c("team", "manager_stability", "injury_burden")))
  }
  d <- d |> left_join(man, by = "team")
  if (!"manager_stability" %in% names(d)) d$manager_stability <- 0
  if (!"injury_burden" %in% names(d))     d$injury_burden <- 0

  d <- d |> mutate(
    z_tenure = .z(tenure_raw),
    mgr_term = ifelse(!is.na(manager_stability) & manager_stability != 0,
                      manager_stability, z_tenure),
    inj_term = ifelse(!is.na(injury_burden) & injury_burden != 0,
                      injury_burden, injury_z),
    mgr_term = coalesce(mgr_term, 0),
    inj_term = coalesce(inj_term, 0),
    adjust_raw = mix[["manager"]] * mgr_term - mix[["injury"]] * inj_term
  )

  d |> select(team, appointed, tenure_years, tenure_raw, injured,
              z_chronic, z_current, mgr_term, inj_term, adjust_raw)
}

## --- the model -------------------------------------------------------------

#' Build a predicted table.
#'
#' @param clubs      character vector of the clubs to rank
#' @param history    output of fetch_history()
#' @param xg         output of fetch_xg(), team-resolved
#' @param values     output of fetch_squad_values(), team-resolved
#' @param adjust     manual manager/injury table, or NULL
#' @param weights    component weights; renormalised over what is present
predict_table <- function(clubs, history, xg = NULL, values = NULL,
                          adjust = NULL, managers = NULL, injuries = NULL,
                          chronic = NULL, ref_date = Sys.Date(),
                          seasons = MODEL$seasons,
                          weights = MODEL$weights) {

  dom <- component_domestic(clubs, history, seasons)
  eur <- component_europe(clubs, history, seasons)
  xgc <- if (!is.null(xg)) component_xg(clubs, xg, seasons)
         else tibble(team = clubs, xgd_pg = NA_real_, xg_coverage = 0)

  val <- tibble(team = clubs) |>
    left_join(if (is.null(values)) tibble(team = character(), squad_value = double())
              else values, by = "team") |>
    mutate(log_value = ifelse(is.na(squad_value) | squad_value <= 0,
                              NA_real_, log10(squad_value)))

  adj <- component_adjust(clubs, managers = managers, injuries = injuries,
                          chronic = chronic, manual = adjust, ref_date = ref_date)

  d <- tibble(team = clubs) |>
    left_join(dom, by = "team") |> left_join(xgc, by = "team") |>
    left_join(eur, by = "team") |>
    left_join(select(val, team, squad_value, log_value), by = "team") |>
    left_join(adj, by = "team")

  ## z-scores; a component with nothing in it contributes nothing
  d <- d |> mutate(
    z_domestic = .z(domestic_ppg),
    z_xg       = .z(xgd_pg),
    z_value    = .z(log_value),
    z_europe   = .z(europe),
    z_adjust   = .z(adjust_raw) * MODEL$adjust_scale
  )

  ## Per-club weight renormalisation. A club with no xG on record (a promoted
  ## side, say) is scored on the components it does have, rather than being
  ## handed a league-average xG it never earned.
  zcols <- c(domestic = "z_domestic", xg = "z_xg", value = "z_value",
             europe = "z_europe", adjust = "z_adjust")
  have <- list(
    domestic = !is.na(d$domestic_ppg),
    xg       = !is.na(d$xgd_pg) & d$xg_coverage > 0,
    value    = !is.na(d$log_value),
    europe   = rep(TRUE, nrow(d)),   # 0 legitimately means "no European football"
    adjust   = rep(TRUE, nrow(d))
  )
  present <- vapply(have, any, logical(1))
  wall <- weights[names(present)][present]
  if (!length(wall)) stop("No usable components.", call. = FALSE)

  zmat <- vapply(names(wall), function(k) {
    v <- d[[zcols[[k]]]]; v[is.na(v)] <- 0; v
  }, numeric(nrow(d)))
  wmat <- vapply(names(wall), function(k) {
    as.numeric(have[[k]]) * wall[[k]]
  }, numeric(nrow(d)))
  denom <- rowSums(wmat)
  d$index <- rowSums(zmat * wmat) / ifelse(denom > 0, denom, 1)
  d$components_used <- apply(wmat > 0, 1, function(r) paste(names(wall)[r], collapse = "+"))

  out <- d |>
    arrange(desc(index)) |>
    mutate(position = row_number(), .before = 1)

  attr(out, "weights_used") <- wall / sum(wall)
  attr(out, "components_present") <- present
  out
}

## --- backtesting -----------------------------------------------------------

#' Predict one past season using only what was knowable before it started --
#' the three previous seasons, and the squad values on record at that season's
#' start -- then compare with what actually happened.
#'
#' Returns one row per club with predicted, actual and absolute error, plus a
#' `prev` column holding the naive "same as last season" baseline.
backtest_season <- function(target, fetch_hist, xg = NULL, fetch_values = NULL) {
  hist_window <- fetch_hist(seasons = (target - 3):(target - 1))
  actual <- fetch_hist(seasons = target) |>
    filter(league == "eng.1", season == target) |>
    transmute(team, actual = rank)
  if (!nrow(actual)) return(NULL)

  vals <- if (is.null(fetch_values)) NULL else fetch_values(target)

  pred <- predict_table(
    clubs = actual$team, history = hist_window, xg = xg,
    values = vals, adjust = NULL, seasons = (target - 3):(target - 1)
  ) |> select(team, predicted = position)

  prev <- fetch_hist(seasons = target - 1) |>
    filter(league == "eng.1", season == target - 1) |>
    transmute(team, prev_rank = rank)

  actual |>
    left_join(pred, by = "team") |>
    left_join(prev, by = "team") |>
    ## a promoted club has no previous top-flight rank; the naive baseline has
    ## to put them somewhere, and the bottom of the table is the fair guess
    mutate(prev_rank = ifelse(is.na(prev_rank), 18, prev_rank),
           prev_rank = rank(prev_rank, ties.method = "first"),
           err_model = abs(predicted - actual),
           err_prev  = abs(prev_rank - actual),
           season = target)
}

#' Run backtest_season() over several targets and summarise.
backtest_model <- function(targets, fetch_hist, xg = NULL, fetch_values = NULL) {
  rows <- lapply(targets, function(t) backtest_season(t, fetch_hist, xg, fetch_values))
  rows <- bind_rows(Filter(Negate(is.null), rows))
  if (!nrow(rows)) return(NULL)

  per_season <- rows |>
    group_by(season) |>
    summarise(
      model_mae = mean(err_model), prev_mae = mean(err_prev),
      model_exact = sum(err_model == 0), prev_exact = sum(err_prev == 0),
      .groups = "drop"
    )

  ## Paired test across every club-season: does the model beat the baseline by
  ## more than sampling noise?
  d <- rows$err_prev - rows$err_model
  tt <- tryCatch(stats::t.test(d), error = function(e) NULL)

  list(rows = rows, per_season = per_season,
       model_mae = mean(rows$err_model), prev_mae = mean(rows$err_prev),
       diff = mean(d),
       ci = if (is.null(tt)) c(NA, NA) else as.numeric(tt$conf.int),
       p = if (is.null(tt)) NA_real_ else tt$p.value,
       n = nrow(rows))
}
