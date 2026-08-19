# ---------------------------------------------------------------------------
# simulate.R -- turn one predicted table into a distribution of seasons
# ---------------------------------------------------------------------------
# A single predicted table is a claim no season will ever match. What the model
# actually supports is a *range*, and the width of that range is not a matter
# of taste: it is measured.
#
# Two facts pin it down, both established earlier in this project:
#
#   1. analyse_carryover.R found a full season's table is about 76% signal and
#      24% luck (split-half reliability, 22 seasons of match data). Even a
#      model that knew every club's true strength would miss by ~2.3 places.
#   2. run_model.R's backtest found this model misses by ~3.4 places against
#      real seasons it had not seen.
#
# (2) is the number to calibrate against, because it already contains (1) plus
# everything the model gets wrong. Noise is scaled so that simulated seasons
# sit as far from the point prediction as the model's real errors sit from real
# tables. Simulating tighter would be claiming an accuracy the backtest denies.

suppressPackageStartupMessages({ library(dplyr) })

#' Mean absolute position error implied by a latent correlation `r`.
.mae_at <- function(r, n = 20, sims = 3000, seed = 1) {
  set.seed(seed)
  mean(replicate(sims, {
    z <- rnorm(n)
    a <- r * z + sqrt(1 - r^2) * rnorm(n)
    mean(abs(rank(z) - rank(a)))
  }))
}

#' Find the latent correlation whose implied error matches the backtest.
#' Returns the correlation and the equivalent noise multiplier.
calibrate_noise <- function(target_mae, n = 20) {
  f <- function(r) .mae_at(r, n) - target_mae
  r <- stats::uniroot(f, interval = c(0.01, 0.999), tol = 1e-4)$root
  list(r = r,
       noise_sd = sqrt(1 - r^2) / r,     # noise per 1 SD of the index
       implied_mae = .mae_at(r, n))
}

#' Simulate `n_sims` seasons.
#'
#' The index is standardised, noise of the calibrated size is added, and the
#' result ranked. Returns a matrix of positions, clubs in rows.
simulate_seasons <- function(index, clubs, n_sims = 10000, r = NULL,
                             target_mae = NULL, seed = 20262027) {
  stopifnot(length(index) == length(clubs))
  if (is.null(r)) {
    if (is.null(target_mae)) stop("Give either r or target_mae.", call. = FALSE)
    r <- calibrate_noise(target_mae, length(clubs))$r
  }
  z <- as.numeric(scale(index))
  noise_sd <- sqrt(1 - r^2) / r

  set.seed(seed)
  out <- matrix(NA_integer_, nrow = length(clubs), ncol = n_sims,
                dimnames = list(clubs, NULL))
  for (i in seq_len(n_sims)) {
    out[, i] <- rank(-(z + rnorm(length(z), 0, noise_sd)), ties.method = "random")
  }
  attr(out, "r") <- r
  attr(out, "noise_sd") <- noise_sd
  out
}

#' How many simulations are enough? The Monte Carlo standard error of a
#' probability p from N draws is sqrt(p(1-p)/N), worst at p = 0.5.
mc_error <- function(n_sims, p = 0.5) sqrt(p * (1 - p) / n_sims)

#' Per-club outcome probabilities.
summarise_sims <- function(sims, ucl = 4, releg = 3) {
  n <- ncol(sims); n_clubs <- nrow(sims)
  tibble(
    team      = rownames(sims),
    mean_pos  = rowMeans(sims),
    median_pos = apply(sims, 1, median),
    p05       = apply(sims, 1, quantile, 0.05),
    p95       = apply(sims, 1, quantile, 0.95),
    p_title   = rowMeans(sims == 1),
    p_top4    = rowMeans(sims <= ucl),
    p_releg   = rowMeans(sims > n_clubs - releg)
  ) |> arrange(mean_pos)
}

#' Pick representative simulated seasons.
#'
#' Every simulation is a real, internally consistent season. They are ordered by
#' how far they stray from the point prediction, and one is taken at each
#' requested percentile -- so the three tables span the range honestly instead
#' of being the three most entertaining ones.
pick_representative <- function(sims, base_pos, probs = c(0.15, 0.50, 0.90)) {
  dev <- colMeans(abs(sims - base_pos))
  idx <- vapply(probs, function(p) {
    target <- quantile(dev, p, names = FALSE)
    which.min(abs(dev - target))
  }, integer(1))
  list(index = idx, deviation = dev[idx], all_dev = dev)
}
