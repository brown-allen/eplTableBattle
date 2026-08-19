# ---------------------------------------------------------------------------
# manager_tenure.R -- turn a manager's appointment date into a score
# ---------------------------------------------------------------------------
# The shape asked for: a small bump for a brand-new appointment, which fades
# into a dip, after which longer tenure is steadily rewarded.
#
#   score(t) = H*exp(-t/tau_new) + S*(1 - exp(-t/tau_long)) - D
#
# The first term is the new-manager bounce, decaying over a few months. The
# second is accumulated stability, growing over a couple of years. D sets the
# baseline so the middle of the curve sits slightly below zero -- the awkward
# stretch after the bounce has worn off but before anything has been built.
#
# It is one continuous function rather than a lookup table, so there are no
# cliff edges: a manager appointed a week before another is scored a week
# differently, not a whole band differently.

tenure_score <- function(appointed, ref_date = Sys.Date(),
                         p = MODEL$tenure) {
  appointed <- as.Date(appointed)
  t <- as.numeric(as.Date(ref_date) - appointed) / 365.25   # years, may be < 0
  t <- pmax(t, 0)

  out <- p$new_boost * exp(-t / p$tau_new) +
         p$long_max  * (1 - exp(-t / p$tau_long)) -
         p$baseline
  out[is.na(appointed)] <- NA_real_
  out
}

#' Where the curve turns, and what it is worth at each end. Handy for checking
#' the parameters do what they claim.
tenure_curve_summary <- function(p = MODEL$tenure) {
  ts <- seq(0, 8, by = 0.01)
  v  <- p$new_boost * exp(-ts / p$tau_new) +
        p$long_max * (1 - exp(-ts / p$tau_long)) - p$baseline
  i <- which.min(v)
  list(
    at_zero   = v[1],
    trough_at = ts[i],
    trough    = v[i],
    at_2yr    = v[which.min(abs(ts - 2))],
    at_5yr    = v[which.min(abs(ts - 5))],
    asymptote = p$long_max - p$baseline
  )
}
