# ---------------------------------------------------------------------------
# config.R -- single place to change how the competition behaves
# ---------------------------------------------------------------------------

CONFIG <- list(

  ## Competition -----------------------------------------------------------
  season_label   = "2026-27",
  espn_league    = "eng.1",           # ESPN league slug for the Premier League

  ## Lock: predictions are frozen at this instant. Stored as US/Eastern.
  ## 11:59 pm Eastern on Thursday, August 20, 2026.
  lock_time      = as.POSIXct("2026-08-20 23:59:00", tz = "America/New_York"),

  ## Scoring ---------------------------------------------------------------
  ## Which scoring rule to use. See R/scoring.R for the implementations.
  ##   "abs_diff"     sum of |predicted position - actual position|  (low = good)
  ##   "squared_diff" sum of (predicted - actual)^2                  (low = good)
  ##   "banded"       points awarded per team by how close the guess was
  ##                  (high = good) -- edit SCORING_BANDS below
  ## >>> CHANGE THIS ONE LINE when the points method is finalised. <<<
  scoring_method = "abs_diff",

  ## Used only by the "banded" method: points for being off by N places.
  ## Anything further out than the last band scores `banded_floor`.
  banded_points  = c("0" = 10, "1" = 6, "2" = 4, "3" = 2, "4" = 1),
  banded_floor   = 0,

  ## Bonus points (banded method only) for calling the champion / the
  ## relegated three exactly right. Set to 0 to switch off.
  bonus_champion   = 5,
  bonus_relegation = 5,

  ## Presentation ----------------------------------------------------------
  site_title     = "EPL Table Battle",
  timezone       = "America/New_York"
)

## Derived -----------------------------------------------------------------
## TRUE once predictions can no longer be changed.
is_locked <- function(now = Sys.time()) now >= CONFIG$lock_time

## "lower score wins" vs "higher score wins", derived from the method.
score_direction <- function(method = CONFIG$scoring_method) {
  if (method %in% c("abs_diff", "squared_diff")) "lower" else "higher"
}
