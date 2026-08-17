# ---------------------------------------------------------------------------
# config.R -- single place to change how the competition behaves
# ---------------------------------------------------------------------------

CONFIG <- list(

  ## Competition -----------------------------------------------------------
  season_label   = "2026-27",

  ## Where the live table comes from. The primary source is tried first and
  ## the other is the automatic fallback, so a bad key or an outage degrades
  ## rather than breaking the build.
  ##   "football-data"  api.football-data.org v4 -- documented, needs a key
  ##   "espn"           ESPN's public endpoint  -- undocumented, no key
  data_source    = "football-data",
  fd_competition = "PL",              # football-data.org competition code
  fd_key_env     = "FOOTBALL_DATA_API_KEY",
  espn_league    = "eng.1",           # ESPN league slug for the Premier League

  ## Lock: predictions are frozen at this instant. Stored as US/Eastern.
  ## 11:59 pm Eastern on Thursday, August 20, 2026.
  lock_time      = as.POSIXct("2026-08-20 23:59:00", tz = "America/New_York"),

  ## Scoring ---------------------------------------------------------------
  ## Which scoring rule to use. See R/scoring.R for the implementations.
  ##   "proximity5"   5 points for an exact call, -1 per place out, floor 0
  ##                  (high = good) -- THE COMPETITION'S RULE
  ##   "abs_diff"     sum of |predicted position - actual position|  (low = good)
  ##   "squared_diff" sum of (predicted - actual)^2                  (low = good)
  ##   "banded"       arbitrary points per closeness band, see banded_points
  scoring_method = "proximity5",

  ## Used only by the "banded" method: points for being off by N places.
  ## Anything further out than the last band scores `banded_floor`.
  banded_points  = c("0" = 10, "1" = 6, "2" = 4, "3" = 2, "4" = 1),
  banded_floor   = 0,

  ## Bonus points for calling the champion / the relegated three exactly right.
  ## Both are 0 so that totals are the basic per-club scores and nothing else.
  ## Set them when the bonus rules are decided; the leaderboard grows a Bonus
  ## column automatically as soon as either is non-zero.
  bonus_champion   = 0,
  bonus_relegation = 0,

  ## Presentation ----------------------------------------------------------
  site_title     = "EPL Table Battle",
  timezone       = "America/New_York"
)

## Secrets ------------------------------------------------------------------
## The API key lives in .Renviron in the project root, which is gitignored and
## never read by any page. R loads that file automatically when it starts in
## this directory; this line covers the case where the project is sourced from
## somewhere else.
if (file.exists(".Renviron")) readRenviron(".Renviron")

#' The football-data.org key, or "" if none is set. Never print this.
fd_api_key <- function() Sys.getenv(CONFIG$fd_key_env, "")

has_fd_key <- function() nzchar(fd_api_key())

## Derived -----------------------------------------------------------------
## TRUE once predictions can no longer be changed.
is_locked <- function(now = Sys.time()) now >= CONFIG$lock_time

## "lower score wins" vs "higher score wins", derived from the method.
score_direction <- function(method = CONFIG$scoring_method) {
  if (method %in% c("abs_diff", "squared_diff")) "lower" else "higher"
}
