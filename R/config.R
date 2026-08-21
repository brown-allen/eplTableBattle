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

  ## Two different instants, deliberately.
  ##
  ## submission_deadline is the one entrants are held to and the only one the
  ## site ever shows them: get your code to the commissioner by here.
  ##
  ## lock_time is when the build stops accepting changes to predictions.csv.
  ## It sits later purely so the commissioner has a window to ingest the last
  ## codes, rebuild and check the result before anything is frozen. It is
  ## never displayed; it is not an extension, and entrants are not told about
  ## it because it is not theirs to use.
  submission_deadline = as.POSIXct("2026-08-20 23:59:00", tz = "America/New_York"),
  lock_time           = as.POSIXct("2026-08-21 10:00:00", tz = "America/New_York"),

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

  ## When scoring starts. Everyone shows 0 until this instant, however the
  ## league table looks. 5pm Eastern on Monday 24 August 2026.
  scoring_start  = as.POSIXct("2026-08-24 17:00", tz = "America/New_York"),

  ## When bonuses start counting. Set in UTC deliberately: 18:00 UTC on
  ## Sunday 30 May 2027, which is 14:00 Eastern the same day. That is safely
  ## after the final round (expected 23 May 2027), so bonuses are only ever
  ## computed against a finished table.
  bonus_start    = as.POSIXct("2027-05-30 18:00", tz = "UTC"),

  ## Bonus values.
  bonus_champion        = 5,  # predicted the champion exactly
  bonus_relegation_set  = 5,  # named the relegated three, any order
  bonus_ucl_each        = 1,  # per club correctly placed inside the top 4
  bonus_relegation_each = 1,  # per club correctly placed inside the bottom 3

  ## How many places count as "Champions League" and "relegation".
  ucl_places        = 4,
  relegation_places = 3,

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
## TRUE once entrants may no longer submit. This is what every page shows.
submissions_closed <- function(now = Sys.time()) now >= CONFIG$submission_deadline

## TRUE once the build itself stops accepting edits to predictions.csv.
## Used by enforce_lock() only -- never for anything an entrant reads.
is_locked <- function(now = Sys.time()) now >= CONFIG$lock_time

## TRUE once per-club points count. Before this, every total reads 0.
scoring_active <- function(now = Sys.time()) now >= CONFIG$scoring_start

## TRUE once end-of-season bonuses are added on top.
bonuses_active <- function(now = Sys.time()) now >= CONFIG$bonus_start

## "lower score wins" vs "higher score wins", derived from the method.
score_direction <- function(method = CONFIG$scoring_method) {
  if (method %in% c("abs_diff", "squared_diff")) "lower" else "higher"
}
