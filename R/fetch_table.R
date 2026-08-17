# ---------------------------------------------------------------------------
# fetch_table.R -- pull the live Premier League table from ESPN's public API
# ---------------------------------------------------------------------------
# No API key required. Every successful fetch is cached to data/live_table.csv
# so the site still builds (from the last good snapshot) if ESPN is down or
# the machine is offline.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
})

ESPN_STANDINGS_URL <- function(league = CONFIG$espn_league) {
  sprintf("https://site.api.espn.com/apis/v2/sports/soccer/%s/standings", league)
}

.stat <- function(stats, name) {
  hit <- Filter(function(s) identical(s$name, name), stats)
  if (!length(hit)) return(NA_real_)
  as.numeric(hit[[1]]$value)
}

#' Fetch and tidy the current league table.
#' Returns a tibble: position, team, played, won, drawn, lost, gf, ga, gd, points.
fetch_live_table <- function(league = CONFIG$espn_league) {
  raw <- jsonlite::fromJSON(ESPN_STANDINGS_URL(league), simplifyVector = FALSE)

  season_node <- raw$children[[1]]
  entries <- season_node$standings$entries
  if (!length(entries)) stop("ESPN returned no standings entries.", call. = FALSE)

  tbl <- lapply(entries, function(e) {
    tibble::tibble(
      espn_name = e$team$displayName,
      espn_rank = .stat(e$stats, "rank"),
      played    = .stat(e$stats, "gamesPlayed"),
      won       = .stat(e$stats, "wins"),
      drawn     = .stat(e$stats, "ties"),
      lost      = .stat(e$stats, "losses"),
      gf        = .stat(e$stats, "pointsFor"),
      ga        = .stat(e$stats, "pointsAgainst"),
      gd        = .stat(e$stats, "pointDifferential"),
      points    = .stat(e$stats, "points")
    )
  }) |> bind_rows()

  tbl$team <- resolve_team(tbl$espn_name)

  ## Trust ESPN's own rank when it is a clean 1..n permutation; otherwise
  ## (e.g. pre-season, when every rank is 1) derive it from the league's
  ## own tiebreak order: points, then goal difference, then goals scored.
  ranks_ok <- !anyNA(tbl$espn_rank) &&
    setequal(sort(tbl$espn_rank), seq_len(nrow(tbl)))

  if (ranks_ok) {
    tbl <- tbl |> arrange(espn_rank) |> mutate(position = as.integer(espn_rank))
  } else {
    tbl <- tbl |>
      arrange(desc(points), desc(gd), desc(gf), team) |>
      mutate(position = row_number())
  }

  tbl |>
    transmute(
      position, team,
      played = as.integer(played), won = as.integer(won),
      drawn = as.integer(drawn), lost = as.integer(lost),
      gf = as.integer(gf), ga = as.integer(ga), gd = as.integer(gd),
      points = as.integer(points)
    )
}

## The cache carries a sidecar recording when it was last written and whether
## the most recent fetch attempt succeeded. Pages read this rather than
## guessing, so "refreshed 20 minutes ago" and "ESPN is down, this is stale"
## are told apart correctly.
.status_path <- function(cache) file.path(dirname(cache), "live_table_status.csv")

.write_status <- function(cache, ok) {
  prev <- .read_status(cache)
  fetched_at <- if (ok) format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z") else prev$fetched_at
  write_csv(
    tibble::tibble(
      fetched_at = fetched_at,
      last_attempt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      last_attempt_ok = ok
    ),
    .status_path(cache)
  )
}

.read_status <- function(cache) {
  p <- .status_path(cache)
  if (!file.exists(p)) {
    return(list(fetched_at = NA_character_, last_attempt_ok = NA))
  }
  s <- read_csv(p, show_col_types = FALSE)
  list(fetched_at = s$fetched_at[1], last_attempt_ok = isTRUE(s$last_attempt_ok[1]))
}

#' Fetch with caching. On failure, fall back to the cached snapshot and warn.
#'
#' Returns the table with attributes:
#'   `fetched_at` -- when the data itself was last successfully pulled
#'   `stale`      -- TRUE if the most recent fetch attempt failed, so the
#'                   numbers on screen are older than this build
#'
#' build.R calls this once with refresh = TRUE; the .Rmd pages call it with
#' refresh = FALSE so a three-page render is one API call, not three.
get_live_table <- function(cache = "data/live_table.csv",
                           refresh = TRUE,
                           league = CONFIG$espn_league) {

  if (refresh) {
    tbl <- tryCatch(fetch_live_table(league), error = function(e) {
      warning("Live fetch failed (", conditionMessage(e), "); using cache.",
              call. = FALSE)
      NULL
    })
    dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
    .write_status(cache, ok = !is.null(tbl))
    if (!is.null(tbl)) write_csv(tbl, cache)
  }

  if (!file.exists(cache)) {
    stop("No live data and no cache at ", cache,
         ". Run build.R while online at least once.", call. = FALSE)
  }

  tbl <- read_csv(cache, show_col_types = FALSE)
  st <- .read_status(cache)
  attr(tbl, "fetched_at") <- if (!is.na(st$fetched_at)) {
    as.POSIXct(st$fetched_at, format = "%Y-%m-%dT%H:%M:%S%z")
  } else file.info(cache)$mtime
  attr(tbl, "stale") <- isFALSE(st$last_attempt_ok)
  tbl
}
