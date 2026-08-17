# ---------------------------------------------------------------------------
# fetch_table.R -- pull the live Premier League table
# ---------------------------------------------------------------------------
# Two sources, both returning the same tidy 20-row table:
#
#   football-data.org  documented v4 API, needs a free key in .Renviron
#   ESPN               undocumented public endpoint, no key
#
# CONFIG$data_source picks the primary; the other is the automatic fallback.
# Every successful fetch is cached to data/live_table.csv, so the site still
# builds from the last good snapshot if both sources fail or you are offline.
#
# The key is used ONLY here, at build time, in a request header. It never
# reaches the rendered pages -- they read the cached CSV, which holds nothing
# but football. build.R re-checks that before finishing.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(httr)
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

  ## Pre-season ESPN reports every rank as 1, which .finalise_table detects
  ## and replaces with the league's own tiebreak order.
  .finalise_table(tbl, tbl$espn_rank)
}

## --- football-data.org v4 --------------------------------------------------

FD_STANDINGS_URL <- function(comp = CONFIG$fd_competition) {
  sprintf("https://api.football-data.org/v4/competitions/%s/standings", comp)
}

#' Fetch and tidy the current table from football-data.org.
#' Requires CONFIG$fd_key_env to be set; see README for .Renviron setup.
fetch_live_table_fd <- function(comp = CONFIG$fd_competition) {
  key <- fd_api_key()
  if (!nzchar(key)) {
    stop("No football-data.org key found in $", CONFIG$fd_key_env,
         ". Add it to .Renviron (see README) or set CONFIG$data_source ",
         "to \"espn\".", call. = FALSE)
  }

  resp <- httr::GET(
    FD_STANDINGS_URL(comp),
    httr::add_headers(`X-Auth-Token` = key),
    httr::timeout(30)
  )

  ## Report status without ever echoing the key.
  if (httr::status_code(resp) %in% c(400, 401)) {
    stop("football-data.org did not accept the token (HTTP ",
         httr::status_code(resp), "). It is usually a malformed key -- check ",
         "for stray quotes, spaces or a trailing newline in .Renviron.",
         call. = FALSE)
  }
  if (httr::status_code(resp) == 403) {
    stop("football-data.org rejected the key (403). Check it is correct and ",
         "that your plan covers competition ", comp, ".", call. = FALSE)
  }
  if (httr::status_code(resp) == 429) {
    stop("football-data.org rate limit hit (429). The free tier allows 10 ",
         "requests a minute; wait a moment and rebuild.", call. = FALSE)
  }
  if (httr::http_error(resp)) {
    stop("football-data.org returned HTTP ", httr::status_code(resp), ".",
         call. = FALSE)
  }

  raw <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  ## A LEAGUE competition returns TOTAL, HOME and AWAY standings; we want the
  ## overall one.
  total <- Filter(function(s) identical(s$type, "TOTAL"), raw$standings)
  if (!length(total)) {
    stop("football-data.org returned no TOTAL standings for ", comp, ".",
         call. = FALSE)
  }
  rows <- total[[1]]$table
  if (!length(rows)) stop("football-data.org returned an empty table.", call. = FALSE)

  tbl <- lapply(rows, function(r) {
    tibble::tibble(
      src_position = as.integer(r$position),
      src_name     = r$team$name %||% NA_character_,
      src_short    = r$team$shortName %||% NA_character_,
      played       = as.integer(r$playedGames),
      won          = as.integer(r$won),
      drawn        = as.integer(r$draw),
      lost         = as.integer(r$lost),
      gf           = as.integer(r$goalsFor),
      ga           = as.integer(r$goalsAgainst),
      gd           = as.integer(r$goalDifference),
      points       = as.integer(r$points)
    )
  }) |> bind_rows()

  ## Club names arrive as "Liverpool FC", "Hull City AFC" and so on; the
  ## resolver strips those suffixes. shortName covers anything it cannot.
  tbl$team <- resolve_team(tbl$src_name, strict = FALSE)
  if (anyNA(tbl$team)) {
    idx <- is.na(tbl$team)
    tbl$team[idx] <- resolve_team(tbl$src_short[idx], strict = TRUE)
  }

  .finalise_table(tbl, tbl$src_position)
}

## --- shared ----------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Apply the source's ranking when it is a clean 1..n permutation, otherwise
#' derive it from the league's own tiebreaks, then return the canonical shape.
.finalise_table <- function(tbl, src_rank) {
  tbl$.src_rank <- src_rank
  ranks_ok <- !anyNA(src_rank) && setequal(sort(src_rank), seq_len(nrow(tbl)))

  tbl <- if (ranks_ok) {
    tbl |> arrange(.src_rank) |> mutate(position = as.integer(.src_rank))
  } else {
    tbl |>
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

#' Try the configured source, then the other one.
fetch_any_source <- function(primary = CONFIG$data_source) {
  order <- if (identical(primary, "espn")) c("espn", "football-data")
           else c("football-data", "espn")

  errs <- character(0)
  for (src in order) {
    tbl <- tryCatch({
      t <- if (src == "espn") fetch_live_table() else fetch_live_table_fd()
      attr(t, "provider") <- src
      t
    }, error = function(e) {
      errs[[src]] <<- conditionMessage(e)
      NULL
    })
    if (!is.null(tbl)) {
      if (length(errs)) {
        warning("Primary source failed (", names(errs)[1], ": ", errs[[1]],
                "); used ", src, " instead.", call. = FALSE)
      }
      return(tbl)
    }
  }
  stop("All sources failed.\n",
       paste0("  ", names(errs), ": ", unlist(errs), collapse = "\n"),
       call. = FALSE)
}

## The cache carries a sidecar recording when it was last written and whether
## the most recent fetch attempt succeeded. Pages read this rather than
## guessing, so "refreshed 20 minutes ago" and "ESPN is down, this is stale"
## are told apart correctly.
.status_path <- function(cache) file.path(dirname(cache), "live_table_status.csv")

.write_status <- function(cache, ok, provider = NA_character_) {
  prev <- .read_status(cache)
  fetched_at <- if (ok) format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z") else prev$fetched_at
  write_csv(
    tibble::tibble(
      fetched_at = fetched_at,
      last_attempt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      last_attempt_ok = ok,
      provider = if (ok) provider else prev$provider
    ),
    .status_path(cache)
  )
}

.read_status <- function(cache) {
  p <- .status_path(cache)
  if (!file.exists(p)) {
    return(list(fetched_at = NA_character_, last_attempt_ok = NA,
                provider = NA_character_))
  }
  s <- read_csv(p, show_col_types = FALSE)
  list(fetched_at = s$fetched_at[1],
       last_attempt_ok = isTRUE(s$last_attempt_ok[1]),
       provider = if ("provider" %in% names(s)) s$provider[1] else NA_character_)
}

#' Human-readable name of a source, for the site's attribution line.
provider_label <- function(p) {
  switch(as.character(p),
         "football-data" = "football-data.org",
         "espn" = "ESPN",
         "the data provider")
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
                           primary = CONFIG$data_source) {

  if (refresh) {
    tbl <- tryCatch(fetch_any_source(primary), error = function(e) {
      warning("Live fetch failed (", conditionMessage(e), "); using cache.",
              call. = FALSE)
      NULL
    })
    dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
    .write_status(cache, ok = !is.null(tbl),
                  provider = if (is.null(tbl)) NA_character_
                             else attr(tbl, "provider"))
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
  attr(tbl, "provider") <- st$provider
  tbl
}
