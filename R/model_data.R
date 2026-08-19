# ---------------------------------------------------------------------------
# model_data.R -- assemble the inputs the prediction model needs
# ---------------------------------------------------------------------------
# Everything here is fetched once and cached under data/model/. Re-run with
# refresh = TRUE to pull fresh copies.
#
# Sources, and what each is actually good for:
#   ESPN            final league tables (PL, Championship, UCL, UEL) by season
#   worldfootballR  FBref match-level xG, read from that project's pre-scraped
#                   GitHub data repo rather than FBref directly (FBref blocks
#                   automated requests)
#   Transfermarkt   current total squad market value
#
# Manager stability and injury burden have no free machine-readable source, so
# they come from data/model/adjustments.csv, which you fill in by hand.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(jsonlite); library(rvest)
})

MODEL_DIR <- "data/model"

.cache <- function(name, expr, refresh = FALSE) {
  path <- file.path(MODEL_DIR, paste0(name, ".csv"))
  if (!refresh && file.exists(path)) return(read_csv(path, show_col_types = FALSE))
  val <- force(expr)
  dir.create(MODEL_DIR, showWarnings = FALSE, recursive = TRUE)
  write_csv(val, path)
  val
}

## --- ESPN league tables ----------------------------------------------------

.espn_stat <- function(stats, name) {
  hit <- Filter(function(s) identical(s$name, name), stats)
  if (!length(hit)) return(NA_real_) else as.numeric(hit[[1]]$value)
}

#' Final table for one league-season. `season` is the starting year:
#' 2025 means the 2025-26 season.
fetch_espn_table <- function(league, season) {
  url <- sprintf(
    "https://site.api.espn.com/apis/v2/sports/soccer/%s/standings?season=%d",
    league, season)
  raw <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  entries <- raw$children[[1]]$standings$entries
  if (!length(entries)) return(NULL)

  lapply(entries, function(e) {
    tibble::tibble(
      league = league, season = season,
      raw_name = e$team$displayName,
      rank   = .espn_stat(e$stats, "rank"),
      played = .espn_stat(e$stats, "gamesPlayed"),
      points = .espn_stat(e$stats, "points"),
      gf     = .espn_stat(e$stats, "pointsFor"),
      ga     = .espn_stat(e$stats, "pointsAgainst")
    )
  }) |> bind_rows()
}

#' All league-seasons the model needs, in one long table.
fetch_history <- function(seasons = MODEL$seasons, refresh = FALSE) {
  ## The cache key carries the season range: the backtest asks for a different
  ## window than the prediction does, and they must not overwrite each other.
  key <- sprintf("history_%d_%d", min(seasons), max(seasons))
  .cache(key, refresh = refresh, expr = {
    jobs <- expand.grid(
      league = c("eng.1", "eng.2", "uefa.champions", "uefa.europa"),
      season = seasons, stringsAsFactors = FALSE)
    out <- lapply(seq_len(nrow(jobs)), function(i) {
      message("  fetching ", jobs$league[i], " ", jobs$season[i])
      tryCatch(fetch_espn_table(jobs$league[i], jobs$season[i]),
               error = function(e) { warning(jobs$league[i], " ", jobs$season[i],
                                             ": ", conditionMessage(e), call. = FALSE); NULL })
    })
    bind_rows(out) |>
      mutate(ppg = ifelse(played > 0, points / played, NA_real_))
  })
}

## --- xG (FBref match data via the worldfootballR data repo) ----------------

#' Team-season xG for and against, plus how much of the season is covered.
#' `season_end` is the ending year: 2026 means the 2025-26 season.
fetch_xg <- function(season_ends = MODEL$seasons + 1, refresh = FALSE) {
  .cache("xg", refresh = refresh, expr = {
    if (!requireNamespace("worldfootballR", quietly = TRUE)) {
      warning("worldfootballR not installed; the model will run without xG.",
              call. = FALSE)
      return(tibble::tibble(season_end = integer(), raw_name = character(),
                            matches = integer(), xg = double(), xga = double()))
    }
    m <- worldfootballR::load_match_results(
      country = "ENG", gender = "M", season_end_year = season_ends, tier = "1st")
    m <- m |> filter(!is.na(Home_xG), !is.na(Away_xG))
    if (!nrow(m)) return(tibble::tibble(season_end = integer(), raw_name = character(),
                                        matches = integer(), xg = double(), xga = double()))

    home <- m |> transmute(season_end = Season_End_Year, raw_name = Home,
                           xg = Home_xG, xga = Away_xG)
    away <- m |> transmute(season_end = Season_End_Year, raw_name = Away,
                           xg = Away_xG, xga = Home_xG)
    bind_rows(home, away) |>
      group_by(season_end, raw_name) |>
      summarise(matches = n(), xg = sum(xg), xga = sum(xga), .groups = "drop")
  })
}

## --- Transfermarkt squad values --------------------------------------------

.parse_money <- function(x) {
  x <- gsub("[€ \\s]", "", x, perl = TRUE)
  mult <- ifelse(grepl("bn$", x), 1e9, ifelse(grepl("m$", x), 1e6,
          ifelse(grepl("k$", x), 1e3, 1)))
  as.numeric(gsub("[a-zA-Z]", "", x)) * mult
}

#' Total squad market value per club. `season` is the starting year; omit it
#' for the current season. Historical values are what was on record at the
#' start of that season, so using them in a backtest leaks nothing.
fetch_squad_values <- function(season = NULL, refresh = FALSE) {
  key <- if (is.null(season)) "squad_values" else sprintf("squad_values_%d", season)
  url <- if (is.null(season)) {
    "https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1"
  } else {
    sprintf(paste0("https://www.transfermarkt.com/premier-league/startseite/",
                   "wettbewerb/GB1/plus/?saison_id=%d"), season)
  }

  .cache(key, refresh = refresh, expr = {
    pg <- tryCatch(
      rvest::read_html(httr::GET(url, httr::user_agent(MODEL$user_agent),
                                 httr::timeout(30))),
      error = function(e) { warning("Transfermarkt fetch failed: ",
                                    conditionMessage(e), call. = FALSE); NULL })
    if (is.null(pg)) {
      return(tibble::tibble(raw_name = character(), squad_value = double()))
    }
    tbl <- pg |> rvest::html_element("table.items") |> rvest::html_table()
    names(tbl) <- make.unique(names(tbl))

    ## Club name is the second column; total squad value is the last
    ## money-shaped column on the row.
    club_col <- names(tbl)[2]
    money <- vapply(tbl, function(c) mean(grepl("\u20ac", as.character(c))) > 0.8,
                    logical(1))
    val_col <- tail(names(tbl)[money], 1)

    tibble::tibble(
      raw_name = trimws(tbl[[club_col]]),
      squad_value = .parse_money(as.character(tbl[[val_col]]))
    ) |> filter(nzchar(raw_name), !is.na(squad_value))
  })
}
