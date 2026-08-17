# ---------------------------------------------------------------------------
# teams.R -- canonical club names and fuzzy-ish name resolution
# ---------------------------------------------------------------------------
# Everything downstream (predictions, live table, scoring) is keyed on the
# canonical `team` column in data/teams.csv. Participants can write "Man Utd"
# or "Spurs" in their prediction and resolve_team() maps it to the canonical
# name, so a typo becomes a loud error instead of a silently wrong score.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

load_teams <- function(path = "data/teams.csv") {
  read_csv(path, show_col_types = FALSE)
}

## Normalise a string for matching: lowercase, strip punctuation/accents/spaces.
.norm <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "", x)
  x
}

#' Build a lookup from any accepted spelling -> canonical team name.
team_lookup <- function(teams = load_teams()) {
  out <- list()
  for (i in seq_len(nrow(teams))) {
    canonical <- teams$team[i]
    variants <- c(
      teams$team[i], teams$short_name[i], teams$abbrev[i],
      strsplit(teams$aliases[i], "|", fixed = TRUE)[[1]]
    )
    for (v in unique(.norm(variants))) out[[v]] <- canonical
  }
  out
}

#' Resolve a vector of user-written club names to canonical names.
#' Unmatched entries become NA; `strict = TRUE` turns that into an error
#' naming the offending values, which is what the build script wants.
resolve_team <- function(x, teams = load_teams(), strict = TRUE) {
  lk <- team_lookup(teams)
  key <- .norm(x)
  out <- unname(vapply(key, function(k) {
    if (!is.null(lk[[k]])) lk[[k]] else NA_character_
  }, character(1)))

  if (strict && anyNA(out)) {
    bad <- unique(x[is.na(out)])
    stop(
      "Unrecognised club name(s): ", paste(sQuote(bad), collapse = ", "),
      "\nAdd a spelling to the `aliases` column of data/teams.csv, ",
      "or fix the prediction file.",
      call. = FALSE
    )
  }
  out
}

#' Check that a set of 20 names is exactly the league's 20 clubs, once each.
validate_full_table <- function(x, teams = load_teams(), label = "table") {
  canon <- resolve_team(x, teams)
  missing   <- setdiff(teams$team, canon)
  duplicated_teams <- canon[duplicated(canon)]

  if (length(canon) != nrow(teams)) {
    stop(label, ": expected ", nrow(teams), " clubs, got ", length(canon), call. = FALSE)
  }
  if (length(duplicated_teams)) {
    stop(label, ": club(s) listed more than once: ",
         paste(unique(duplicated_teams), collapse = ", "), call. = FALSE)
  }
  if (length(missing)) {
    stop(label, ": club(s) missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  canon
}
