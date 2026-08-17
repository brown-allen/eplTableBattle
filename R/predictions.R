# ---------------------------------------------------------------------------
# predictions.R -- read, validate and lock the entrants' tables
# ---------------------------------------------------------------------------
# data/predictions.csv is stored WIDE, which is the shape people can actually
# edit in Excel or Google Sheets:
#
#   position,allen,sam,...
#   1,Liverpool,Arsenal,...
#   2,Arsenal,Liverpool,...
#
# Column names are the player *keys*; data/players.csv maps each key to a
# display name and the timestamp their entry was submitted.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

load_players <- function(path = "data/players.csv") {
  read_csv(path, show_col_types = FALSE) |>
    mutate(submitted_at = as.POSIXct(submitted_at, tz = CONFIG$timezone))
}

#' Read the wide prediction sheet and return it long + validated.
#' Columns: player, position, team.
load_predictions <- function(path = "data/predictions.csv",
                             teams = load_teams()) {
  wide <- read_csv(path, show_col_types = FALSE)

  if (!"position" %in% names(wide)) {
    stop("data/predictions.csv must have a `position` column.", call. = FALSE)
  }
  if (!setequal(wide$position, seq_len(nrow(teams)))) {
    stop("data/predictions.csv must have exactly positions 1..", nrow(teams),
         ", once each.", call. = FALSE)
  }

  long <- wide |>
    pivot_longer(-position, names_to = "player", values_to = "team_raw") |>
    arrange(player, position)

  ## Validate each entrant's table in isolation so the error names the culprit.
  long$team <- NA_character_
  for (p in unique(long$player)) {
    idx <- long$player == p
    long$team[idx] <- validate_full_table(
      long$team_raw[idx], teams, label = paste0("Prediction for '", p, "'")
    )
  }

  long |> select(player, position, team)
}

#' Enforce the freeze. Once CONFIG$lock_time has passed, predictions.csv must
#' not change; this compares it against the snapshot taken at lock time and
#' stops the build if they diverge.
#'
#' Before the deadline, every build refreshes the snapshot.
#' After it, the snapshot is the authority and is what the site renders.
enforce_lock <- function(path = "data/predictions.csv",
                         frozen = "data/predictions_frozen.csv",
                         now = Sys.time()) {

  if (!is_locked(now)) {
    file.copy(path, frozen, overwrite = TRUE)
    return(invisible(list(locked = FALSE, drift = FALSE)))
  }

  if (!file.exists(frozen)) {
    warning("Deadline has passed but no frozen snapshot exists; freezing the ",
            "current predictions.csv now.", call. = FALSE)
    file.copy(path, frozen, overwrite = TRUE)
    return(invisible(list(locked = TRUE, drift = FALSE)))
  }

  drift <- !identical(
    readr::read_file(path), readr::read_file(frozen)
  )
  if (drift) {
    warning("predictions.csv has changed since the ",
            format(CONFIG$lock_time, "%b %d %Y %H:%M %Z"), " deadline. ",
            "The site is rendering the FROZEN snapshot ",
            "(data/predictions_frozen.csv); the edit is ignored.",
            call. = FALSE)
  }
  invisible(list(locked = TRUE, drift = drift))
}

#' The predictions the site should actually display: the frozen snapshot once
#' the deadline has passed, otherwise the working file.
load_active_predictions <- function(path = "data/predictions.csv",
                                    frozen = "data/predictions_frozen.csv",
                                    teams = load_teams(),
                                    now = Sys.time()) {
  status <- enforce_lock(path, frozen, now)
  src <- if (isTRUE(status$locked) && file.exists(frozen)) frozen else path
  preds <- load_predictions(src, teams)
  attr(preds, "locked") <- isTRUE(status$locked)
  attr(preds, "source_file") <- src
  preds
}

#' Write a blank sheet an entrant can fill in.
write_prediction_template <- function(path = "data/predictions_template.csv",
                                      teams = load_teams()) {
  tibble::tibble(
    position = seq_len(nrow(teams)),
    your_name = NA_character_
  ) |> write_csv(path, na = "")
  message("Template written to ", path,
          " -- clubs available:\n  ", paste(teams$team, collapse = ", "))
  invisible(path)
}
