# ---------------------------------------------------------------------------
# ingest.R -- turn pick codes from picks.html back into predictions.csv
# ---------------------------------------------------------------------------
# The picker page emits one line per entrant:
#
#   Allen Brown:LIV,ARS,MCI,CHE,...       (20 club abbreviations, in order)
#
# The name is carried verbatim and becomes the display name; the column key is
# derived from it by squashing to lowercase letters and digits.
#
# Collect the six lines however they reach you, drop them in a text file
# (one per line, blank lines and # comments ignored), and run:
#
#   Rscript -e 'source("R/ingest.R"); ingest_codes("data/codes.txt")'
#
# That validates every code, rewrites data/predictions.csv, and adds any new
# entrant to data/players.csv. Re-running it with a corrected code for someone
# just replaces their column.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

#' Parse one "Name:ABB,ABB,..." line into a list(player, display, teams).
decode_pick_code <- function(code, teams = load_teams()) {
  code <- trimws(code)
  parts <- strsplit(code, ":", fixed = TRUE)[[1]]

  if (length(parts) != 2) {
    stop("Malformed code (expected 'Name:ABB,ABB,...'): ", sQuote(code),
         call. = FALSE)
  }

  display <- trimws(parts[1])
  player <- tolower(gsub("[^A-Za-z0-9]", "", display))
  if (!nzchar(player)) {
    stop("Code has no usable name: ", sQuote(code), call. = FALSE)
  }

  abbrevs <- trimws(strsplit(parts[2], ",", fixed = TRUE)[[1]])
  abbrevs <- abbrevs[nzchar(abbrevs)]

  # validate_full_table() gives the "wrong count / duplicate / missing club"
  # errors, and resolve_team() already accepts abbreviations.
  clubs <- validate_full_table(abbrevs, teams,
                               label = paste0("Code for '", player, "'"))

  list(player = player, display = display, teams = clubs)
}

#' Read a file of codes and rewrite the prediction + player sheets.
#'
#' @param path         text file, one code per line
#' @param predictions  wide sheet to write
#' @param players      roster to update
#' @param replace      if TRUE, entrants absent from the code file are dropped;
#'                     if FALSE (default) they are kept as they are
ingest_codes <- function(path,
                         predictions = "data/predictions.csv",
                         players = "data/players.csv",
                         teams = load_teams(),
                         replace = FALSE) {

  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (!length(lines)) stop("No codes found in ", path, call. = FALSE)

  decoded <- lapply(lines, decode_pick_code, teams = teams)
  names(decoded) <- vapply(decoded, `[[`, character(1), "player")

  dupes <- names(decoded)[duplicated(names(decoded))]
  if (length(dupes)) {
    stop("More than one code for: ", paste(unique(dupes), collapse = ", "),
         ". Keep only the entry you want to count.", call. = FALSE)
  }

  ## --- predictions ---------------------------------------------------------
  new_cols <- lapply(decoded, `[[`, "teams")
  wide <- tibble::tibble(position = seq_len(nrow(teams)))

  if (!replace && file.exists(predictions)) {
    existing <- read_csv(predictions, show_col_types = FALSE)
    keep <- setdiff(names(existing), c("position", names(new_cols)))
    for (k in keep) wide[[k]] <- existing[[k]]
  }
  for (k in names(new_cols)) wide[[k]] <- new_cols[[k]]

  write_csv(wide, predictions)

  ## --- roster --------------------------------------------------------------
  roster <- if (file.exists(players)) {
    read_csv(players, show_col_types = FALSE) |>
      mutate(submitted_at = as.character(submitted_at))
  } else {
    tibble::tibble(player = character(), display_name = character(),
                   submitted_at = character())
  }

  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  for (k in names(decoded)) {
    if (k %in% roster$player) {
      roster$submitted_at[roster$player == k] <- stamp
      roster$display_name[roster$player == k] <- decoded[[k]]$display
    } else {
      roster <- bind_rows(roster, tibble::tibble(
        player = k,
        display_name = decoded[[k]]$display,
        submitted_at = stamp
      ))
    }
  }
  if (replace) roster <- filter(roster, player %in% names(decoded))
  write_csv(roster, players)

  message("Ingested ", length(decoded), " entr",
          if (length(decoded) == 1) "y" else "ies", ": ",
          paste(vapply(decoded, `[[`, character(1), "display"), collapse = ", "))
  message("Submission times are set to now, not to when each person actually ",
          "sent their code -- edit ", players, " if that matters to you.")
  message("Then run: Rscript build.R")

  invisible(wide)
}
