#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# build.R -- refresh the live table, validate entries, render the site
# ---------------------------------------------------------------------------
# Run from the project root:   Rscript build.R
# Output lands in docs/, which is what GitHub Pages serves.

source("R/config.R")
source("R/teams.R")
source("R/fetch_table.R")
source("R/scoring.R")
source("R/predictions.R")

message("== EPL Table Battle build ==")
message("Season:   ", CONFIG$season_label)
message("Entries due: ", format(CONFIG$submission_deadline, "%Y-%m-%d %H:%M %Z"),
        if (submissions_closed()) "  [closed to entrants]" else "  [open]")
message("Freeze at:   ", format(CONFIG$lock_time, "%Y-%m-%d %H:%M %Z"),
        if (is_locked()) "  [LOCKED - edits ignored]" else "  [build still accepts edits]")
message("Scoring:  ", CONFIG$scoring_method,
        " (", score_direction(), " wins)")
message("Source:   ", CONFIG$data_source,
        if (identical(CONFIG$data_source, "football-data")) {
          if (has_fd_key()) "  [key found]" else "  [NO KEY -- will fall back to ESPN]"
        } else "")

## 1. Validate entries (and snapshot / enforce the freeze) ------------------
teams <- load_teams()
preds <- load_active_predictions(teams = teams)
message("Entries:  ", length(unique(preds$player)),
        " (", attr(preds, "source_file"), ")")

## 2. Refresh the live table -----------------------------------------------
live <- get_live_table(refresh = TRUE)
message("Table:    ", if (isTRUE(attr(live, "stale"))) "STALE (fetch failed)" else "fresh",
        " via ", provider_label(attr(live, "provider")),
        ", ", sum(live$played), " club-matches played")

## 3. Render ----------------------------------------------------------------
rmarkdown::render_site(encoding = "UTF-8")

## GitHub Pages otherwise runs the output through Jekyll, which is unnecessary
## here and can swallow files. Dotfiles are not copied by render_site.
invisible(file.create("docs/.nojekyll"))

## 4. Refuse to ship a build containing the API key ------------------------
## The key should only ever exist in a request header at build time. This is
## a backstop against a future edit accidentally printing it into a page.
key <- fd_api_key()
if (nzchar(key)) {
  published <- list.files("docs", recursive = TRUE, full.names = TRUE)
  leaked <- Filter(function(f) {
    any(grepl(key, readLines(f, warn = FALSE), fixed = TRUE))
  }, published[!grepl("\\.(png|jpg|jpeg|gif|woff2?|ttf|eot|map)$", published)])

  if (length(leaked)) {
    unlink("docs", recursive = TRUE)
    stop("API key found in the rendered site (", paste(leaked, collapse = ", "),
         "). docs/ has been deleted rather than published. Fix the leak ",
         "before rebuilding.", call. = FALSE)
  }
  message("Secrets:  key not present anywhere in docs/ (", length(published),
          " files checked)")
}

## 5. Refuse to ship a build containing the private model ------------------
## render_site() renders every root-level .md and ignores .gitignore, so a
## file added later could be published without anyone noticing. This checks
## the output rather than trusting the config.
private_markers <- c("Transfermarkt", "worldfootballR", "squad market value",
                     "P(win)", "prediction model")
published <- list.files("docs", recursive = TRUE, full.names = TRUE)
published <- published[!grepl("\\.(png|jpg|jpeg|gif|woff2?|ttf|eot|map)$", published)]
hits <- Filter(function(f) {
  txt <- readLines(f, warn = FALSE)
  any(vapply(private_markers, function(m) any(grepl(m, txt, fixed = TRUE)), logical(1)))
}, published)
stray <- Filter(file.exists, c("MODEL.html", "docs/MODEL.html"))

if (length(hits) || length(stray)) {
  stop("Private model content found in the build: ",
       paste(c(hits, stray), collapse = ", "),
       ". Nothing has been published. Keep the source underscore-prefixed so ",
       "render_site() skips it, then delete the stray output.", call. = FALSE)
}
message("Private:  no model content in docs/ (", length(published), " files checked)")

message("Done. Open docs/index.html")
