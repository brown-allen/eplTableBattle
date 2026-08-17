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
message("Deadline: ", format(CONFIG$lock_time, "%Y-%m-%d %H:%M %Z"),
        if (is_locked()) "  [LOCKED]" else "  [open]")
message("Scoring:  ", CONFIG$scoring_method,
        " (", score_direction(), " wins)")

## 1. Validate entries (and snapshot / enforce the freeze) ------------------
teams <- load_teams()
preds <- load_active_predictions(teams = teams)
message("Entries:  ", length(unique(preds$player)),
        " (", attr(preds, "source_file"), ")")

## 2. Refresh the live table -----------------------------------------------
live <- get_live_table(refresh = TRUE)
message("Table:    ", if (isTRUE(attr(live, "stale"))) "STALE (fetch failed)" else "fresh",
        ", ", sum(live$played), " club-matches played")

## 3. Render ----------------------------------------------------------------
rmarkdown::render_site(encoding = "UTF-8")

## GitHub Pages otherwise runs the output through Jekyll, which is unnecessary
## here and can swallow files. Dotfiles are not copied by render_site.
invisible(file.create("docs/.nojekyll"))

message("Done. Open docs/index.html")
