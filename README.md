# EPL Table Battle

A small R Markdown website for a private competition: a handful of people each
predict the exact finishing order of the 2026-27 Premier League table, their
entries are frozen at a deadline, and the site scores them against the live
table for the rest of the season.

Built with `rmarkdown::render_site()` — no Quarto, no blogdown. Rendered output
goes to `docs/`, which is what GitHub Pages serves.

## Rebuilding

```bash
Rscript build.R
```

That fetches the live table, validates every entry, enforces the freeze, and
re-renders all three pages. Run it whenever you want the standings refreshed —
after each matchweek is plenty.

Requires: `rmarkdown`, `knitr`, `kableExtra`, `dplyr`, `tidyr`, `readr`,
`jsonlite`, `htmltools`, `tibble`. All were already installed on the machine
this was built on.

## Layout

| Path | What it is |
|---|---|
| `index.Rmd` | Leaderboard, live Premier League table, all entries side by side |
| `predictions.Rmd` | One section per entrant: their sealed table vs. reality |
| `rules.Rmd` | Deadline, scoring rule, worked example, club list |
| `_setup.Rmd` | Shared setup chunk sourced by all three pages |
| `_player_section.Rmd` | Child template rendered once per entrant |
| `R/config.R` | **Every knob worth turning lives here** |
| `R/scoring.R` | The scoring rules |
| `R/fetch_table.R` | ESPN standings fetch + cache |
| `R/predictions.R` | Reading, validating and freezing entries |
| `R/teams.R` | Canonical club names and alias matching |
| `R/render_helpers.R` | Table formatting for the pages |
| `data/` | The competition's actual data (see below) |
| `docs/` | Rendered site — commit this, it is what gets published |

## Adding the real entrants

Two files, both plain CSV.

**`data/predictions.csv`** is stored wide, one column per entrant, so it opens
sensibly in Excel or Google Sheets:

```
position,allen,sam,jo,...
1,Liverpool,Arsenal,Man City,...
2,Arsenal,Liverpool,Liverpool,...
```

Column headers are short lowercase *keys*. Each column must contain all 20 clubs
exactly once. Common nicknames and abbreviations are accepted — `Man Utd`,
`Spurs`, `Forest`, `MCFC` all resolve — and anything unrecognised stops the
build with a message naming the bad value and whose column it was in.
`Rscript -e 'source("R/predictions.R"); write_prediction_template()'` writes a
blank sheet.

**`data/players.csv`** maps each key to a display name and a submission time:

```
player,display_name,submitted_at
allen,Allen Brown,2026-08-19 21:14:00
```

The repo currently ships six placeholder entrants (`player1`…`player6`) with
randomly jittered tables, purely so the site renders. Replace both files and
rebuild.

## The freeze

`CONFIG$lock_time` in `R/config.R` is set to **23:59 US/Eastern on Thursday,
20 August 2026**.

Before that instant, every build copies `data/predictions.csv` to
`data/predictions_frozen.csv`. After it, the snapshot becomes the authority: the
site renders the frozen file, and if the working file has been edited since, the
build warns loudly and ignores the change. Commit `predictions_frozen.csv` once
the deadline passes — that is the tamper-evident record.

## Changing the scoring method

The points method is deliberately pluggable, because it was not final when this
was built. Three rules ship:

| Method | Behaviour |
|---|---|
| `abs_diff` *(active)* | Sum of places between pick and reality. Lowest wins; a perfect table scores 0. |
| `squared_diff` | Same, squared. Punishes single wild misses far harder. Lowest wins. |
| `banded` | Points per club by closeness, plus champion and relegation bonuses. Highest wins. |

Switching is one line — `scoring_method` in `R/config.R`. The leaderboard,
per-entrant tables, sort direction, rules page prose and worked example all
follow automatically.

For something else entirely, add a function to `SCORERS` in `R/scoring.R`. It
receives a data frame with `team`, `predicted` and `actual` columns and returns
one score per club; add a matching sentence to `describe_method()` and, if the
rule is "highest wins", list it in `score_direction()`.

## Data source

ESPN's public standings endpoint
(`site.api.espn.com/apis/v2/sports/soccer/eng.1/standings`) — no API key, no
account. Each successful fetch is cached to `data/live_table.csv`, so a failed
fetch degrades to the last good snapshot and the page says the numbers may be
behind rather than silently showing stale data.

If ESPN ever changes shape, `fetch_live_table()` in `R/fetch_table.R` is the
only function that needs rewriting — it returns a tidy 20-row table and nothing
upstream cares where it came from.

## Publishing

The site is served from the `docs/` folder on the default branch:

```bash
git add -A && git commit -m "Update standings" && git push
```

then in the repo's **Settings → Pages**, set Source to *Deploy from a branch*,
branch `main`, folder `/docs`.

Note: this project lives inside a Box-synced folder. Box and `.git` generally
coexist, but if you ever see index corruption, move the working copy outside Box
and keep Box for the data files only.
