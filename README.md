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
| `picks.Rmd` | Drag-to-reorder picker entrants use to build their table |
| `rules.Rmd` | Deadline, scoring rule, worked example, club list |
| `_setup.Rmd` | Shared setup chunk sourced by all three pages |
| `_player_section.Rmd` | Child template rendered once per entrant |
| `R/config.R` | **Every knob worth turning lives here** |
| `R/ingest.R` | Turning pick codes back into `predictions.csv` |
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

## Collecting entries with the picker

`picks.Rmd` builds a page where an entrant drags the 20 clubs into order (or
nudges them with arrows, which is what actually works on a phone). Their draft
saves to their own browser as they go, so they can close the tab and come back.
Hitting **Copy my code** puts one line on their clipboard:

```
Allen Brown:TOT,MAN,COV,AVL,SUN,BOU,HUL,BHA,IPS,CHE,...
```

GitHub Pages is static hosting — there is no server, so the page cannot submit
anything anywhere. Entrants send you that line however you already talk to them.

Collect them in **`data/codes.txt`**, one code per line. Blank lines and lines
starting with `#` are ignored, so you can annotate as you go:

```
# received by text, Tue evening
Allen Brown:LIV,ARS,MCI,CHE,NEW,AVL,TOT,MAN,BHA,NFO,CRY,EVE,BRE,FUL,BOU,SUN,LEE,IPS,HUL,COV
Sam O'Neill:ARS,LIV,CHE,MCI,TOT,NEW,MAN,AVL,BHA,CRY,NFO,BRE,EVE,FUL,LEE,BOU,SUN,IPS,COV,HUL
```

That path is the default, so ingesting takes no argument:

```bash
Rscript -e 'source("R/ingest.R"); ingest_codes()'
```

`data/codes.txt` is gitignored on purpose: pushing it before Thursday would
publish people's entries early. `predictions.csv` and `predictions_frozen.csv`
are the records that count, and those are committed.

That validates every code, rewrites `data/predictions.csv`, and updates
`data/players.csv` — the name before the colon is carried through verbatim, so
`Sam O'Neill` stays `Sam O'Neill` rather than becoming a squashed key. Re-running
it with a corrected code for one person replaces just that column and leaves
everyone else alone. Bad codes fail loudly and name the problem: wrong number of
clubs, a club listed twice, an unrecognised abbreviation.

Submission times are stamped at ingest, not at the moment someone sent their
code; edit `players.csv` if you want them accurate.

If you later want entries to arrive without you copying anything, the picker
already emits the right format — point a Google Form at it and have `build.R`
read the published sheet CSV.

The picker greys itself out after the deadline, but that check uses the
*entrant's* device clock. Real enforcement stays in `build.R`, which uses yours.

## The freeze

`CONFIG$lock_time` in `R/config.R` is set to **23:59 US/Eastern on Thursday,
20 August 2026**.

Before that instant, every build copies `data/predictions.csv` to
`data/predictions_frozen.csv`. After it, the snapshot becomes the authority: the
site renders the frozen file, and if the working file has been edited since, the
build warns loudly and ignores the change. Commit `predictions_frozen.csv` once
the deadline passes — that is the tamper-evident record.

## Changing the scoring method

The points method is pluggable. Four rules ship:

| Method | Behaviour |
|---|---|
| `proximity5` *(active)* | 5 points for an exact call, one less per place out, floor 0. Highest wins; a perfect table scores 100. |
| `abs_diff` | Sum of places between pick and reality. Lowest wins; a perfect table scores 0. |
| `squared_diff` | Same, squared. Punishes single wild misses far harder. Lowest wins. |
| `banded` | Arbitrary points per closeness band, see `banded_points`. Highest wins. |

On top of the per-club points, four end-of-season bonuses apply from
`CONFIG$bonus_start`: +5 for the champion, +5 for the relegated three in any
order, +1 per club correctly placed inside the top four, and +1 per club
correctly placed inside the bottom three. Maximum bonus +17, so the ceiling is
117. Totals read 0 until `CONFIG$scoring_start`.

Switching is one line — `scoring_method` in `R/config.R`. The leaderboard,
per-entrant tables, sort direction, rules page prose and worked example all
follow automatically.

For something else entirely, add a function to `SCORERS` in `R/scoring.R`. It
receives a data frame with `team`, `predicted` and `actual` columns and returns
one score per club; add a matching sentence to `describe_method()` and, if the
rule is "highest wins", list it in `score_direction()`.

## Data source

Two providers, both reduced to the same tidy 20-row table:

| Source | Endpoint | Key |
|---|---|---|
| **football-data.org** *(primary)* | `api.football-data.org/v4/competitions/PL/standings` | yes |
| ESPN *(fallback)* | `site.api.espn.com/apis/v2/sports/soccer/eng.1/standings` | no |

`CONFIG$data_source` picks the primary; the other is tried automatically if the
first fails, so a missing key, a 403, a rate limit or an outage degrades instead
of breaking the build. If both fail, the cached `data/live_table.csv` is used and
the front page says the numbers may be behind. Whichever source actually
supplied the data is named in the attribution line.

### The API key

Get a free key at <https://www.football-data.org/client/register> (10 requests a
minute, Premier League included — far more than a once-a-matchweek rebuild
needs). Then:

```bash
cp .Renviron.example .Renviron
```

and paste the key after the `=`, no quotes, no spaces, no trailing blank line:

```
FOOTBALL_DATA_API_KEY=your_actual_key
```

R reads `.Renviron` automatically when it starts in this directory, so
`Rscript build.R` picks it up with no further setup. Restart any R session that
was already open.

**How the key stays private.** This matters because `docs/` is published to a
public repo:

- `.Renviron` is gitignored (`.Renviron.example` is the committed placeholder).
- The key is used **only at build time**, on your machine, in an `X-Auth-Token`
  request header. The published pages never call the API — they read
  `data/live_table.csv`, which contains nothing but football.
- `build.R` greps every rendered file for the key before finishing. If it ever
  appears, the build **deletes `docs/` and stops** rather than leaving something
  publishable on disk. Verified by deliberately triggering it.
- Error messages report HTTP status codes and never echo the token.

The one thing the tooling cannot check is your own shell history — set the key by
editing `.Renviron`, not by running `export FOOTBALL_DATA_API_KEY=...`.

If either provider changes shape, `fetch_live_table_fd()` and
`fetch_live_table()` in `R/fetch_table.R` are the only functions that need
touching; both hand back the same tidy table and nothing upstream cares which
one ran.

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

---

# The prediction model

A separate, optional piece: `run_model.R` builds a predicted 2026-27 table from
historical results, xG, squad values and European form. It is independent of the
competition itself — nothing on the site depends on it.

```bash
Rscript run_model.R            # cached inputs
Rscript run_model.R --refresh  # re-fetch everything
Rscript tune_model.R           # grid-search the weights
```

Outputs land in `data/model/`: `prediction.csv` (readable table),
`prediction_full.csv` (every component), `backtest.csv`, and
`prediction_code.txt` — a pick code in the picker's own format, so the model's
table can be entered as an entry or diffed against one.

## How it works

Each component becomes a z-score across the twenty clubs; the z-scores are
combined with `MODEL$weights` and the clubs ranked by the result. That keeps
components comparable regardless of their units. It is a weighted composite,
**not a fitted statistical model** — no coefficients are estimated from data.

| Component | Weight | Source |
|---|---|---|
| Domestic results, 3 seasons, recency-weighted | 38% | ESPN |
| xG difference per game, recency-weighted | 22% | FBref via worldfootballR |
| Current squad market value (log) | 25% | Transfermarkt |
| Champions/Europa League league-phase finish | 10% | ESPN |
| Manager tenure + injury burden | 5% | Premier League site, Premier Injuries, BBC, Sports Gazette |

Weights are renormalised per club over the components that club actually has, so
a promoted side with no xG record is scored on what it has rather than being
handed a league-average xG it never earned. Championship seasons are discounted
to `MODEL$championship_discount` of face value.

## How well does it work?

Badly enough to be worth saying plainly. Backtested over four seasons
(2022-23 to 2025-26), each predicted using only the three seasons before it plus
the squad values on record at that season's start:

| | Mean error |
|---|---|
| Model | 3.40 places |
| **Reusing last season's table** | **3.45 places** |
| Random ordering | 7.00 places |

The model beats the naive baseline by 0.05 places, over 80 club-seasons, with a
95% confidence interval of −0.48 to +0.58 and p = 0.85. That is no difference at
all.

`tune_model.R` grid-searches 192 weight combinations scored by leave-one-season-out
cross-validation. The best in-sample weights reach 3.20 places, but held out they
give 3.48 — slightly *worse* than the baseline. The apparent gain is the search
fitting noise in four seasons.

So the honest summary: **this model is about as accurate as writing down last
season's table**, and both are much better than guessing. That is not a bug in
the implementation; last season's table is a famously strong baseline, and a
season is only 380 matches of a high-variance sport.

What the model does add over the naive baseline is a principled way to place
promoted clubs, which last season's table cannot rank at all, and a defensible
ordering with the reasoning attached rather than a hunch.

## Manager tenure and injuries

Both are now computed rather than typed, from `data/model/managers.csv`,
`data/model/injuries.csv` and `data/model/injuries_chronic.csv`. Each file has a
`_SOURCE.txt` beside it recording where the numbers came from and when.

### The tenure curve

`tenure_score()` in `R/manager_tenure.R` maps an appointment date to a score:

```
score(t) = new_boost * exp(-t/tau_new) + long_max * (1 - exp(-t/tau_long)) - baseline
```

The first term is the new-manager bounce, the second is accumulated stability,
and `baseline` pulls the middle below zero. With the shipped parameters:

| Tenure | Score |
|---|---|
| day one | **+0.12** — the new-appointment bump |
| 3 months | −0.03 |
| **6 months** | **−0.06** — the trough |
| 1 year | +0.01 |
| 2 years | +0.16 |
| 5 years | +0.36 |
| long run | +0.42 |

One continuous function rather than banded categories, so a manager appointed a
week before another scores a week differently, not a whole band differently.
Tune it via `MODEL$tenure` in `R/model_config.R`.

### Injuries

Three measures, each z-scored separately before being combined — a rate near 8,
a day-count near 400 and a headcount near 5 cannot be averaged raw:

| Measure | Source | Coverage |
|---|---|---|
| Injuries per 1000 min, 2021-24 | Sports Gazette / Premier Injuries | 12 of the 20 |
| Days lost, 2025-26 to 22 rounds | BBC Sport / Premier Injuries | 17 of the 20 |
| Absences right now | premierinjuries.com, 18 Aug 2026 | all 20 |

The first two form a *chronic* score, blended with the *current* snapshot via
`MODEL$injury_mix`. A club with no chronic record — the three promoted sides —
is carried on the snapshot alone rather than being assigned a league-average
proneness it has not earned.

### What it is worth

Measured, not assumed: `run_model.R` re-ranks with the component switched off
and reports the difference. As it stands the manager and injury terms together
move **3 of 20 clubs, by at most 2 places**. That is the intended size — the
whole prize above a naive baseline is about one place, so a component that
reordered the table would be overclaiming.

### Overriding by hand

`data/model/adjustments.csv` still takes `manager_stability` and
`injury_burden` per club, roughly −1 to +1. A non-zero value **replaces** the
computed one for that club, on the assumption that if you typed it you know
something the sources do not. Zero means "not set", so the file can stay mostly
empty.

## Data limitations worth knowing

- **xG covers two full seasons, not three.** FBref and Understat both block
  automated requests; the worldfootballR project's pre-scraped data repo is the
  way in, and it was last updated 2025-09-16. So 2023-24 and 2024-25 are
  complete, and 2025-26 has 40 matches. Each season's xG is weighted by its
  coverage, so the partial season counts for about a ninth of a full one.
- **European form uses league-phase finishing position only**, not knockout
  progress.
- Squad values are a single number per club and say nothing about squad balance.
- **Newcastle United had no manager listed** when the tenure data was taken, so
  they score neutral on the manager term. Fill in `data/model/managers.csv` and
  re-run once that is settled.
- The current injury snapshot is a **headcount taken on one pre-season day**. It
  does not weigh a first-choice striker out until Christmas against a reserve
  full-back missing a fortnight.
- The 2021-24 chronic rates cover only clubs continuously in the league across
  that window, and the 2025-26 figures stop at 22 rounds.
