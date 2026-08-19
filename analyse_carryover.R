#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# analyse_carryover.R -- how much does one season's table tell you about the next?
# ---------------------------------------------------------------------------
# For each pair of consecutive seasons, correlate a club's finishing position
# with its position the season before. Only clubs present in both seasons can
# be correlated at all, so promoted clubs are excluded from the coefficient and
# counted separately -- the turnover is itself part of the answer.
#
# Spearman is the headline: positions are ranks, and rank correlation is what a
# ranking question actually asks. Pearson is reported alongside for comparison.

source("R/config.R"); source("R/teams.R")
source("R/model_config.R"); source("R/model_data.R")
suppressPackageStartupMessages({ library(dplyr); library(readr) })

N_TRANSITIONS <- 20
LAST <- 2025                                   # 2025-26 is the latest complete season
FIRST <- LAST - N_TRANSITIONS + 1              # first season being predicted
SEASONS <- (FIRST - 1):LAST                    # plus the predecessor of the first

resolve_keep <- function(x) { r <- resolve_team(x, strict = FALSE); ifelse(is.na(r), trimws(x), r) }

tbl <- .cache(sprintf("pl_tables_%d_%d", min(SEASONS), max(SEASONS)), expr = {
  bind_rows(lapply(SEASONS, function(s) {
    message("  fetching eng.1 ", s)
    fetch_espn_table("eng.1", s)
  }))
}) |>
  mutate(team = resolve_keep(raw_name)) |>
  select(season, team, rank, points)

stopifnot(all(table(tbl$season) == 20))

pairs <- lapply(FIRST:LAST, function(s) {
  now  <- tbl |> filter(season == s)     |> select(team, pos = rank, pts = points)
  prev <- tbl |> filter(season == s - 1) |> select(team, prev_pos = rank, prev_pts = points)
  j <- inner_join(now, prev, by = "team")
  tibble(
    season      = s,
    label       = sprintf("%d-%02d", s, (s + 1) %% 100),
    n_survivors = nrow(j),
    n_promoted  = 20 - nrow(j),
    spearman    = suppressWarnings(cor(j$pos, j$prev_pos, method = "spearman")),
    pearson     = suppressWarnings(cor(j$pos, j$prev_pos)),
    pts_r       = suppressWarnings(cor(j$pts, j$prev_pts)),
    mean_move   = mean(abs(j$pos - j$prev_pos)),
    within3     = mean(abs(j$pos - j$prev_pos) <= 3)
  )
}) |> bind_rows()

cat("\n=== Position carry-over, season to season ===\n")
cat("Clubs present in both seasons only; promoted clubs cannot be correlated.\n\n")
print(as.data.frame(pairs |> transmute(
  Season = label, Kept = n_survivors, Up = n_promoted,
  Spearman = round(spearman, 3), Pearson = round(pearson, 3),
  `Points r` = round(pts_r, 3),
  `Mean move` = round(mean_move, 2),
  `Within 3` = sprintf("%.0f%%", 100 * within3)
)), row.names = FALSE)

## Fisher z-transform to average correlations properly: averaging r directly
## understates the mean because r is bounded and its sampling distribution skewed.
fisher_mean <- function(r) tanh(mean(atanh(r)))

cat("\n=== Summary over ", nrow(pairs), " transitions (",
    pairs$label[1], " to ", pairs$label[nrow(pairs)], ") ===\n", sep = "")
cat(sprintf("  Spearman  mean %.3f (Fisher)   median %.3f   range %.3f to %.3f\n",
            fisher_mean(pairs$spearman), median(pairs$spearman),
            min(pairs$spearman), max(pairs$spearman)))
cat(sprintf("  Pearson   mean %.3f (Fisher)   median %.3f\n",
            fisher_mean(pairs$pearson), median(pairs$pearson)))
cat(sprintf("  Points r  mean %.3f (Fisher)\n", fisher_mean(pairs$pts_r)))
cat(sprintf("  Shared variance implied by mean Spearman: %.0f%%\n",
            100 * fisher_mean(pairs$spearman)^2))
cat(sprintf("  Mean position move: %.2f places   |   within 3 places: %.0f%%\n",
            mean(pairs$mean_move), 100 * mean(pairs$within3)))
cat(sprintf("  Clubs surviving each season, on average: %.1f of 20\n",
            mean(pairs$n_survivors)))

## Is the league becoming more or less predictable?
fit <- lm(atanh(spearman) ~ season, data = pairs)
sl <- summary(fit)$coefficients["season", ]
cat(sprintf("\n  Trend in Spearman over time: %+.4f per season (p = %.3f) -- %s\n",
            sl[["Estimate"]], sl[["Pr(>|t|)"]],
            if (sl[["Pr(>|t|)"]] < 0.05) "statistically detectable"
            else "no detectable trend"))

## Split-half comparison
h1 <- pairs[1:floor(nrow(pairs)/2), ]; h2 <- pairs[(floor(nrow(pairs)/2)+1):nrow(pairs), ]
cat(sprintf("  First half  (%s-%s): mean Spearman %.3f\n",
            h1$label[1], h1$label[nrow(h1)], fisher_mean(h1$spearman)))
cat(sprintf("  Second half (%s-%s): mean Spearman %.3f\n",
            h2$label[1], h2$label[nrow(h2)], fisher_mean(h2$spearman)))

write_csv(pairs, "data/model/carryover.csv")
cat("\nwritten: data/model/carryover.csv\n")

## --- how much of a single season's table is signal at all? ------------------
## Split each season's gameweeks into two halves, build a table from each, and
## correlate them. Two halves of the same season share the same teams and the
## same year, so whatever they disagree about is noise. Corrected for length by
## Spearman-Brown, this estimates the reliability of a full table -- and so how
## much of one any model could ever hope to predict.
##
## Two details matter. Split by GAMEWEEK, not by individual match, so both
## halves contain a near-complete round-robin rather than an arbitrary subset
## of fixtures. And rank on points PER GAME, so that a club which happens to
## land more matches in one half is not flattered by the extra fixtures.

if (requireNamespace("worldfootballR", quietly = TRUE)) {
  suppressPackageStartupMessages(library(worldfootballR))
  mm <- load_match_results(country = "ENG", gender = "M",
                           season_end_year = 2004:2026, tier = "1st") |>
    filter(!is.na(HomeGoals), !is.na(AwayGoals))
  full <- mm |> count(Season_End_Year) |> filter(n == 380) |> pull(Season_End_Year)

  table_from <- function(d) {
    pts <- function(gf, ga) ifelse(gf > ga, 3, ifelse(gf == ga, 1, 0))
    bind_rows(
      d |> transmute(team = Home, p = pts(HomeGoals, AwayGoals), gd = HomeGoals - AwayGoals),
      d |> transmute(team = Away, p = pts(AwayGoals, HomeGoals), gd = AwayGoals - HomeGoals)
    ) |>
      group_by(team) |>
      summarise(m = n(), ppg = sum(p) / n(), gdpg = sum(gd) / n(), .groups = "drop") |>
      arrange(desc(ppg), desc(gdpg)) |>
      mutate(pos = row_number())
  }

  ## Gameweek column is named Wk in the FBref data; fall back to date order.
  wk_of <- function(d) {
    if ("Wk" %in% names(d) && sum(!is.na(d$Wk)) > nrow(d) * 0.9) as.integer(d$Wk)
    else as.integer(cut(as.Date(d$Date), breaks = 38, labels = FALSE))
  }

  set.seed(42)
  reps <- 40
  sh <- vapply(full, function(s) {
    d <- mm |> filter(Season_End_Year == s)
    d$wk <- wk_of(d)
    wks <- sort(unique(d$wk[!is.na(d$wk)]))
    mean(vapply(seq_len(reps), function(i) {
      pick <- sample(wks, length(wks) %/% 2)
      a <- table_from(d[d$wk %in% pick, ]); b <- table_from(d[!d$wk %in% pick, ])
      j <- inner_join(a, b, by = "team")
      suppressWarnings(cor(j$pos.x, j$pos.y, method = "spearman"))
    }, numeric(1)))
  }, numeric(1))

  half <- tanh(mean(atanh(sh)))
  full_rel <- 2 * half / (1 + half)          # Spearman-Brown, half -> whole

  cat("\n=== Noise floor: how repeatable is one season's table? ===\n")
  cat(sprintf("  Seasons used: %d (%d-%d), %d random splits each\n",
              length(full), min(full), max(full), reps))
  cat(sprintf("  Half-season vs half-season Spearman : %.3f\n", half))
  cat(sprintf("  Implied full-season reliability     : %.3f (Spearman-Brown)\n", full_rel))
  cat(sprintf("  So a full table is about %.0f%% signal, %.0f%% luck.\n",
              100 * full_rel, 100 * (1 - full_rel)))
  obs <- fisher_mean(pairs$spearman)
  corrected <- obs / full_rel
  cat(sprintf("\n  Season-to-season carry-over observed : %.3f\n", obs))
  if (corrected >= 1) {
    cat("  Corrected for that noise             : >= 1.0, which is out of range\n")
    cat("  and means the reliability estimate above is too low to trust.\n")
  } else {
    cat(sprintf("  Carry-over corrected for that noise  : %.3f\n", corrected))
    cat("  (underlying club strength persists more strongly than the raw\n")
    cat("   correlation suggests: part of the year-to-year movement is the\n")
    cat("   table being an imperfect measure of the teams in the first place.)\n")
  }
}

## --- what would a perfect model score? -------------------------------------
## Correlations are hard to feel. Convert them to mean absolute position error
## over 20 clubs by simulation: draw two correlated latent strengths, rank both,
## measure the average gap. A model that knew each club's true strength exactly
## would still miss by the amount implied by the table's own reliability.

  mae_for_spearman <- function(target, n = 20, sims = 4000) {
    ## invert the bivariate-normal relation rho_s = (6/pi) * asin(r/2)
    r <- 2 * sin(pi * target / 6)
    mean(replicate(sims, {
      z1 <- rnorm(n); z2 <- r * z1 + sqrt(1 - r^2) * rnorm(n)
      mean(abs(rank(z1) - rank(z2)))
    }))
  }

  obs_rho  <- fisher_mean(pairs$spearman)
  ceiling_rho <- sqrt(full_rel)   # a perfect strength model vs a noisy table

  cat("\n=== The same numbers as places of error ===\n")
  cat(sprintf("  Random ordering                    rho 0.00  -> %.2f places\n",
              mae_for_spearman(0)))
  cat(sprintf("  Copying last season's table        rho %.2f  -> %.2f places\n",
              obs_rho, mae_for_spearman(obs_rho)))
  cat(sprintf("  A model that knew true strength    rho %.2f  -> %.2f places\n",
              ceiling_rho, mae_for_spearman(ceiling_rho)))
  cat(sprintf("\n  Headroom between copying last season and omniscience: %.2f places.\n",
              mae_for_spearman(obs_rho) - mae_for_spearman(ceiling_rho)))
  cat("  That gap is the entire prize any model is competing for.\n")
