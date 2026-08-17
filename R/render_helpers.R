# ---------------------------------------------------------------------------
# render_helpers.R -- shared table/format helpers for the .Rmd pages
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(knitr)
  library(kableExtra)
  library(htmltools)
})

## Colour a cell by how far off a pick was. Green = spot on, red = way out.
.miss_colour <- function(off) {
  off <- pmin(off, 8)
  ifelse(off == 0, "#1a7f37",
  ifelse(off <= 2, "#3f8f5c",
  ifelse(off <= 4, "#b08900",
  ifelse(off <= 6, "#c8642a", "#b42318"))))
}

#' The one canonical order entrants appear in, used by every page so the
#' side-by-side grid and the per-entrant sections always line up. Alphabetical
#' by display name; `tolower` keeps it case-insensitive.
entrant_order <- function(players) {
  players[order(tolower(players$display_name), players$player), , drop = FALSE]
}

fmt_when <- function(x, tz = CONFIG$timezone) {
  if (is.null(x) || all(is.na(x))) return("unknown")
  format(as.POSIXct(x), "%A %d %B %Y, %H:%M %Z", tz = tz)
}

#' Countdown / lock banner shown at the top of the site.
lock_banner <- function(now = Sys.time()) {
  deadline <- CONFIG$lock_time
  if (now >= deadline) {
    div(class = "banner banner-locked",
        strong("Predictions are locked."),
        sprintf(" Entries closed %s. Nothing below can change now — only the league can.",
                fmt_when(deadline)))
  } else {
    left <- difftime(deadline, now, units = "hours")
    div(class = "banner banner-open",
        strong("Predictions are still open."),
        sprintf(" They freeze at %s — %.0f hours from now.",
                fmt_when(deadline), as.numeric(left)))
  }
}

#' The live Premier League table.
render_live_table <- function(live) {
  live |>
    transmute(
      `#` = position, Club = team, Pl = played, W = won, D = drawn, L = lost,
      GF = gf, GA = ga, GD = ifelse(gd > 0, paste0("+", gd), as.character(gd)),
      Pts = points
    ) |>
    kbl(align = c("r", "l", rep("r", 8)), escape = FALSE) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE) |>
    row_spec(1:4, background = "#eaf5ee") |>       # Champions League places
    row_spec(nrow(live) - 2:0, background = "#fdecea") |>  # relegation
    column_spec(2, bold = TRUE)
}

#' The leaderboard.
render_leaderboard <- function(res) {
  lb <- res$leaderboard
  arrow <- if (res$direction == "lower") {
    "lowest wins"
  } else {
    cap <- max_club_points(res$method)
    if (is.na(cap)) "highest wins"
    else sprintf("highest wins — %d is a perfect table", cap * nrow(LIVE))
  }

  out <- tibble(Rank = lb$rank, Entrant = lb$display_name)

  ## Once bonuses are live the total needs breaking out, otherwise a single
  ## Score column is all there is to say.
  if (isTRUE(res$bonuses_active)) {
    out$Points <- lb$base_score
    out$Bonus  <- lb$bonus
    out$Total  <- lb$total_score
  } else {
    out$Score <- lb$total_score
  }

  out$Exact        <- lb$exact_hits
  out$`±1`         <- lb$within_one
  out$`±3`         <- lb$within_three
  out$`Worst miss` <- lb$worst_miss

  ## The banner above already explains a closed scoring window, so the caption
  ## only carries the scoring direction once points actually count.
  cap <- if (!isTRUE(res$scoring_active)) NULL else paste0("Score = ", arrow)

  k <- out |>
    kbl(align = c("r", "l", rep("r", ncol(out) - 2)), escape = FALSE,
        caption = cap) |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) |>
    column_spec(2, bold = TRUE)

  ## Highlighting a leader is meaningless while everyone is on 0.
  if (isTRUE(res$scoring_active)) {
    k <- row_spec(k, which(lb$rank == 1), background = "#fff6d6")
  }
  k
}

#' Banner explaining what is and is not being counted yet.
scoring_banner <- function(res, now = Sys.time()) {
  if (!isTRUE(res$scoring_active)) {
    return(div(class = "banner banner-open",
      strong("Scoring opens "), fmt_when(CONFIG$scoring_start), ". ",
      "Until then every total below reads 0, whatever the league table does."))
  }
  if (!isTRUE(res$bonuses_active)) {
    return(div(class = "banner banner-locked",
      strong("Bonuses are not in play yet. "),
      "Totals below are per-club points only. The champion, relegation and ",
      "top-four bonuses are added in the final week, from ",
      fmt_when(CONFIG$bonus_start), "."))
  }
  div(class = "banner banner-locked",
      strong("Final week. "), "End-of-season bonuses are now included in every total.")
}

#' One entrant's frozen table next to where those clubs actually sit.
render_player_table <- function(res, player_key) {
  d <- res$detail |> filter(player == player_key) |> arrange(predicted)

  moved <- ifelse(
    d$diff == 0, "–",
    sprintf("%s%d", ifelse(d$diff > 0, "▼", "▲"), abs(d$diff))
  )

  tibble(
    `Picked` = d$predicted,
    Club = d$team,
    `Actual` = d$actual,
    `Off by` = cell_spec(moved, color = .miss_colour(d$off), bold = TRUE),
    `Score` = d$score
  ) |>
    kbl(align = c("r", "l", "r", "r", "r"), escape = FALSE) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE) |>
    column_spec(2, bold = TRUE)
}

#' Small "who is where" grid: rows = actual position, columns = entrant.
render_comparison_grid <- function(res, players) {
  wide <- res$detail |>
    select(player, predicted, team) |>
    tidyr::pivot_wider(names_from = player, values_from = team) |>
    arrange(predicted)

  ## Columns follow entrant_order(), not whatever order the prediction sheet
  ## happens to store, so this grid and the per-entrant sections on the
  ## predictions page always run in the same sequence.
  ord <- entrant_order(players)
  keys <- intersect(ord$player, names(wide))
  wide <- wide[, c("predicted", keys)]
  names(wide) <- c("#", ord$display_name[match(keys, ord$player)])

  wide |>
    kbl(escape = FALSE, align = c("r", rep("l", ncol(wide) - 1))) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = TRUE, font_size = 12) |>
    scroll_box(width = "100%")
}
