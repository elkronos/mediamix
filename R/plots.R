#' The package's chart palette
#'
#' Eight categorical colours in a fixed order, checked for colour-vision
#' deficiency separation between neighbours, plus the neutral inks used for
#' text, axes and gridlines. Every `plot()` method in the package uses them,
#' so figures read as one system; they are exported so your own figures can
#' match.
#'
#' @param n Number of categorical colours, at most 8. Colours are always taken
#'   from the front of the list, never cycled.
#' @param role Either `"categorical"` (the default) or `"ink"` for the
#'   neutral surface, text, axis and grid colours.
#'
#' @return A named character vector of hex colours.
#'
#' @details
#' Assign the colours to series in order, and keep the assignment fixed to the
#' series rather than to its rank. Three of them (aqua, yellow, magenta) have
#' less than 3:1 contrast against a white background, so a figure using them
#' should carry a legend or direct labels. Past eight series, fold the smallest
#' into "Other" or use small multiples rather than inventing more colours.
#'
#' @examples
#' mm_palette(3)
#' mm_palette(role = "ink")
#'
#' w <- rbind(geometric = adstock_weights(10, 0.6),
#'            delayed = adstock_weights_delayed(10, 0.6, peak = 2))
#' matplot(0:9, t(w), type = "l", lty = 1, lwd = 2, col = mm_palette(2),
#'         xlab = "Lag", ylab = "Weight")
#' legend("topright", rownames(w), col = mm_palette(2), lwd = 2, bty = "n")
#' @export
mm_palette <- function(n = 8L, role = c("categorical", "ink")) {
  role <- match.arg(role)
  if (role == "ink") return(.mm_ink())
  n <- .mm_check_count(n, "n", min = 1L)
  if (n > 8L) {
    cli::cli_abort(c(
      "There are only 8 categorical colours.",
      i = "Fold the smallest series into \"Other\", or use small multiples."
    ))
  }
  .mm_cat()[seq_len(n)]
}

#' @keywords internal
#' @noRd
.mm_cat <- function() {
  c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a",
    yellow = "#eda100", magenta = "#e87ba4", green = "#008300",
    violet = "#4a3aa7", red = "#e34948")
}

#' @keywords internal
#' @noRd
.mm_ink <- function() {
  c(surface = "#fcfcfb", primary = "#0b0b0b", secondary = "#52514e",
    muted = "#898781", grid = "#e1e0d9", axis = "#c3c2b7")
}

# Open an empty plotting region with the package's chrome: surface
# background, hairline horizontal grid, muted axes, no box.
#' @keywords internal
#' @noRd
.mm_frame <- function(xlim, ylim, main = NULL, xlab = "", ylab = "",
                      sub = NULL, xaxt = TRUE, yaxt = TRUE, yat = NULL,
                      ylabels = TRUE, las_y = 1) {
  ink <- .mm_ink()
  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim)
  gy <- if (is.null(yat)) pretty(ylim) else yat
  gy <- gy[gy >= ylim[1] & gy <= ylim[2]]
  graphics::abline(h = gy, col = ink[["grid"]], lwd = 0.8)
  if (xaxt) .mm_xaxis()
  if (yaxt) {
    labs <- if (isTRUE(ylabels)) .mm_num(gy) else ylabels
    graphics::axis(2, at = gy, labels = labs, las = las_y, lwd = 0,
                   col.axis = ink[["muted"]], cex.axis = 0.85)
  }
  graphics::title(main = main, col.main = ink[["primary"]], font.main = 1,
                  cex.main = 1, adj = 0, line = 1.2)
  .mm_axis_titles(xlab, ylab)
  if (!is.null(sub)) {
    graphics::mtext(sub, side = 3, line = 0.2, adj = 0, cex = 0.8,
                    col = ink[["secondary"]])
  }
  invisible(NULL)
}

# Tick labels, never in scientific notation. From ten thousand up, a k, M, B
# or T suffix replaces trailing zeros, using the largest unit at which two
# decimals still print every tick exactly; failing that, plain numbers.
#' @keywords internal
#' @noRd
.mm_num <- function(v) {
  out <- as.character(v)
  ok <- is.finite(v)
  if (!any(ok)) return(out)
  s <- v[ok]
  s[abs(s) < 1e-12 * max(abs(s))] <- 0
  sfx <- ""
  if (max(abs(s)) >= 1e4) {
    units <- c(T = 1e12, B = 1e9, M = 1e6, k = 1e3)
    fits <- units <= max(abs(s)) &
      vapply(units, function(u) .mm_decimals(s / u) <= 2L, logical(1))
    if (any(fits)) {
      sfx <- names(units)[which(fits)[1L]]
      s <- s / units[[sfx]]
    }
  }
  lab <- formatC(s, format = "f", digits = .mm_decimals(s), big.mark = ",",
                 drop0trailing = TRUE)
  out[ok] <- ifelse(s == 0, "0", paste0(lab, sfx))
  out
}

# Value labels, such as the figure at the end of a bar: three significant
# figures, with a k, M, B or T suffix from ten thousand up.
#' @keywords internal
#' @noRd
.mm_short <- function(v, digits = 3L) {
  out <- as.character(v)
  ok <- is.finite(v)
  cut <- c(1e12, 1e9, 1e6, 1e4)
  div <- c(1e12, 1e9, 1e6, 1e3)
  sfx <- c("T", "B", "M", "k")
  out[ok] <- vapply(signif(v[ok], digits), function(z) {
    i <- which(abs(z) >= cut)[1L]
    if (is.na(i)) {
      return(format(z, big.mark = ",", scientific = FALSE, trim = TRUE))
    }
    paste0(format(signif(z / div[i], digits), big.mark = ",",
                  scientific = FALSE, trim = TRUE), sfx[i])
  }, character(1))
  out
}

# Percentage tick labels with the fewest decimals that keep every tick
# exact, so that 0.5% and 1% are not both printed as 1%.
#' @keywords internal
#' @noRd
.mm_pct <- function(v) {
  p <- 100 * v
  p[is.finite(p) & abs(p) < 1e-9] <- 0
  paste0(formatC(p, format = "f", digits = .mm_decimals(p),
                 drop0trailing = TRUE), "%")
}

# The fewest decimals, at most `max`, that print every finite value exactly.
#' @keywords internal
#' @noRd
.mm_decimals <- function(v, max = 6L) {
  v <- v[is.finite(v)]
  for (d in 0:max) {
    if (all(abs(round(v, d) - v) <= 1e-9 * pmax(1, abs(v)))) return(d)
  }
  max
}

#' @keywords internal
#' @noRd
.mm_xaxis <- function(at = NULL, labels = NULL) {
  ink <- .mm_ink()
  if (is.null(at)) at <- graphics::axTicks(1)
  if (is.null(labels)) labels <- .mm_num(at)
  graphics::axis(1, at = at, labels = labels, col = ink[["axis"]],
                 col.ticks = ink[["axis"]], col.axis = ink[["muted"]],
                 lwd = 0.8, cex.axis = 0.85)
}

# Axis titles centred on their axes, in their own margin line, so they never
# collide with tick labels.
#' @keywords internal
#' @noRd
.mm_axis_titles <- function(xlab = "", ylab = "", xline = 2.4,
                            yline = NULL) {
  col <- .mm_ink()[["secondary"]]
  if (nzchar(xlab)) graphics::mtext(xlab, side = 1, line = xline, cex = 0.9,
                                    col = col)
  if (nzchar(ylab)) {
    if (is.null(yline)) yline <- graphics::par("mar")[2] - 1.1
    graphics::mtext(ylab, side = 2, line = yline, cex = 0.9, col = col)
  }
}

#' @keywords internal
#' @noRd
.mm_par <- function(...) {
  # Defaults overridden by the caller's values, never passed twice: par()
  # returns one "old" value per argument, so a repeated `mar` would make the
  # restore on exit put back the default rather than the user's setting.
  args <- utils::modifyList(
    list(bg = .mm_ink()[["surface"]], fg = .mm_ink()[["secondary"]],
         mar = c(4, 5.2, 3.2, 1), mgp = c(2.4, 0.6, 0), tcl = -0.25),
    list(...)
  )
  do.call(graphics::par, args)
}

#' @keywords internal
#' @noRd
.mm_legend <- function(pos, legend, col, lwd = NA, pch = NA, pt.bg = NA,
                       ...) {
  graphics::legend(pos, legend = legend, col = col, lwd = lwd, pch = pch,
                   pt.bg = pt.bg, bty = "n", cex = 0.85,
                   text.col = .mm_ink()[["secondary"]], ...)
}

# A graphical argument passed through `...`, or the method's default.
#' @keywords internal
#' @noRd
.mm_arg <- function(dots, name, default) {
  v <- dots[[name]]
  if (is.null(v)) default else v
}

# The columns a plot method reads, checked before anything is drawn, so an
# edited object fails with a message naming what is missing rather than with
# a base graphics error.
#' @keywords internal
#' @noRd
.mm_plot_needs <- function(d, cols, from, where = "x", call = parent.frame()) {
  if (!is.data.frame(d)) {
    cli::cli_abort(c(
      "{.arg {where}} must be a data frame.",
      i = "Plot the object as {.fn {from}} returned it."
    ), call = call)
  }
  miss <- setdiff(cols, names(d))
  if (length(miss) > 0L) {
    cli::cli_abort(c(
      "{.arg {where}} lacks {length(miss)} column{?s} this plot needs: \\
       {.field {miss}}.",
      i = "Plot the object as {.fn {from}} returned it."
    ), call = call)
  }
  if (nrow(d) == 0L) {
    cli::cli_abort("{.arg {where}} has no rows to plot.", call = call)
  }
  invisible(d)
}

# A finite axis range over `v`, padded below and above by the fractions in
# `pad`. A range of zero width is widened so the axis still has ticks.
#' @keywords internal
#' @noRd
.mm_lim <- function(v, pad = c(0, 0), what = "values", call = parent.frame()) {
  v <- v[is.finite(v)]
  if (length(v) == 0L) {
    cli::cli_abort("{.arg x} has no finite {what} to plot.", call = call)
  }
  r <- range(v)
  w <- diff(r)
  if (w == 0) {
    w <- if (r[1L] == 0) 1 else abs(r[1L]) / 10
    r <- r + c(-0.5, 0.5) * w
  }
  r + c(-pad[1L], pad[2L]) * w
}

# Category labels for the left axis, shortened with "..." when the longest
# would take more than 40% of the device width, and the margin in lines they
# need at the current text size.
#' @keywords internal
#' @noRd
.mm_fit_labels <- function(labels, cex = 0.85) {
  labels <- as.character(labels)
  labels[is.na(labels)] <- "NA"
  width <- function(l) {
    max(0, graphics::strwidth(l, units = "inches", cex = cex))
  }
  room <- 0.4 * graphics::par("din")[1L]
  n <- max(0L, nchar(labels))
  while (width(labels) > room && n > 4L) {
    n <- n - 1L
    long <- nchar(labels) > n
    labels[long] <- paste0(substr(labels[long], 1L, n - 3L), "...")
  }
  inches <- width(labels)
  line <- graphics::par("csi") * graphics::par("mex")
  list(labels = labels, inches = inches, lines = max(4, inches / line + 1.5))
}

# ---- carryover ------------------------------------------------------------------

#' Plot a carryover cross-validation profile
#'
#' The cross-validated error across the decay grid, one line per `max_lag`,
#' with the minimum (filled) and the one-standard-error choice (open) marked
#' on the line they belong to. A flat profile is the finding: the data does
#' not pin carryover down, however precise `best` looks.
#'
#' @param x An object from [tune_carryover()] or [tune_carryover_joint()].
#' @param truth Optional known decay (a number, or for the joint version a
#'   vector named by channel), drawn as a muted reference line. Useful on
#'   simulated data.
#' @param ... Graphical arguments: `main`, `xlab` and `ylab` replace the
#'   titles, and `col` gives the line colours, one per `max_lag` in legend
#'   order. In the joint version `main` is the overall title and `xlab` and
#'   `ylab` label every panel. Others are ignored.
#'
#' @return `x`, invisibly.
#'
#' @details
#' With more than eight `max_lag` values, the seven with the lowest error keep
#' a colour each and the others are drawn as thin grey lines, with a message.
#'
#' @examples
#' set.seed(42)
#' spend <- pmax(0, rnorm(120, 500, 250))
#' kpi <- 200 + 0.5 * adstock_geometric(spend, decay = 0.7) + rnorm(120, sd = 30)
#' tuned <- tune_carryover(spend, kpi, decays = seq(0.05, 0.95, by = 0.05))
#' plot(tuned, truth = 0.7)
#' @name plot_carryover
NULL

#' @rdname plot_carryover
#' @export
plot.mm_carryover <- function(x, truth = NULL, ...) {
  r <- x$results
  .mm_plot_needs(r, c("max_lag", "decay", "metric"), "tune_carryover",
                 "x$results")
  .mm_plot_needs(x$best, c("max_lag", "decay"), "tune_carryover", "x$best")
  dots <- list(...)
  metric <- if (is.null(x$metric)) "metric" else x$metric
  xl <- .mm_lim(r$decay, what = "decays")
  yl <- .mm_lim(r$metric, c(0.04, 0.08), "metric values")
  lc <- .mm_lag_colours(.mm_lag_rank(r, x$best), dots[["col"]])
  op <- .mm_par()
  on.exit(graphics::par(op), add = TRUE)
  .mm_frame(xl, yl,
            main = .mm_arg(dots, "main",
                           sprintf("Cross-validated %s by decay", metric)),
            sub = "Filled: minimum. Open: one-standard-error choice.",
            xlab = .mm_arg(dots, "xlab", "Decay"),
            ylab = .mm_arg(dots, "ylab", metric))
  .mm_truth_line(truth)
  .mm_profile_lines(r, lc)
  .mm_mark_choice(x$best, x$best_1se, r, lc)
  if (length(c(lc$named, lc$other)) > 1L) .mm_lag_legend(lc)
  invisible(x)
}

#' @rdname plot_carryover
#' @export
plot.mm_carryover_joint <- function(x, truth = NULL, ...) {
  prof <- x$profiles
  .mm_plot_needs(prof, c("channel", "max_lag", "decay", "metric"),
                 "tune_carryover_joint", "x$profiles")
  .mm_plot_needs(x$best, c("channel", "max_lag", "decay"),
                 "tune_carryover_joint", "x$best")
  dots <- list(...)
  ch <- x$best$channel
  k <- length(ch)
  nc <- min(3L, k)
  one_se <- x$best_1se
  if (!is.data.frame(one_se) || !"channel" %in% names(one_se)) one_se <- NULL
  lc <- .mm_lag_colours(.mm_lag_rank(prof, x$best), dots[["col"]])
  xlab <- .mm_arg(dots, "xlab", "Decay")
  ylab <- .mm_arg(dots, "ylab", "")
  op <- .mm_par(mfrow = c(ceiling(k / nc), nc), oma = c(0, 0, 1.5, 0))
  on.exit(graphics::par(op), add = TRUE)
  for (i in seq_len(k)) {
    c0 <- ch[[i]]
    p <- prof[prof$channel %in% c0, , drop = FALSE]
    .mm_frame(.mm_lim(p$decay, what = "decays"),
              .mm_lim(p$metric, c(0.05, 0.12), "metric values"),
              main = c0, xlab = xlab, ylab = ylab)
    if (!is.null(truth) && c0 %in% names(truth)) .mm_truth_line(truth[[c0]])
    .mm_profile_lines(p, lc)
    .mm_mark_choice(x$best[x$best$channel %in% c0, , drop = FALSE],
                    if (!is.null(one_se)) {
                      one_se[one_se$channel %in% c0, , drop = FALSE]
                    }, p, lc)
    if (i == 1L && length(c(lc$named, lc$other)) > 1L) .mm_lag_legend(lc)
  }
  metric <- if (is.null(x$metric_name)) "metric" else x$metric_name
  main <- .mm_arg(dots, "main", sprintf(
    "Cross-validated %s by channel decay, others held at their selected values",
    metric))
  graphics::mtext(main, outer = TRUE, side = 3, adj = 0, cex = 0.85,
                  col = .mm_ink()[["secondary"]])
  invisible(x)
}

# max_lag values in order of their lowest error, the selected ones first.
#' @keywords internal
#' @noRd
.mm_lag_rank <- function(profile, best) {
  ranked <- profile$max_lag[order(profile$metric, na.last = TRUE)]
  unique(c(intersect(best$max_lag, profile$max_lag), ranked))
}

# One colour per max_lag, by rank. Past eight values the first seven keep a
# colour each and the rest share the muted ink, so no line is left out.
#' @keywords internal
#' @noRd
.mm_lag_colours <- function(lags, col = NULL) {
  named <- if (length(lags) > 8L) lags[seq_len(7L)] else lags
  other <- lags[!lags %in% named]
  if (length(other) > 0L) {
    cli::cli_inform(c(
      "The plot has {length(lags)} max_lag values; the {length(named)} with \\
       the lowest error keep a colour each.",
      i = "The other {length(other)} are drawn as thin grey lines."
    ))
  }
  pal <- unname(rep_len(if (is.null(col)) .mm_cat() else col, length(named)))
  list(named = named, other = other, col = pal,
       of = function(lag) {
         i <- match(lag, named)
         if (is.na(i)) .mm_ink()[["muted"]] else pal[[i]]
       })
}

# One line per max_lag, each in decay order. The best-ranked line is drawn
# last so it sits on top.
#' @keywords internal
#' @noRd
.mm_profile_lines <- function(profile, lc) {
  for (lag in c(lc$other, rev(lc$named))) {
    s <- profile[profile$max_lag %in% lag, , drop = FALSE]
    s <- s[order(s$decay), , drop = FALSE]
    graphics::lines(s$decay, s$metric, col = lc$of(lag),
                    lwd = if (lag %in% lc$named) 2 else 1)
  }
}

#' @keywords internal
#' @noRd
.mm_lag_legend <- function(lc) {
  labs <- paste("max_lag", format(lc$named, trim = TRUE, scientific = FALSE))
  cols <- lc$col
  lwd <- rep(2, length(lc$named))
  if (length(lc$other) > 0L) {
    labs <- c(labs, "other max_lag")
    cols <- c(cols, .mm_ink()[["muted"]])
    lwd <- c(lwd, 1)
  }
  .mm_legend("topleft", labs, col = cols, lwd = lwd)
}

#' @keywords internal
#' @noRd
.mm_truth_line <- function(truth) {
  if (is.null(truth)) return(invisible(NULL))
  ink <- .mm_ink()
  graphics::abline(v = truth, col = ink[["muted"]], lwd = 1)
  usr <- graphics::par("usr")
  graphics::text(truth, usr[4], "truth", pos = 4, offset = 0.25, cex = 0.75,
                 col = ink[["muted"]], xpd = TRUE)
}

# Mark the minimum (filled) and the one-standard-error choice (open) at their
# height on the profile line they belong to, matched on both max_lag and
# decay. Any metric stored on the choice rows is ignored, so the markers
# always sit on the drawn lines.
#' @keywords internal
#' @noRd
.mm_mark_choice <- function(best, one_se, profile, lc) {
  ink <- .mm_ink()
  on_line <- function(row) {
    if (!is.data.frame(row) || nrow(row) != 1L ||
        !all(c("max_lag", "decay") %in% names(row))) {
      return(NULL)
    }
    hit <- which(profile$max_lag %in% row$max_lag &
                   abs(profile$decay - row$decay) < 1e-9)
    if (length(hit) == 0L) return(NULL)
    list(x = row$decay, y = profile$metric[hit[1L]], lag = row$max_lag)
  }
  b <- on_line(best)
  o <- on_line(one_se)
  if (!is.null(b)) {
    graphics::points(b$x, b$y, pch = 21, bg = lc$of(b$lag),
                     col = ink[["surface"]], cex = 1.6, lwd = 2)
    graphics::text(b$x, b$y, .mm_num(b$x), pos = 3, cex = 0.75,
                   col = ink[["secondary"]], offset = 0.7)
  }
  if (!is.null(o) && !(!is.null(b) && o$lag == b$lag && o$x == b$x)) {
    graphics::points(o$x, o$y, pch = 21, bg = ink[["surface"]],
                     col = lc$of(o$lag), cex = 1.5, lwd = 2)
    graphics::text(o$x, o$y, .mm_num(o$x), pos = 3, cex = 0.75,
                   col = ink[["secondary"]], offset = 0.7)
  }
}

# ---- bootstrap ------------------------------------------------------------------

#' Plot bootstrap intervals
#'
#' One row per term: the estimate on the original data as a dot and the
#' percentile interval as a line, ordered by estimate.
#'
#' @param x An `mm_bootstrap` object from [block_bootstrap()].
#' @param reference Optional value drawn as a muted vertical line -- `1` for
#'   an ROI's break-even point, `0` for a coefficient.
#' @param xlab Axis label.
#' @param ... Graphical arguments: `main` and `ylab` replace the defaults, and
#'   `col` colours each term's interval and dot, recycled in the row order of
#'   `x`. Others are ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(a = rnorm(100, 2), b = rnorm(100, 1))
#' b <- block_bootstrap(d, function(d) colMeans(d), times = 100, seed = 1)
#' plot(b, reference = 0)
#' @export
plot.mm_bootstrap <- function(x, reference = NULL, xlab = "Estimate", ...) {
  d <- as.data.frame(x)
  .mm_plot_needs(d, c("term", "estimate", "lower", "upper"),
                 "block_bootstrap")
  dots <- list(...)
  level <- attr(x, "level")
  if (is.null(level)) level <- 0.9
  cols <- rep_len(.mm_arg(dots, "col", .mm_cat()[["blue"]]), nrow(d))
  ord <- order(d$estimate)
  d <- d[ord, , drop = FALSE]
  cols <- cols[ord]
  k <- nrow(d)
  xl <- .mm_lim(c(d$lower, d$upper, d$estimate, reference),
                what = "estimates")
  labs <- .mm_fit_labels(d$term)
  op <- .mm_par(mar = c(4, labs$lines, 3, 1))
  on.exit(graphics::par(op), add = TRUE)
  ink <- .mm_ink()
  graphics::plot.new()
  graphics::plot.window(xlim = xl + c(-0.03, 0.03) * diff(xl),
                        ylim = c(0.5, k + 0.5))
  graphics::abline(v = pretty(xl), col = ink[["grid"]], lwd = 0.8)
  .mm_xaxis()
  graphics::axis(2, at = seq_len(k), labels = labs$labels, las = 1, lwd = 0,
                 col.axis = ink[["secondary"]], cex.axis = 0.85)
  if (!is.null(reference)) {
    graphics::abline(v = reference, col = ink[["muted"]], lwd = 1.2)
  }
  graphics::segments(d$lower, seq_len(k), d$upper, seq_len(k), col = cols,
                     lwd = 2)
  graphics::points(d$estimate, seq_len(k), pch = 21, bg = cols,
                   col = ink[["surface"]], cex = 1.5, lwd = 2)
  main <- .mm_arg(dots, "main",
                  sprintf("Estimates with %d%% block-bootstrap intervals",
                          round(100 * level)))
  graphics::title(main = main, col.main = ink[["primary"]], font.main = 1,
                  cex.main = 1, adj = 0, line = 1.2)
  .mm_axis_titles(xlab, .mm_arg(dots, "ylab", ""))
  invisible(x)
}

# ---- contributions and response ----------------------------------------------------

#' Plot a contribution decomposition
#'
#' `type = "time"` stacks each channel's contribution period by period, which
#' shows *when* media worked; `type = "total"` compares channel totals. The
#' baseline is left out of both, because it is usually several times the size
#' of all media combined and would flatten everything else; its total is
#' reported in the subtitle.
#'
#' @param x A data frame from [contributions()].
#' @param type `"time"` (the default) or `"total"`.
#' @param ... Graphical arguments: `main`, `xlab` and `ylab` replace the
#'   defaults, and `col` gives one colour per channel, in the order the
#'   channels first appear in `x`. Others are ignored.
#'
#' @return `x`, invisibly.
#'
#' @details
#' Periods are drawn in sorted order: dates and date-times on a date axis,
#' numbers at their values, and text labels in sort order, so pass a factor
#' to set the order of text labels. With more than eight channels the time
#' chart keeps the seven largest and draws the rest together as "Other", with
#' a message. A missing contribution is counted as zero, with a warning.
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' ch <- c("tv", "search")
#' tr <- attr(mm_weekly, "truth")
#' media <- as.data.frame(Map(function(x, d, h, s) media_transform(
#'   x, adstock = list(decay = d), saturation = list(half_max = h, shape = s)),
#'   north[ch], tr$decay[ch], tr$half_max[ch], tr$shape[ch]))
#' fit <- lm(north$revenue ~ ., data = media)
#' contrib <- contributions(media, fit, index = north$date)
#' plot(contrib)
#' plot(contrib, type = "total")
#' @export
plot.mm_contributions <- function(x, type = c("time", "total"), ...) {
  type <- match.arg(type)
  d <- as.data.frame(x)
  .mm_plot_needs(d, c("period", "channel", "contribution"), "contributions")
  dots <- list(...)
  d$channel <- as.character(d$channel)
  bad <- !is.finite(d$contribution)
  if (any(bad)) {
    cli::cli_warn(c(
      "{sum(bad)} contribution{?s} {?is/are} missing or infinite and \\
       {?is/are} counted as zero.",
      i = "Channel{?s} affected: {.val {unique(d$channel[bad])}}."
    ))
    d$contribution[bad] <- 0
  }
  base <- d$channel %in% "(baseline)"
  m <- d[!base, , drop = FALSE]
  if (nrow(m) == 0L) {
    cli::cli_abort(c(
      "{.arg x} has no media channel to plot.",
      i = "It holds only the {.val (baseline)} rows."
    ))
  }
  ch <- unique(m$channel)
  col_for <- function(chs) {
    stats::setNames(unname(rep_len(.mm_arg(dots, "col", .mm_cat()),
                                   length(chs))), chs)
  }
  cols <- col_for(ch)
  ink <- .mm_ink()
  base_total <- sum(d$contribution[base])
  pred <- sum(d$contribution)
  share <- 100 * base_total / pred
  sub <- if (is.finite(share) && share >= 0 && share <= 100) {
    sprintf("Baseline (not shown): %s in total, %.0f%% of the prediction.",
            .mm_short(base_total), share)
  } else {
    sprintf("Baseline (not shown): %s in total, against a prediction of %s.",
            .mm_short(base_total), .mm_short(pred))
  }
  if (type == "total") {
    tot <- sort(tapply(m$contribution, m$channel, sum))
    k <- length(tot)
    labs <- .mm_fit_labels(names(tot))
    op <- .mm_par(mar = c(4, labs$lines, 3.4, 1))
    on.exit(graphics::par(op), add = TRUE)
    lo <- min(0, tot)
    hi <- max(0, tot)
    if (hi == lo) hi <- 1
    w <- hi - lo
    xl <- c(lo - if (lo < 0) 0.14 * w else 0, hi + if (hi > 0) 0.14 * w else 0)
    at <- pretty(c(lo, hi))
    at <- at[at >= xl[1L] & at <= xl[2L]]
    graphics::plot.new()
    graphics::plot.window(xlim = xl, ylim = c(0.4, k + 0.6))
    graphics::abline(v = at, col = ink[["grid"]], lwd = 0.8)
    bar_col <- if (is.null(dots[["col"]])) .mm_cat()[["blue"]] else
      cols[names(tot)]
    graphics::rect(0, seq_len(k) - 0.28, tot, seq_len(k) + 0.28,
                   col = bar_col, border = ink[["surface"]], lwd = 2)
    .mm_xaxis(at = at)
    graphics::axis(2, at = seq_len(k), labels = labs$labels, las = 1, lwd = 0,
                   col.axis = ink[["secondary"]], cex.axis = 0.85)
    graphics::text(tot, seq_len(k), .mm_short(tot),
                   pos = ifelse(tot < 0, 2, 4), cex = 0.75,
                   col = ink[["secondary"]], xpd = TRUE)
    main <- .mm_arg(dots, "main", "Total contribution by channel")
    graphics::title(main = main, col.main = ink[["primary"]], font.main = 1,
                    cex.main = 1, adj = 0, line = 2)
    .mm_axis_titles(.mm_arg(dots, "xlab", "Contribution"),
                    .mm_arg(dots, "ylab", ""))
    graphics::mtext(sub, side = 3, line = 0.6, adj = 0, cex = 0.75,
                    col = ink[["secondary"]])
    return(invisible(x))
  }
  series <- stats::setNames(as.list(ch), ch)
  if (length(ch) > 8L) {
    size <- tapply(abs(m$contribution), m$channel, sum)[ch]
    big <- ch[order(-size)][seq_len(7L)]
    small <- setdiff(ch, big)
    cli::cli_inform(c(
      "The plot has {length(ch)} channels; the {length(small)} smallest are \\
       drawn together as {.val Other}.",
      i = "Folded: {.val {small}}."
    ))
    kept <- ch[ch %in% big]
    series <- c(stats::setNames(as.list(kept), kept), list(Other = small))
    cols <- c(if (is.null(dots[["col"]])) col_for(kept) else cols[kept],
              Other = ink[["axis"]])
  }
  undated <- is.na(m$period)
  if (any(undated)) {
    cli::cli_warn("{sum(undated)} row{?s} with a missing period {?is/are} \\
                   left out of the time chart.")
  }
  op <- .mm_par(mar = c(3, 5.2, 3.4, 1))
  on.exit(graphics::par(op), add = TRUE)
  per <- sort(unique(m$period))
  at_per <- factor(match(m$period, per), levels = seq_along(per))
  mat <- vapply(series, function(s) {
    rows <- m$channel %in% s
    v <- tapply(m$contribution[rows], at_per[rows], sum)
    v[is.na(v)] <- 0
    as.numeric(v)
  }, numeric(length(per)))
  mat <- matrix(mat, nrow = length(per))
  cum <- mat
  for (j in seq_len(ncol(cum))[-1L]) cum[, j] <- cum[, j - 1L] + mat[, j]
  dated <- inherits(per, c("Date", "POSIXct"))
  xs <- if (dated || is.numeric(per)) as.numeric(per) else seq_along(per)
  .mm_frame(.mm_lim(xs, what = "periods"), .mm_lim(c(0, cum), c(0, 0.05)),
            main = .mm_arg(dots, "main", "Media contribution by period"),
            sub = sub, xaxt = FALSE, xlab = .mm_arg(dots, "xlab", ""),
            ylab = .mm_arg(dots, "ylab", "Contribution"))
  if (inherits(per, "Date")) {
    graphics::axis.Date(1, per, col = ink[["axis"]], col.axis = ink[["muted"]],
                        lwd = 0.8, cex.axis = 0.85)
  } else if (inherits(per, "POSIXct")) {
    graphics::axis.POSIXct(1, per, col = ink[["axis"]],
                           col.axis = ink[["muted"]], lwd = 0.8,
                           cex.axis = 0.85)
  } else if (is.numeric(per)) {
    at <- graphics::axTicks(1)
    .mm_xaxis(at = at, labels = format(at, scientific = FALSE, trim = TRUE,
                                       drop0trailing = TRUE))
  } else {
    at <- unique(c(1L, round(pretty(xs))))
    at <- at[at >= 1L & at <= length(per)]
    .mm_xaxis(at = at, labels = as.character(per)[at])
  }
  lower <- rep(0, length(per))
  for (j in seq_along(series)) {
    upper <- cum[, j]
    graphics::polygon(c(xs, rev(xs)), c(upper, rev(lower)), col = cols[[j]],
                      border = ink[["surface"]], lwd = 1.5)
    lower <- upper
  }
  .mm_legend("topleft", rev(names(series)), col = rev(unname(cols)), pch = 15,
             pt.cex = 1.4, horiz = FALSE, ncol = min(length(series), 5L))
  invisible(x)
}

#' Plot a response curve
#'
#' Two stacked panels on a shared spend axis: the response and its marginal
#' return. Spend outside the observed range, below or above it, is shaded,
#' because there the curve is extrapolation. The point where the marginal
#' return is largest in size is marked as the steepest.
#'
#' @param x A data frame from [response_curve()].
#' @param observed Optional numeric vector of the spend levels seen in the
#'   data (for example the adstocked series); its range is left unshaded.
#' @param ... Graphical arguments: `main` and `ylab` replace the upper panel's
#'   title and axis label, `xlab` the spend axis label, and `col` the line
#'   colour. Others are ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' rc <- response_curve(seq(0, 6000, by = 100), coefficient = 6000,
#'                      half_max = 2250, shape = 1.6)
#' plot(rc, observed = c(0, 3450))
#' @export
plot.mm_response_curve <- function(x, observed = NULL, ...) {
  d <- as.data.frame(x)
  .mm_plot_needs(d, c("spend", "response", "marginal"), "response_curve")
  dots <- list(...)
  d <- d[order(d$spend), , drop = FALSE]
  xl <- .mm_lim(d$spend, what = "spend values")
  obs <- NULL
  if (!is.null(observed)) {
    if (!is.numeric(observed)) {
      cli::cli_abort("{.arg observed} must be numeric, not \\
                      {.obj_type_friendly {observed}}.")
    }
    if (any(is.finite(observed))) obs <- range(observed[is.finite(observed)])
  }
  y1 <- .mm_lim(c(0, d$response), what = "responses")
  y2 <- .mm_lim(c(0, d$marginal), what = "marginal returns")
  op <- .mm_par(mfrow = c(2, 1), mar = c(2.2, 5.2, 2.6, 1))
  on.exit(graphics::par(op), add = TRUE)
  ink <- .mm_ink()
  col <- .mm_arg(dots, "col", .mm_cat()[["blue"]])[[1L]]
  shade <- function(yl) {
    if (is.null(obs)) return(invisible(NULL))
    label <- " extrapolation"
    fits <- function(w) w >= graphics::strwidth(label, cex = 0.75)
    if (obs[1L] > xl[1L]) {
      graphics::rect(xl[1L], yl[1], obs[1L], yl[2], col = "#f0efec",
                     border = NA)
      if (fits(obs[1L] - xl[1L])) {
        graphics::text(xl[1L], yl[2], label, adj = c(0, 1.4), cex = 0.75,
                       col = ink[["muted"]])
      }
    }
    if (obs[2L] < xl[2L]) {
      graphics::rect(obs[2L], yl[1], xl[2L], yl[2], col = "#f0efec",
                     border = NA)
      graphics::text(obs[2L], yl[2], label, adj = c(0, 1.4), cex = 0.75,
                     col = ink[["muted"]])
    }
  }
  .mm_frame(xl, y1, main = .mm_arg(dots, "main", "Response"),
            ylab = .mm_arg(dots, "ylab", "Response"), xaxt = FALSE)
  shade(y1)
  graphics::abline(h = pretty(y1), col = ink[["grid"]], lwd = 0.8)
  graphics::lines(d$spend, d$response, col = col, lwd = 2)
  graphics::par(mar = c(4, 5.2, 2.2, 1))
  .mm_frame(xl, y2, main = "Marginal return per unit of spend",
            xlab = .mm_arg(dots, "xlab", "Spend"), ylab = "Marginal")
  shade(y2)
  graphics::abline(h = pretty(y2), col = ink[["grid"]], lwd = 0.8)
  graphics::lines(d$spend, d$marginal, col = col, lwd = 2)
  pk <- which.max(abs(d$marginal))
  if (length(pk) == 1L && pk > 1L && pk < nrow(d)) {
    graphics::points(d$spend[pk], d$marginal[pk], pch = 21, bg = col,
                     col = ink[["surface"]], cex = 1.5, lwd = 2)
    graphics::text(d$spend[pk], d$marginal[pk], "steepest", pos = 4,
                   cex = 0.75, col = ink[["secondary"]])
  }
  invisible(x)
}

# ---- attribution ------------------------------------------------------------------

#' Plot attribution shares across rules
#'
#' One row per channel. The grey bar spans the lowest to the highest share any
#' rule gives it; first-touch, last-touch and Markov shares are marked, and
#' the other rules appear as grey ticks. A long bar is a channel whose value
#' depends on the convention, not the data.
#'
#' @param x A data frame from [attribute()].
#' @param n Show the `n` channels with the largest mean share.
#' @param ... Graphical arguments: `main`, `xlab` and `ylab` replace the
#'   defaults, and `col` gives the colours of the first-touch, last-touch and
#'   Markov markers. Others are ignored.
#'
#' @return `x`, invisibly.
#'
#' @details
#' A missing share is left out of the plot with a warning. The mean share
#' that ranks the channels is taken over the shares that are present, and a
#' channel with no share at all ranks last.
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' plot(attribute(paths))
#' @export
plot.mm_attribution <- function(x, n = 10L, ...) {
  d <- as.data.frame(x)
  .mm_plot_needs(d, c("rule", "channel", "share"), "attribute")
  n <- .mm_check_count(n, "n", min = 1L)
  dots <- list(...)
  miss <- !is.finite(d$share)
  if (any(miss)) {
    cli::cli_warn(c(
      "{sum(miss)} share{?s} {?is/are} missing and left out of the plot.",
      i = "Channel{?s} affected: {.val {unique(d$channel[miss])}}."
    ))
    d$share[miss] <- NA_real_
  }
  mean_share <- tapply(d$share, d$channel, mean, na.rm = TRUE)
  mean_share[!is.finite(mean_share)] <- NA_real_
  keep <- utils::tail(names(mean_share)[order(mean_share, na.last = FALSE)], n)
  k <- length(keep)
  labs <- .mm_fit_labels(keep)
  op <- .mm_par(mar = c(4, labs$lines, 3.4, 1))
  on.exit(graphics::par(op), add = TRUE)
  ink <- .mm_ink()
  xl <- .mm_lim(c(0, d$share[d$channel %in% keep]), what = "shares")
  at <- pretty(xl)
  graphics::plot.new()
  graphics::plot.window(xlim = xl, ylim = c(0.5, k + 0.5))
  graphics::abline(v = at, col = ink[["grid"]], lwd = 0.8)
  .mm_xaxis(at = at, labels = .mm_pct(at))
  graphics::axis(2, at = seq_len(k), labels = labs$labels, las = 1, lwd = 0,
                 col.axis = ink[["secondary"]], cex.axis = 0.85)
  marked <- stats::setNames(
    unname(rep_len(.mm_arg(dots, "col", .mm_cat()[c("blue", "orange", "aqua")]),
                   3L)),
    c("first", "last", "markov"))
  pch <- c(first = 21, last = 24, markov = 23)
  for (i in seq_len(k)) {
    s <- d[d$channel == keep[i] & !is.na(d$share), , drop = FALSE]
    if (nrow(s) == 0L) next
    graphics::segments(min(s$share), i, max(s$share), i, col = "#d6d5ce",
                       lwd = 6, lend = 1)
    other <- s[!s$rule %in% names(marked), ]
    graphics::points(other$share, rep(i, nrow(other)), pch = "|",
                     col = ink[["muted"]], cex = 0.9)
    for (r in intersect(names(marked), s$rule)) {
      graphics::points(s$share[s$rule == r], i, pch = pch[[r]],
                       bg = marked[[r]], col = ink[["surface"]],
                       cex = 1.5, lwd = 1.5)
    }
  }
  present <- intersect(names(marked), d$rule)
  rule_labs <- c(first = "first touch", last = "last touch", markov = "Markov")
  .mm_legend("bottomright", c(rule_labs[present], "other rules"),
             col = c(rep(ink[["surface"]], length(present)), ink[["muted"]]),
             pch = c(pch[present], 124),
             pt.bg = c(marked[present], NA), pt.cex = 1.3)
  main <- .mm_arg(dots, "main", "Share of conversions by attribution rule")
  graphics::title(main = main, col.main = ink[["primary"]], font.main = 1,
                  cex.main = 1, adj = 0, line = 2)
  .mm_axis_titles(.mm_arg(dots, "xlab", "Share of credited conversions"),
                  .mm_arg(dots, "ylab", ""))
  graphics::mtext("Bar: range across every rule. A long bar is a convention, not a measurement.",
                  side = 3, line = 0.6, adj = 0, cex = 0.75,
                  col = ink[["secondary"]])
  invisible(x)
}
