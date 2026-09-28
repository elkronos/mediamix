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

#' @keywords internal
#' @noRd
.mm_num <- function(v) {
  format(v, big.mark = ",", scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
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

# ---- carryover ------------------------------------------------------------------

#' Plot a carryover cross-validation profile
#'
#' The cross-validated error across the decay grid, with the minimum (filled)
#' and the one-standard-error choice (open) marked. A flat profile is the
#' finding: the data does not pin carryover down, however precise `best`
#' looks.
#'
#' @param x An object from [tune_carryover()] or [tune_carryover_joint()].
#' @param truth Optional known decay (a number, or for the joint version a
#'   vector named by channel), drawn as a muted reference line. Useful on
#'   simulated data.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
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
  op <- .mm_par()
  on.exit(graphics::par(op), add = TRUE)
  r <- x$results
  lags <- unique(r$max_lag)
  if (length(lags) > 8L) lags <- lags[seq_len(8L)]
  cols <- .mm_cat()[seq_along(lags)]
  yl <- range(r$metric[r$max_lag %in% lags], na.rm = TRUE)
  .mm_frame(range(r$decay), yl + c(-0.04, 0.08) * diff(yl),
            main = sprintf("Cross-validated %s by decay", x$metric),
            sub = "Filled: minimum. Open: one-standard-error choice.",
            xlab = "Decay", ylab = x$metric)
  .mm_truth_line(truth)
  for (i in seq_along(lags)) {
    s <- r[r$max_lag == lags[i], ]
    s <- s[order(s$decay), ]
    graphics::lines(s$decay, s$metric, col = cols[[i]], lwd = 2)
  }
  .mm_mark_choice(x$best, x$best_1se, col = cols[[match(x$best$max_lag, lags,
                                                        nomatch = 1L)]])
  if (length(lags) > 1L) {
    .mm_legend("topleft", paste("max_lag", format(lags)), col = cols, lwd = 2)
  }
  invisible(x)
}

#' @rdname plot_carryover
#' @export
plot.mm_carryover_joint <- function(x, truth = NULL, ...) {
  ch <- x$best$channel
  k <- length(ch)
  nc <- min(3L, k)
  op <- .mm_par(mfrow = c(ceiling(k / nc), nc), oma = c(0, 0, 1.5, 0))
  on.exit(graphics::par(op), add = TRUE)
  col <- .mm_cat()[["blue"]]
  for (c0 in ch) {
    p <- x$profiles[x$profiles$channel == c0, ]
    p <- p[order(p$decay), ]
    yl <- range(p$metric, na.rm = TRUE)
    if (diff(yl) == 0) yl <- yl + c(-1, 1)
    .mm_frame(range(p$decay), yl + c(-0.05, 0.12) * diff(yl), main = c0,
              xlab = "Decay", ylab = "")
    if (!is.null(truth) && c0 %in% names(truth)) .mm_truth_line(truth[[c0]])
    graphics::lines(p$decay, p$metric, col = col, lwd = 2)
    .mm_mark_choice(x$best[x$best$channel == c0, ],
                    x$best_1se[x$best_1se$channel == c0, ], col = col,
                    metric_of = p)
  }
  graphics::mtext(sprintf("Cross-validated %s by channel decay, others held at their selected values",
                          x$metric_name),
                  outer = TRUE, side = 3, adj = 0, cex = 0.85,
                  col = .mm_ink()[["secondary"]])
  invisible(x)
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

#' @keywords internal
#' @noRd
.mm_mark_choice <- function(best, one_se, col, metric_of = NULL) {
  ink <- .mm_ink()
  get_m <- function(row) {
    if (!is.null(row$metric)) return(row$metric)
    metric_of$metric[match(row$decay, metric_of$decay)]
  }
  mb <- get_m(best)
  graphics::points(best$decay, mb, pch = 21, bg = col, col = ink[["surface"]],
                   cex = 1.6, lwd = 2)
  graphics::text(best$decay, mb, format(best$decay), pos = 3, cex = 0.75,
                 col = ink[["secondary"]], offset = 0.7)
  if (!is.null(one_se) && nrow(one_se) == 1L && one_se$decay != best$decay) {
    m1 <- get_m(one_se)
    graphics::points(one_se$decay, m1, pch = 21, bg = ink[["surface"]],
                     col = col, cex = 1.5, lwd = 2)
    graphics::text(one_se$decay, m1, format(one_se$decay), pos = 3, cex = 0.75,
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
#' @param ... Ignored.
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
  op <- .mm_par(mar = c(4, 7, 3, 1))
  on.exit(graphics::par(op), add = TRUE)
  level <- attr(x, "level")
  if (is.null(level)) level <- 0.9
  x <- as.data.frame(x)
  x <- x[order(x$estimate), ]
  k <- nrow(x)
  xl <- range(c(x$lower, x$upper, reference), na.rm = TRUE)
  ink <- .mm_ink()
  col <- .mm_cat()[["blue"]]
  graphics::plot.new()
  graphics::plot.window(xlim = xl + c(-0.03, 0.03) * diff(xl),
                        ylim = c(0.5, k + 0.5))
  graphics::abline(v = pretty(xl), col = ink[["grid"]], lwd = 0.8)
  .mm_xaxis()
  graphics::axis(2, at = seq_len(k), labels = x$term, las = 1, lwd = 0,
                 col.axis = ink[["secondary"]], cex.axis = 0.85)
  if (!is.null(reference)) {
    graphics::abline(v = reference, col = ink[["muted"]], lwd = 1.2)
  }
  graphics::segments(x$lower, seq_len(k), x$upper, seq_len(k), col = col,
                     lwd = 2)
  graphics::points(x$estimate, seq_len(k), pch = 21, bg = col,
                   col = ink[["surface"]], cex = 1.5, lwd = 2)
  graphics::title(main = sprintf("Estimates with %d%% block-bootstrap intervals",
                                 round(100 * level)),
                  col.main = ink[["primary"]], font.main = 1, cex.main = 1,
                  adj = 0, line = 1.2)
  .mm_axis_titles(xlab)
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
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
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
  x <- as.data.frame(x)
  base_total <- sum(x$contribution[x$channel == "(baseline)"])
  m <- x[x$channel != "(baseline)", ]
  ch <- unique(m$channel)
  if (length(ch) > 8L) {
    cli::cli_abort("Plot at most 8 channels; fold the smallest into one column.")
  }
  cols <- stats::setNames(.mm_cat()[seq_along(ch)], ch)
  ink <- .mm_ink()
  sub <- sprintf("Baseline (not shown): %s in total, %.0f%% of the prediction.",
                 format(round(base_total), big.mark = ","),
                 100 * base_total / sum(x$contribution))
  if (type == "total") {
    op <- .mm_par(mar = c(4, 7, 3.4, 1))
    on.exit(graphics::par(op), add = TRUE)
    tot <- sort(tapply(m$contribution, m$channel, sum))
    k <- length(tot)
    xl <- range(c(0, tot))
    graphics::plot.new()
    graphics::plot.window(xlim = xl * c(1, 1.12), ylim = c(0.4, k + 0.6))
    graphics::abline(v = pretty(xl), col = ink[["grid"]], lwd = 0.8)
    graphics::rect(0, seq_len(k) - 0.28, tot, seq_len(k) + 0.28,
                   col = .mm_cat()[["blue"]], border = ink[["surface"]], lwd = 2)
    .mm_xaxis(at = pretty(xl))
    graphics::axis(2, at = seq_len(k), labels = names(tot), las = 1, lwd = 0,
                   col.axis = ink[["secondary"]], cex.axis = 0.85)
    graphics::text(tot, seq_len(k), format(round(tot), big.mark = ","),
                   pos = 4, cex = 0.75, col = ink[["secondary"]], xpd = TRUE)
    graphics::title(main = "Total contribution by channel",
                    col.main = ink[["primary"]], font.main = 1, cex.main = 1,
                    adj = 0, line = 2)
    .mm_axis_titles("Contribution")
    graphics::mtext(sub, side = 3, line = 0.6, adj = 0, cex = 0.75,
                    col = ink[["secondary"]])
    return(invisible(x))
  }
  op <- .mm_par(mar = c(3, 5.2, 3.4, 1))
  on.exit(graphics::par(op), add = TRUE)
  per <- unique(m$period)
  mat <- vapply(ch, function(c0) {
    v <- tapply(m$contribution[m$channel == c0], m$period[m$channel == c0], sum)
    as.numeric(v[as.character(per)])
  }, numeric(length(per)))
  mat[!is.finite(mat)] <- 0
  xs <- if (inherits(per, "Date") || inherits(per, "POSIXct")) per else
    seq_along(per)
  cum <- t(apply(mat, 1, cumsum))
  if (length(ch) == 1L) cum <- matrix(cum, ncol = 1L)
  yl <- range(c(0, cum))
  .mm_frame(range(as.numeric(xs)), yl * c(1, 1.05),
            main = "Media contribution by period", sub = sub,
            xaxt = FALSE, xlab = "", ylab = "Contribution")
  if (inherits(xs, "Date")) {
    graphics::axis.Date(1, xs, col = ink[["axis"]], col.axis = ink[["muted"]],
                        lwd = 0.8, cex.axis = 0.85)
  } else {
    .mm_xaxis()
  }
  lower <- rep(0, length(per))
  for (j in seq_along(ch)) {
    upper <- cum[, j]
    graphics::polygon(c(as.numeric(xs), rev(as.numeric(xs))),
                      c(upper, rev(lower)), col = cols[[j]],
                      border = ink[["surface"]], lwd = 1.5)
    lower <- upper
  }
  .mm_legend("topleft", rev(ch), col = rev(cols), pch = 15, pt.cex = 1.4,
             horiz = FALSE, ncol = min(length(ch), 5L))
  invisible(x)
}

#' Plot a response curve
#'
#' Two stacked panels on a shared spend axis: the response and its marginal
#' return. Spend beyond the observed range is shaded, because there the curve
#' is extrapolation.
#'
#' @param x A data frame from [response_curve()].
#' @param observed Optional numeric vector of the spend levels actually seen
#'   (for example the adstocked series); its range is left unshaded.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' rc <- response_curve(seq(0, 6000, by = 100), coefficient = 6000,
#'                      half_max = 2250, shape = 1.6)
#' plot(rc, observed = c(0, 3450))
#' @export
plot.mm_response_curve <- function(x, observed = NULL, ...) {
  op <- .mm_par(mfrow = c(2, 1), mar = c(2.2, 5.2, 2.6, 1))
  on.exit(graphics::par(op), add = TRUE)
  ink <- .mm_ink()
  col <- .mm_cat()[["blue"]]
  x <- as.data.frame(x)
  xl <- range(x$spend)
  shade <- function(yl) {
    if (is.null(observed)) return(invisible(NULL))
    hi <- max(observed, na.rm = TRUE)
    if (hi < xl[2]) {
      graphics::rect(hi, yl[1], xl[2], yl[2], col = "#f0efec", border = NA)
      graphics::text(hi, yl[2], " extrapolation", adj = c(0, 1.4), cex = 0.75,
                     col = ink[["muted"]])
    }
  }
  yl <- range(c(0, x$response))
  .mm_frame(xl, yl, main = "Response", ylab = "Response", xaxt = FALSE)
  shade(yl)
  graphics::abline(h = pretty(yl), col = ink[["grid"]], lwd = 0.8)
  graphics::lines(x$spend, x$response, col = col, lwd = 2)
  graphics::par(mar = c(4, 5.2, 2.2, 1))
  yl2 <- range(c(0, x$marginal))
  .mm_frame(xl, yl2, main = "Marginal return per unit of spend",
            xlab = "Spend", ylab = "Marginal")
  shade(yl2)
  graphics::abline(h = pretty(yl2), col = ink[["grid"]], lwd = 0.8)
  graphics::lines(x$spend, x$marginal, col = col, lwd = 2)
  pk <- which.max(x$marginal)
  if (pk > 1L && pk < nrow(x)) {
    graphics::points(x$spend[pk], x$marginal[pk], pch = 21, bg = col,
                     col = ink[["surface"]], cex = 1.5, lwd = 2)
    graphics::text(x$spend[pk], x$marginal[pk], "steepest", pos = 4,
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
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' plot(attribute(paths))
#' @export
plot.mm_attribution <- function(x, n = 10L, ...) {
  x <- as.data.frame(x)
  n <- .mm_check_count(n, "n", min = 1L)
  mean_share <- sort(tapply(x$share, x$channel, mean))
  keep <- utils::tail(names(mean_share), n)
  k <- length(keep)
  op <- .mm_par(mar = c(4, 8, 3.4, 1))
  on.exit(graphics::par(op), add = TRUE)
  ink <- .mm_ink()
  xl <- range(c(0, x$share[x$channel %in% keep]))
  graphics::plot.new()
  graphics::plot.window(xlim = xl, ylim = c(0.5, k + 0.5))
  graphics::abline(v = pretty(xl), col = ink[["grid"]], lwd = 0.8)
  graphics::axis(1, at = pretty(xl), labels = paste0(round(100 * pretty(xl)), "%"),
                 col = ink[["axis"]], col.axis = ink[["muted"]], lwd = 0.8,
                 cex.axis = 0.85)
  graphics::axis(2, at = seq_len(k), labels = keep, las = 1, lwd = 0,
                 col.axis = ink[["secondary"]], cex.axis = 0.85)
  marked <- c(first = "blue", last = "orange", markov = "aqua")
  pch <- c(first = 21, last = 24, markov = 23)
  for (i in seq_len(k)) {
    s <- x[x$channel == keep[i], ]
    graphics::segments(min(s$share), i, max(s$share), i, col = "#d6d5ce",
                       lwd = 6, lend = 1)
    other <- s[!s$rule %in% names(marked), ]
    graphics::points(other$share, rep(i, nrow(other)), pch = "|",
                     col = ink[["muted"]], cex = 0.9)
    for (r in intersect(names(marked), s$rule)) {
      graphics::points(s$share[s$rule == r], i, pch = pch[[r]],
                       bg = .mm_cat()[[marked[[r]]]], col = ink[["surface"]],
                       cex = 1.5, lwd = 1.5)
    }
  }
  present <- intersect(names(marked), x$rule)
  labs <- c(first = "first touch", last = "last touch", markov = "Markov")
  .mm_legend("bottomright", c(labs[present], "other rules"),
             col = c(rep(ink[["surface"]], length(present)), ink[["muted"]]),
             pch = c(pch[present], 124),
             pt.bg = c(.mm_cat()[marked[present]], NA), pt.cex = 1.3)
  graphics::title(main = "Share of conversions by attribution rule",
                  col.main = ink[["primary"]], font.main = 1, cex.main = 1,
                  adj = 0, line = 2)
  .mm_axis_titles("Share of credited conversions")
  graphics::mtext("Bar: range across every rule. A long bar is a convention, not a measurement.",
                  side = 3, line = 0.6, adj = 0, cex = 0.75,
                  col = ink[["secondary"]])
  invisible(x)
}
