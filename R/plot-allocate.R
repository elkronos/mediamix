#' Plot a budget allocation
#'
#' Two panels with one row per channel, largest recommended spend at the top.
#' The first joins each channel's current spend (open) to its recommended
#' spend (filled); the second shows the change in the channel's predicted
#' response between the two, gains and losses in different colours.
#'
#' @param x An `mm_allocation` object from [allocate_budget()].
#' @param ... Graphical arguments: `main` replaces the title, and `col` gives
#'   two colours, the first for recommended spend and response gains, the
#'   second for response losses. Others are ignored.
#'
#' @return `x`, invisibly.
#'
#' @seealso [allocate_budget()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' truth <- attr(mm_weekly, "truth")
#' channels <- c("tv", "search", "display")
#' spec <- lapply(stats::setNames(channels, channels), function(ch) list(
#'   adstock = list(decay = truth$decay[[ch]]),
#'   saturation = list(half_max = truth$half_max[[ch]],
#'                     shape = truth$shape[[ch]])
#' ))
#' plot(allocate_budget(north, spec, truth$beta, steps = 50))
#' @export
plot.mm_allocation <- function(x, ...) {
  a <- x$allocation
  .mm_plot_needs(a, c("channel", "current_spend", "recommended_spend",
                      "current_response", "recommended_response"),
                 "allocate_budget", "x$allocation")
  dots <- list(...)
  pal <- unname(rep_len(.mm_arg(dots, "col", mm_palette(2)), 2L))
  ink <- mm_palette(role = "ink")
  a <- a[order(a$recommended_spend, a$current_spend), , drop = FALSE]
  k <- nrow(a)
  y <- seq_len(k)
  change <- a$recommended_response - a$current_response
  spend_lim <- .mm_lim(c(0, a$current_spend, a$recommended_spend),
                       c(0, 0.04), "spend values")
  if (!any(is.finite(change))) {
    cli::cli_abort("{.arg x} has no finite response changes to plot.")
  }

  op <- .mm_par(mfrow = c(1L, 1L), oma = c(0, 0, 3.4, 0))
  on.exit(graphics::par(op), add = TRUE)
  labs <- .mm_fit_labels(a$channel)
  line <- graphics::par("csi") * graphics::par("mex")
  left <- c(labs$lines, 1)
  right <- c(1, 1.2)
  room <- graphics::par("din")[1L] - sum(left + right) * line
  graphics::layout(matrix(1:2, 1L),
                   widths = (left + right) * line + max(room / 2, 0.5))
  panel_title <- function(text) {
    graphics::title(main = text, col.main = ink[["primary"]], font.main = 1,
                    cex.main = 0.95, adj = 0, line = 0.6)
  }
  ticks <- function(lim) {
    at <- pretty(lim)
    at[at >= lim[1L] & at <= lim[2L]]
  }

  ylim <- c(0.5, k + 0.5)

  # Spend: one row per channel, current (open) joined to recommended (filled).
  # The legend sits under the panel, centred on it, where the spend axis
  # title would go; the panel title already names the axis.
  graphics::par(mar = c(4, left[[1L]], 2, right[[1L]]))
  graphics::plot.new()
  graphics::plot.window(xlim = spend_lim, ylim = ylim)
  at <- ticks(spend_lim)
  graphics::abline(v = at, col = ink[["grid"]], lwd = 0.8)
  .mm_xaxis(at = at)
  graphics::axis(2, at = y, labels = labs$labels, las = 1, lwd = 0,
                 col.axis = ink[["secondary"]], cex.axis = 0.85)
  graphics::segments(a$current_spend, y, a$recommended_spend, y,
                     col = ink[["axis"]], lwd = 2)
  graphics::points(a$current_spend, y, pch = 21, bg = ink[["surface"]],
                   col = pal[[1L]], cex = 1.5, lwd = 2)
  graphics::points(a$recommended_spend, y, pch = 21, bg = pal[[1L]],
                   col = ink[["surface"]], cex = 1.75, lwd = 2)
  usr <- graphics::par("usr")
  per_line <- line * diff(usr[3:4]) / graphics::par("pin")[2L]
  .mm_legend(graphics::grconvertX(0.5, "nfc", "user"),
             c("current", "recommended"), y = usr[3L] - 2.7 * per_line,
             xjust = 0.5, yjust = 0.5, horiz = TRUE, xpd = NA,
             col = c(pal[[1L]], ink[["surface"]]), pch = 21,
             pt.bg = c(ink[["surface"]], pal[[1L]]), pt.cex = c(1.3, 1.5),
             pt.lwd = 2, text.width = NA)
  panel_title("Total spend")

  # Response change: a bar from zero, its value at the tip. The limits leave
  # room for the widest value label on each side of zero.
  graphics::par(mar = c(4, left[[2L]], 2, right[[2L]]))
  graphics::plot.new()
  shown <- is.finite(change)
  value <- .mm_short(change)
  value <- ifelse(change > 0, paste0("+", value), value)
  lo <- min(0, change, na.rm = TRUE)
  hi <- max(0, change, na.rm = TRUE)
  if (hi == lo) hi <- 1
  room_for <- function(side) {
    labels <- value[shown & side]
    if (length(labels) == 0L) return(0)
    inches <- max(graphics::strwidth(labels, units = "inches", cex = 0.75)) +
      graphics::strwidth("m", units = "inches", cex = 0.75)
    inches / graphics::par("pin")[1L]
  }
  fl <- room_for(change < 0)
  fr <- room_for(change > 0)
  squeeze <- min(1, 0.7 / max(fl + fr, 1e-9))
  fl <- fl * squeeze
  fr <- fr * squeeze
  span <- (hi - lo) / (1 - fl - fr)
  change_lim <- c(lo - fl * span, hi + fr * span)
  graphics::plot.window(xlim = change_lim, ylim = ylim)
  at <- ticks(change_lim)
  graphics::abline(v = at, col = ink[["grid"]], lwd = 0.8)
  graphics::abline(v = 0, col = ink[["axis"]], lwd = 1)
  .mm_xaxis(at = at)
  fill <- ifelse(change >= 0, pal[[1L]], pal[[2L]])
  graphics::rect(0, y[shown] - 0.22, change[shown], y[shown] + 0.22,
                 col = fill[shown], border = ink[["surface"]], lwd = 2)
  graphics::text(change[shown], y[shown], value[shown],
                 pos = ifelse(change[shown] < 0, 2, 4), cex = 0.75,
                 col = ink[["secondary"]], xpd = TRUE)
  panel_title("Change in response")
  # Long channel names can leave this panel too narrow for its axis title,
  # which is then left out rather than cut off at the edge of the device.
  xlab <- "Recommended minus current"
  if (graphics::strwidth(xlab, units = "inches", cex = 0.9) <=
      graphics::par("pin")[1L] + 2 * min(left[[2L]], right[[2L]]) * line) {
    .mm_axis_titles(xlab, "")
  }

  before <- sum(a$current_response)
  gain <- sum(a$recommended_response) - before
  pct <- if (is.finite(before) && before != 0) {
    sprintf(" (%+.1f%%)", 100 * gain / abs(before))
  } else ""
  spent <- sum(a$current_spend)
  budget <- if (isTRUE(abs(sum(a$recommended_spend) - spent) <=
                       1e-9 * max(1, spent))) {
    "The same total budget"
  } else {
    sprintf("A total budget of %s, against %s now",
            .mm_short(sum(a$recommended_spend)), .mm_short(spent))
  }
  sign <- if (is.finite(gain) && gain > 0) "+" else ""
  sub <- sprintf("%s; predicted media response %s%s%s.", budget, sign,
                 .mm_short(gain), pct)
  graphics::mtext(.mm_arg(dots, "main", "Budget allocation"), outer = TRUE,
                  side = 3, line = 1.7, adj = 0, cex = 1,
                  col = ink[["primary"]])
  graphics::mtext(sub, outer = TRUE, side = 3, line = 0.5, adj = 0, cex = 0.8,
                  col = ink[["secondary"]])
  invisible(x)
}
