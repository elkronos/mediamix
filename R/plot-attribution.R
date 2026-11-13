#' Plot attribution shares with intervals or by segment
#'
#' One row per channel, with each rule's share as a dot in that rule's colour.
#' When [attribute()] computed bootstrap intervals, each dot sits on a line
#' spanning its interval, so the gap between rules can be read against the
#' sampling noise in each. A result of `attribute(by = )` gets one panel per
#' segment, on a shared axis.
#'
#' @param x A result of [attribute()] with `times` above 0, with `by`, or
#'   with both.
#' @param n Show the `n` channels with the largest mean share.
#' @param ... Graphical arguments: `main` and `xlab` replace the defaults, and
#'   `col` gives one colour per rule, in the order the rules appear in `x`.
#'   Others are ignored.
#'
#' @return `x`, invisibly.
#'
#' @details
#' A rule keeps its colour whichever rules ran, and first touch, last touch
#' and Markov have the colours [plot.mm_attribution()] gives them. Channels
#' are ranked by their mean share over every rule and segment drawn. With
#' more than four segments the first four are drawn, with a message; subset
#' the rows of `x` to choose others. A missing share is left out with a
#' warning, and a missing bound leaves out its line.
#'
#' @seealso [attribute()], [plot.mm_attribution()] for a result with neither
#'   intervals nor segments.
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' boot <- attribute(paths, rules = c("first", "last", "linear", "markov"),
#'                   times = 50, seed = 1)
#' plot(boot, n = 6)
#'
#' # One panel per segment
#' ev <- mm_events
#' ev$device <- ifelse(grepl("[02468]$", ev$customer_id), "mobile", "desktop")
#' kept <- build_paths(ev, id = "customer_id", channel = "channel",
#'                     timestamp = "timestamp", conversion = "conversion",
#'                     keep = "device")
#' plot(attribute(kept, rules = c("first", "last"), by = "device"), n = 6)
#' @export
plot.mm_attribution_intervals <- function(x, n = 10L, ...) {
  .mm_plot_rule_shares(x, n, ...)
}

#' @rdname plot.mm_attribution_intervals
#' @export
plot.mm_attribution_segments <- function(x, n = 10L, ...) {
  .mm_plot_rule_shares(x, n, ...)
}

# ---- internals ---------------------------------------------------------------

# Each rule's share per channel, dodged within the channel's row, with its
# interval when `x` has one, in one panel per segment.
#' @keywords internal
#' @noRd
.mm_plot_rule_shares <- function(x, n = 10L, ...) {
  d <- as.data.frame(x)
  .mm_plot_needs(d, c("rule", "channel", "share"), "attribute")
  n <- .mm_check_count(n, "n", min = 1L)
  dots <- list(...)
  d$rule <- as.character(d$rule)
  d$channel <- as.character(d$channel)
  miss <- !is.finite(d$share)
  if (any(miss)) {
    cli::cli_warn(c(
      "{sum(miss)} share{?s} {?is/are} missing and left out of the plot.",
      i = "Channel{?s} affected: {.val {unique(d$channel[miss])}}."
    ))
    d <- d[!miss, , drop = FALSE]
    if (nrow(d) == 0L) cli::cli_abort("{.arg x} has no share to plot.")
  }
  has_ci <- all(c("lower", "upper") %in% names(d))
  by <- attr(x, "segments")
  by <- by[by %in% names(d)]
  if (length(by) > 0L) {
    key <- .mm_paste_key(.mm_plain_cols(d, by))
    segs <- unique(key)
    if (length(segs) > 4L) {
      cli::cli_inform(c(
        "The plot has {length(segs)} segments; the first 4 are drawn.",
        i = "Subset the rows of {.arg x} to choose others."
      ))
      segs <- segs[seq_len(4L)]
      d <- d[key %in% segs, , drop = FALSE]
      key <- key[key %in% segs]
    }
    seg_labels <- vapply(.mm_split_key(segs), function(v) {
      paste0(by, ": ", v, collapse = ", ")
    }, character(1))
  } else {
    key <- rep("", nrow(d))
    segs <- ""
    seg_labels <- NULL
  }

  channels <- unique(d$channel)
  mean_share <- vapply(channels, function(c0) mean(d$share[d$channel == c0]),
                       numeric(1))
  keep <- utils::tail(channels[order(mean_share, method = "radix")], n)
  shown <- d$channel %in% keep
  d <- d[shown, , drop = FALSE]
  key <- key[shown]
  rules <- unique(d$rule)
  cols <- .mm_rule_colours(rules, dots[["col"]])
  k <- length(keep)
  n_rules <- length(rules)
  half <- min(0.3, 0.1 * (n_rules - 1L))
  offset <- if (n_rules == 1L) 0 else seq(half, -half, length.out = n_rules)

  ink <- .mm_ink()
  vals <- c(0, d$share, if (has_ci) c(d$lower, d$upper))
  xl <- .mm_lim(vals, c(0, 0.04), "shares")
  at <- pretty(xl)
  at <- at[at >= xl[1L] - 1e-12 & at <= xl[2L] + 1e-12]
  n_seg <- length(segs)
  op <- .mm_par(mfrow = c(1L, 1L), oma = c(0, 0, 3.6, 0))
  on.exit(graphics::par(op), add = TRUE)
  labs <- .mm_fit_labels(keep)
  line <- graphics::par("csi") * graphics::par("mex")
  left <- c(labs$lines, rep(1, n_seg - 1L))
  right <- 1
  room <- graphics::par("din")[1L] - sum(left + right) * line
  widths <- (left + right) * line + max(room / n_seg, 0.5)
  graphics::layout(matrix(seq_len(n_seg), 1L), widths = widths)
  xlab <- .mm_arg(dots, "xlab", "Share of credited conversions")

  for (j in seq_len(n_seg)) {
    graphics::par(mar = c(4, left[[j]], if (n_seg > 1L) 2 else 0.8, right))
    s <- d[key == segs[[j]], , drop = FALSE]
    graphics::plot.new()
    graphics::plot.window(xlim = xl, ylim = c(0.5, k + 0.5))
    graphics::abline(v = at, col = ink[["grid"]], lwd = 0.8)
    if (k > 1L) {
      graphics::abline(h = seq_len(k - 1L) + 0.5, col = ink[["grid"]],
                       lwd = 0.8, lty = 3)
    }
    .mm_xaxis(at = at, labels = .mm_pct(at))
    if (j == 1L) {
      graphics::axis(2, at = seq_len(k), labels = labs$labels, las = 1,
                     lwd = 0, col.axis = ink[["secondary"]], cex.axis = 0.85)
    }
    for (i in seq_len(n_rules)) {
      r <- s[s$rule == rules[[i]], , drop = FALSE]
      y <- match(r$channel, keep) + offset[[i]]
      if (has_ci) {
        graphics::segments(r$lower, y, r$upper, y, col = cols[[i]], lwd = 1.8,
                           lend = 1)
      }
      graphics::points(r$share, y, pch = 21, bg = cols[[i]],
                       col = ink[["surface"]], cex = 1.15, lwd = 1)
    }
    if (n_seg > 1L) {
      graphics::title(main = seg_labels[[j]], col.main = ink[["primary"]],
                      font.main = 1, cex.main = 0.9, adj = 0, line = 0.6)
    }
    .mm_axis_titles(xlab, "")
    if (j == n_seg) {
      .mm_legend("bottomright", .mm_rule_labels(rules), col = ink[["surface"]],
                 pch = 21, pt.bg = cols, pt.cex = 1.2)
    }
  }

  # A column subset drops the level and times attributes, so the titles
  # describe the intervals without them when they are gone.
  level <- attr(x, "level")
  times <- attr(x, "times")
  main <- .mm_arg(dots, "main", if (has_ci) {
    paste0("Share of conversions by rule, with ",
           if (is.numeric(level) && length(level) == 1L) {
             paste0(format(round(100 * level, 1)), "% ")
           } else {
             "bootstrap "
           },
           "intervals")
  } else {
    "Share of conversions by rule and segment"
  })
  sub <- if (has_ci) {
    paste0("Dot: share on the journeys observed. Line: percentile interval",
           if (is.numeric(times) && length(times) == 1L) {
             sprintf(" from %d resamples of journeys", as.integer(times))
           },
           ".")
  } else {
    "Dot: share of the segment's credited conversions under each rule."
  }
  graphics::mtext(main, outer = TRUE, side = 3, line = 1.9, adj = 0, cex = 1,
                  col = ink[["primary"]])
  graphics::mtext(sub, outer = TRUE, side = 3, line = 0.7, adj = 0, cex = 0.75,
                  col = ink[["secondary"]])
  invisible(x)
}

# One colour per rule, fixed to the rule rather than to its position. First
# touch, last touch and Markov match plot.mm_attribution(); the other rules
# take the next colours of the palette in order.
#' @keywords internal
#' @noRd
.mm_rule_colours <- function(rules, col = NULL) {
  if (!is.null(col)) return(unname(rep_len(col, length(rules))))
  pal <- .mm_cat()
  fixed <- c(first = pal[["blue"]], last = pal[["orange"]],
             markov = pal[["aqua"]], linear = pal[["yellow"]],
             position = pal[["magenta"]], time_decay = pal[["green"]])
  out <- unname(fixed[rules])
  other <- is.na(out)
  if (any(other)) {
    out[other] <- rep_len(unname(pal[c("violet", "red")]), sum(other))
  }
  out
}

#' @keywords internal
#' @noRd
.mm_rule_labels <- function(rules) {
  names <- c(first = "first touch", last = "last touch", markov = "Markov",
             linear = "linear", position = "position", time_decay = "time decay")
  out <- unname(names[rules])
  out[is.na(out)] <- rules[is.na(out)]
  out
}
