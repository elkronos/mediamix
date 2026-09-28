#' Markov-chain removal effects
#'
#' Data-driven attribution from a first-order Markov model of the journey
#' table (Anderl, Becker, von Wangenheim and Schumann, 2016). Journeys are
#' treated as walks from a start state through channel states to one of two
#' absorbing states, conversion or null. A channel's *removal effect* is the
#' proportional drop in the probability of reaching conversion when every
#' transition into that channel is redirected to the null state; conversions
#' are then divided among channels in proportion to their removal effects.
#'
#' @param paths An `mm_paths` object from [build_paths()]. Keep the
#'   non-converting journeys (`keep_null_paths = TRUE`, the default): they are
#'   what the null state is estimated from.
#'
#' @return A data frame with one row per channel, ordered by descending
#'   `conversions`: `channel`, `removal_effect`, `conversions` (credited),
#'   `share`, and `value` when the journey table carries conversion values.
#'   The baseline conversion probability is attached as the attribute
#'   `"p_conversion"`.
#'
#' @details
#' The probabilities come from the absorbing-chain fundamental matrix, so they
#' are exact for the fitted transition matrix rather than simulated: with
#' \eqn{Q} the transitions among transient states and \eqn{R} those into the
#' absorbing states, absorption probabilities are \eqn{(I - Q)^{-1} R}.
#'
#' This is the order-1 model, with no dependency beyond base R. For
#' higher-order chains, or journey tables with millions of distinct paths,
#' hand the output of [as_channel_paths()] to \pkg{ChannelAttribution}, whose
#' `markov_model()` implements the same removal-effect definition in C++. The
#' two should agree for `order = 1`.
#'
#' Removal effects are sensitive to journey construction: repeats collapsed or
#' not, direct traffic kept or dropped, lookback length. That is the argument
#' for [build_paths()] making each an explicit choice.
#'
#' @section What this is not:
#' A removal effect is a statement about the fitted transition matrix, not
#' about what customers would do if a channel were switched off. Removing a
#' state from a chain assumes that everyone who would have passed through it
#' is lost, and that nobody reaches the same conversion another way. It is a
#' more data-driven convention than first- or last-touch, and it is still a
#' convention. Use it alongside the heuristic rules in [attribute()], where the
#' spread between them is the honest summary, and use experiments for
#' incrementality.
#'
#' @references
#' Anderl, E., Becker, I., von Wangenheim, F. and Schumann, J. H. (2016).
#' Mapping the customer journey: Lessons learned from graph-based online
#' attribution modeling. *International Journal of Research in Marketing*,
#' 33(3), 457--474. \doi{10.1016/j.ijresmar.2016.03.001}
#'
#' @seealso [attribute()], which includes this as the `"markov"` rule,
#'   [as_channel_paths()] for \pkg{ChannelAttribution}.
#'
#' @examples
#' # A three-journey example small enough to check by hand: P(conversion) is
#' # 2/3; removing "a" halves it, removing "b" makes conversion impossible.
#' ev <- data.frame(
#'   id = c("j1", "j1", "j2", "j3"),
#'   ch = c("a", "b", "a", "b"),
#'   ts = c(1, 2, 1, 1),
#'   conv = c(0, 1, 0, 1)
#' )
#' p <- build_paths(ev, id = "id", channel = "ch", timestamp = "ts",
#'                  conversion = "conv")
#' markov_removal(p)
#'
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion",
#'                      value = "value")
#' markov_removal(paths)
#' @export
markov_removal <- function(paths) {
  .mm_check_paths(paths)
  empty <- data.frame(channel = character(0), removal_effect = numeric(0),
                      conversions = numeric(0), share = numeric(0),
                      stringsAsFactors = FALSE)
  if (nrow(paths) == 0L) return(empty)

  p <- paths[order(paths$path_id, paths$touch_rank), , drop = FALSE]
  p <- as.data.frame(p)
  chans <- sort(unique(p$channel))
  k <- length(chans)
  # State indices: 1 = start, 2..k+1 = channels, k+2 = conversion, k+3 = null.
  conv_s <- k + 2L
  null_s <- k + 3L
  code <- match(p$channel, chans) + 1L

  first <- !duplicated(p$path_id)
  last <- !duplicated(p$path_id, fromLast = TRUE)
  from <- c(rep(1L, sum(first)), code[!last], code[last])
  to <- c(code[first], code[-1L][!last[-length(last)]],
          ifelse(p$converted[last], conv_s, null_s))
  counts <- matrix(0, k + 3L, k + 3L)
  agg <- stats::aggregate(list(n = rep(1, length(from))),
                          by = list(from = from, to = to), FUN = sum)
  counts[cbind(agg$from, agg$to)] <- agg$n

  n_conv <- sum(first & p$converted)
  n_null <- sum(first & !p$converted)
  if (n_null == 0L) {
    cli::cli_warn(c(
      "The journey table has no non-converting journeys.",
      i = "Without them the null state is never observed, and every removal \\
           effect is overstated.",
      i = "Build the paths with {.code keep_null_paths = TRUE}."
    ))
  }
  if (n_conv == 0L) {
    out <- data.frame(channel = chans, removal_effect = 0, conversions = 0,
                      share = 0, stringsAsFactors = FALSE)
    attr(out, "p_conversion") <- 0
    return(out)
  }

  p_conv <- function(removed = NULL) {
    cm <- counts
    if (!is.null(removed)) {
      # Everything that would have entered the removed state is lost.
      cm[, null_s] <- cm[, null_s] + cm[, removed]
      cm[, removed] <- 0
      cm[removed, ] <- 0
    }
    rs <- rowSums(cm)
    live <- which(rs[seq_len(k + 1L)] > 0)
    if (!1L %in% live) return(0)
    P <- cm[live, , drop = FALSE] / rs[live]
    Q <- P[, live, drop = FALSE]
    R <- P[, conv_s]
    b <- solve(diag(length(live)) - Q, R)
    unname(b[match(1L, live)])
  }

  base <- p_conv()
  re <- vapply(seq_len(k) + 1L, function(s) {
    if (base <= 0) return(0)
    max(0, 1 - p_conv(s) / base)
  }, numeric(1))
  share <- if (sum(re) > 0) re / sum(re) else rep(0, k)
  out <- data.frame(channel = chans, removal_effect = re,
                    conversions = n_conv * share, share = share,
                    stringsAsFactors = FALSE)
  if (!all(is.na(p$conversion_value))) {
    v <- p$conversion_value[first & p$converted]
    out$value <- sum(v[!is.na(v)]) * share
  }
  out <- out[order(-out$conversions, out$channel), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "p_conversion") <- base
  out
}
