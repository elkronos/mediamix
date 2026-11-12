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
#'   `share`, `value` when the journey table carries conversion values, and
#'   `p_without`, the probability of conversion once the channel is removed.
#'   Three attributes let you check the removal effects by hand:
#'   \describe{
#'     \item{`p_conversion`}{The baseline conversion probability.
#'       `removal_effect` is `1 - p_without / p_conversion`, or 0 when no
#'       journey converts.}
#'     \item{`transitions`}{The fitted first-order transition matrix, with one
#'       row for each state a walk can leave (`"(start)"` and every channel)
#'       and one column for each state it can enter (every channel,
#'       `"(conversion)"` and `"(null)"`). Each row sums to 1.}
#'     \item{`transition_counts`}{The transitions observed, in the same
#'       layout, from which `transitions` is estimated.}
#'   }
#'   A zero-row journey table gives a zero-row result without these
#'   attributes.
#'
#' @details
#' The probabilities come from the absorbing-chain fundamental matrix, so they
#' are exact for the fitted transition matrix rather than simulated: with
#' \eqn{Q} the transitions among transient states and \eqn{R} those into the
#' absorbing states, absorption probabilities are \eqn{(I - Q)^{-1} R}. One
#' inverse serves every channel. Once channel \eqn{s} is removed, a walk
#' converts only if it converts without reaching \eqn{s}, so with
#' \eqn{N = (I - Q)^{-1}} and \eqn{x = N R} the conversion probability is
#' \eqn{x_1 - (N_{1s} / N_{ss}) x_s}, where \eqn{N_{1s} / N_{ss}} is the
#' probability of ever reaching \eqn{s} from the start and \eqn{x_s} that of
#' converting from \eqn{s}. That probability is the `p_without` column.
#'
#' A channel named `"(start)"`, `"(conversion)"` or `"(null)"` shares its
#' label with a special state in `transitions`; index the matrix by position
#' in that case.
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
#' mk <- markov_removal(p)
#' mk
#'
#' # The fitted chain behind those numbers
#' attr(mk, "transitions")
#' attr(mk, "p_conversion")
#' 1 - mk$p_without / attr(mk, "p_conversion")
#'
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion",
#'                      value = "value")
#' markov_removal(paths)
#' @export
markov_removal <- function(paths) {
  paths <- .mm_check_paths(paths)
  .mm_markov_removal(paths)
}

# ---- internals ---------------------------------------------------------------

# markov_removal() on a checked journey table. With `warn = FALSE` a table
# without non-converting journeys is left for the caller to report.
#' @keywords internal
#' @noRd
.mm_markov_removal <- function(paths, warn = TRUE) {
  empty <- data.frame(channel = character(0), removal_effect = numeric(0),
                      conversions = numeric(0), share = numeric(0),
                      p_without = numeric(0), stringsAsFactors = FALSE)
  if (nrow(paths) == 0L) return(empty)

  p <- as.data.frame(paths)
  p <- p[order(p$path_id, p$touch_rank, method = "radix"), , drop = FALSE]
  chans <- sort(unique(p$channel), method = "radix")
  k <- length(chans)
  walks <- .mm_markov_walks(p, chans)
  counts <- .mm_markov_counts(walks$cell, k)

  first <- !duplicated(p$path_id)
  n_conv <- sum(first & p$converted)
  n_null <- sum(first & !p$converted)
  if (warn && n_null == 0L) {
    cli::cli_warn(c(
      "The journey table has no non-converting journeys.",
      i = "Without them the null state is never observed, so the baseline \\
           conversion probability is 1 and each removal effect moves up or \\
           down, depending on how the missing journeys differ.",
      i = "Build the paths with {.code keep_null_paths = TRUE}."
    ))
  }

  fit <- .mm_markov_fit(counts)
  base <- fit$base
  re <- if (base > 0) pmax(0, 1 - fit$p_without / base) else rep(0, k)
  share <- if (sum(re) > 0) re / sum(re) else rep(0, k)
  out <- data.frame(channel = chans, removal_effect = re,
                    conversions = n_conv * share, share = share,
                    stringsAsFactors = FALSE)
  if (n_conv > 0L && !all(is.na(p$conversion_value))) {
    v <- p$conversion_value[first & p$converted]
    out$value <- sum(v[!is.na(v)]) * share
  }
  out$p_without <- fit$p_without
  out <- out[order(-out$conversions, out$channel, method = "radix"), ,
             drop = FALSE]
  rownames(out) <- NULL

  states <- list(from = c("(start)", chans),
                 to = c(chans, "(conversion)", "(null)"))
  leave <- seq_len(k + 1L)
  # Nothing enters the start state, so its column is left out.
  probs <- fit$P[, -1L, drop = FALSE]
  observed <- counts[leave, -1L, drop = FALSE]
  dimnames(probs) <- states
  dimnames(observed) <- states
  attr(out, "p_conversion") <- base
  attr(out, "transitions") <- probs
  attr(out, "transition_counts") <- observed
  out
}

# Every transition the journeys make, in a chain whose states are 1 for the
# start, 2 to k + 1 for the channels in `chans`, k + 2 for conversion and
# k + 3 for null. `p` is sorted by journey and rank. `cell` is the
# transition's position in a (k + 3) x (k + 3) count matrix and `row` the
# row of `p` whose journey made it.
#' @keywords internal
#' @noRd
.mm_markov_walks <- function(p, chans) {
  k <- length(chans)
  code <- match(p$channel, chans) + 1L
  first <- !duplicated(p$path_id)
  last <- !duplicated(p$path_id, fromLast = TRUE)
  from <- c(rep(1L, sum(first)), code[!last], code[last])
  to <- c(code[first], code[-1L][!last[-length(last)]],
          ifelse(p$converted[last], k + 2L, k + 3L))
  list(cell = from + (to - 1L) * (k + 3L),
       row = c(which(first), which(!last), which(last)))
}

# The (k + 3) x (k + 3) matrix of transition counts. `times` repeats each
# transition, as a resample that draws its journey several times does.
#' @keywords internal
#' @noRd
.mm_markov_counts <- function(cell, k, times = NULL) {
  m <- k + 3L
  if (!is.null(times)) cell <- rep.int(cell, times)
  matrix(tabulate(cell, m * m), m, m)
}

# The baseline conversion probability, the probability once each channel is
# removed, and the transition matrix of the states a walk can leave, from a
# matrix of counts. A channel with no transitions out, which a resample can
# leave out, is not part of the chain: removing it changes nothing.
#' @keywords internal
#' @noRd
.mm_markov_fit <- function(counts) {
  k <- nrow(counts) - 3L
  conv_s <- k + 2L
  tr <- seq_len(k + 1L)
  rs <- rowSums(counts)[tr]
  P <- counts[tr, , drop = FALSE] / pmax(rs, 1)
  if (sum(counts[, conv_s]) == 0) {
    return(list(base = 0, p_without = rep(0, k), P = P))
  }
  # Every state a walk enters it also leaves, by a touch, a conversion or
  # nothing, so the start and each channel present have a non-empty row.
  live <- tr[rs > 0]
  N <- solve(diag(length(live)) - P[live, live, drop = FALSE])
  x <- drop(N %*% P[live, conv_s])
  base <- x[1L]
  # Removing channel s sends every transition into it to null, so the start
  # loses exactly the conversions that pass through s: the probability of
  # ever reaching s, N[1, s] / N[s, s], times the chance of converting from
  # there, x[s].
  s <- seq_along(live)[-1L]
  p_without <- rep(base, k)
  p_without[live[-1L] - 1L] <- base - N[1L, s] / N[cbind(s, s)] * x[s]
  list(base = base, p_without = p_without, P = P)
}
