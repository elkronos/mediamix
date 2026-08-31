#' Assign conversion credit across a journey
#'
#' Five rules for dividing one conversion among the touchpoints that preceded
#' it, plus a hook for your own. Each returns the journey table with a `credit`
#' column added, so they compose and can be compared side by side.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param first_weight,last_weight Share of credit reserved for the first and
#'   last touch in `credit_position()`. Must be non-negative and sum to at most
#'   1; the remainder is split evenly among the middle touches.
#' @param decay Geometric decay coefficient in `[0, 1)` for
#'   `credit_time_decay()`. Use [decay_from_half_life()] to express it as a
#'   half-life. The default is a seven-period half-life.
#' @param period Time span, in the journey table's time units, over which
#'   `decay` applies once. See [decay_from_half_life()].
#' @param fn For `credit_custom()`, a function of
#'   `(touch_rank, touch_n, recency)` returning a numeric vector of weights the
#'   same length as its inputs. `recency` is time from each touch to the
#'   conversion, in the journey table's units -- see the `units` argument of
#'   [build_paths()], which sets them. `fn` is called once per *converting*
#'   journey and never for a non-converting one, so `recency` is always
#'   present and the weights need no missing-value handling. Weights must be
#'   finite and non-negative; returning all zeros declines the journey.
#' @param normalise For `credit_custom()`, should weights be rescaled to sum to
#'   1 within each journey? Defaults to `TRUE`. Setting it to `FALSE` breaks the
#'   guarantee that total credit equals total conversions, and is only sensible
#'   when `fn` already returns weights that sum to 1.
#'
#' @return The input `mm_paths` object with a numeric `credit` column added, and
#'   a `credit_value` column when conversion values are present. Credit is `0`
#'   on every touch of a non-converting journey, and sums to 1 within each
#'   converting journey.
#'
#'   The five built-in rules always assign positive weight somewhere, so for
#'   them total credit always equals the number of converting journeys.
#'   `credit_custom()` can decline a journey by returning all-zero weights --
#'   see its entry under *Custom rules*.
#'
#' @details
#' `credit_first()` and `credit_last()` are the two defaults most reporting
#' systems ship with, and they disagree with each other by design: comparing
#' them is the cheapest read on whether a channel opens journeys or closes them.
#' [attribute()] runs several rules at once for exactly this reason.
#'
#' `credit_position()` gives the first and last touch a fixed share and splits
#' the rest evenly. Two-touch journeys are a genuine edge case, since there are
#' no middle touches to receive the middle weight; here the middle share is
#' divided between the two touches rather than discarded, so credit still sums
#' to 1 and short journeys are not quietly under-counted.
#'
#' `credit_time_decay()` is [adstock_geometric()] run backwards. Adstock takes
#' one impulse of spend and spreads its effect forward in time with geometric
#' decay; time-decay attribution takes one conversion and spreads its credit
#' backward across prior touchpoints with the same geometric kernel. Same
#' arithmetic, opposite arrow, and the same `decay` vocabulary in both
#' directions.
#'
#' @section Custom rules:
#' `credit_custom()` takes a function of `(touch_rank, touch_n, recency)` and is
#' called once per converting journey, with vectors as long as that journey.
#' Non-converting journeys are never passed to it; they always get zero.
#'
#' A rule that qualifies only some touches -- "credit only touches within a day
#' of conversion", say -- will meet journeys where *nothing* qualifies. When
#' `fn` returns all zeros for a journey, that journey receives no credit at all
#' and is counted in a message. Crediting its touches equally instead would
#' invent an answer the rule never gave, and it is a surprisingly easy way to
#' hand a channel thousands of conversions it was never eligible for. The cost
#' is that total credit is then *below* the number of converting journeys, by
#' exactly the number of declined journeys.
#'
#' @section What credit is not:
#' These rules divide credit; they do not measure contribution. A rule cannot
#' tell you what would have happened if a channel had not run, because that
#' outcome is not in the log. Use them for consistent bookkeeping and for
#' comparing channels' roles, and use experiments for incrementality.
#'
#' @seealso [attribute()], [build_paths()], [adstock_geometric()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion",
#'                      value = "value")
#'
#' lin <- credit_linear(paths)
#' head(lin[, c("path_id", "channel", "touch_rank", "touch_n", "credit")])
#'
#' # Credit sums to 1 within every converting journey
#' conv <- lin[lin$converted, ]
#' per_journey <- as.numeric(tapply(conv$credit, conv$path_id, sum))
#' all.equal(per_journey, rep(1, length(per_journey)))
#'
#' # Time decay with a three-day half-life
#' td <- credit_time_decay(paths, decay = decay_from_half_life(3))
#'
#' # Your own rule: credit only touches within a day of conversion. Journeys
#' # whose every touch is older than that qualify for nothing, and are reported
#' # rather than being credited equally.
#' recent_only <- credit_custom(paths, function(rank, n, recency) {
#'   as.numeric(recency <= 1)
#' })
#' c(credited = sum(recent_only$credit),
#'   converting = path_summary(paths)$converting_journeys)
#' @name credit
NULL

#' @rdname credit
#' @export
credit_linear <- function(paths) {
  .mm_check_paths(paths)
  .mm_assign_credit(paths, function(rank, n, recency) rep(1 / n[1L], length(rank)))
}

#' @rdname credit
#' @export
credit_first <- function(paths) {
  .mm_check_paths(paths)
  .mm_assign_credit(paths, function(rank, n, recency) as.numeric(rank == 1L))
}

#' @rdname credit
#' @export
credit_last <- function(paths) {
  .mm_check_paths(paths)
  .mm_assign_credit(paths, function(rank, n, recency) as.numeric(rank == n))
}

#' @rdname credit
#' @export
credit_position <- function(paths, first_weight = 0.4, last_weight = 0.4) {
  .mm_check_paths(paths)
  first_weight <- .mm_check_scalar(first_weight, "first_weight", lower = 0, upper = 1)
  last_weight <- .mm_check_scalar(last_weight, "last_weight", lower = 0, upper = 1)
  if (first_weight + last_weight > 1) {
    cli::cli_abort(c(
      "{.arg first_weight} and {.arg last_weight} must sum to at most 1.",
      x = "They sum to {first_weight + last_weight}."
    ))
  }
  middle <- 1 - first_weight - last_weight

  .mm_assign_credit(paths, function(rank, n, recency) {
    nn <- n[1L]
    if (nn == 1L) return(rep(1, length(rank)))
    if (nn == 2L) {
      return(ifelse(rank == 1L,
                    first_weight + middle / 2,
                    last_weight + middle / 2))
    }
    out <- rep(middle / (nn - 2L), length(rank))
    out[rank == 1L] <- first_weight
    out[rank == nn] <- last_weight
    out
  })
}

#' @rdname credit
#' @export
credit_time_decay <- function(paths, decay = decay_from_half_life(7),
                              period = 1) {
  .mm_check_paths(paths)
  decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1,
                            inclusive = c(TRUE, FALSE))
  period <- .mm_check_scalar(period, "period", lower = 0,
                             inclusive = c(FALSE, TRUE))

  .mm_assign_credit(paths, function(rank, n, recency) {
    k <- length(rank)
    if (anyNA(recency)) return(rep(1 / k, k))
    if (decay == 0) {
      # No carryover at all: everything goes to the touch at the moment of
      # conversion. `0^0` is 1 and `0^positive` is 0, which is exactly this.
      w <- as.numeric(recency <= 0)
      return(if (sum(w) > 0) w / sum(w) else c(rep(0, k - 1L), 1))
    }
    # Normalise in log space. Evaluating decay^(recency/period) directly
    # underflows to all-zero once recency/period passes a few hundred -- which
    # a journey table in seconds reaches easily -- and an all-zero kernel
    # cannot be normalised into the geometric weights the rule specifies.
    lw <- (recency / period) * log(decay)
    lw <- lw - max(lw)
    w <- exp(lw)
    w / sum(w)
  }, normalise = FALSE)
}

#' @rdname credit
#' @export
credit_custom <- function(paths, fn, normalise = TRUE) {
  .mm_check_paths(paths)
  if (!is.function(fn)) cli::cli_abort("{.arg fn} must be a function.")
  .mm_check_flag(normalise, "normalise")
  .mm_assign_credit(paths, fn, normalise = normalise, user_supplied = TRUE)
}

# ---- internals ---------------------------------------------------------------

#' @keywords internal
#' @noRd
.mm_assign_credit <- function(paths, fn, normalise = TRUE,
                              user_supplied = FALSE, call = parent.frame(2)) {
  if (nrow(paths) == 0L) {
    paths$credit <- numeric(0)
    paths$credit_value <- numeric(0)
    return(paths)
  }

  cred <- numeric(nrow(paths))
  declined <- 0L
  idx <- split(seq_len(nrow(paths)), paths$path_id)
  rank <- paths$touch_rank
  n <- paths$touch_n
  rec <- paths$time_to_conversion
  converted <- paths$converted

  for (i in idx) {
    if (!converted[i[1L]]) next
    w <- fn(rank[i], n[i], rec[i])
    if (user_supplied) {
      if (!is.numeric(w) || length(w) != length(i)) {
        cli::cli_abort(c(
          "{.arg fn} must return a numeric vector as long as the journey.",
          x = "Journey {.val {paths$path_id[i[1L]]}} has {length(i)} touch{?es} \\
               but {.arg fn} returned {length(w)} value{?s}."
        ), call = call)
      }
      if (anyNA(w) || any(w < 0) || any(!is.finite(w))) {
        cli::cli_abort(c(
          "{.arg fn} must return finite, non-negative, non-missing weights.",
          i = "Journey {.val {paths$path_id[i[1L]]}} produced \\
               {.val {utils::head(w, 4)}}."
        ), call = call)
      }
    }
    if (normalise) {
      total <- sum(w)
      if (is.finite(total) && total > 0) {
        w <- w / total
      } else {
        # No touch in this journey qualified under the rule. Crediting them
        # equally would invent an answer the rule did not give; the honest
        # result is no credit, counted and reported.
        w <- rep(0, length(i))
        declined <- declined + 1L
      }
    }
    cred[i] <- w
  }

  if (declined > 0L) {
    cli::cli_inform(c(
      "{declined} converting journey{?s} received no credit: the rule assigned \\
       zero weight to every touch.",
      i = "Those conversions are not counted in any channel's total.",
      i = "Total credit is {round(sum(cred), 4)}, against \\
           {length(idx) - sum(!vapply(idx, function(j) paths$converted[j[1L]], \\
           logical(1)))} converting journey{?s}."
    ))
  }
  paths$credit <- cred
  attr(paths, "mm_credit_declined") <- declined
  if (!all(is.na(paths$conversion_value))) {
    v <- paths$conversion_value
    v[is.na(v)] <- 0
    paths$credit_value <- cred * v
  }
  paths
}
