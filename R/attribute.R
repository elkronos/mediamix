#' Attribute conversions to channels under several rules at once
#'
#' Runs a set of credit rules over the same journey table and returns their
#' results stacked, so the spread between rules is visible rather than hidden by
#' a choice of default. That spread is the most useful output of rule-based
#' attribution: it bounds how much a channel's apparent value depends on the
#' convention rather than the data.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param rules Character vector of rules to apply: any of `"linear"`,
#'   `"first"`, `"last"`, `"position"` and `"time_decay"`.
#' @param first_weight,last_weight Passed to [credit_position()].
#' @param decay,period Passed to [credit_time_decay()].
#'
#' @return A data frame with one row per rule and channel:
#'   \describe{
#'     \item{`rule`}{Which credit rule produced the row.}
#'     \item{`channel`}{Channel label.}
#'     \item{`conversions`}{Fractional conversions credited to the channel.}
#'     \item{`share`}{`conversions` as a share of all conversions credited
#'       under that rule. Shares sum to 1 within each rule.}
#'     \item{`value`}{Credited conversion value, present only when the journey
#'       table carries values.}
#'     \item{`touches`}{Number of touchpoints on the channel across all
#'       journeys, converting or not.}
#'   }
#'   Rows are ordered by rule, then by descending conversions.
#'
#' @seealso [credit_linear()] and friends, [attribution_spread()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion",
#'                      value = "value")
#'
#' res <- attribute(paths)
#' head(res, 10)
#'
#' # Total credited conversions equals observed converting journeys, per rule
#' tapply(res$conversions, res$rule, sum)
#'
#' # How much does the answer depend on the rule?
#' attribution_spread(res)
#' @export
attribute <- function(paths,
                      rules = c("linear", "first", "last", "position",
                                "time_decay"),
                      first_weight = 0.4, last_weight = 0.4,
                      decay = decay_from_half_life(7), period = 1) {
  .mm_check_paths(paths)
  rules <- match.arg(rules, c("linear", "first", "last", "position",
                              "time_decay"), several.ok = TRUE)

  if (nrow(paths) == 0L) {
    return(data.frame(rule = character(0), channel = character(0),
                      conversions = numeric(0), share = numeric(0),
                      touches = integer(0), stringsAsFactors = FALSE))
  }

  touch_tab <- table(paths$channel)
  touch_lab <- names(touch_tab)
  has_value <- !all(is.na(paths$conversion_value))

  out <- lapply(rules, function(r) {
    scored <- switch(
      r,
      linear = credit_linear(paths),
      first = credit_first(paths),
      last = credit_last(paths),
      position = credit_position(paths, first_weight, last_weight),
      time_decay = credit_time_decay(paths, decay = decay, period = period)
    )
    agg <- stats::aggregate(list(conversions = scored$credit),
                            by = list(channel = scored$channel), FUN = sum)
    total <- sum(agg$conversions)
    agg$share <- if (total > 0) agg$conversions / total else 0
    if (has_value) {
      av <- stats::aggregate(list(value = scored$credit_value),
                             by = list(channel = scored$channel), FUN = sum)
      agg <- merge(agg, av, by = "channel", sort = FALSE)
    }
    agg$touches <- as.integer(touch_tab[match(agg$channel, touch_lab)])
    agg$rule <- r
    agg[order(-agg$conversions), , drop = FALSE]
  })

  res <- do.call(rbind, out)
  front <- c("rule", "channel", "conversions", "share")
  res <- res[, c(front, setdiff(names(res), front)), drop = FALSE]
  rownames(res) <- NULL
  res
}

#' How much does the answer depend on the rule?
#'
#' Summarises the output of [attribute()] into one row per channel, showing the
#' range of credited conversions across rules. A channel whose share swings from
#' 8% to 31% depending on the convention has not been measured; it has been
#' assigned a number.
#'
#' @param attribution The data frame returned by [attribute()].
#'
#' @return A data frame with one row per channel, ordered by descending spread:
#'   `channel`, `min_share`, `max_share`, `mean_share`, `spread` (the difference
#'   between the first two), and -- only when both the `"first"` and `"last"`
#'   rules were run -- `first_last_ratio`.
#'
#'   Note that `first_last_ratio` here is a ratio of credited *shares*, which is
#'   not the same quantity as the column of the same name in
#'   [channel_positions()], a ratio of touchpoint *counts*. They answer the same
#'   question in different currencies and will not agree numerically.
#'
#' @seealso [attribute()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' attribution_spread(attribute(paths))
#' @export
attribution_spread <- function(attribution) {
  if (!is.data.frame(attribution) ||
      !all(c("rule", "channel", "share") %in% names(attribution))) {
    cli::cli_abort(c(
      "{.arg attribution} must be the data frame returned by {.fn attribute}.",
      i = "It needs {.field rule}, {.field channel} and {.field share} columns."
    ))
  }
  if (nrow(attribution) == 0L) {
    return(data.frame(channel = character(0), min_share = numeric(0),
                      max_share = numeric(0), mean_share = numeric(0),
                      spread = numeric(0), stringsAsFactors = FALSE))
  }
  ch <- unique(attribution$channel)
  agg <- function(f) vapply(ch, function(c0)
    f(attribution$share[attribution$channel == c0]), numeric(1))

  out <- data.frame(
    channel = ch,
    min_share = agg(min),
    max_share = agg(max),
    mean_share = agg(mean),
    stringsAsFactors = FALSE
  )
  out$spread <- out$max_share - out$min_share

  if (all(c("first", "last") %in% attribution$rule)) {
    # Matched by position, not by name: a channel label may legitimately be the
    # empty string, and `x[""]` never matches in R.
    pick <- function(rule) {
      sub <- attribution[attribution$rule == rule, , drop = FALSE]
      sub$share[match(ch, sub$channel)]
    }
    fs <- pick("first")
    ls <- pick("last")
    out$first_last_ratio <- ifelse(!is.na(ls) & ls > 0, fs / ls, NA_real_)
  }
  out <- out[order(-out$spread), , drop = FALSE]
  rownames(out) <- NULL
  out
}
