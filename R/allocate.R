#' Allocate a budget across channels
#'
#' Splits a total budget across the channels of a plan so that the media
#' response the model predicts is as large as possible. Each channel's whole
#' series is rescaled, so its flighting is kept, and each channel's total
#' spend stays within its bounds.
#'
#' @inheritParams simulate_plan
#' @param total Total spend to allocate across the channels in `spec`.
#'   Defaults to their total in `plan`, which asks how the current budget
#'   would best be split. It must lie between the sum of `lower` and the sum
#'   of `upper`.
#' @param lower,upper Bounds on each channel's total spend: a numeric vector
#'   named by channel, or one number for every channel. A channel not named
#'   gets `0` and `Inf`.
#' @param steps Number of equal increments that the budget above the lower
#'   bounds is divided into, at most 10,000. More steps give a finer answer
#'   and take longer: the number of transforms grows with `steps` times the
#'   number of channels, and the search with `steps` squared.
#'
#' @return An `mm_allocation` object, a list with:
#'   \describe{
#'     \item{`allocation`}{A data frame with one row per channel in `spec`, in
#'       its order, and columns `channel`, `current_spend`,
#'       `recommended_spend`, `current_response`, `recommended_response` and
#'       `mroi`. Response is as [simulate_plan()] defines it. `mroi` is the
#'       marginal return at the recommended spend, computed as
#'       [marginal_roi()] does: the extra response from a 1% increase in that
#'       spend, per unit of the increase. It is `NA` where the recommended
#'       spend is zero.}
#'     \item{`plan`}{The recommended plan: `plan` with each channel's column
#'       rescaled to its recommended total.}
#'     \item{`total`, `step`, `steps`}{The budget allocated, the size of one
#'       increment and the number of increments.}
#'   }
#'   It has a `print()` method, `as.data.frame()` and `tidy()` methods that
#'   return `allocation`, and a [plot()][plot.mm_allocation] method.
#'
#' @details
#' Each channel's response is computed at its lower bound and at every
#' multiple of the step above it, up to its upper bound, by rescaling the
#' channel's whole series. Dynamic programming then picks the combination of
#' levels, one per channel, that spends `total` exactly with the largest
#' total response.
#'
#' The result is the best allocation on that grid for curves of any shape,
#' S-shaped Hill curves (`shape > 1`) included. Such a curve returns little
#' on its first increments, so a search that hands out the budget one
#' increment at a time, each to the channel whose next increment returns
#' most, can leave the channel unfunded however well it pays further up.
#' Comparing every level of every channel avoids that. Nothing between grid
#' points is searched, so the answer is only as fine as the step; rerun with
#' a larger `steps` to check that it has settled. Where every curve is
#' concave and no bound binds, the channels' `mroi` values come out close to
#' equal; a channel held at a bound can have a higher or a lower one.
#'
#' The whole of `total` is spent, since the question is how to split a given
#' budget. An `mroi` below 1 says the last part of a channel's budget returns
#' less than it costs, and a smaller `total` may then be better.
#'
#' The answer is only as good as the fitted curves. A curve is identified
#' only over the spend levels the data contains, so a recommendation that
#' takes a channel far beyond the spend it has run at is extrapolation; test
#' a large move before making it (see [marginal_roi()]).
#'
#' A channel with no spend in `plan` has no flighting to rescale, so it stays
#' at zero, with a message, and a lower bound above zero for it is an error.
#' To consider it, give it small weights in the periods it should run, for
#' example `plan$video <- 1e-6` for an even spread.
#'
#' @seealso [plot.mm_allocation()], [simulate_plan()], [reallocate()],
#'   [marginal_roi()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' truth <- attr(mm_weekly, "truth")
#' channels <- c("search", "social", "display")
#' spec <- lapply(stats::setNames(channels, channels), function(ch) list(
#'   adstock = list(decay = truth$decay[[ch]]),
#'   saturation = list(half_max = truth$half_max[[ch]],
#'                     shape = truth$shape[[ch]])
#' ))
#'
#' # The same budget, split to maximise the predicted response
#' best <- allocate_budget(north, spec, truth$beta, steps = 100)
#' best
#'
#' # Keep every channel within 25% of its current spend
#' now <- colSums(north[channels])
#' allocate_budget(north, spec, truth$beta, lower = 0.75 * now,
#'                 upper = 1.25 * now, steps = 100)
#'
#' plot(best)
#' @export
allocate_budget <- function(plan, spec, coefficients, total = NULL,
                            lower = NULL, upper = NULL, by = NULL,
                            steps = 200L, extend = 0L) {
  here <- environment()
  .mm_check_plan(plan)
  .mm_note_dplyr_groups(plan, by, "allocate_budget", arg = "plan")
  channels <- .mm_check_spec(spec)
  .mm_check_channels(plan, channels, values = TRUE)
  cf <- .mm_plan_coefficients(coefficients, channels)
  key <- .mm_plan_by(by, plan, channels)
  steps <- .mm_check_bounded_count(steps, "steps", min = 1L, max = 10000L)
  extend <- .mm_check_bounded_count(extend, "extend", min = 0L, max = 1e6)
  x <- lapply(stats::setNames(channels, channels),
              function(ch) as.numeric(plan[[ch]]))
  current <- vapply(x, sum, numeric(1))
  if (is.null(total)) total <- sum(current)
  total <- .mm_check_scalar(total, "total", lower = 0)
  lower <- .mm_check_bound(lower, "lower", channels, 0)
  upper <- .mm_check_bound(upper, "upper", channels, Inf)
  crossed <- channels[lower > upper]
  if (length(crossed) > 0L) {
    cli::cli_abort("{.arg upper} is below {.arg lower} for {.val {crossed}}.")
  }

  dark <- channels[current == 0]
  if (length(dark) > 0L) {
    forced <- dark[lower[dark] > 0]
    if (length(forced) > 0L) {
      cli::cli_abort(c(
        "{.val {forced}} {?has/have} no spend in {.arg plan}, so there is no \\
         flighting to rescale to {?its/their} lower bound.",
        i = "Give {?it/them} a pattern first: small weights in the periods \\
             {?it/they} should run, for example \\
             {.code plan${forced[[1L]]} <- 1e-6} for an even spread."
      ))
    }
    cli::cli_inform(c(
      "{.val {dark}} {?has/have} no spend in {.arg plan}, so {?it stays/they \\
       stay} at zero.",
      i = "{.fn allocate_budget} rescales each channel's flighting, and an \\
           all-zero column has none."
    ))
    upper[dark] <- 0
  }
  if (total < sum(lower) || total > sum(upper)) {
    cli::cli_abort(c(
      "The bounds cannot hold a total of {signif(total, 6)}.",
      x = if (total < sum(lower)) {
        "The lower bounds add up to {signif(sum(lower), 6)}."
      } else {
        "The upper bounds add up to {signif(sum(upper), 6)}."
      }
    ))
  }

  tr <- lapply(stats::setNames(channels, channels), function(ch) {
    .mm_channel_transformer(spec[[ch]], ch, here)
  })
  response <- function(ch, level) {
    s <- if (current[[ch]] > 0) level / current[[ch]] else 0
    .mm_total_response(x[[ch]] * s, cf[[ch]], tr[[ch]], key, extend)
  }
  free <- total - sum(lower)
  step <- free / steps

  search <- function() {
    if (free == 0) return(lower)
    tops <- vapply(channels, function(ch) {
      as.integer(min(steps, floor((upper[[ch]] - lower[[ch]]) / step + 1e-9)))
    }, integer(1))
    grid <- lapply(channels, function(ch) {
      vapply(lower[[ch]] + (0:tops[[ch]]) * step,
             function(level) response(ch, level), numeric(1))
    })
    odd <- channels[!vapply(grid, function(r) all(is.finite(r)), logical(1))]
    if (length(odd) > 0L) {
      cli::cli_abort("The predicted response of {.val {odd}} is not finite \\
                      at every spend level searched.", call = here)
    }
    picks <- .mm_grid_allocate(grid, steps)
    if (!is.null(picks)) return(lower + picks * step)
    # The upper bounds leave less than one increment of room per channel, so
    # no point of the grid spends the total: every channel goes to its top
    # level and the remainder is shared out in proportion to the room left
    # under each bound.
    level <- lower + tops * step
    room <- upper - level
    level + if (sum(room) > 0) (total - sum(level)) * room / sum(room) else 0
  }

  # Each channel is transformed many times; a warning it gives is shown once.
  seen <- character(0)
  once <- function(w) {
    msg <- conditionMessage(w)
    if (msg %in% seen) invokeRestart("muffleWarning")
    seen <<- c(seen, msg)
  }
  lift <- 0.01
  withCallingHandlers({
    now <- vapply(channels, function(ch) response(ch, current[[ch]]),
                  numeric(1))
    recommended <- pmin(upper, pmax(lower, search()))
    then <- vapply(channels, function(ch) response(ch, recommended[[ch]]),
                   numeric(1))
    mroi <- vapply(channels, function(ch) {
      r <- recommended[[ch]]
      if (r <= 0) return(NA_real_)
      (response(ch, r * (1 + lift)) - then[[ch]]) / (lift * r)
    }, numeric(1))
  }, warning = once)

  live <- channels[current > 0]
  new_plan <- .mm_set_cols(plan, lapply(stats::setNames(live, live), function(ch) {
    x[[ch]] * (recommended[[ch]] / current[[ch]])
  }))
  allocation <- data.frame(
    channel = channels,
    current_spend = unname(current),
    recommended_spend = unname(recommended),
    current_response = unname(now),
    recommended_response = unname(then),
    mroi = unname(mroi),
    stringsAsFactors = FALSE
  )
  structure(
    list(allocation = allocation, plan = new_plan, total = total,
         step = step, steps = steps),
    class = "mm_allocation"
  )
}

#' @export
print.mm_allocation <- function(x, ...) {
  a <- x$allocation
  show <- function(v) {
    formatC(signif(v, 6), format = "fg", digits = 6, big.mark = ",")
  }
  cli::cli_h3("Budget allocation")
  cli::cli_text("A total of {show(x$total)}, in {x$steps} increment{?s} of \\
                 {show(x$step)}.")
  cli::cli_text("Predicted media response: {show(sum(a$current_response))} \\
                 now, {show(sum(a$recommended_response))} recommended.")
  print(as.data.frame(x), digits = 4, row.names = FALSE)
  cli::cli_text("{.emph The recommended plan is in $plan.}")
  invisible(x)
}

#' @export
as.data.frame.mm_allocation <- function(x, row.names = NULL, optional = FALSE,
                                        ...) {
  out <- x$allocation
  if (!is.null(row.names)) rownames(out) <- row.names
  out
}

#' @exportS3Method generics::tidy
tidy.mm_allocation <- function(x, ...) {
  as.data.frame(x)
}

# Bounds on each channel's total spend, named by channel: `default` for a
# channel not named, or one number for every channel.
#' @keywords internal
#' @noRd
.mm_check_bound <- function(b, arg, channels, default, call = parent.frame()) {
  out <- stats::setNames(rep(default, length(channels)), channels)
  if (is.null(b)) return(out)
  if (!is.numeric(b) || length(b) == 0L || anyNA(b)) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector named by channel, \\
                    or one number for every channel.", call = call)
  }
  nms <- names(b)
  if (is.null(nms)) {
    if (length(b) != 1L) {
      cli::cli_abort(c(
        "{.arg {arg}} has {length(b)} values but no names.",
        i = "Name each value after its channel, or give one number for \\
             every channel."
      ), call = call)
    }
    out[] <- b
  } else {
    if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms) > 0L) {
      cli::cli_abort("The names of {.arg {arg}} must be distinct channel \\
                      names.", call = call)
    }
    unknown <- setdiff(nms, channels)
    if (length(unknown) > 0L) {
      cli::cli_abort("{.arg {arg}} names {cli::qty(length(unknown))}{?a \\
                      channel/channels} not in {.arg spec}: \\
                      {.val {unknown}}.", call = call)
    }
    out[nms] <- b
  }
  if (any(out < 0)) {
    cli::cli_abort("{.arg {arg}} must not be negative.", call = call)
  }
  if (any(is.infinite(out)) && arg == "lower") {
    cli::cli_abort("{.arg lower} must be finite.", call = call)
  }
  out
}

# The best point of the grid. `r[[c]][j + 1]` is channel c's response with j
# increments above its lower bound. Returns the number of increments for each
# channel, adding up to `steps`, with the largest summed response, or NULL
# when the channels together cannot take `steps` increments. Ties go to the
# fewest increments for the later channel, so the answer is deterministic.
#' @keywords internal
#' @noRd
.mm_grid_allocate <- function(r, steps) {
  k <- length(r)
  top <- vapply(r, function(v) min(length(v) - 1L, steps), numeric(1))
  value <- rep(-Inf, steps + 1L)
  first <- seq_len(top[[1L]] + 1L)
  value[first] <- r[[1L]][first]
  pick <- vector("list", k)
  for (c in seq_len(k)[-1L]) {
    rc <- r[[c]]
    next_value <- rep(-Inf, steps + 1L)
    choice <- rep(NA_integer_, steps + 1L)
    for (b in 0:steps) {
      j <- 0:min(b, top[[c]])
      v <- value[b - j + 1L] + rc[j + 1L]
      best <- which.max(v)
      if (length(best) == 1L && v[[best]] > -Inf) {
        next_value[[b + 1L]] <- v[[best]]
        choice[[b + 1L]] <- j[[best]]
      }
    }
    value <- next_value
    pick[[c]] <- choice
  }
  if (!is.finite(value[[steps + 1L]])) return(NULL)
  out <- integer(k)
  b <- steps
  for (c in rev(seq_len(k))) {
    j <- if (c == 1L) b else pick[[c]][[b + 1L]]
    out[[c]] <- j
    b <- b - j
  }
  out
}
