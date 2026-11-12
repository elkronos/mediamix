#' One adstock step per channel
#'
#' Adds one [step_adstock()] per media column, so that each channel gets its
#' own decay, or its own tuning parameter. A single step applies one set of
#' kernel parameters to every column it selects, and channels rarely share a
#' carryover: `mm_weekly` was simulated with a decay of 0.85 for television
#' and 0.15 for search.
#'
#' @param recipe A recipe object.
#' @param channels Character vector naming the media columns, one step each.
#' @param ... Named arguments for [step_adstock()], such as `index`, `by`,
#'   `decay` and `kernel`, applied to every channel. The kernel parameters
#'   `decay`, `max_lag`, `shape`, `scale` and `peak` also take one value per
#'   channel, as a vector or list named by channel:
#'   `decay = c(tv = 0.85, search = 0.15)`. Marking one of them with `tune()`
#'   gives each channel a tuning parameter of its own, with the ids
#'   `decay_tv`, `decay_search` and so on; `tune("carry")` gives `carry_tv`.
#'   A `tune()` without an id inside a list named by channel gets the same
#'   form of id.
#'
#' @return `recipe` with one `step_adstock()` added per channel, in the order
#'   of `channels`.
#'
#' @details
#' Each step selects its channel with `all_of()` and the channel's name as a
#' string, fixed when the step is added. A loop that writes `all_of(ch)`
#' selects the last channel in every step instead, since selectors are
#' evaluated at `prep()`, after the loop has finished. Step ids start with
#' `adstock_` and the channel's name, so `tidy()` on the recipe shows which
#' step is which.
#'
#' With `tune()`, `hardhat::extract_parameter_set_dials()` lists one parameter
#' per channel, and `tune::tune_grid()` and `tune::tune_bayes()` search them
#' together with the rest of the workflow's parameters.
#'
#' @seealso [step_adstock()] for the arguments, [carryover_decay()] for the
#'   tuning ranges.
#'
#' @examplesIf requireNamespace("recipes", quietly = TRUE)
#' library(recipes)
#' data(mm_weekly)
#'
#' # A fixed decay for each channel
#' rec <- recipe(revenue ~ ., data = mm_weekly) |>
#'   step_adstock_channels(c("tv", "search"), index = "date", by = "geo",
#'                         decay = c(tv = 0.85, search = 0.15)) |>
#'   prep()
#' tidy(rec)
#' tidy(rec, number = 2)
#' @examplesIf requireNamespace("recipes", quietly = TRUE) && requireNamespace("hardhat", quietly = TRUE) && requireNamespace("dials", quietly = TRUE)
#'
#' # One tuning parameter per channel
#' tuned <- recipe(revenue ~ ., data = mm_weekly) |>
#'   step_adstock_channels(c("tv", "search"), index = "date", by = "geo",
#'                         decay = hardhat::tune())
#' hardhat::extract_parameter_set_dials(tuned)
#' @export
step_adstock_channels <- function(recipe, channels, ...) {
  .mm_require_recipes()
  here <- environment()
  .mm_check_channels(channels)
  args <- list(...)
  .mm_check_channel_args(args)

  per_channel <- intersect(names(args), .mm_channel_params())
  split <- lapply(per_channel, function(p) {
    .mm_split_channel_arg(args[[p]], p, channels, call = here)
  })
  names(split) <- per_channel

  for (j in seq_along(channels)) {
    ch <- channels[[j]]
    step_args <- args
    for (p in per_channel) step_args[p] <- list(split[[p]][[j]])
    step_args$id <- .mm_rand_id(paste0("adstock_", ch))
    # An argument error names the channel whose step it came from.
    recipe <- withCallingHandlers(
      .mm_add_channel_step(recipe, ch, step_args),
      error = function(e) {
        cli::cli_abort("The step for channel {.val {ch}} could not be added.",
                       parent = e, call = here)
      }
    )
  }
  recipe
}

# ---- internals ---------------------------------------------------------------

# The step_adstock() arguments that may differ between channels: the kernel's
# parameters, which are also the tunable ones.
#' @keywords internal
#' @noRd
.mm_channel_params <- function() c("decay", "max_lag", "shape", "scale", "peak")

#' @keywords internal
#' @noRd
.mm_check_channels <- function(channels, call = parent.frame()) {
  if (!is.character(channels) || length(channels) == 0L) {
    cli::cli_abort(
      "{.arg channels} must be a character vector of column names, not \\
       {.obj_type_friendly {channels}}.",
      call = call
    )
  }
  if (anyNA(channels) || !all(nzchar(channels))) {
    cli::cli_abort("{.arg channels} must not contain missing or empty names.",
                   call = call)
  }
  dups <- unique(channels[duplicated(channels)])
  if (length(dups) > 0L) {
    cli::cli_abort(c(
      "{.arg channels} names {.val {dups}} more than once.",
      i = "Each channel gets one step."
    ), call = call)
  }
  invisible(channels)
}

# Everything in `...` goes to step_adstock() by name. A selector there would
# be taken for a column, and the fields prep() fills, the id and `skip` are
# set for each step here.
#' @keywords internal
#' @noRd
.mm_check_channel_args <- function(args, call = parent.frame()) {
  nms <- names(args)
  if (length(args) > 0L && (is.null(nms) || any(!nzchar(nms)))) {
    cli::cli_abort(c(
      "Every argument after {.arg channels} must be named.",
      i = "Name the media columns in {.arg channels}; the other arguments go \\
           to {.fn step_adstock}."
    ), call = call)
  }
  set_here <- c("trained", "columns", "states", "last_index", "history",
                "seeds", "skip", "id")
  allowed <- setdiff(names(formals(step_adstock)),
                     c("recipe", "...", set_here))
  reserved <- intersect(nms, set_here)
  if (length(reserved) > 0L) {
    cli::cli_abort(
      "{.arg {reserved}} {?is/are} set for each step by \\
       {.fn step_adstock_channels} or by {.fn prep}.",
      call = call
    )
  }
  unknown <- setdiff(nms, allowed)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "{.fn step_adstock} has no argument{?s} {.arg {unknown}}.",
      call = call
    )
  }
  invisible(args)
}

# One value of a per-channel argument for each channel, in the order of
# `channels`. A single value is shared; a vector or list named by channel is
# split; `tune()` becomes one placeholder per channel with its own id.
#' @keywords internal
#' @noRd
.mm_split_channel_arg <- function(value, arg, channels,
                                  call = parent.frame()) {
  n <- length(channels)
  if (.mm_is_tune(value)) {
    id <- .mm_tune_id(value)
    stem <- if (nzchar(id)) id else arg
    return(lapply(channels, function(ch) .mm_tune_call(paste0(stem, "_", ch))))
  }
  nms <- names(value)
  if (is.null(nms)) {
    if (is.null(value) || (length(value) == 1L && !is.list(value))) {
      return(rep(list(value), n))
    }
    cli::cli_abort(c(
      "{.arg {arg}} must be a single value, or one value per channel named \\
       by channel.",
      i = "For example {.code {arg} = c({channels[1]} = ...)}, with an \\
           element for each of {.val {channels}}."
    ), call = call)
  }
  missing_ch <- setdiff(channels, nms)
  unknown <- setdiff(nms, channels)
  if (length(missing_ch) > 0L || length(unknown) > 0L || anyDuplicated(nms)) {
    cli::cli_abort(c(
      "{.arg {arg}} must name each channel once.",
      x = if (length(missing_ch) > 0L) "Missing: {.val {missing_ch}}.",
      x = if (length(unknown) > 0L) "Not in {.arg channels}: {.val {unknown}}.",
      x = if (anyDuplicated(nms)) {
        "Named more than once: {.val {unique(nms[duplicated(nms)])}}."
      }
    ), call = call)
  }
  values <- as.list(value)[channels]
  lapply(seq_len(n), function(j) {
    v <- values[[j]]
    if (.mm_is_tune(v) && !nzchar(.mm_tune_id(v))) {
      return(.mm_tune_call(paste0(arg, "_", channels[[j]])))
    }
    v
  })
}

# The id inside a `tune()` placeholder, or "" when it has none.
#' @keywords internal
#' @noRd
.mm_tune_id <- function(x) {
  if (inherits(x, "quosure") || inherits(x, "formula")) x <- x[[length(x)]]
  if (length(x) >= 2L) as.character(x[[2L]]) else ""
}

# The call `hardhat::tune(id)` returns, built without needing hardhat.
#' @keywords internal
#' @noRd
.mm_tune_call <- function(id) as.call(list(as.name("tune"), id))

# Add one step_adstock() that selects `channel`. Each argument is bound to a
# name in an environment of its own, so a `tune()` call is passed as the value
# it is rather than evaluated, and the selector is a quosure that resolves to
# the column whatever happens to `channel` afterwards.
#' @keywords internal
#' @noRd
.mm_add_channel_step <- function(recipe, channel, args) {
  env <- new.env(parent = asNamespace("mediamix"))
  assign(".recipe", recipe, envir = env)
  for (nm in names(args)) assign(nm, args[[nm]], envir = env)
  selector <- rlang::new_quosure(rlang::call2("all_of", channel),
                                 env = baseenv())
  call <- as.call(c(
    list(as.name("step_adstock"), as.name(".recipe"), selector),
    stats::setNames(lapply(names(args), as.name), names(args))
  ))
  eval(call, env)
}
