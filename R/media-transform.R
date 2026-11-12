#' Adstock then saturate, in that order
#'
#' Composes a carryover transform with a saturation curve to produce a single
#' model-ready regressor. The point of the function is the *order*: adstock
#' first, saturation second.
#'
#' @param x Numeric vector of media spend in time order.
#' @param adstock A named list of arguments for the carryover step, including a
#'   `kernel` element of `"geometric"` (the default), `"weibull"`,
#'   `"delayed"` or `"none"`. Remaining elements are passed to
#'   [adstock_geometric()], [adstock_weibull()] or [adstock_delayed()]. The list
#'   may carry `by` in place of the `by` argument, but not both, and it may not
#'   carry `x`.
#'
#'   The default applies geometric carryover at `decay = 0.5`, including when
#'   only `saturation` is given. Pass `adstock = NULL`, or
#'   `list(kernel = "none")`, for saturation alone.
#' @param saturation A named list of arguments for the saturation step,
#'   including a `type` element of `"hill"`, `"exponential"`,
#'   `"michaelis_menten"`, `"power"` or `"none"`. Remaining elements are passed
#'   to the corresponding `saturate_*()` function -- for `"hill"` that means
#'   `half_max`, which has no default because a sensible value depends entirely
#'   on the scale of your spend.
#'
#'   Saturation defaults to `"none"`, so calling `media_transform()` with only
#'   an `adstock` argument applies carryover alone. `saturation = NULL` does
#'   the same.
#' @param by Optional grouping vector, or data frame of grouping vectors, the
#'   same length as `x`.
#' @param order Transform order. `"adstock_then_saturate"` is the convention and
#'   the default. `"saturate_then_adstock"` is permitted but warns, because it
#'   is nearly always a mistake rather than a choice.
#'
#' @return A numeric vector the same length as `x`, in the same order, with the
#'   names and time-series attributes of `x`.
#'
#' @details
#' The order is not arbitrary. Saturation represents a ceiling on what a given
#' weight of media can achieve in a period. Applying it after adstock caps the
#' *total media pressure* in each period, carryover included, which is what a
#' saturation curve is meant to mean. Applying it before adstock puts each
#' period's spend through the curve in isolation, so carryover from earlier
#' periods never counts towards saturation. With the default normalised
#' kernel the weights sum to one and the result stays under the ceiling; with
#' `normalise = FALSE` they sum to more than one, and carryover also
#' accumulates the saturated values past the ceiling you specified.
#'
#' This function will do it backwards if you insist, but it will not do it
#' backwards quietly.
#'
#' @seealso [adstock_geometric()], [saturate()], and [transform_plan()] to
#'   transform every channel of a plan, each with its own parameters
#'
#' @examples
#' spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
#'
#' # Carryover only -- saturation is off by default
#' round(media_transform(spend, adstock = list(decay = 0.5)), 4)
#'
#' # Carryover and saturation together, in the conventional order
#' round(media_transform(
#'   spend,
#'   adstock = list(decay = decay_from_half_life(2)),
#'   saturation = list(half_max = 300)
#' ), 4)
#'
#' # Saturation alone needs the carryover switched off explicitly
#' round(media_transform(spend, adstock = NULL,
#'                       saturation = list(half_max = 300)), 4)
#'
#' # A delayed-peak kernel with an S-shaped response
#' round(media_transform(
#'   spend,
#'   adstock = list(kernel = "weibull", shape = 2, scale = 2, max_lag = 6,
#'                  type = "pdf"),
#'   saturation = list(type = "hill", half_max = 300, shape = 2)
#' ), 4)
#'
#' # Grouped: geo-level panels transform within each geography
#' spend_panel <- c(100, 50, 25, 200, 100, 50)
#' geo <- rep(c("north", "south"), each = 3)
#' round(media_transform(spend_panel, adstock = list(decay = 0.5),
#'                       by = geo), 4)
#' @export
media_transform <- function(x,
                            adstock = list(kernel = "geometric", decay = 0.5),
                            saturation = list(type = "none"),
                            by = NULL,
                            order = c("adstock_then_saturate",
                                      "saturate_then_adstock")) {
  order <- match.arg(order)
  x_in <- x
  x <- .mm_check_numeric(x, "x")
  if (is.null(adstock)) adstock <- list(kernel = "none")
  if (is.null(saturation)) saturation <- list(type = "none")
  if (!is.list(adstock)) {
    cli::cli_abort("{.arg adstock} must be a list or {.code NULL}.")
  }
  if (!is.list(saturation)) {
    cli::cli_abort("{.arg saturation} must be a list or {.code NULL}.")
  }
  if ("x" %in% names(adstock) || "x" %in% names(saturation)) {
    cli::cli_abort(c(
      "The {.arg adstock} and {.arg saturation} lists must not contain \\
       {.code x}.",
      i = "{.fn media_transform} passes its own {.arg x} to both steps."
    ))
  }
  # The adstock list's other elements go to the adstock function, which takes
  # `by`, so a `by` given there is honoured; given in both places it is
  # ambiguous.
  if (!is.null(adstock[["by"]])) {
    if (!is.null(by)) {
      cli::cli_abort(c(
        "{.arg by} is given twice, as an argument and inside {.arg adstock}.",
        i = "Give it once."
      ))
    }
    by <- adstock[["by"]]
  }
  adstock[["by"]] <- NULL
  # Validate `by` here as well as inside the adstock call, so that a mismatched
  # grouping vector is still caught when the carryover step is "none".
  .mm_check_by(by, length(x))

  if (order == "saturate_then_adstock") {
    why <- if (isFALSE(adstock[["normalise"]])) {
      "The convention is adstock first. With {.code normalise = FALSE} the \\
       kernel weights sum to more than one, so carryover accumulates the \\
       saturated values past the ceiling."
    } else {
      "The convention is adstock first. Saturating first applies the curve \\
       to each period's spend rather than to the accumulated media pressure, \\
       so carryover from earlier periods does not count towards saturation."
    }
    cli::cli_warn(c(
      "Applying saturation before adstock.",
      i = why,
      i = "Set {.code order = \"adstock_then_saturate\"} unless you mean this."
    ))
  }

  ad_step <- function(v) .mm_do_adstock(v, adstock, by)
  sat_step <- function(v) .mm_do_saturate(v, saturation)

  out <- if (order == "adstock_then_saturate") {
    sat_step(ad_step(x))
  } else {
    ad_step(sat_step(x))
  }
  .mm_like_input(out, x_in)
}

#' @keywords internal
#' @noRd
.mm_do_adstock <- function(x, args, by) {
  kernel <- if (is.null(args[["kernel"]])) "geometric" else args[["kernel"]]
  if (!.mm_is_string(kernel) ||
      !kernel %in% c("geometric", "weibull", "delayed", "none")) {
    cli::cli_abort(
      '{.arg adstock$kernel} must be one of "geometric", "weibull", \\
       "delayed" or "none".'
    )
  }
  if (kernel == "none") return(x)
  args[["kernel"]] <- NULL
  args[["by"]] <- by
  .mm_call_by_name(paste0("adstock_", kernel), c(list(x = x), args))
}

#' @keywords internal
#' @noRd
.mm_do_saturate <- function(x, args) {
  type <- if (is.null(args[["type"]])) "hill" else args[["type"]]
  valid <- c("hill", "exponential", "michaelis_menten", "power", "none")
  if (!.mm_is_string(type) || !type %in% valid) {
    cli::cli_abort("{.arg saturation$type} must be one of {.val {valid}}.")
  }
  if (type == "none") return(x)
  needed <- switch(type, hill = "half_max", exponential = "rate",
                   michaelis_menten = "km", power = "exponent")
  if (!needed %in% names(args)) {
    cli::cli_abort(c(
      'Saturation type {.val {type}} needs a {.arg {needed}} value.',
      i = "Add it to the {.arg saturation} list, for example \\
           {.code saturation = list(type = \"{type}\", {needed} = ...)}.",
      i = "There is no default: the right value depends on the scale of \\
           your media."
    ))
  }
  args[["type"]] <- NULL
  .mm_call_by_name(paste0("saturate_", type), c(list(x = x), args))
}

# Call an exported transform by name with each argument bound to a symbol, so
# an error names the function and its call stays short however long the data.
#' @keywords internal
#' @noRd
.mm_call_by_name <- function(name, args) {
  env <- new.env(parent = asNamespace("mediamix"))
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  syms <- ifelse(nzchar(nms), nms, paste0(".arg", seq_along(args)))
  for (i in seq_along(args)) assign(syms[[i]], args[[i]], envir = env)
  call <- as.call(c(list(as.name(name)),
                    stats::setNames(lapply(syms, as.name), nms)))
  eval(call, env)
}
