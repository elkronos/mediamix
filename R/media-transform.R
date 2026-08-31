#' Adstock then saturate, in that order
#'
#' Composes a carryover transform with a saturation curve to produce a single
#' model-ready regressor. The point of the function is the *order*: adstock
#' first, saturation second.
#'
#' @param x Numeric vector of media spend in time order.
#' @param adstock A named list of arguments for the carryover step, including a
#'   `kernel` element of `"geometric"` (the default), `"weibull"` or `"none"`.
#'   Remaining elements are passed to [adstock_geometric()] or
#'   [adstock_weibull()].
#' @param saturation A named list of arguments for the saturation step,
#'   including a `type` element of `"hill"`, `"exponential"`,
#'   `"michaelis_menten"`, `"power"` or `"none"`. Remaining elements are passed
#'   to the corresponding `saturate_*()` function -- for `"hill"` that means
#'   `half_max`, which has no default because a sensible value depends entirely
#'   on the scale of your spend.
#'
#'   Saturation defaults to `"none"`, so calling `media_transform()` with only
#'   an `adstock` argument applies carryover alone.
#' @param by Optional grouping vector, or data frame of grouping vectors, the
#'   same length as `x`.
#' @param order Transform order. `"adstock_then_saturate"` is the convention and
#'   the default. `"saturate_then_adstock"` is permitted but warns, because it
#'   is nearly always a mistake rather than a choice.
#'
#' @return A numeric vector the same length as `x`, in the same order.
#'
#' @details
#' The order is not arbitrary. Saturation represents a ceiling on what a given
#' weight of media can achieve in a period. Applying it before adstock caps each
#' period's spend in isolation and then lets carryover accumulate those capped
#' values past the cap, so the composed transform is no longer bounded by the
#' ceiling you specified. Applying it after adstock caps the *total media
#' pressure* in each period, which is what a saturation curve is meant to mean.
#'
#' This function will do it backwards if you insist, but it will not do it
#' backwards quietly.
#'
#' @seealso [adstock_geometric()], [saturate()]
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
  x <- .mm_check_numeric(x, "x")
  if (!is.list(adstock)) cli::cli_abort("{.arg adstock} must be a list.")
  if (!is.list(saturation)) cli::cli_abort("{.arg saturation} must be a list.")
  # Validate `by` here as well as inside the adstock call, so that a mismatched
  # grouping vector is still caught when the carryover step is "none".
  .mm_check_by(by, length(x))

  if (order == "saturate_then_adstock") {
    cli::cli_warn(c(
      "Applying saturation before adstock.",
      i = "The convention is adstock first: saturating first caps each \\
           period's spend in isolation, then lets carryover accumulate the \\
           capped values past the ceiling.",
      i = "Set {.code order = \"adstock_then_saturate\"} unless you mean this."
    ))
  }

  ad_step <- function(v) .mm_do_adstock(v, adstock, by)
  sat_step <- function(v) .mm_do_saturate(v, saturation)

  if (order == "adstock_then_saturate") sat_step(ad_step(x)) else ad_step(sat_step(x))
}

#' @keywords internal
#' @noRd
.mm_do_adstock <- function(x, args, by) {
  kernel <- if (is.null(args$kernel)) "geometric" else args$kernel
  if (!.mm_is_string(kernel) || !kernel %in% c("geometric", "weibull", "none")) {
    cli::cli_abort(
      '{.arg adstock$kernel} must be one of "geometric", "weibull" or "none".'
    )
  }
  if (kernel == "none") return(x)
  args$kernel <- NULL
  args$x <- x
  args$by <- by
  do.call(if (kernel == "geometric") "adstock_geometric" else "adstock_weibull",
          args, envir = parent.frame())
}

#' @keywords internal
#' @noRd
.mm_do_saturate <- function(x, args) {
  type <- if (is.null(args$type)) "hill" else args$type
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
  args$type <- NULL
  args$x <- x
  do.call("saturate", c(args, list(type = type)), envir = parent.frame())
}
