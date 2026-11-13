#' Root mean squared error and mean absolute error
#'
#' Two metrics, provided only so that [tune_carryover()] has a working default
#' (`rmse()`) and a one-line simple case. Any function of `(actual, predicted)`
#' returning a single number will do, so use \pkg{yardstick} if you want a real
#' metric library.
#'
#' @param actual Numeric vector of observed values.
#' @param predicted Numeric vector of predicted values, the same length as
#'   `actual`.
#' @param na_rm Should pairs where either value is missing or non-finite be
#'   dropped? Defaults to `TRUE`. With `FALSE`, a missing or infinite value
#'   carries through to the result.
#'
#' @return A single number. `NA_real_` if no finite pairs remain. Empty input,
#'   input of different lengths and non-numeric input are errors. `rmse()`
#'   scales the errors before squaring them, so errors near the limits of
#'   double precision neither overflow nor vanish.
#'
#' @seealso [tune_carryover()], which takes any such function as `metric_fn`.
#'
#' @examples
#' rmse(c(1, 2, 3), c(1.1, 1.9, 3.2))
#' mae(c(1, 2, 3), c(1.1, 1.9, 3.2))
#' @name metrics
NULL

#' @rdname metrics
#' @export
rmse <- function(actual, predicted, na_rm = TRUE) {
  v <- .mm_align_metric(actual, predicted, na_rm)
  if (length(v$error) == 0L) return(NA_real_)
  v$scale * .mm_root_mean_square(v$error)
}

#' @rdname metrics
#' @export
mae <- function(actual, predicted, na_rm = TRUE) {
  v <- .mm_align_metric(actual, predicted, na_rm)
  if (length(v$error) == 0L) return(NA_real_)
  v$scale * mean(abs(v$error))
}

# Validated forecast errors. `scale` is 2 when the errors had to be halved:
# the difference of two finite numbers can overflow, and halving both first
# keeps it finite.
#' @keywords internal
#' @noRd
.mm_align_metric <- function(actual, predicted, na_rm = TRUE,
                             call = parent.frame()) {
  .mm_check_flag(na_rm, "na_rm", call = call)
  if (length(actual) != length(predicted)) {
    cli::cli_abort(
      "{.arg actual} ({length(actual)}) and {.arg predicted} \\
       ({length(predicted)}) must be the same length.",
      call = call
    )
  }
  if (!is.numeric(actual) || !is.numeric(predicted)) {
    cli::cli_abort("{.arg actual} and {.arg predicted} must be numeric.",
                   call = call)
  }
  if (length(actual) == 0L) {
    cli::cli_abort("{.arg actual} and {.arg predicted} must have at least one \\
                    element.", call = call)
  }
  if (na_rm) {
    ok <- is.finite(actual) & is.finite(predicted)
    actual <- actual[ok]
    predicted <- predicted[ok]
  }
  error <- actual - predicted
  if (any(is.infinite(error) & is.finite(actual) & is.finite(predicted))) {
    return(list(error = actual / 2 - predicted / 2, scale = 2))
  }
  list(error = error, scale = 1)
}

# sqrt(mean(e^2)), computed on errors divided by the largest one so the squares
# neither overflow nor underflow.
#' @keywords internal
#' @noRd
.mm_root_mean_square <- function(e) {
  s <- max(abs(e))
  if (!is.finite(s) || s == 0) return(sqrt(mean(e^2)))
  s * sqrt(mean((e / s)^2))
}
