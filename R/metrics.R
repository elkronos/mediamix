#' Minimal error metrics
#'
#' Two metrics, provided only so that [tune_carryover()] has a working default
#' and a one-line simple case. Any function of `(actual, predicted)` returning a
#' single number will do, so use \pkg{yardstick} if you want a real metric
#' library.
#'
#' @param actual Numeric vector of observed values.
#' @param predicted Numeric vector of predicted values, the same length as
#'   `actual`.
#' @param na_rm Should pairs where either value is missing or non-finite be
#'   dropped? Defaults to `TRUE`.
#'
#' @return A single number. `NA_real_` if no complete pairs remain.
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
  if (length(v$actual) == 0L) return(NA_real_)
  sqrt(mean((v$actual - v$predicted)^2))
}

#' @rdname metrics
#' @export
mae <- function(actual, predicted, na_rm = TRUE) {
  v <- .mm_align_metric(actual, predicted, na_rm)
  if (length(v$actual) == 0L) return(NA_real_)
  mean(abs(v$actual - v$predicted))
}

#' @keywords internal
#' @noRd
.mm_align_metric <- function(actual, predicted, na_rm = TRUE,
                             call = parent.frame()) {
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
  if (!na_rm) return(list(actual = actual, predicted = predicted))
  ok <- is.finite(actual) & is.finite(predicted)
  list(actual = actual[ok], predicted = predicted[ok])
}
