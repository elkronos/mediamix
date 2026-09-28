#' Moving block bootstrap for time-series statistics
#'
#' Puts uncertainty intervals on anything computed from a weekly panel --
#' coefficients, contributions, average or marginal ROI -- by resampling
#' *blocks* of consecutive rows, refitting, and recomputing. Resampling single
#' rows would destroy the autocorrelation that carryover creates and make the
#' intervals too narrow; resampling blocks keeps it within each block
#' (Kunsch, 1989).
#'
#' @param data A data frame, rows in time order (within each group when `by`
#'   is supplied). It should hold everything `statistic` needs: typically the
#'   KPI, the *transformed* media, the controls and the raw spend.
#' @param statistic A function of one data frame returning a named numeric
#'   vector. It is called once on `data` for the point estimate and once per
#'   replicate on a resampled copy.
#' @param times Number of bootstrap replicates.
#' @param block_length Rows per block. The default, `ceiling(n^(1/3))`, is the
#'   usual rate-optimal order for the moving block bootstrap; for weekly media
#'   data a block of at least the carryover's effective window is a sensible
#'   floor.
#' @param by Optional character vector of grouping columns. Blocks are then
#'   drawn within each group, and each group keeps its own size, so a panel
#'   of geographies is resampled as a panel.
#' @param level Confidence level of the percentile intervals.
#' @param seed Optional random seed, for reproducible intervals.
#'
#' @return An object of class `mm_bootstrap`: a data frame with one row per
#'   element of the statistic -- `term`, `estimate` (on the original data),
#'   `lower`, `upper` (percentile interval) and `std_error` -- with the
#'   replicate matrix attached as the attribute `"replicates"` (one row per
#'   replicate, one column per term). Replicates in which `statistic` failed
#'   are dropped and counted in a message.
#'
#' @details
#' Transform the media *before* bootstrapping and put the transformed columns
#' in `data`. Resampled blocks are glued end to end, so re-running the adstock
#' filter on resampled rows would carry spend across block boundaries that
#' were never adjacent in time; the transformed columns already hold each
#' week's correct carryover.
#'
#' The interval reflects sampling variation given the model specification,
#' including the transform parameters. It does not include the uncertainty in
#' choosing those parameters unless `statistic` re-tunes them, which is
#' possible but slow.
#'
#' @references
#' Kunsch, H. R. (1989). The jackknife and the bootstrap for general stationary
#' observations. *The Annals of Statistics*, 17(3), 1217--1241.
#'
#' Lahiri, S. N. (2003). *Resampling Methods for Dependent Data*. Springer.
#'
#' @seealso [contributions()], [roi()], [marginal_roi()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' channels <- c("tv", "video", "search", "social", "display")
#' truth <- attr(mm_weekly, "truth")
#'
#' # Transform once, on the full series, then bootstrap the model on top.
#' media <- as.data.frame(Map(function(x, d, h, s) media_transform(
#'   x, adstock = list(decay = d), saturation = list(half_max = h, shape = s)),
#'   north[channels], truth$decay[channels], truth$half_max[channels],
#'   truth$shape[channels]))
#' names(media) <- paste0(channels, "_t")
#' d <- cbind(north, media, week = seq_len(nrow(north)))
#'
#' average_roi <- function(d) {
#'   fit <- lm(revenue ~ tv_t + video_t + search_t + social_t + display_t +
#'               week + price + seasonality + holiday, data = d)
#'   contrib <- colSums(sweep(as.matrix(d[paste0(channels, "_t")]), 2,
#'                            coef(fit)[paste0(channels, "_t")], `*`))
#'   stats::setNames(contrib / colSums(d[channels]), channels)
#' }
#'
#' boot <- block_bootstrap(d, average_roi, times = 50, block_length = 13,
#'                         seed = 1)
#' boot
#' @export
block_bootstrap <- function(data, statistic, times = 200L, block_length = NULL,
                            by = NULL, level = 0.9, seed = NULL) {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  if (!is.function(statistic)) cli::cli_abort("{.arg statistic} must be a function.")
  times <- .mm_check_count(times, "times", min = 2L)
  level <- .mm_check_scalar(level, "level", lower = 0, upper = 1,
                            inclusive = c(FALSE, FALSE))
  n <- nrow(data)
  if (n < 2L) cli::cli_abort("{.arg data} needs at least two rows.")
  if (!is.null(by)) .mm_assert_cols(data, by)
  groups <- .mm_step_groups(data, by)
  if (is.null(block_length)) block_length <- ceiling(n^(1 / 3))
  block_length <- .mm_check_count(block_length, "block_length", min = 1L)

  est <- statistic(data)
  if (!is.numeric(est) || length(est) == 0L) {
    cli::cli_abort("{.arg statistic} must return a numeric vector.")
  }
  if (is.null(names(est))) names(est) <- paste0("stat", seq_along(est))

  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else NULL
    on.exit({
      if (is.null(old)) rm(".Random.seed", envir = globalenv()) else
        assign(".Random.seed", old, envir = globalenv())
    }, add = TRUE)
    set.seed(seed)
  }

  draw <- function(rows) {
    m <- length(rows)
    b <- min(block_length, m)
    n_blocks <- ceiling(m / b)
    starts <- sample.int(m - b + 1L, n_blocks, replace = TRUE)
    idx <- unlist(lapply(starts, function(s) s:(s + b - 1L)), use.names = FALSE)
    rows[idx[seq_len(m)]]
  }

  reps <- matrix(NA_real_, times, length(est), dimnames = list(NULL, names(est)))
  failed <- 0L
  for (r in seq_len(times)) {
    idx <- unlist(lapply(groups, draw), use.names = FALSE)
    val <- try(statistic(data[idx, , drop = FALSE]), silent = TRUE)
    if (inherits(val, "try-error") || !is.numeric(val) ||
        length(val) != length(est)) {
      failed <- failed + 1L
      next
    }
    reps[r, ] <- val
  }
  if (failed > 0L) {
    cli::cli_inform("{failed} of {times} replicate{?s} failed and {?was/were} \\
                     dropped.")
  }
  reps <- reps[stats::complete.cases(reps), , drop = FALSE]
  if (nrow(reps) < 2L) cli::cli_abort("Fewer than two replicates succeeded.")

  a <- (1 - level) / 2
  q <- apply(reps, 2, stats::quantile, probs = c(a, 1 - a), names = FALSE)
  out <- data.frame(term = names(est), estimate = unname(est),
                    lower = q[1, ], upper = q[2, ],
                    std_error = apply(reps, 2, stats::sd),
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  structure(out, class = c("mm_bootstrap", "data.frame"),
            replicates = reps, level = level, block_length = block_length,
            times = nrow(reps))
}

#' @export
print.mm_bootstrap <- function(x, ...) {
  cli::cli_h3("Block bootstrap")
  cli::cli_text("{attr(x, 'times')} replicates, blocks of \\
                 {attr(x, 'block_length')} rows, \\
                 {round(100 * attr(x, 'level'))}% percentile intervals")
  print(as.data.frame(x), digits = 4, row.names = FALSE)
  invisible(x)
}
