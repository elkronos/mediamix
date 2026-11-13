#' Moving block bootstrap for time-series statistics
#'
#' Puts uncertainty intervals on anything computed from a weekly panel
#' (coefficients, contributions, average or marginal ROI) by resampling
#' *blocks* of consecutive rows, refitting, and recomputing. Resampling single
#' rows would destroy the autocorrelation that carryover creates and make the
#' intervals too narrow; resampling blocks keeps it within each block
#' (\enc{Künsch}{Kunsch}, 1989).
#'
#' @param data A data frame, rows in time order (within each group when `by`
#'   is supplied), unless `index` names a column to order them by. It should
#'   hold everything `statistic` needs: typically the KPI, the *transformed*
#'   media, the controls and the raw spend.
#' @param statistic A function of one data frame returning a named numeric
#'   vector. It is called once on `data` for the point estimate and once per
#'   replicate on a resampled copy. Replicates are matched to the estimate by
#'   name, so the order of the elements may change between calls; a replicate
#'   that returns a name the estimate does not have counts as failed, and a
#'   name it leaves out is missing for that replicate.
#' @param times Number of bootstrap replicates, at most 1,000,000.
#' @param block_length Rows per block, or periods per block when `by` is
#'   supplied with `panel = "shared"`. The default, `ceiling(n^(1/3))` with `n`
#'   the number of rows of `data`, is the usual rate-optimal order for the
#'   moving block bootstrap. It is often too short for media data; see
#'   "Choosing the block length".
#' @param by Optional character vector of grouping columns, for a panel such
#'   as one series per geography. How the groups are resampled is set by
#'   `panel`.
#' @param level Confidence level of the percentile intervals.
#' @param seed Optional random seed, one whole number, for reproducible
#'   intervals. Every replicate's rows are drawn and the point estimate is
#'   computed after seeding, and the caller's random number stream is restored
#'   afterwards. Each replicate's call to `statistic` starts from a seed of
#'   its own, drawn before any replicate is evaluated, so a statistic that
#'   draws random numbers is reproducible too.
#' @param panel How groups are resampled when `by` is supplied. `"shared"`, the
#'   default, draws one set of blocks of periods and applies it to every
#'   group, so a shock that hit every geography in the same week stays
#'   together in each replicate. `"independent"` draws separate blocks for each
#'   group. That treats the groups as unrelated series, and for a statistic
#'   pooled across groups it gives intervals that are too narrow whenever the
#'   groups share shocks.
#' @param index Optional name of a numeric, `Date` or `POSIXct` column holding
#'   each row's period. Rows are put in period order within each group, and
#'   with `panel = "shared"` they are matched across groups by period, so the
#'   groups may cover different periods. Without `index`, row `t` of every
#'   group is taken to be the same period, and `panel = "shared"` needs every
#'   group to have the same number of rows.
#' @param circular If `TRUE`, blocks wrap around from the end of each series to
#'   its start (the circular block bootstrap of Politis and Romano, 1992), so
#'   every row is equally likely to be drawn. The default `FALSE` is the moving
#'   block bootstrap, which draws the first and last `block_length - 1` rows of
#'   each series less often than the rest.
#' @param cores Number of cores over which the replicates are evaluated.
#'   Values above 1 use [parallel::mclapply()] on Unix-alikes and a socket
#'   cluster elsewhere. Every replicate's rows, and the seed its call to
#'   `statistic` starts from, are drawn in this session before any replicate
#'   is evaluated, so the result does not depend on `cores`. The workers of a
#'   socket cluster are new R sessions that load \pkg{mediamix} and nothing
#'   else, so there `statistic` must call other packages with `pkg::fun()`
#'   and must not rely on functions or data defined in your session.
#'
#' @return An object of class `mm_bootstrap`: a data frame with one row per
#'   element of the statistic and columns `term`, `estimate` (on the original
#'   data), `lower`, `upper` (percentile interval) and `std_error`, with the
#'   replicate matrix attached as the attribute `"replicates"` (one row per
#'   replicate, one column per term). Replicates in which `statistic` failed
#'   are dropped and counted in a message that quotes the first error. Each
#'   term's interval and standard error use the replicates in which that term
#'   is finite; missing or infinite values stay in the replicate matrix and are
#'   counted per term in a message.
#'
#'   Subsetting the rows keeps the replicate matrix in step with the terms.
#'   Removing one of the five columns returns a plain data frame, as do
#'   `as.data.frame()` and, with the broom column names `std.error`, `conf.low`
#'   and `conf.high`, `tidy()`.
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
#' Every replicate's rows are drawn before `statistic` is first called, so a
#' statistic that sets its own seed, as a cross-validated fit often does,
#' cannot make the resamples repeat. Each replicate's call to `statistic`
#' then starts from a seed drawn for it in advance, which is what makes the
#' replicates the same in a worker process as in this one. Random numbers
#' that `statistic` draws on the replicates do not advance the caller's
#' stream.
#'
#' @section Panels:
#' With `by` and the default `panel = "shared"`, each replicate draws one set
#' of blocks of periods and takes every group's rows for those periods, the
#' moving blocks bootstrap for panels of \enc{Gonçalves}{Goncalves} (2011). Shocks common to
#' every group, such as a national holiday or a competitor's launch, then stay
#' together, which they must for a statistic pooled across groups. Each
#' group's rows are passed to `statistic` together, in time order, one group
#' after another.
#'
#' Groups are matched period by period. With `index`, a group has no row for
#' a period it does not cover, so group sizes vary from replicate to
#' replicate. Without `index`, row `t` of each group is period `t`, and groups
#' of different lengths are an error: supply `index`, or use
#' `panel = "independent"`.
#'
#' With `panel = "independent"`, every group draws its own blocks and keeps
#' its own size. That is only right when the groups share no shocks.
#'
#' @section Choosing the block length:
#' A block must be long enough to hold the dependence that matters for the
#' statistic. In media data that is mostly carryover, so a block at least as
#' long as the longest channel's [effective_window()] is a sensible floor.
#' The default `ceiling(n^(1/3))` is a rate rather than a length chosen for
#' the data, and it is often shorter: 6 for 156 weekly rows.
#'
#' Percentile intervals from a block bootstrap of a short series cover less
#' than their nominal level, and the shortfall grows with the autocorrelation.
#' For the mean of 156 values from an AR(1) process, nominal 90% intervals
#' from 200 replicates covered the true mean in about 81% of 2,000 simulated
#' series when the autocorrelation was 0.5 and blocks were 6 rows long, and in
#' about 71% when it was 0.8, rising to about 75% with blocks of 13 rows.
#' Circular blocks gave much the same figures. Read the intervals as a lower
#' bound on the uncertainty.
#'
#' The block length is checked against the series it resamples: longer than
#' half the shortest series gives a warning, because few distinct resamples
#' remain, and as long as the series is an error, because every replicate
#' would repeat the data.
#'
#' @references
#' \enc{Gonçalves}{Goncalves}, S. (2011). The moving blocks bootstrap for
#' panel linear regression models with individual fixed effects.
#' *Econometric Theory*, 27(5), 1048--1082.
#' \doi{10.1017/S0266466610000630}
#'
#' \enc{Künsch}{Kunsch}, H. R. (1989). The jackknife and the bootstrap for
#' general stationary observations. *The Annals of Statistics*, 17(3),
#' 1217--1241. \doi{10.1214/aos/1176347265}
#'
#' Lahiri, S. N. (2003). *Resampling Methods for Dependent Data*. Springer.
#' \doi{10.1007/978-1-4757-3803-2}
#'
#' Politis, D. N. and Romano, J. P. (1992). A circular block-resampling
#' procedure for stationary data. In R. LePage and L. Billard (eds.),
#' *Exploring the Limits of Bootstrap*, 263--270. Wiley.
#'
#' @seealso [plot.mm_bootstrap()], [contributions()], [roi()],
#'   [marginal_roi()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' channels <- c("tv", "video", "search", "social", "display")
#' truth <- attr(mm_weekly, "truth")
#'
#' # Transform once, on the full series, then bootstrap the model on top.
#' spec <- lapply(stats::setNames(channels, channels), function(ch) list(
#'   adstock = list(decay = truth$decay[[ch]]),
#'   saturation = list(half_max = truth$half_max[[ch]],
#'                     shape = truth$shape[[ch]])))
#' media <- transform_plan(north[channels], spec)
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
#'
#' # A panel: every geography takes the same blocks of weeks.
#' pooled_mean <- function(d) c(revenue = mean(d$revenue))
#' block_bootstrap(mm_weekly, pooled_mean, times = 50, block_length = 13,
#'                 by = "geo", index = "date", seed = 1)
#' @export
block_bootstrap <- function(data, statistic, times = 200L, block_length = NULL,
                            by = NULL, level = 0.9, seed = NULL,
                            panel = c("shared", "independent"), index = NULL,
                            circular = FALSE, cores = 1L) {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  if (!is.function(statistic)) cli::cli_abort("{.arg statistic} must be a function.")
  times <- .mm_check_bounded_count(times, "times", min = 2L, max = 1e6)
  level <- .mm_check_scalar(level, "level", lower = 0, upper = 1,
                            inclusive = c(FALSE, FALSE))
  seed <- .mm_check_seed(seed)
  panel <- .mm_check_option(panel, c("shared", "independent"), "panel")
  circular <- .mm_check_flag(circular, "circular")
  cores <- .mm_check_count(cores, "cores", min = 1L)
  n <- nrow(data)
  if (n < 2L) cli::cli_abort("{.arg data} needs at least two rows.")
  if (!is.null(by)) .mm_assert_cols(data, by)
  if (!is.null(index)) {
    if (!.mm_is_string(index)) {
      cli::cli_abort("{.arg index} must be a single column name.")
    }
    .mm_assert_cols(data, index)
  }
  .mm_note_dplyr_groups(data, by, "block_bootstrap")

  units <- .mm_boot_units(data, by, index, panel)
  if (is.null(block_length)) block_length <- ceiling(n^(1 / 3))
  block_length <- .mm_check_bounded_count(block_length, "block_length",
                                          min = 1L, max = .Machine$integer.max)
  .mm_boot_check_block(block_length, units)

  b <- block_length
  lens <- vapply(units, function(u) u$len, integer(1))
  n_blocks <- as.integer(ceiling(lens / b))
  n_starts <- if (circular) lens else lens - b + 1L
  per_replicate <- sum(n_blocks)
  if (as.numeric(times) * per_replicate > 1e8) {
    cli::cli_abort(c(
      "{times} replicate{?s} of {per_replicate} block{?s} each are too many to \\
       draw in advance.",
      i = "Use fewer replicates or longer blocks."
    ))
  }

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

  # Every replicate's block starts are drawn before `statistic` runs even once.
  # Drawing between calls would let a statistic that calls set.seed() reset the
  # stream, and every replicate after the first would repeat the same rows.
  cols <- split(seq_len(per_replicate), rep(seq_along(units), n_blocks))
  starts <- matrix(0L, times, per_replicate)
  for (r in seq_len(times)) {
    for (u in seq_along(units)) {
      starts[r, cols[[u]]] <- sample.int(n_starts[[u]], n_blocks[[u]],
                                         replace = TRUE)
    }
  }
  rows_for <- function(r) {
    unlist(lapply(seq_along(units), function(u) {
      pos <- .mm_boot_positions(starts[r, cols[[u]]], b, lens[[u]], circular)
      unlist(lapply(units[[u]]$lookups, function(lk) {
        out <- lk[pos]
        out[!is.na(out)]
      }), use.names = FALSE)
    }), use.names = FALSE)
  }

  est <- statistic(data)
  if (!is.numeric(est) || length(est) == 0L) {
    cli::cli_abort("{.arg statistic} must return a numeric vector.")
  }
  terms <- .mm_boot_names(names(est), length(est))
  if (anyDuplicated(terms)) {
    cli::cli_abort(c(
      "{.arg statistic} must return uniquely named elements.",
      x = "Repeated: {.val {unique(terms[duplicated(terms)])}}.",
      i = "Replicates are matched to the estimate by name."
    ))
  }
  if (as.numeric(times) * length(terms) > 1e8) {
    cli::cli_abort(c(
      "{times} replicate{?s} of {length(terms)} term{?s} are too many to hold \\
       in memory.",
      i = "Use fewer replicates, or return fewer terms from {.arg statistic}."
    ))
  }

  # Each replicate's call to `statistic` starts from a seed drawn here. The
  # seeds are drawn after the rows and the point estimate, so neither depends
  # on them, and a worker process starts from the seed it is handed, so the
  # replicates do not depend on `cores`.
  seeds <- sample.int(.Machine$integer.max, times, replace = TRUE)
  kinds <- RNGkind()
  replicate_one <- function(r) {
    # A socket worker starts with the default generator.
    if (!identical(RNGkind(), kinds)) do.call(RNGkind, as.list(kinds))
    set.seed(seeds[[r]])
    val <- tryCatch(statistic(data[rows_for(r), , drop = FALSE]),
                    error = function(e) e)
    if (inherits(val, "error")) conditionMessage(val) else
      .mm_boot_align(val, terms)
  }
  # Seeding each replicate moves the session's stream, so it is put back
  # where the draws above left it.
  stream <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  raw <- .mm_maybe_parallel(seq_len(times), replicate_one, cores)
  if (!is.null(stream)) assign(".Random.seed", stream, envir = globalenv())

  reps <- matrix(NA_real_, times, length(terms),
                 dimnames = list(NULL, terms))
  ok <- logical(times)
  first_error <- NULL
  for (r in seq_len(times)) {
    aligned <- raw[[r]]
    if (inherits(aligned, "try-error")) {
      cond <- attr(aligned, "condition")
      aligned <- paste0("A parallel worker failed: ", if (is.null(cond)) {
        as.character(aligned)
      } else {
        conditionMessage(cond)
      })
    }
    if (is.character(aligned)) {
      if (is.null(first_error)) first_error <- aligned
      next
    }
    reps[r, ] <- aligned
    ok[r] <- TRUE
  }
  failed <- times - sum(ok)
  reps <- reps[ok, , drop = FALSE]
  if (failed > 0L && nrow(reps) >= 2L) {
    cli::cli_inform(c(
      "{failed} of {times} replicate{?s} failed and \\
       {cli::qty(failed)}{?was/were} dropped.",
      i = "First failure: {first_error}"
    ))
  }
  if (nrow(reps) < 2L) {
    cli::cli_abort(c(
      "Fewer than two replicates succeeded.",
      x = "{failed} of {times} failed. First failure: {first_error}"
    ))
  }

  finite <- is.finite(reps)
  lost <- colSums(!finite)
  if (any(lost > 0L)) {
    hit <- lost[lost > 0L]
    by_term <- paste0(names(hit), " (", hit, ")", collapse = ", ")
    cli::cli_inform(c(
      "{length(hit)} term{?s} had a missing or infinite value in some \\
       replicates, which {?its/their} interval{?s} leave{?s/} out.",
      i = "Replicates left out, by term: {by_term}."
    ))
  }

  a <- (1 - level) / 2
  interval <- function(j) {
    v <- reps[finite[, j], j]
    if (length(v) < 2L) return(c(NA_real_, NA_real_, NA_real_))
    c(stats::quantile(v, probs = c(a, 1 - a), names = FALSE), stats::sd(v))
  }
  s <- vapply(seq_along(terms), interval, numeric(3))
  out <- data.frame(term = terms, estimate = unname(as.numeric(est)),
                    lower = s[1L, ], upper = s[2L, ], std_error = s[3L, ],
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  structure(out, class = c("mm_bootstrap", "data.frame"),
            replicates = reps, level = level, block_length = block_length,
            times = nrow(reps),
            panel = if (length(units) > 1L || length(units[[1L]]$lookups) > 1L)
              panel else NULL,
            circular = circular)
}

#' @export
print.mm_bootstrap <- function(x, ...) {
  cli::cli_h3("Block bootstrap")
  kind <- if (isTRUE(attr(x, "circular"))) "circular blocks" else "blocks"
  panel <- attr(x, "panel")
  unit <- if (identical(panel, "shared")) {
    "periods shared across groups"
  } else if (identical(panel, "independent")) {
    "rows drawn within each group"
  } else {
    "rows"
  }
  cli::cli_text("{attr(x, 'times')} replicates, {kind} of \\
                 {attr(x, 'block_length')} {unit}, \\
                 {round(100 * attr(x, 'level'))}% percentile intervals")
  print(as.data.frame(x), digits = 4, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.mm_bootstrap <- function(x, row.names = NULL, optional = FALSE,
                                       ...) {
  .mm_boot_plain(x)
}

#' @export
`[.mm_bootstrap` <- function(x, i, j, ..., drop) {
  out <- NextMethod()
  .mm_boot_restore(out, x)
}

#' @exportS3Method generics::tidy
tidy.mm_bootstrap <- function(x, ...) {
  out <- .mm_boot_plain(x)
  data.frame(term = out$term, estimate = out$estimate,
             std.error = out$std_error, conf.low = out$lower,
             conf.high = out$upper, stringsAsFactors = FALSE)
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.mm_bootstrap <- function(data, template) {
  .mm_boot_restore(data, template)
}

# The resampling units. Each unit is one series of `len` positions, and each
# of its lookups maps a position to a row of `data` (NA where a group has no
# row for that period). With `panel = "shared"` all groups form one unit, so
# they share every block; otherwise each group is a unit of its own.
#' @keywords internal
#' @noRd
.mm_boot_units <- function(data, by, index, panel, call = parent.frame()) {
  n <- nrow(data)
  groups <- .mm_step_groups(.mm_plain_cols(data, by), by)
  pos <- NULL
  if (!is.null(index)) {
    iv <- data[[index]]
    if (anyNA(iv)) {
      cli::cli_abort("{.arg index} must not contain missing values.",
                     call = call)
    }
    iv <- .mm_index_numeric(iv, "index", call = call)
    grid <- sort(unique(iv))
    pos <- match(iv, grid)
    groups <- lapply(groups, function(g) g[order(pos[g])])
    dup <- vapply(groups, function(g) anyDuplicated(pos[g]) > 0L, logical(1))
    if (any(dup)) {
      affected <- .mm_show_key(names(groups)[dup])
      cli::cli_abort(c(
        "{.arg index} repeats a period within a series.",
        x = if (!is.null(by)) "Affected: {.val {affected}}.",
        i = "Each series needs one row per period."
      ), call = call)
    }
  }
  if (panel == "shared" && length(groups) > 1L) {
    if (is.null(pos)) {
      sizes <- lengths(groups)
      if (length(unique(sizes)) > 1L) {
        cli::cli_abort(c(
          "With {.code panel = \"shared\"}, groups are matched row by row, so \\
           they must have the same number of rows.",
          x = "Group sizes range from {min(sizes)} to {max(sizes)}.",
          i = "Supply {.arg index} to match rows by period, or use \\
               {.code panel = \"independent\"} to resample each group on its \\
               own."
        ), call = call)
      }
      return(list(list(len = sizes[[1L]], lookups = unname(groups),
                       what = "periods")))
    }
    len <- max(pos)
    lookups <- lapply(unname(groups), function(g) {
      lk <- rep(NA_integer_, len)
      lk[pos[g]] <- g
      lk
    })
    return(list(list(len = len, lookups = lookups, what = "periods")))
  }
  lapply(unname(groups), function(g) {
    list(len = length(g), lookups = list(g), what = "rows")
  })
}

#' @keywords internal
#' @noRd
.mm_boot_check_block <- function(b, units, call = parent.frame()) {
  lens <- vapply(units, function(u) u$len, integer(1))
  shortest <- min(lens)
  what <- units[[which.min(lens)]]$what
  if (b >= shortest) {
    cli::cli_abort(c(
      "{.arg block_length} ({b}) must be shorter than the series it resamples.",
      x = "The shortest series has {shortest} {what}.",
      i = "A block that covers a whole series leaves nothing to resample: \\
           every replicate would repeat the data and the interval would have \\
           zero width."
    ), call = call)
  }
  if (b > shortest / 2) {
    cli::cli_warn(c(
      "{.arg block_length} ({b}) is more than half the shortest series \\
       ({shortest} {what}).",
      i = "Few distinct resamples remain, so the intervals understate the \\
           uncertainty."
    ), call = call)
  }
  invisible(b)
}

# Positions covered by blocks starting at `s`, cut to the series length.
#' @keywords internal
#' @noRd
.mm_boot_positions <- function(s, b, len, circular) {
  pos <- rep(s, each = b) + rep.int(seq_len(b) - 1L, length(s))
  if (circular) pos <- (pos - 1L) %% len + 1L
  pos[seq_len(len)]
}

# Term names for a statistic's value: unnamed or blank elements are named by
# position, as `stat1`, `stat2` and so on.
#' @keywords internal
#' @noRd
.mm_boot_names <- function(nm, k) {
  if (is.null(nm)) return(paste0("stat", seq_len(k)))
  blank <- is.na(nm) | nm == ""
  nm[blank] <- paste0("stat", seq_len(k))[blank]
  nm
}

# Places one replicate's values in the estimate's term order. Returns a numeric
# vector, or a character string saying why the replicate cannot be used.
#' @keywords internal
#' @noRd
.mm_boot_align <- function(val, terms) {
  if (!is.numeric(val)) {
    return("the statistic returned a non-numeric value.")
  }
  if (is.null(names(val))) {
    if (length(val) != length(terms)) {
      return(sprintf("the statistic returned %d unnamed values for %d terms.",
                     length(val), length(terms)))
    }
    return(as.numeric(val))
  }
  nm <- .mm_boot_names(names(val), length(val))
  unknown <- setdiff(nm, terms)
  if (length(unknown) > 0L) {
    return(sprintf("the statistic returned a name the estimate does not have (%s).",
                   paste(utils::head(unknown, 3), collapse = ", ")))
  }
  if (anyDuplicated(nm)) {
    return("the statistic returned a name twice.")
  }
  out <- rep(NA_real_, length(terms))
  out[match(nm, terms)] <- as.numeric(val)
  out
}

#' @keywords internal
#' @noRd
.mm_boot_attrs <- function() {
  c("replicates", "level", "block_length", "times", "panel", "circular")
}

#' @keywords internal
#' @noRd
.mm_boot_plain <- function(x) {
  for (a in .mm_boot_attrs()) attr(x, a) <- NULL
  class(x) <- setdiff(class(x), "mm_bootstrap")
  x
}

# After a subset, keep the replicate matrix in step with the remaining terms.
# Without all five columns, or with terms the matrix does not hold, the result
# is no longer a bootstrap summary and becomes a plain data frame.
#' @keywords internal
#' @noRd
.mm_boot_restore <- function(out, template) {
  if (!is.data.frame(out)) return(out)
  reps <- attr(template, "replicates", exact = TRUE)
  core <- c("term", "estimate", "lower", "upper", "std_error")
  if (!all(core %in% names(out)) || !is.matrix(reps) ||
      anyNA(match(out$term, colnames(reps)))) {
    return(.mm_boot_plain(out))
  }
  for (a in .mm_boot_attrs()) attr(out, a) <- attr(template, a, exact = TRUE)
  attr(out, "replicates") <- reps[, match(out$term, colnames(reps)),
                                  drop = FALSE]
  class(out) <- class(template)
  out
}

# A whole-number count with an upper bound. The bound stops a typo such as
# `times = 1e9` from trying to allocate gigabytes before anything is checked.
#' @keywords internal
#' @noRd
.mm_check_bounded_count <- function(x, arg, min, max, call = parent.frame()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-missing number.",
                   call = call)
  }
  if (!is.finite(x) || x %% 1 != 0) {
    cli::cli_abort("{.arg {arg}} must be a finite whole number, not \\
                    {.val {x}}.", call = call)
  }
  if (x < min || x > max) {
    cli::cli_abort("{.arg {arg}} must be between {min} and \\
                    {format(max, big.mark = ',', scientific = FALSE)}, not \\
                    {.val {x}}.", call = call)
  }
  as.integer(x)
}

#' @keywords internal
#' @noRd
.mm_check_seed <- function(seed, call = parent.frame()) {
  if (is.null(seed)) return(NULL)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed %% 1 != 0 || abs(seed) > .Machine$integer.max) {
    got <- if (is.numeric(seed) && length(seed) == 1L) {
      format(seed)
    } else {
      cli::format_inline("{.obj_type_friendly {seed}}")
    }
    cli::cli_abort(c(
      "{.arg seed} must be {.code NULL} or one finite whole number.",
      x = "Got {got}."
    ), call = call)
  }
  as.integer(seed)
}

#' @keywords internal
#' @noRd
.mm_check_option <- function(x, choices, arg, call = parent.frame()) {
  if (identical(x, choices)) return(choices[[1L]])
  if (!.mm_is_string(x) || !x %in% choices) {
    cli::cli_abort("{.arg {arg}} must be one of {.val {choices}}.",
                   call = call)
  }
  x
}

# The named columns as a plain data frame. Indexing a data.table with a
# character vector is a join, not a column selection, so columns are taken
# one at a time.
#' @keywords internal
#' @noRd
.mm_plain_cols <- function(data, cols) {
  if (is.null(cols) || length(cols) == 0L) {
    return(structure(list(), names = character(0), row.names = c(NA, -nrow(data)),
                     class = "data.frame"))
  }
  structure(lapply(cols, function(cn) data[[cn]]), names = cols,
            row.names = c(NA, -nrow(data)), class = "data.frame")
}
