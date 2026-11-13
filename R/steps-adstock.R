#' Adstock transformation as a recipe step
#'
#' Applies adstock to one or more columns inside a \pkg{recipes} pipeline,
#' carrying the filter's state across the train/test boundary instead of
#' emitting missing values there. The kernel is geometric by default; the
#' Weibull and delayed kernels can put the peak effect after the period of
#' spend.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions choosing the media columns. See
#'   "Selecting media columns" below.
#' @param index Name of the time column, as a string. Required: adstock is a
#'   time-ordered filter, so the step needs to know the order and the spacing.
#'   The column is used for ordering and validation and is not itself
#'   transformed. It must be numeric, `Date` or `POSIXct`, with no missing
#'   values.
#' @param by Optional character vector naming grouping columns. Adstock is
#'   applied independently within each group, which is what geo-level and panel
#'   models need. This is an explicit argument because \pkg{recipes} has no
#'   group awareness and a grouped data frame does not reliably survive into
#'   `bake()`. Grouping columns must not contain missing values.
#' @param decay Geometric decay coefficient in `[0, 1]`, or for
#'   `kernel = "delayed"` the retention rate, in `(0, 1)`. Ignored by the
#'   Weibull kernel. Tunable.
#' @param max_lag Kernel length; `Inf` (the default) uses the infinite recursive
#'   form of the geometric kernel. The Weibull and delayed kernels need a
#'   finite value. Tunable.
#' @param normalise Should the kernel sum to 1? See [adstock_geometric()].
#' @param carry_over How to treat state at the start of new data.
#'   `"auto"` (the default) continues the filter when the new data continues
#'   or repeats the training series, and otherwise cold-starts with a warning.
#'   `"warm"` errors instead of cold-starting: on a gap, on a start that does
#'   not line up with the training periods, on an unseen group and when the
#'   period is unknown. `"cold"` always restarts from zero, which reproduces
#'   the behaviour of a lag-based step; under `start = "steady"` it restarts
#'   from the steady state of the new data's first periods.
#' @param na_action What to do about missing media values. `"zero"` (the
#'   default) treats missing media as no media, which is usually right for
#'   spend and is what lets a recipe run over a real, patchy panel without
#'   stopping. It is still a substantive assumption (an `NA` becomes a real
#'   number in the output), so `"error"` is available when you would rather
#'   find out. This differs from [adstock_geometric()], whose default is
#'   `"error"`; a step has to survive resampling, and a bare function call
#'   does not. Infinite values are an error under either setting.
#' @param tol Relative tolerance on the step between periods when deciding
#'   whether two blocks are contiguous, in `(0, 0.5)`. The default of `0.25`
#'   is loose so that calendar months, which vary from 28 to 31 days, are not
#'   mistaken for gaps. It stays below one half so that a block one period
#'   later, two periods later and part-way through a period remain distinct.
#' @param period The spacing between consecutive periods: a single positive
#'   number in the units of `index` (days for a `Date`, seconds for a
#'   `POSIXct`), or a `difftime`. `NULL` (the default) infers it within each
#'   group as the median step of that group's training index, so geographies
#'   that report on different weekdays each keep their own grid. A group with
#'   a single training time point takes the median of the other groups'
#'   periods.
#' @param kernel The carryover kernel: `"geometric"` (the default),
#'   `"weibull"` or `"delayed"`, applied by [adstock_geometric()],
#'   [adstock_weibull()] or [adstock_delayed()]. See "Kernels" below. An
#'   argument the kernel ignores must still be a valid value of its kind,
#'   such as a `decay` in `[0, 1]` or a non-negative `peak`, so that a typo is
#'   caught rather than kept in the step.
#' @param shape,scale Weibull shape and scale, both positive; `scale` is in
#'   periods. Required when `kernel = "weibull"` and ignored otherwise.
#'   Tunable.
#' @param type The form of the Weibull kernel, `"cdf"` (the default) or
#'   `"pdf"`. See [adstock_weights_weibull()]. Ignored by the other kernels.
#' @param peak Lag of the peak effect when `kernel = "delayed"`, in periods,
#'   in `[0, max_lag - 1]`. Ignored by the other kernels. Tunable.
#' @param start How each series starts. `"zero"` (the default) assumes no
#'   media ran before its first row. `"steady"` starts it at the steady state
#'   of its own first periods, as [adstock_steady_state()] computes it, so a
#'   long carryover has no burn-in. See "Steady-state starts" below.
#' @param keep_original_cols Should the selected columns be kept unchanged?
#'   `FALSE` (the default) replaces each one with its adstocked values. `TRUE`
#'   writes the adstocked values to a new column named with `prefix` and keeps
#'   the raw spend, which ROI calculations need after the model.
#' @param prefix The start of each new column's name when
#'   `keep_original_cols = TRUE`: the default `"adstock_"` turns `tv` into
#'   `adstock_tv`. Unused otherwise.
#' @param role The role of the new columns made with
#'   `keep_original_cols = TRUE`, `"predictor"` by default. Unused when the
#'   columns are transformed in place.
#' @param trained Has the step been prepared?
#' @param columns,states,last_index,history,seeds Populated by `prep()`; not
#'   set directly.
#' @param skip Should the step be skipped when baking? Must remain `FALSE`:
#'   adstock is a predictor transform and has to run at prediction time.
#' @param id A unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @section The problem this solves:
#' Adstock is a filter with memory. Prepare a recipe on January to June and bake
#' it on July to December, and the first rows of July have no history to draw
#' on. A lag built with `recipes::step_lag()` leaves `NA`, or the filler given
#' as its `default`, in the first rows of new data at every prep/bake
#' boundary, and dropping those rows with `step_naomit(skip = FALSE)` removes
#' the boundary weeks from training and evaluation (with its default,
#' `skip = TRUE`, `step_naomit()` drops them from the training data only).
#' `step_adstock()` carries the filter's state across the boundary instead, so
#' no row is dropped or filled in.
#'
#' Geometric adstock is an infinite impulse response filter whose memory is
#' one number per series,
#' \deqn{a_t = x_t + \theta a_{t-1},}
#' so the filter can be continued from where the training data left off.
#' `prep()` stores, for each group, the training time points and the training
#' values of each selected column. `bake()` runs the filter over the training
#' rows that come before the new data and then over the new rows, so the new
#' rows get exactly the values a filter over the whole series gives them. That
#' holds whether the new data follows the training data, repeats its last few
#' periods or repeats all of it.
#'
#' @section Storage:
#' The stored values take as much space as the selected columns of the
#' training data, plus one time index per group: about 25 KB for the three
#' geographies, 156 weeks and five channels of `mm_weekly`. They are media
#' values only; no outcome is stored. A recipe prepared with
#' `retain = TRUE`, the default of [recipes::prep()], already keeps the whole
#' training set. Selecting only the media columns keeps the stored part small.
#'
#' @section What bake() does at the boundary:
#' `prep()` records, per group, the training time points and values, and the
#' period: the median step of that group's index unless `period` is given.
#' `bake()` then takes, for each group, the training time points strictly
#' before the first new row and compares the first new row with the last of
#' them:
#'
#' \describe{
#'   \item{About one period later, or on a training time point}{The new data
#'     continues or repeats the series. The filter runs over the earlier
#'     training rows and then the new rows, and the result is identical to
#'     filtering the whole series at once. A block that starts at or before the
#'     first training time point has nothing before it, so it starts as the
#'     training series did (from zero, or from its steady state under
#'     `start = "steady"`), which is why `bake(rec, new_data = training)`
#'     reproduces the prepared output. A block that repeats the last periods
#'     of training, as `rsample::rolling_origin(lag = )` builds, continues from
#'     the rows before it.}
#'   \item{More than one period later}{A gap. Cold-start, and warn, naming the
#'     gap size and the group.}
#'   \item{Less than one period later}{A misaligned start, as when daily rows
#'     are baked into a weekly step or the reporting day moves. Cold-start,
#'     and warn.}
#'   \item{An unseen group}{Cold-start, and warn.}
#' }
#'
#' Under `carry_over = "warm"` each of these warnings is an error instead.
#' A cold start begins the block from zero, or under `start = "steady"` from
#' the steady state of the block's own first periods.
#' Row count and row order are preserved in every case. The step sorts by
#' `index` internally and restores the original order before returning, so
#' nothing downstream sees a reordered table.
#'
#' @section Kernels:
#' The geometric kernel is the only one with a recursive form, and the only
#' one that accepts `max_lag = Inf`. The Weibull and delayed kernels are
#' finite, and either can put the peak effect one or more periods after the
#' spend; see [adstock_weibull()] and [adstock_delayed()]. A finite kernel
#' reaches back `max_lag - 1` periods, so `bake()` puts at most that many
#' training rows in front of the new data, and the boundary cases above work
#' the same way for every kernel.
#'
#' Each kernel uses its own arguments and ignores the others, and only its own
#' can be marked with `tune()`:
#'
#' \describe{
#'   \item{`"geometric"`}{`decay` and `max_lag`, tuned over
#'     [carryover_decay()] and [carryover_max_lag()].}
#'   \item{`"weibull"`}{`shape`, `scale` and `max_lag`, tuned over
#'     [carryover_shape()], [carryover_scale()] and [carryover_max_lag()].}
#'   \item{`"delayed"`}{`decay`, `peak` and `max_lag`. Its `decay` is tuned
#'     over `[0.05, 0.95]`, since the kernel needs a positive retention rate.
#'     [carryover_peak()] stops at `max_lag - 1` when `max_lag` is fixed, and
#'     a tuned `max_lag` starts above the largest peak the search can propose,
#'     so every candidate is a valid kernel.}
#' }
#'
#' [step_saturation()] also has a `shape` argument. When both steps tune one,
#' give them distinct ids, for example `shape = tune("adstock_shape")`.
#'
#' @section Steady-state starts:
#' A filter started from zero understates the first periods of a
#' long-carryover channel. With `start = "steady"`, `prep()` stores for each
#' group and column the steady state of the training series: the state the
#' filter would hold had the mean spend of the series' first periods been
#' running forever, from [adstock_steady_state()]. The mean covers the
#' geometric kernel's 90% effective window, capped at `max_lag - 1` periods
#' for a finite `max_lag`; the Weibull and delayed kernels average the first
#' `max_lag - 1` periods. It uses training media only, never the outcome.
#'
#' `bake()` starts the filter from that state wherever the filter starts at the
#' beginning of a training series: when it re-bakes the training data, and
#' when a block repeats or continues the series. The new rows then get exactly
#' the values that a filter over the whole series, started from the same
#' state, gives them. A cold-started block (after a gap, misaligned, from an
#' unseen group, with an unknown period, or under `carry_over = "cold"`) is
#' treated as a series of its own and starts at the steady state of its own
#' first periods. That state is computed from the block's media, so a block
#' baked in pieces starts differently from the same block baked whole.
#'
#' @section Keeping the raw columns:
#' With `keep_original_cols = TRUE` each selected column is left as it is and
#' its adstocked values go to a new column, `adstock_tv` for `tv` with the
#' default `prefix`. The new columns get the role given by `role`. The raw
#' columns keep theirs, so a model fitted on the baked data uses both unless
#' the raw ones get another role before this step, for example with
#' `update_role(tv, new_role = "spend")`. A new name that is already in the
#' data is an error.
#'
#' @section Selecting media columns:
#' Select the media columns only. `all_predictors()` also picks up controls
#' such as price, seasonality and holiday flags, and adstocking those is
#' rarely what a model needs. Name the columns, or keep their names in a
#' character vector and use `all_of()`:
#'
#' ```r
#' channels <- c("tv", "video", "search", "social", "display")
#' recipe(revenue ~ ., data = train) |>
#'   step_adstock(all_of(channels), index = "date", by = "geo")
#' ```
#'
#' The step drops `index` and `by` from the selection before checking column
#' types. To give each channel its own step in a loop, write `all_of(!!ch)`:
#' selectors are evaluated when the recipe is prepared, after the loop has
#' finished, so a bare `all_of(ch)` selects the last channel in every step.
#' [step_adstock_channels()] adds one step per channel without a loop, with a
#' decay or a tuning parameter for each.
#'
#' @section Tidying:
#' `tidy()` returns a tibble with one row per selected column per group and
#' the columns `terms`, `group` (`NA` without `by`), `kernel`, `decay`,
#' `max_lag`, `shape`, `scale`, `type`, `peak`, `start`, `state`,
#' `state_length` and `id`. The kernel's parameters are `NA` where the kernel
#' ignores them, and before `prep()` when they are marked with `tune()`.
#' `state` is the filter's state at the end of the training data. For
#' `max_lag = Inf` it is the raw recursive accumulator \eqn{a_t}, before the
#' `normalise` scaling by \eqn{1 - \theta}, and under `start = "steady"` it
#' includes what remains of the starting state. For a finite `max_lag` it is
#' the most recent of the `max_lag - 1` training values the kernel still
#' reaches, and `state_length` gives their number. Before `prep()` the state
#' columns are `NA`.
#'
#' @section Resampling and leakage:
#' Stored state does not leak across a resampling boundary. Under any
#' \pkg{rsample} scheme, `prep()` re-runs on each split's analysis set, so the
#' state is re-learned from that split's training rows alone and describes
#' only media that precedes the assessment set. The information that crosses
#' the boundary is past *media spend*, which is genuinely known at prediction
#' time; no outcome information crosses at all.
#'
#' This step is strictly causal, unlike `recipes::step_window()` and
#' `recipes::step_impute_roll()`, which are centred and fill the leading edge
#' of a series using future values. Applied to a media filter, that would leak
#' future spend into past adstock and inflate in-sample fit. The exception is
#' `start = "steady"`, whose starting state averages the media of a series'
#' first periods.
#'
#' @seealso [adstock_geometric()], [adstock_weibull()], [adstock_delayed()],
#'   [adstock_steady_state()], [step_adstock_channels()], [step_saturation()],
#'   [carryover_decay()]
#'
#' @examplesIf requireNamespace("recipes", quietly = TRUE)
#' library(recipes)
#' data(mm_weekly)
#'
#' train <- mm_weekly[mm_weekly$date < as.Date("2025-01-01"), ]
#' test <- mm_weekly[mm_weekly$date >= as.Date("2025-01-01"), ]
#'
#' rec <- recipe(revenue ~ ., data = train) |>
#'   step_adstock(tv, video, search, index = "date", by = "geo", decay = 0.7) |>
#'   prep()
#'
#' # Row count and order are preserved; no NA at the boundary
#' baked <- bake(rec, new_data = test)
#' nrow(baked) == nrow(test)
#' sum(is.na(baked$tv))
#'
#' # The filter state at the end of training, per column and geo
#' tidy(rec, number = 1)
#'
#' # A delayed peak, started at steady state, with the raw spend kept
#' rec_delayed <- recipe(revenue ~ ., data = train) |>
#'   step_adstock(tv, index = "date", by = "geo", kernel = "delayed",
#'                decay = 0.6, peak = 1, max_lag = 6, start = "steady",
#'                keep_original_cols = TRUE) |>
#'   prep()
#' baked_delayed <- bake(rec_delayed, new_data = test)
#' head(baked_delayed[c("date", "geo", "tv", "adstock_tv")])
#' @export
step_adstock <- function(recipe, ...,
                         index = NULL,
                         by = NULL,
                         decay = 0.5,
                         max_lag = Inf,
                         normalise = TRUE,
                         carry_over = c("auto", "warm", "cold"),
                         na_action = c("zero", "error"),
                         tol = 0.25,
                         period = NULL,
                         kernel = c("geometric", "weibull", "delayed"),
                         shape = NULL,
                         scale = NULL,
                         type = c("cdf", "pdf"),
                         peak = 0,
                         start = c("zero", "steady"),
                         keep_original_cols = FALSE,
                         prefix = "adstock_",
                         role = "predictor",
                         trained = FALSE,
                         columns = NULL,
                         states = NULL,
                         last_index = NULL,
                         history = NULL,
                         seeds = NULL,
                         skip = FALSE,
                         id = .mm_rand_id("adstock")) {
  .mm_require_recipes()
  carry_over <- match.arg(carry_over)
  na_action <- match.arg(na_action)
  kernel <- match.arg(kernel)
  type <- match.arg(type)
  start <- match.arg(start)
  if (isTRUE(skip)) {
    cli::cli_abort(c(
      "{.arg skip} must be {.code FALSE} for {.fn step_adstock}.",
      i = "Adstock is a predictor transform and must run at prediction time."
    ))
  }
  if (!is.null(index) && !.mm_is_string(index)) {
    cli::cli_abort("{.arg index} must be a single column name given as a string.")
  }
  if (!is.null(by) && !is.character(by)) {
    cli::cli_abort("{.arg by} must be a character vector of column names.")
  }
  .mm_check_kernel_tune(kernel, list(decay = decay, shape = shape,
                                     scale = scale, peak = peak))
  # The kernel's parameters are tunable, so a `tune()` placeholder passes
  # through unchecked; prep() checks the values tuning puts in its place.
  params <- .mm_check_kernel_args(kernel, decay, max_lag, shape, scale, peak,
                                  start)
  tol <- .mm_check_scalar(tol, "tol", lower = 0, upper = 0.5,
                          inclusive = c(FALSE, FALSE))
  .mm_check_period(period)
  .mm_check_flag(keep_original_cols, "keep_original_cols")
  if (!.mm_is_string(prefix) || !nzchar(prefix)) {
    cli::cli_abort(
      "{.arg prefix} must be a single non-empty string, not \\
       {.obj_type_friendly {prefix}}."
    )
  }
  recipes::add_step(recipe, .mm_step_adstock_new(
    terms = rlang::enquos(...),
    index = index, by = by, decay = params$decay, max_lag = params$max_lag,
    normalise = normalise, carry_over = carry_over, na_action = na_action,
    tol = tol, period = period, kernel = kernel, shape = params$shape,
    scale = params$scale, type = type, peak = params$peak, start = start,
    keep_original_cols = keep_original_cols, prefix = prefix, role = role,
    trained = trained, columns = columns, states = states,
    last_index = last_index, history = history, seeds = seeds, skip = skip,
    id = id
  ))
}

#' @keywords internal
#' @noRd
.mm_step_adstock_new <- function(terms, index, by, decay, max_lag, normalise,
                                 carry_over, na_action, tol, period, kernel,
                                 shape, scale, type, peak, start,
                                 keep_original_cols, prefix, role, trained,
                                 columns, states, last_index, history, seeds,
                                 skip, id) {
  recipes::step(
    subclass = "adstock", terms = terms, index = index, by = by,
    decay = decay, max_lag = max_lag, normalise = normalise,
    carry_over = carry_over, na_action = na_action, tol = tol,
    period = period, kernel = kernel, shape = shape, scale = scale,
    type = type, peak = peak, start = start,
    keep_original_cols = keep_original_cols, prefix = prefix, role = role,
    trained = trained, columns = columns, states = states,
    last_index = last_index, history = history, seeds = seeds, skip = skip,
    id = id
  )
}

#' @exportS3Method recipes::prep
prep.step_adstock <- function(x, training, info = NULL, ...) {
  selected <- recipes::recipes_eval_select(x$terms, training, info)

  if (is.null(x$index)) {
    cli::cli_abort(c(
      "{.arg index} is required by {.fn step_adstock}.",
      i = "Adstock is a time-ordered filter, so it must know the order and \\
           spacing of the rows.",
      i = 'Name the time column, for example {.code index = "date"}.'
    ))
  }
  .mm_assert_cols(training, c(x$index, x$by))
  # Remove the index and grouping columns BEFORE type-checking, so that a
  # selection that happens to include the date and the geography does not fail
  # on columns the step was told about explicitly.
  col_names <- setdiff(selected, c(x$index, x$by))
  if (length(col_names) == 0L) {
    cli::cli_abort(c(
      "{.fn step_adstock} selected no media columns to transform.",
      i = if (length(selected) == 0L) {
        "The selection matched no columns in the training data."
      } else {
        "The selection resolved only to {.arg index} and {.arg by} columns."
      }
    ))
  }
  recipes::check_type(training[, col_names, drop = FALSE],
                      types = c("double", "integer"))
  # Tuning writes its values straight into the step, past the checks in
  # step_adstock(), so they are checked again here.
  opts <- .mm_adstock_opts(x)
  params <- .mm_check_kernel_args(opts$kernel, x$decay, x$max_lag, opts$shape,
                                  opts$scale, opts$peak, opts$start)
  steady <- opts$start == "steady"

  grouped <- .mm_has_by(x$by)
  idx <- .mm_index_numeric(training[[x$index]], x$index)
  grp <- .mm_step_groups(training, x$by)
  .mm_check_index_finite(idx, grp, grouped, x$index)
  media <- .mm_step_media(training, col_names, x$na_action)
  period <- .mm_period_value(x$period, training[[x$index]])
  periods <- .mm_group_periods(idx, grp, period)

  history <- states <- last_index <- stats::setNames(
    vector("list", length(grp)), names(grp)
  )
  seeds <- if (steady) history else NULL
  irregular <- character(0)
  for (i in seq_along(grp)) {
    rows <- grp[[i]]
    o <- rows[order(idx[rows])]
    .mm_check_duplicated_index(
      idx[o], if (grouped) .mm_show_key(names(grp)[i]) else NULL
    )
    if (.mm_irregular_steps(idx[o], periods[i], tol = x$tol) > 0L) {
      irregular <- c(irregular, names(grp)[i])
    }
    values <- lapply(media, function(v) v[o])
    history[[i]] <- list(index = idx[o], period = periods[i], values = values)
    last_index[[i]] <- idx[o][length(o)]
    starts <- lapply(values, function(v) 0)
    if (steady) {
      seeds[[i]] <- lapply(values, .mm_adstock_seed, kernel = opts$kernel,
                           decay = params$decay, max_lag = params$max_lag)
      starts <- seeds[[i]]
    }
    states[[i]] <- Map(function(v, s) {
      as.numeric(.mm_adstock_end_state(v, opts$kernel, params$decay,
                                       params$max_lag, s))
    }, values, starts)
  }

  if (any(!is.finite(periods))) {
    cli::cli_warn(c(
      "The spacing between periods could not be inferred from {.arg index}.",
      i = "The training data has fewer than two distinct time points in every \\
           series.",
      i = "Set {.arg period}; otherwise {.fn bake} cold-starts new data that \\
           comes after the training data."
    ))
  }
  if (length(irregular) > 0L) {
    shown <- .mm_show_key(irregular)
    cli::cli_warn(c(
      if (grouped) {
        "The time index is unevenly spaced in {length(irregular)} series."
      } else {
        "The time index is unevenly spaced."
      },
      i = paste("Adstock treats consecutive rows as consecutive periods, so \\
                 gaps in the index change what",
                .mm_kernel_meaning(opts$kernel)),
      i = if (grouped) "Affected: {.val {utils::head(shown, 5)}}.",
      i = "Fill the missing periods with zero spend before prepping."
    ))
  }

  .mm_step_adstock_new(
    terms = x$terms, index = x$index, by = x$by, decay = params$decay,
    max_lag = params$max_lag, normalise = x$normalise,
    carry_over = x$carry_over, na_action = x$na_action, tol = x$tol,
    period = x$period, kernel = opts$kernel, shape = params$shape,
    scale = params$scale, type = opts$type, peak = params$peak,
    start = opts$start, keep_original_cols = opts$keep_original_cols,
    prefix = opts$prefix, role = x$role, trained = TRUE, columns = col_names,
    states = states, last_index = last_index, history = history,
    seeds = seeds, skip = x$skip, id = x$id
  )
}

#' @exportS3Method recipes::bake
bake.step_adstock <- function(object, new_data, ...) {
  recipes::check_new_data(c(object$columns, object$index, object$by),
                          object, new_data)
  # A step prepared before the kernel options existed has none of their
  # fields, and bakes as the geometric kernel it was.
  opts <- .mm_adstock_opts(object)
  out_names <- object$columns
  if (opts$keep_original_cols) {
    out_names <- paste0(opts$prefix, object$columns)
    .mm_check_new_names(out_names, new_data)
  }
  if (nrow(new_data) == 0L) {
    for (nm in setdiff(out_names, names(new_data))) {
      new_data[[nm]] <- numeric(0)
    }
    return(new_data)
  }
  if (is.null(object$history)) {
    cli::cli_abort(c(
      "This {.fn step_adstock} was prepared by an earlier version of \\
       {.pkg mediamix}, which did not store the training series.",
      i = "Prepare the recipe again with {.fn prep}, or refit the workflow, \\
           before baking."
    ))
  }

  grouped <- .mm_has_by(object$by)
  idx <- .mm_index_numeric(new_data[[object$index]], object$index)
  grp <- .mm_step_groups(new_data, object$by)
  .mm_check_index_finite(idx, grp, grouped, object$index)
  # Each column is pulled out once, filled group by group and written back
  # once. Assigning into the data frame inside the group loop copies the
  # column every time, which made bake() quadratic in the number of groups.
  media <- .mm_step_media(new_data, object$columns, object$na_action)
  hist_pos <- match(names(grp), names(object$history))
  known_periods <- vapply(object$history, function(h) h$period, numeric(1))
  known_periods <- known_periods[is.finite(known_periods)]
  fallback_period <- if (length(known_periods) > 0L) {
    stats::median(known_periods)
  } else {
    NA_real_
  }

  gap_keys <- misaligned_keys <- unseen_keys <- irregular_keys <- character(0)
  gap_sizes <- numeric(0)
  no_period <- FALSE

  for (i in seq_along(grp)) {
    g <- names(grp)[i]
    shown <- if (grouped) .mm_show_key(g) else NULL
    rows <- grp[[i]]
    o <- rows[order(idx[rows])]
    # The same preconditions prep() enforces. An un-aggregated join produces
    # repeated periods at prediction time far more often than at training
    # time, and treating two rows for one week as two consecutive weeks is a
    # silent arithmetic error, not a stylistic one.
    .mm_check_duplicated_index(idx[o], shown)
    h <- if (is.na(hist_pos[i])) NULL else object$history[[hist_pos[i]]]
    period <- if (is.null(h)) fallback_period else h$period
    carry <- .mm_carry_case(idx[o], h$index, period, tol = object$tol)
    gap <- if (identical(carry$case, "gap")) {
      signif(.mm_gap_size(idx[o], h$index[carry$k], period), 3)
    } else {
      NA_real_
    }

    # Uneven spacing is reported for data prep() has not already seen. The
    # training data itself was checked, and warned about, once.
    seen <- !is.null(h) && length(h$index) == length(o) &&
      all(h$index == idx[o])
    if (!seen && .mm_irregular_steps(idx[o], period, tol = object$tol) > 0L) {
      irregular_keys <- c(irregular_keys, g)
    }

    continues <- carry$case %in% c("overlap", "contiguous")
    if (!continues && object$carry_over == "warm") {
      .mm_carry_abort(carry$case, shown, gap)
    }
    if (!continues && object$carry_over == "auto") {
      switch(
        carry$case,
        gap = {
          gap_keys <- c(gap_keys, g)
          gap_sizes <- c(gap_sizes, gap)
        },
        misaligned = misaligned_keys <- c(misaligned_keys, g),
        unseen = unseen_keys <- c(unseen_keys, g),
        unknown_period = no_period <- TRUE
      )
    }
    restart <- !continues || object$carry_over == "cold"
    k <- if (restart) 0L else carry$k

    # The filter runs over the training rows before the block and then over
    # the block. A finite kernel reaches back only `max_lag - 1` rows, so only
    # those are needed; the recursive kernel needs all of them.
    from <- if (is.finite(object$max_lag)) {
      max(1L, k - object$max_lag + 2L)
    } else {
      1L
    }
    for (cn in object$columns) {
      earlier <- if (k >= from) h$values[[cn]][from:k] else numeric(0)
      block <- media[[cn]][o]
      # A steady start seeds the filter where a series begins: a restarted
      # block from its own first periods, and otherwise the training series
      # when the rows run back to its first one. A prefix that starts later
      # already covers everything the kernel reaches.
      state <- 0
      if (opts$start == "steady") {
        state <- if (restart) {
          .mm_adstock_seed(block, opts$kernel, object$decay, object$max_lag)
        } else if (from == 1L) {
          object$seeds[[hist_pos[i]]][[cn]]
        } else {
          0
        }
      }
      out <- .mm_adstock_run(c(earlier, block), object, opts, state)
      media[[cn]][o] <- out[length(earlier) + seq_along(o)]
    }
  }
  for (j in seq_along(object$columns)) {
    new_data[[out_names[j]]] <- media[[object$columns[j]]]
  }

  if (length(gap_keys) > 0L) {
    if (grouped) {
      shown_gap <- sprintf("%s (%s missing)", .mm_show_key(gap_keys),
                           .mm_n_periods(gap_sizes))
      cli::cli_warn(c(
        "Cold-starting adstock for {length(gap_keys)} series that \\
         start{?s/} after a gap.",
        i = "Carryover from before the gap is discarded.",
        i = "Affected: {.val {utils::head(shown_gap, 5)}}."
      ))
    } else {
      gap_one <- gap_sizes[1L]
      cli::cli_warn(c(
        "Cold-starting adstock: the new data starts after a gap of \\
         {gap_one} period{?s}.",
        i = "Carryover from before the gap is discarded."
      ))
    }
  }
  if (length(misaligned_keys) > 0L) {
    shown_mis <- .mm_show_key(misaligned_keys)
    cli::cli_warn(c(
      if (grouped) {
        "Cold-starting adstock for {length(misaligned_keys)} series whose \\
         new data does not line up with the training periods."
      } else {
        "Cold-starting adstock: the new data does not line up with the \\
         training periods."
      },
      i = "The first new row falls less than one period after the training \\
           time point before it.",
      i = if (grouped) "Affected: {.val {utils::head(shown_mis, 5)}}.",
      i = "Check the time index, or set {.arg period} if the inferred \\
           spacing is wrong."
    ))
  }
  if (length(unseen_keys) > 0L) {
    shown_unseen <- .mm_show_key(unseen_keys)
    cli::cli_warn(c(
      "Cold-starting adstock for {length(unseen_keys)} group{?s} not seen \\
       during {.fn prep}.",
      i = "Affected: {.val {utils::head(shown_unseen, 5)}}."
    ))
  }
  if (length(irregular_keys) > 0L) {
    shown_irr <- .mm_show_key(irregular_keys)
    cli::cli_warn(c(
      if (grouped) {
        "The time index is unevenly spaced within the new data for \\
         {length(irregular_keys)} series."
      } else {
        "The time index is unevenly spaced within the new data."
      },
      i = paste("Adstock treats consecutive rows as consecutive periods, so \\
                 missing periods inside the new data change what",
                .mm_kernel_meaning(opts$kernel)),
      i = if (grouped) "Affected: {.val {utils::head(shown_irr, 5)}}.",
      i = "Fill the missing periods with zero spend before baking."
    ))
  }
  if (no_period) {
    cli::cli_warn(c(
      "Cold-starting adstock: the period spacing is unknown.",
      i = "{.fn prep} saw fewer than two distinct time points per series, so \\
           there is nothing to judge contiguity against.",
      i = "Set {.arg period} in {.fn step_adstock} to give the spacing."
    ))
  }
  new_data
}

#' @export
print.step_adstock <- function(x, width = max(20, options()$width - 30), ...) {
  opts <- .mm_adstock_opts(x)
  shown <- switch(opts$kernel,
                  geometric = character(0),
                  weibull = paste("weibull", opts$type),
                  delayed = "delayed")
  for (p in .mm_kernel_params(opts$kernel)) {
    value <- .mm_fmt_param(.mm_adstock_param(x, opts, p))
    shown <- c(shown, paste(p, "=", value))
  }
  if (opts$start == "steady") shown <- c(shown, "steady start")
  if (opts$keep_original_cols) {
    shown <- c(shown, paste0("new columns ", opts$prefix, "*"))
  }
  title <- sprintf("Adstock (%s) on ", paste(shown, collapse = ", "))
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_adstock <- function(x, ...) {
  opts <- .mm_adstock_opts(x)
  # A parameter the kernel ignores is reported as NA, as step_saturation()
  # does; reporting the supplied value would suggest it did something.
  uses <- .mm_kernel_params(opts$kernel)
  param <- function(p) {
    v <- .mm_adstock_param(x, opts, p)
    if (p %in% uses && !is.null(v)) as.numeric(.mm_param_value(v)) else NA_real_
  }
  kernel_cols <- list(
    kernel = opts$kernel, decay = param("decay"), max_lag = param("max_lag"),
    shape = param("shape"), scale = param("scale"),
    type = if (opts$kernel == "weibull") opts$type else NA_character_,
    peak = param("peak"), start = opts$start
  )
  if (!isTRUE(x$trained)) {
    terms <- .mm_sel2char(x$terms)
    n <- length(terms)
    return(do.call(.mm_tibble, c(
      list(terms = terms, group = rep(NA_character_, n)),
      lapply(kernel_cols, rep, n),
      list(state = rep(NA_real_, n), state_length = rep(NA_integer_, n),
           id = rep(x$id, n))
    )))
  }
  keys <- names(x$states)
  n <- length(keys) * length(x$columns)
  # For the recursive kernel the state is one number, the accumulator. For a
  # truncated kernel it is a tail of `max_lag - 1` values, and the most recent
  # of those is what is reported here, with the full length alongside so the
  # difference is visible.
  last_value <- function(s) if (length(s) == 0L) NA_real_ else s[length(s)]
  do.call(.mm_tibble, c(
    list(
      terms = rep(x$columns, times = length(keys)),
      group = if (.mm_has_by(x$by)) {
        rep(.mm_show_key(keys), each = length(x$columns))
      } else {
        rep(NA_character_, n)
      }
    ),
    lapply(kernel_cols, rep, n),
    list(
      state = unlist(lapply(x$states, function(s) {
        vapply(s[x$columns], last_value, numeric(1), USE.NAMES = FALSE)
      }), use.names = FALSE),
      state_length = unlist(lapply(x$states, function(s) {
        vapply(s[x$columns], length, integer(1), USE.NAMES = FALSE)
      }), use.names = FALSE),
      id = rep(x$id, n)
    )
  ))
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_adstock <- function(x, ...) "mediamix"

#' @exportS3Method generics::tunable
tunable.step_adstock <- function(x, ...) {
  # Only the parameters the chosen kernel uses are offered, over ranges that
  # kernel accepts, so tune_grid() never proposes a value that aborts
  # mid-resample.
  opts <- .mm_adstock_opts(x)
  info <- .mm_adstock_call_info(opts$kernel, x$max_lag, opts$peak)
  data.frame(
    name = names(info),
    call_info = I(unname(info)),
    source = "recipe",
    component = "step_adstock",
    component_id = x$id,
    stringsAsFactors = FALSE
  )
}

# ---- internals ---------------------------------------------------------------

# The selected media columns as numeric vectors, with missing values handled
# as `na_action` says. Errors name the column rather than the internal `x`.
#' @keywords internal
#' @noRd
.mm_step_media <- function(data, cols, na_action, call = parent.frame()) {
  .mm_check_media_finite(data, cols, call = call)
  out <- lapply(cols, function(cn) as.numeric(data[[cn]]))
  names(out) <- cols
  has_na <- cols[vapply(out, anyNA, logical(1))]
  if (length(has_na) > 0L && na_action == "error") {
    cli::cli_abort(c(
      "{length(has_na)} media column{?s} contain{?s/} missing values: \\
       {.val {has_na}}.",
      i = 'Use {.code na_action = "zero"} to treat missing media as no media, \\
           or impute before this step.'
    ), call = call)
  }
  out[has_na] <- lapply(out[has_na], function(v) {
    v[is.na(v)] <- 0
    v
  })
  out
}

# `period` as given to step_adstock(): NULL, a positive number or a difftime.
#' @keywords internal
#' @noRd
.mm_check_period <- function(period, call = parent.frame()) {
  if (is.null(period)) return(invisible(NULL))
  value <- if (inherits(period, "difftime")) unclass(period) else period
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value <= 0) {
    cli::cli_abort(
      "{.arg period} must be {.code NULL}, a single positive number or a \\
       {.cls difftime}, not {.obj_type_friendly {period}}.",
      call = call
    )
  }
  invisible(period)
}

# `period` in the units .mm_index_numeric() gives the index: days for a Date,
# seconds for a POSIXct.
#' @keywords internal
#' @noRd
.mm_period_value <- function(period, index, call = parent.frame()) {
  if (is.null(period) || !inherits(period, "difftime")) return(period)
  if (inherits(index, "POSIXct")) return(as.numeric(period, units = "secs"))
  if (inherits(index, "Date")) return(as.numeric(period, units = "days"))
  cli::cli_abort(c(
    "A {.cls difftime} {.arg period} needs a {.cls Date} or {.cls POSIXct} \\
     index.",
    i = "For a numeric index, give {.arg period} as a number in the units of \\
         the index."
  ), call = call)
}

#' @keywords internal
#' @noRd
.mm_n_periods <- function(n) {
  paste(format(n, trim = TRUE), ifelse(n == 1, "period", "periods"))
}

# The error carry_over = "warm" raises when a block cannot be continued.
#' @keywords internal
#' @noRd
.mm_carry_abort <- function(case, shown, gap, call = parent.frame()) {
  where <- if (is.null(shown)) {
    "The new data"
  } else {
    "The new data for group {.val {shown}}"
  }
  problem <- switch(
    case,
    gap = paste(where, "starts after a gap of {gap} period{?s}."),
    misaligned = paste(where, "starts less than one period after the \\
                       training time point before it."),
    unseen = "Group {.val {shown}} was not seen during {.fn prep}.",
    unknown_period = "The period is unknown: {.fn prep} saw fewer than two \\
                      distinct time points per series."
  )
  cli::cli_abort(c(
    '{.code carry_over = "warm"} requires the new data to continue the \\
     training series.',
    x = problem,
    i = 'Use {.code carry_over = "auto"} to cold-start with a warning instead.'
  ), call = call)
}

# ---- kernels -----------------------------------------------------------------

# The kernel options of a step, with the defaults that a step prepared before
# they existed was built with: geometric, from zero, transformed in place.
#' @keywords internal
#' @noRd
.mm_adstock_opts <- function(x) {
  or <- function(v, default) if (is.null(v)) default else v
  list(
    kernel = or(x$kernel, "geometric"),
    type = or(x$type, "cdf"),
    start = or(x$start, "zero"),
    keep_original_cols = isTRUE(x$keep_original_cols),
    prefix = or(x$prefix, "adstock_"),
    shape = x$shape,
    scale = x$scale,
    peak = or(x$peak, 0)
  )
}

#' @keywords internal
#' @noRd
.mm_adstock_param <- function(x, opts, p) {
  if (p %in% c("decay", "max_lag")) x[[p]] else opts[[p]]
}

# The arguments each kernel uses, which are also the ones it can tune.
#' @keywords internal
#' @noRd
.mm_kernel_params <- function(kernel) {
  switch(kernel,
         geometric = c("decay", "max_lag"),
         weibull = c("shape", "scale", "max_lag"),
         delayed = c("decay", "peak", "max_lag"))
}

# The end of the uneven-spacing warnings: which parameters lose their meaning.
#' @keywords internal
#' @noRd
.mm_kernel_meaning <- function(kernel) {
  if (identical(kernel, "weibull")) {
    "{.arg shape} and {.arg scale} mean."
  } else {
    "{.arg decay} means."
  }
}

# Tuning an argument the kernel ignores spends the search on a dimension that
# changes nothing, and tune() would not find the placeholder anyway, since
# tunable() offers only the kernel's own arguments.
#' @keywords internal
#' @noRd
.mm_check_kernel_tune <- function(kernel, args, call = parent.frame()) {
  ignored <- setdiff(names(args), .mm_kernel_params(kernel))
  tuned <- ignored[vapply(args[ignored], .mm_is_tune, logical(1))]
  if (length(tuned) == 0L) return(invisible(NULL))
  uses <- .mm_kernel_params(kernel)
  cli::cli_abort(c(
    '{.arg {tuned}} cannot be tuned when {.code kernel = "{kernel}"}.',
    i = "That kernel ignores {cli::qty(length(tuned))}{?it/them}. It uses \\
         {.arg {uses}}."
  ), call = call)
}

# Check the kernel's parameters and return them coerced. A `tune()`
# placeholder passes unchecked. `shape` and `scale` may be NULL except for the
# Weibull kernel, which needs both.
#' @keywords internal
#' @noRd
.mm_check_kernel_args <- function(kernel, decay, max_lag, shape, scale, peak,
                                  start, call = parent.frame()) {
  if (!.mm_is_tune(decay)) {
    decay <- if (kernel == "delayed") {
      .mm_check_scalar(decay, "decay", lower = 0, upper = 1,
                       inclusive = c(FALSE, FALSE), call = call)
    } else {
      .mm_check_scalar(decay, "decay", lower = 0, upper = 1, call = call)
    }
  }
  if (!.mm_is_tune(max_lag)) {
    max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L, allow_inf = TRUE,
                               call = call)
    if (kernel != "geometric" && is.infinite(max_lag)) {
      cli::cli_abort(c(
        '{.arg max_lag} must be finite for {.code kernel = "{kernel}"}.',
        i = "Only the geometric kernel has an infinite, recursive form."
      ), call = call)
    }
  }
  if (!is.null(shape) && !.mm_is_tune(shape)) {
    shape <- .mm_check_scalar(shape, "shape", lower = 0,
                              inclusive = c(FALSE, TRUE), call = call)
  }
  if (!is.null(scale) && !.mm_is_tune(scale)) {
    scale <- .mm_check_scalar(scale, "scale", lower = 0,
                              inclusive = c(FALSE, TRUE), call = call)
  }
  if (kernel == "weibull" && (is.null(shape) || is.null(scale))) {
    absent <- c("shape", "scale")[c(is.null(shape), is.null(scale))]
    cli::cli_abort(c(
      '{.code kernel = "weibull"} needs {.arg {absent}}.',
      i = "The Weibull kernel has no default shape or scale. Give both, or \\
           mark them with {.fn tune}."
    ), call = call)
  }
  if (!.mm_is_tune(peak)) {
    upper <- if (kernel == "delayed" && !.mm_is_tune(max_lag)) {
      max_lag - 1
    } else {
      Inf
    }
    peak <- .mm_check_scalar(peak, "peak", lower = 0, upper = upper,
                             call = call)
  }
  if (start == "steady" && kernel == "geometric" && !.mm_is_tune(decay) &&
      !.mm_is_tune(max_lag) && is.infinite(max_lag) && decay == 1) {
    cli::cli_abort(c(
      "An undecaying infinite kernel has no steady state to start from.",
      i = 'Use a finite {.arg max_lag}, a {.arg decay} below 1, or \\
           {.code start = "zero"}.'
    ), call = call)
  }
  list(decay = decay, max_lag = max_lag, shape = shape, scale = scale,
       peak = peak)
}

# The steady state of one series, in the form the kernel's `state` takes: an
# accumulator for the recursive kernel, the preceding `max_lag - 1` values for
# a finite one. Only the geometric kernel's decay sets how many periods are
# averaged; the others average the periods their state covers.
#' @keywords internal
#' @noRd
.mm_adstock_seed <- function(v, kernel, decay, max_lag) {
  if (kernel == "geometric") {
    adstock_steady_state(v, decay = decay, max_lag = max_lag)
  } else {
    adstock_steady_state(v, max_lag = max_lag)
  }
}

# The state at the end of a training series that started at `state`.
#' @keywords internal
#' @noRd
.mm_adstock_end_state <- function(v, kernel, decay, max_lag, state) {
  if (kernel == "geometric") {
    adstock_state(v, decay = decay, max_lag = max_lag, state = state)
  } else {
    adstock_state(v, max_lag = max_lag, state = state)
  }
}

# Run the step's kernel over one series from `state`.
#' @keywords internal
#' @noRd
.mm_adstock_run <- function(x, object, opts, state) {
  switch(
    opts$kernel,
    geometric = adstock_geometric(x, decay = object$decay,
                                  max_lag = object$max_lag,
                                  normalise = object$normalise, state = state),
    weibull = adstock_weibull(x, shape = opts$shape, scale = opts$scale,
                              max_lag = object$max_lag, type = opts$type,
                              normalise = object$normalise, state = state),
    delayed = adstock_delayed(x, decay = object$decay, peak = opts$peak,
                              max_lag = object$max_lag,
                              normalise = object$normalise, state = state)
  )
}

# New columns must not replace columns that are already there.
#' @keywords internal
#' @noRd
.mm_check_new_names <- function(new_names, data, call = parent.frame()) {
  clash <- intersect(new_names, names(data))
  if (length(clash) == 0L) return(invisible(NULL))
  cli::cli_abort(c(
    "{.fn step_adstock} would overwrite {length(clash)} existing \\
     column{?s}: {.val {clash}}.",
    i = "Choose another {.arg prefix}, or rename those columns first."
  ), call = call)
}

# The dials parameters each kernel offers. The delayed kernel needs a
# positive retention rate, and a peak no later than `max_lag - 1`: the peak's
# range stops there when `max_lag` is fixed, and a tuned `max_lag` starts
# above the largest peak the search can propose.
#' @keywords internal
#' @noRd
.mm_adstock_call_info <- function(kernel, max_lag, peak) {
  info <- list(
    decay = list(pkg = "mediamix", fun = "carryover_decay"),
    shape = list(pkg = "mediamix", fun = "carryover_shape"),
    scale = list(pkg = "mediamix", fun = "carryover_scale"),
    peak = list(pkg = "mediamix", fun = "carryover_peak"),
    max_lag = list(pkg = "mediamix", fun = "carryover_max_lag")
  )
  if (kernel == "delayed") {
    info$decay$range <- c(0.05, 0.95)
    peak_top <- max(eval(formals(carryover_peak)$range))
    if (!.mm_is_tune(max_lag) && is.finite(max_lag) &&
        max_lag - 1 < peak_top) {
      info$peak$range <- c(0, max_lag - 1)
    }
    largest <- if (.mm_is_tune(peak)) peak_top else peak
    lowest_lag <- as.integer(ceiling(largest)) + 1L
    if (lowest_lag > 1L) {
      info$max_lag$range <- c(lowest_lag, max(lowest_lag, 26L))
    }
  }
  info[.mm_kernel_params(kernel)]
}
