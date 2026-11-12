#' Adstock transformation as a recipe step
#'
#' Applies geometric adstock to one or more columns inside a \pkg{recipes}
#' pipeline, carrying the filter's state across the train/test boundary instead
#' of emitting missing values there.
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
#' @param decay Geometric decay coefficient in `[0, 1]`. Tunable.
#' @param max_lag Kernel length; `Inf` (the default) uses the infinite recursive
#'   form. Tunable.
#' @param normalise Should the kernel sum to 1? See [adstock_geometric()].
#' @param carry_over How to treat state at the start of new data.
#'   `"auto"` (the default) continues the filter when the new data continues
#'   or repeats the training series, and otherwise cold-starts with a warning.
#'   `"warm"` errors instead of cold-starting: on a gap, on a start that does
#'   not line up with the training periods, on an unseen group and when the
#'   period is unknown. `"cold"` always restarts from zero, which reproduces
#'   the behaviour of a lag-based step.
#' @param na_action What to do about missing media values. `"zero"` (the
#'   default) treats missing media as no media, which is usually right for
#'   spend and is what lets a recipe run over a real, patchy panel without
#'   stopping. It is still a substantive assumption -- an `NA` becomes a real
#'   number in the output -- so `"error"` is available when you would rather
#'   find out. Note this differs from [adstock_geometric()], whose default is
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
#' @param role Not used by this step, since new columns are not created.
#' @param trained Has the step been prepared?
#' @param columns,states,last_index,history Populated by `prep()`; not set
#'   directly.
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
#' boundary, and dropping those rows with `step_naomit()` removes the boundary
#' weeks from training and evaluation. `step_adstock()` carries the filter's
#' state across the boundary instead, so no row is dropped or filled in.
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
#'     first training time point has nothing before it and starts from zero,
#'     as `prep()` did, which is why `bake(rec, new_data = training)`
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
#' Row count and row order are preserved in every case. The step sorts by
#' `index` internally and restores the original order before returning, so
#' nothing downstream sees a reordered table.
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
#'
#' @section Tidying:
#' `tidy()` returns a tibble with one row per selected column per group and
#' the columns `terms`, `group` (`NA` without `by`), `decay`, `state`,
#' `state_length` and `id`. `state` is the filter's state at the end of the
#' training data. For `max_lag = Inf` it is the raw recursive accumulator
#' \eqn{a_t}, before the `normalise` scaling by \eqn{1 - \theta}. For a finite
#' `max_lag` it is the most recent of the `max_lag - 1` training values the
#' kernel still reaches, and `state_length` gives their number. Before
#' `prep()` the state columns are `NA`, and so is `decay` when it is marked
#' with `tune()`.
#'
#' @section Resampling and leakage:
#' A reviewer will ask whether stored state leaks across a resampling boundary.
#' It does not. Under any \pkg{rsample} scheme, `prep()` re-runs on each split's
#' analysis set, so the state is re-learned from that split's training rows
#' alone and describes only media that precedes the assessment set. The
#' information that crosses the boundary is past *media spend*, which is
#' genuinely known at prediction time; no outcome information crosses at all.
#'
#' Note also that this step is strictly causal, unlike `recipes::step_window()`
#' and `recipes::step_impute_roll()`, which are centred and fill the leading
#' edge of a series using future values. Applied to a media filter, that would
#' leak future spend into past adstock and inflate in-sample fit, which is the
#' exact pathology marketing mix models are already accused of.
#'
#' @seealso [adstock_geometric()], [step_saturation()], [carryover_decay()]
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
                         role = NA,
                         trained = FALSE,
                         columns = NULL,
                         states = NULL,
                         last_index = NULL,
                         history = NULL,
                         skip = FALSE,
                         id = .mm_rand_id("adstock")) {
  .mm_require_recipes()
  carry_over <- match.arg(carry_over)
  na_action <- match.arg(na_action)
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
  # Both of these are tunable, so a `tune()` placeholder must pass straight
  # through unchecked.
  if (!.mm_is_tune(decay)) {
    decay <- .mm_check_scalar(decay, "decay", lower = 0, upper = 1)
  }
  if (!.mm_is_tune(max_lag)) {
    max_lag <- .mm_check_count(max_lag, "max_lag", min = 1L, allow_inf = TRUE)
  }
  tol <- .mm_check_scalar(tol, "tol", lower = 0, upper = 0.5,
                          inclusive = c(FALSE, FALSE))
  .mm_check_period(period)
  recipes::add_step(recipe, .mm_step_adstock_new(
    terms = rlang::enquos(...),
    index = index, by = by, decay = decay, max_lag = max_lag,
    normalise = normalise, carry_over = carry_over, na_action = na_action,
    tol = tol, period = period, role = role, trained = trained,
    columns = columns, states = states, last_index = last_index,
    history = history, skip = skip, id = id
  ))
}

#' @keywords internal
#' @noRd
.mm_step_adstock_new <- function(terms, index, by, decay, max_lag, normalise,
                                 carry_over, na_action, tol, period, role,
                                 trained, columns, states, last_index, history,
                                 skip, id) {
  recipes::step(
    subclass = "adstock", terms = terms, index = index, by = by,
    decay = decay, max_lag = max_lag, normalise = normalise,
    carry_over = carry_over, na_action = na_action, tol = tol,
    period = period, role = role, trained = trained, columns = columns,
    states = states, last_index = last_index, history = history,
    skip = skip, id = id
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
    states[[i]] <- lapply(values, function(v) {
      as.numeric(adstock_state(v, decay = x$decay, max_lag = x$max_lag))
    })
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
      i = "Adstock treats consecutive rows as consecutive periods, so gaps in \\
           the index change what {.arg decay} means.",
      i = if (grouped) "Affected: {.val {utils::head(shown, 5)}}.",
      i = "Fill the missing periods with zero spend before prepping."
    ))
  }

  .mm_step_adstock_new(
    terms = x$terms, index = x$index, by = x$by, decay = x$decay,
    max_lag = x$max_lag, normalise = x$normalise, carry_over = x$carry_over,
    na_action = x$na_action, tol = x$tol, period = x$period, role = x$role,
    trained = TRUE, columns = col_names, states = states,
    last_index = last_index, history = history, skip = x$skip, id = x$id
  )
}

#' @exportS3Method recipes::bake
bake.step_adstock <- function(object, new_data, ...) {
  recipes::check_new_data(c(object$columns, object$index, object$by),
                          object, new_data)
  if (nrow(new_data) == 0L) return(new_data)
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
    k <- if (continues && object$carry_over != "cold") carry$k else 0L

    # The filter runs over the training rows before the block and then over
    # the block. A finite kernel reaches back only `max_lag - 1` rows, so only
    # those are needed; the recursive kernel needs all of them.
    from <- if (is.finite(object$max_lag)) {
      max(1L, k - object$max_lag + 2L)
    } else {
      1L
    }
    for (cn in object$columns) {
      prefix <- if (k >= from) h$values[[cn]][from:k] else numeric(0)
      out <- adstock_geometric(
        c(prefix, media[[cn]][o]), decay = object$decay,
        max_lag = object$max_lag, normalise = object$normalise
      )
      media[[cn]][o] <- out[length(prefix) + seq_along(o)]
    }
  }
  for (cn in object$columns) new_data[[cn]] <- media[[cn]]

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
      i = "Adstock treats consecutive rows as consecutive periods, so missing \\
           periods inside the new data change what {.arg decay} means.",
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
  title <- sprintf("Adstock (decay = %s, max_lag = %s) on ",
                   .mm_fmt_param(x$decay), .mm_fmt_param(x$max_lag))
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_adstock <- function(x, ...) {
  if (!isTRUE(x$trained)) {
    terms <- .mm_sel2char(x$terms)
    n <- length(terms)
    return(.mm_tibble(
      terms = terms, group = rep(NA_character_, n),
      decay = rep(.mm_param_value(x$decay), n), state = rep(NA_real_, n),
      state_length = rep(NA_integer_, n), id = rep(x$id, n)
    ))
  }
  keys <- names(x$states)
  n <- length(keys) * length(x$columns)
  # For the recursive kernel the state is one number, the accumulator. For a
  # truncated kernel it is a tail of `max_lag - 1` values, and the most recent
  # of those is what is reported here, with the full length alongside so the
  # difference is visible.
  last_value <- function(s) if (length(s) == 0L) NA_real_ else s[length(s)]
  .mm_tibble(
    terms = rep(x$columns, times = length(keys)),
    group = if (.mm_has_by(x$by)) {
      rep(.mm_show_key(keys), each = length(x$columns))
    } else {
      rep(NA_character_, n)
    },
    decay = rep(x$decay, n),
    state = unlist(lapply(x$states, function(s) {
      vapply(s[x$columns], last_value, numeric(1), USE.NAMES = FALSE)
    }), use.names = FALSE),
    state_length = unlist(lapply(x$states, function(s) {
      vapply(s[x$columns], length, integer(1), USE.NAMES = FALSE)
    }), use.names = FALSE),
    id = rep(x$id, n)
  )
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_adstock <- function(x, ...) "mediamix"

#' @exportS3Method generics::tunable
tunable.step_adstock <- function(x, ...) {
  data.frame(
    name = c("decay", "max_lag"),
    call_info = I(list(
      list(pkg = "mediamix", fun = "carryover_decay"),
      list(pkg = "mediamix", fun = "carryover_max_lag")
    )),
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
