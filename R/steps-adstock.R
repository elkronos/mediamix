#' Adstock transformation as a recipe step
#'
#' Applies geometric adstock to one or more columns inside a \pkg{recipes}
#' pipeline, carrying the filter's state across the train/test boundary instead
#' of emitting missing values there.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions choosing the media columns.
#' @param index Name of the time column, as a string. Required: adstock is a
#'   time-ordered filter, so the step needs to know the order and the spacing.
#'   The column is used for ordering and validation and is not itself
#'   transformed.
#' @param by Optional character vector naming grouping columns. Adstock is
#'   applied independently within each group, which is what geo-level and panel
#'   models need. This is an explicit argument because \pkg{recipes} has no
#'   group awareness and a grouped data frame does not reliably survive into
#'   `bake()`.
#' @param decay Geometric decay coefficient in `[0, 1]`. Tunable.
#' @param max_lag Kernel length; `Inf` (the default) uses the infinite recursive
#'   form. Tunable.
#' @param normalise Should the kernel sum to 1? See [adstock_geometric()].
#' @param carry_over How to treat state at the start of new data.
#'   `"auto"` (the default) warm-starts when the new data continues the training
#'   series and cold-starts otherwise, warning when it does. `"warm"` errors
#'   rather than silently cold-starting on a gap or an unseen group; data that
#'   overlaps the training period still cold-starts, since re-baking data the
#'   step has already seen should reproduce the prepared output. `"cold"` always
#'   restarts from zero, which reproduces the behaviour of a lag-based step.
#' @param na_action What to do about missing media values. `"zero"` (the
#'   default) treats missing media as no media, which is usually right for
#'   spend and is what lets a recipe run over a real, patchy panel without
#'   stopping. It is still a substantive assumption -- an `NA` becomes a real
#'   number in the output -- so `"error"` is available when you would rather
#'   find out. Note this differs from [adstock_geometric()], whose default is
#'   `"error"`; a step has to survive resampling, and a bare function call
#'   does not.
#' @param tol Relative tolerance on the step between periods when deciding
#'   whether two blocks are contiguous. The default of `0.25` is loose so that
#'   calendar months, which vary from 28 to 31 days, are not mistaken for gaps.
#' @param role Not used by this step, since new columns are not created.
#' @param trained Has the step been prepared?
#' @param columns,states,last_index,period Populated by `prep()`; not set
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
#' on. `recipes::step_lag()` answers this by emitting `NA`, silently; its
#' documented remedy, `step_naomit()`, changes the row count, which breaks
#' `fit_resamples()` outright. That is not an escape hatch, it is a dead end.
#'
#' The way out is to store *filter state* rather than rows. Geometric adstock is
#' an infinite impulse response filter whose entire memory is one number per
#' series,
#' \deqn{a_t = x_t + \theta a_{t-1},}
#' so `prep()` stores one double per column per group. That is \eqn{O(1)} state.
#' It retains no training observations, so it adds nothing meaningful to the
#' size of a fitted workflow and carries no raw data with it. Only the truncated
#' forms, where `max_lag` is finite, need an actual tail of `max_lag - 1`
#' values.
#'
#' @section What bake() does at the boundary:
#' `prep()` records, per group, the terminal filter state, the last time index
#' seen, and the period inferred from the median spacing of the index. `bake()`
#' then compares the start of the new data against that record:
#'
#' \describe{
#'   \item{Exactly contiguous}{Warm-start from the stored state. The result is
#'     identical to filtering the whole series at once, with no prepended rows.}
#'   \item{Overlaps the training data}{Cold-start. This is what catches
#'     `bake(rec, new_data = training)`, which would otherwise double-count the
#'     training period's own carryover.}
#'   \item{A gap}{Cold-start, and warn, naming the gap size and the group.}
#'   \item{An unseen group}{Cold-start, and warn.}
#' }
#'
#' Row count and row order are preserved in every case. The step sorts by
#' `index` internally and restores the original order before returning, so
#' nothing downstream sees a reordered table.
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
#' # The stored state is one number per column per geo
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
                         role = NA,
                         trained = FALSE,
                         columns = NULL,
                         states = NULL,
                         last_index = NULL,
                         period = NULL,
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
  tol <- .mm_check_scalar(tol, "tol", lower = 0, inclusive = c(FALSE, TRUE))
  recipes::add_step(recipe, .mm_step_adstock_new(
    terms = rlang::enquos(...),
    index = index, by = by, decay = decay, max_lag = max_lag,
    normalise = normalise, carry_over = carry_over, na_action = na_action,
    tol = tol, role = role, trained = trained, columns = columns,
    states = states, last_index = last_index, period = period, skip = skip,
    id = id
  ))
}

#' @keywords internal
#' @noRd
.mm_step_adstock_new <- function(terms, index, by, decay, max_lag, normalise,
                                 carry_over, na_action, tol, role, trained,
                                 columns, states, last_index, period, skip, id) {
  recipes::step(
    subclass = "adstock", terms = terms, index = index, by = by,
    decay = decay, max_lag = max_lag, normalise = normalise,
    carry_over = carry_over, na_action = na_action, tol = tol, role = role,
    trained = trained, columns = columns, states = states,
    last_index = last_index, period = period, skip = skip, id = id
  )
}

#' @exportS3Method recipes::prep
prep.step_adstock <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (is.null(x$index)) {
    cli::cli_abort(c(
      "{.arg index} is required by {.fn step_adstock}.",
      i = "Adstock is a time-ordered filter, so it must know the order and \\
           spacing of the rows.",
      i = 'Name the time column, for example {.code index = "date"}.'
    ))
  }
  .mm_assert_cols(training, c(x$index, x$by))
  # Remove the index and grouping columns BEFORE type-checking, so that the
  # natural `all_predictors()` spelling works instead of failing on the date
  # and the geography the step was told about explicitly.
  col_names <- setdiff(col_names, c(x$index, x$by))
  if (length(col_names) == 0L) {
    cli::cli_abort(c(
      "{.fn step_adstock} selected no media columns to transform.",
      i = "The selection resolved only to {.arg index} and {.arg by} columns."
    ))
  }
  recipes::check_type(training[, col_names, drop = FALSE],
                      types = c("double", "integer"))

  idx <- .mm_index_numeric(training[[x$index]], x$index)
  grp <- .mm_step_groups(training, x$by)
  period <- .mm_infer_period(training[[x$index]], x$index)

  states <- list()
  last_index <- list()
  irregular <- character(0)

  for (g in names(grp)) {
    rows <- grp[[g]]
    o <- rows[order(idx[rows])]
    .mm_check_duplicated_index(idx[o], if (is.null(x$by)) NULL else g)
    if (.mm_irregular_steps(idx[o], period, tol = x$tol) > 0L) {
      irregular <- c(irregular, g)
    }
    last_index[[g]] <- max(idx[o])
    states[[g]] <- lapply(col_names, function(cn) {
      adstock_state(as.numeric(training[[cn]][o]), decay = x$decay,
                    max_lag = x$max_lag, na_action = x$na_action)
    })
    names(states[[g]]) <- col_names
  }

  if (!is.finite(period) || period <= 0) {
    cli::cli_warn(c(
      "The spacing between periods could not be inferred from {.arg index}.",
      i = "The training data has fewer than two distinct time points.",
      i = "{.fn bake} will cold-start every series rather than continuing it."
    ))
  }
  if (length(irregular) > 0L) {
    shown <- .mm_show_key(irregular)
    cli::cli_warn(c(
      "The time index is unevenly spaced in {length(irregular)} series.",
      i = "Adstock treats consecutive rows as consecutive periods, so gaps in \\
           the index change what {.arg decay} means.",
      i = "Affected: {.val {utils::head(shown, 5)}}.",
      i = "Fill the missing periods with zero spend before prepping."
    ))
  }

  .mm_step_adstock_new(
    terms = x$terms, index = x$index, by = x$by, decay = x$decay,
    max_lag = x$max_lag, normalise = x$normalise, carry_over = x$carry_over,
    na_action = x$na_action, tol = x$tol, role = x$role, trained = TRUE,
    columns = col_names, states = states, last_index = last_index,
    period = period, skip = x$skip, id = x$id
  )
}

#' @exportS3Method recipes::bake
bake.step_adstock <- function(object, new_data, ...) {
  recipes::check_new_data(c(object$columns, object$index, object$by),
                          object, new_data)
  if (nrow(new_data) == 0L) return(new_data)

  idx <- .mm_index_numeric(new_data[[object$index]], object$index)
  grp <- .mm_step_groups(new_data, object$by)

  gap_warn <- character(0)
  unseen_warn <- character(0)
  irregular_warn <- character(0)
  no_period <- FALSE

  for (g in names(grp)) {
    rows <- grp[[g]]
    o <- rows[order(idx[rows])]
    # The same preconditions prep() enforces. An un-aggregated join produces
    # repeated periods at prediction time far more often than at training
    # time, and treating two rows for one week as two consecutive weeks is a
    # silent arithmetic error, not a stylistic one.
    .mm_check_duplicated_index(idx[o], if (is.null(object$by)) NULL else g)
    known <- object$last_index[[g]]
    case <- .mm_carry_case(idx[o], known, object$period, tol = object$tol)
    if (case == "unknown_period") no_period <- TRUE
    # Only report irregularity for data prep() has not already seen. An overlap
    # is prep()'s own internal re-bake of the training set, which has been
    # checked and warned about once already.
    if (case != "overlap" &&
        .mm_irregular_steps(idx[o], object$period, tol = object$tol) > 0L) {
      irregular_warn <- c(irregular_warn, g)
    }

    warm <- switch(
      object$carry_over,
      cold = FALSE,
      warm = {
        # An overlap means the step is being re-applied to data it already
        # saw, which is exactly what recipes does internally during prep().
        # The right answer there is a cold start reproducing the prep-time
        # output, not an error.
        if (case %in% c("gap", "unseen", "unknown_period")) {
          shown_g <- .mm_show_key(g)
          cli::cli_abort(c(
            '{.code carry_over = "warm"} requires the new data to continue the \\
             training series.',
            x = "Series {.val {shown_g}} is {case} relative to what \\
                 {.fn prep} saw.",
            i = 'Use {.code carry_over = "auto"} to fall back to a cold start.'
          ))
        }
        case == "contiguous"
      },
      auto = {
        if (case == "unknown_period") {
          # Nothing to compare against; cold start, reported once below.
        } else if (case == "gap") {
          gap_warn <- c(gap_warn, sprintf(
            "%s (%.3g periods missing)", .mm_show_key(g),
            .mm_gap_size(idx[o], known, object$period)))
        } else if (case == "unseen") {
          unseen_warn <- c(unseen_warn, g)
        }
        case == "contiguous"
      }
    )

    for (cn in object$columns) {
      st <- if (warm) object$states[[g]][[cn]] else 0
      new_data[[cn]][o] <- adstock_geometric(
        as.numeric(new_data[[cn]][o]), decay = object$decay,
        max_lag = object$max_lag, normalise = object$normalise,
        state = st, na_action = object$na_action
      )
    }
  }

  if (length(gap_warn) > 0L) {
    cli::cli_warn(c(
      "Cold-starting adstock for {length(gap_warn)} series with a gap after \\
       the training period.",
      i = "Carryover from before the gap is discarded.",
      i = "Affected: {.val {utils::head(gap_warn, 5)}}."
    ))
  }
  if (length(unseen_warn) > 0L) {
    shown_unseen <- .mm_show_key(unseen_warn)
    cli::cli_warn(c(
      "Cold-starting adstock for {length(unseen_warn)} group{?s} not seen \\
       during {.fn prep}.",
      i = "Affected: {.val {utils::head(shown_unseen, 5)}}."
    ))
  }
  if (length(irregular_warn) > 0L) {
    shown_irr <- .mm_show_key(irregular_warn)
    cli::cli_warn(c(
      "The time index is unevenly spaced within the new data for \\
       {length(irregular_warn)} series.",
      i = "Adstock treats consecutive rows as consecutive periods, so missing \\
           periods inside the new data change what {.arg decay} means.",
      i = "Affected: {.val {utils::head(shown_irr, 5)}}.",
      i = "Fill the missing periods with zero spend before baking."
    ))
  }
  if (no_period) {
    cli::cli_warn(c(
      "Cold-starting adstock: the period spacing is unknown.",
      i = "{.fn prep} saw fewer than two distinct time points, so there is \\
           nothing to judge contiguity against."
    ))
  }
  new_data
}

#' @export
print.step_adstock <- function(x, width = max(20, options()$width - 30), ...) {
  lag_txt <- if (is.infinite(x$max_lag)) "Inf" else format(x$max_lag)
  title <- sprintf("Adstock (decay = %s, max_lag = %s) on ",
                   format(x$decay), lag_txt)
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_adstock <- function(x, ...) {
  if (!x$trained) {
    return(data.frame(terms = .mm_sel2char(x$terms), group = NA_character_,
                      decay = x$decay, state = NA_real_,
                      state_length = NA_integer_, id = x$id,
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(names(x$states), function(g) {
    data.frame(
      terms = names(x$states[[g]]),
      # Groups are keyed internally by a joined string; show the parts.
      group = .mm_show_key(g),
      decay = x$decay,
      # For the recursive kernel the state is one number, which is the whole
      # point of the step. For a truncated kernel it is a tail of `max_lag - 1`
      # values, and the most recent of those is what is reported here, with the
      # full length alongside so the difference is visible.
      state = vapply(x$states[[g]], function(s) {
        if (length(s) == 0L) NA_real_ else s[length(s)]
      }, numeric(1)),
      state_length = vapply(x$states[[g]], length, integer(1)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$id <- x$id
  rownames(out) <- NULL
  out
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
