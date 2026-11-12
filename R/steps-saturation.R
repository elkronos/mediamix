#' Saturation transformation as a recipe step
#'
#' Applies a diminishing-returns curve to media columns inside a \pkg{recipes}
#' pipeline, with the half-saturation point expressed relative to each column's
#' own scale so that one tunable parameter works across channels.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions choosing the media columns. As
#'   with [step_adstock()], select media only; `all_predictors()` also picks
#'   up the controls.
#' @param type Which curve: `"hill"` (the default), `"exponential"`,
#'   `"michaelis_menten"` or `"power"`.
#' @param half_max Half-saturation point, expressed as a *fraction* of the
#'   column's reference level, in `(0, 2]`. `0.5` means the curve reaches half
#'   its ceiling at half the reference level. Ignored when `type = "power"`,
#'   which has no half-saturation point, and it cannot be tuned there.
#'   Otherwise tunable, over `[0.05, 1]` by default.
#' @param shape Means different things to different curves, and is validated
#'   accordingly. For `"hill"` it is the Hill exponent, any positive number:
#'   `1` gives a concave curve and values above `1` an S-shape with a
#'   threshold. For `"power"` it is the exponent itself and must lie in
#'   `(0, 1]`, since a power above 1 is not saturating at all. Ignored by
#'   `"exponential"` and `"michaelis_menten"`, and it cannot be tuned there.
#'   Tunable for `"hill"` and `"power"`; the advertised range narrows to
#'   `[0.1, 1]` when `type = "power"`, so a joint search cannot propose a value
#'   the curve would reject.
#' @param ref How to set each column's reference level from the training data:
#'   `"max"` (the default), `"q99"`, `"q95"` or `"mean"`. Using a high quantile
#'   rather than the maximum makes the reference robust to a single outlying
#'   week, but a channel that ran in only a few training periods can have a
#'   95th percentile of zero.
#' @param by Optional character vector naming grouping columns. When given,
#'   each group gets its own reference level, so a small geography is placed
#'   on the curve by its own scale rather than by the largest one's. `NULL`
#'   (the default) pools all rows.
#' @param role Not used by this step, since new columns are not created.
#' @param trained Has the step been prepared?
#' @param columns,refs,group_refs Populated by `prep()`; not set directly.
#'
#' @section Negative and missing values:
#' Saturation curves are defined for non-negative media. Missing values are
#' treated as zero spend, and negative values are clamped to zero with a
#' warning. The commonest way to produce negatives is a `step_normalize()`
#' earlier in the recipe, which centres each column on its mean and therefore
#' sends about half of every column below zero. Saturate first, or normalise
#' something other than the media. Infinite values are an error.
#' @param skip Should the step be skipped when baking? Must remain `FALSE`.
#' @param id A unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @section Why the half-saturation point is relative:
#' Television spend might run to six figures a week and affiliate to three. An
#' absolute half-saturation point is therefore a different parameter for every
#' channel, which makes joint tuning across channels either meaningless or a
#' separate parameter per column.
#'
#' Expressing it as a fraction of each column's own reference level fixes this:
#' `half_max = 0.4` means "half the ceiling is reached at 40% of this column's
#' reference level" and reads the same way for every channel, so a single
#' tunable parameter covers them all. With the default `ref = "max"` the
#' reference is the column's training maximum. After [step_adstock()] the
#' column holds adstocked values, so that is the peak adstocked value, not the
#' peak weekly spend. `prep()` stores one number per column, or per column and
#' group with `by`, learned from the training data only, and `bake()` applies
#' it unchanged. A test-set value above the training reference is not clipped:
#' it lands further up the curve.
#'
#' Under the hood, `half_max` is converted to whichever native parameter the
#' chosen curve uses -- the Hill and Michaelis--Menten half-points directly, and
#' \eqn{\log 2 / x_{1/2}} for the exponential rate -- so the fraction means the
#' same thing across all three bounded curves.
#'
#' @section Panel data:
#' Without `by`, the reference level is computed over every row, so on a panel
#' it is set by the largest geography: a small one sits near the bottom of the
#' curve and never saturates. With `by`, each group's reference comes from its
#' own training rows. A group not seen during `prep()` uses the pooled
#' reference, the one computed over every training row, with a warning. So
#' does a group whose own training values give no positive reference, such as
#' one where a channel never ran; `prep()` warns about those.
#'
#' @section Order:
#' Put this step after `step_adstock()`, so that the curve acts on accumulated
#' exposure rather than on each period's spend alone. With the default
#' normalised kernel the result stays below the ceiling in either order; with
#' `normalise = FALSE`, saturating first would also let carryover accumulate
#' the capped values past the ceiling. See [media_transform()].
#'
#' @section Tidying:
#' `tidy()` returns a tibble with one row per selected column, or per column
#' and group with `by`, and the columns `terms`, `group` (only with `by`),
#' `type`, `half_max`, `shape`, `reference` and `id`. `half_max` and `shape`
#' are `NA` where the curve ignores them, and before `prep()` when they are
#' marked with `tune()`. `reference` is the learned reference level, `NA`
#' before `prep()`.
#'
#' @seealso [saturate()], [step_adstock()], [saturation_half_max()]
#'
#' @examplesIf requireNamespace("recipes", quietly = TRUE)
#' library(recipes)
#' data(mm_weekly)
#'
#' rec <- recipe(revenue ~ ., data = mm_weekly) |>
#'   step_adstock(tv, video, search, index = "date", by = "geo", decay = 0.7) |>
#'   step_saturation(tv, video, search, half_max = 0.4, shape = 1.5) |>
#'   prep()
#'
#' out <- bake(rec, new_data = NULL)
#' range(out$tv)
#' tidy(rec, number = 2)
#'
#' # One reference level per geography
#' rec_geo <- recipe(revenue ~ ., data = mm_weekly) |>
#'   step_adstock(tv, index = "date", by = "geo", decay = 0.7) |>
#'   step_saturation(tv, half_max = 0.4, shape = 1.5, by = "geo") |>
#'   prep()
#' tidy(rec_geo, number = 2)
#' @export
step_saturation <- function(recipe, ...,
                            type = c("hill", "exponential",
                                     "michaelis_menten", "power"),
                            half_max = 0.5,
                            shape = 1,
                            ref = c("max", "q99", "q95", "mean"),
                            by = NULL,
                            role = NA,
                            trained = FALSE,
                            columns = NULL,
                            refs = NULL,
                            group_refs = NULL,
                            skip = FALSE,
                            id = .mm_rand_id("saturation")) {
  .mm_require_recipes()
  type <- match.arg(type)
  ref <- match.arg(ref)
  if (!is.null(by) && !is.character(by)) {
    cli::cli_abort("{.arg by} must be a character vector of column names.")
  }
  # Tuning an argument the curve ignores spends the search on a dimension that
  # changes nothing, and for "power" the placeholder used to reach bake().
  if (type == "power" && .mm_is_tune(half_max)) {
    cli::cli_abort(c(
      '{.arg half_max} cannot be tuned when {.code type = "power"}.',
      i = "The power curve has no half-saturation point, so it ignores \\
           {.arg half_max}.",
      i = "Tune {.arg shape}, the exponent, instead."
    ))
  }
  if (type %in% c("exponential", "michaelis_menten") && .mm_is_tune(shape)) {
    cli::cli_abort(c(
      '{.arg shape} cannot be tuned when {.code type = "{type}"}.',
      i = "That curve ignores {.arg shape}.",
      i = 'Tune {.arg half_max} instead, or use {.code type = "hill"}.'
    ))
  }
  # Validate concrete values, but stand aside for `tune()` placeholders:
  # both of these arguments are tunable, and forcing the check on a `tune()`
  # marker would make them untunable.
  if (!.mm_is_tune(half_max)) {
    half_max <- .mm_check_scalar(half_max, "half_max", lower = 0, upper = 2,
                                 inclusive = c(FALSE, TRUE))
  }
  # For `type = "power"`, `shape` IS the exponent and must be in (0, 1]; the
  # other curves take any positive Hill exponent. Checking here means a bad
  # value is caught when the step is written, not part-way through a resample.
  if (!.mm_is_tune(shape)) {
    if (type == "power") {
      shape <- .mm_check_scalar(shape, "shape", lower = 0, upper = 1,
                                inclusive = c(FALSE, TRUE))
    } else {
      shape <- .mm_check_scalar(shape, "shape", lower = 0,
                                inclusive = c(FALSE, TRUE))
    }
  }
  if (isTRUE(skip)) {
    cli::cli_abort(c(
      "{.arg skip} must be {.code FALSE} for {.fn step_saturation}.",
      i = "Saturation is a predictor transform and must run at prediction time."
    ))
  }
  recipes::add_step(recipe, .mm_step_saturation_new(
    terms = rlang::enquos(...), type = type, half_max = half_max,
    shape = shape, ref = ref, by = by, role = role, trained = trained,
    columns = columns, refs = refs, group_refs = group_refs, skip = skip,
    id = id
  ))
}

#' @keywords internal
#' @noRd
.mm_step_saturation_new <- function(terms, type, half_max, shape, ref, by,
                                    role, trained, columns, refs, group_refs,
                                    skip, id) {
  recipes::step(
    subclass = "saturation", terms = terms, type = type, half_max = half_max,
    shape = shape, ref = ref, by = by, role = role, trained = trained,
    columns = columns, refs = refs, group_refs = group_refs, skip = skip,
    id = id
  )
}

#' @exportS3Method recipes::prep
prep.step_saturation <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  grouped <- .mm_has_by(x$by)
  if (grouped) {
    .mm_assert_cols(training, x$by)
    col_names <- setdiff(col_names, x$by)
  }
  recipes::check_type(training[, col_names, drop = FALSE],
                      types = c("double", "integer"))
  .mm_check_media_finite(training, col_names)

  refs <- vapply(col_names, function(cn) .mm_sat_ref(training[[cn]], x$ref),
                 numeric(1))
  .mm_check_refs(refs, training, x$ref)

  group_refs <- NULL
  if (grouped) {
    grp <- .mm_step_groups(training, x$by)
    group_refs <- matrix(NA_real_, nrow = length(grp), ncol = length(col_names),
                         dimnames = list(names(grp), col_names))
    fallback <- character(0)
    for (cn in col_names) {
      v <- training[[cn]]
      r <- vapply(grp, function(rows) .mm_sat_ref(v[rows], x$ref), numeric(1),
                  USE.NAMES = FALSE)
      bad <- !is.finite(r) | r <= 0
      if (any(bad)) {
        fallback <- c(fallback,
                      sprintf("%s in %s", cn, .mm_show_key(names(grp)[bad])))
        r[bad] <- refs[[cn]]
      }
      group_refs[, cn] <- r
    }
    if (length(fallback) > 0L) {
      cli::cli_warn(c(
        "Using the pooled reference level for {length(fallback)} column and \\
         group pair{?s} with no positive reference level of {?its/their} own.",
        i = "Affected: {.val {utils::head(fallback, 5)}}.",
        i = "This happens when a channel never ran in a group during the \\
             training period."
      ))
    }
  }

  .mm_step_saturation_new(
    terms = x$terms, type = x$type, half_max = x$half_max, shape = x$shape,
    ref = x$ref, by = x$by, role = x$role, trained = TRUE, columns = col_names,
    refs = refs, group_refs = group_refs, skip = x$skip, id = x$id
  )
}

#' @exportS3Method recipes::bake
bake.step_saturation <- function(object, new_data, ...) {
  recipes::check_new_data(c(object$columns, object$by), object, new_data)
  if (nrow(new_data) == 0L) return(new_data)
  .mm_check_media_finite(new_data, object$columns)

  if (.mm_has_by(object$by) && !is.null(object$group_refs)) {
    grp <- .mm_step_groups(new_data, object$by)
    pos <- match(names(grp), rownames(object$group_refs))
    unseen <- names(grp)[is.na(pos)]
  } else {
    grp <- list(seq_len(nrow(new_data)))
    pos <- NA_integer_
    unseen <- character(0)
  }

  negatives <- character(0)
  for (cn in object$columns) {
    v <- as.numeric(new_data[[cn]])
    v[is.na(v)] <- 0
    if (any(v < 0)) {
      negatives <- c(negatives, cn)
      v[v < 0] <- 0
    }
    out <- numeric(length(v))
    for (i in seq_along(grp)) {
      rows <- grp[[i]]
      r <- if (is.na(pos[i])) {
        object$refs[[cn]]
      } else {
        object$group_refs[pos[i], cn]
      }
      out[rows] <- .mm_saturate_rel(v[rows], r, object$type, object$half_max,
                                    object$shape)
    }
    new_data[[cn]] <- out
  }
  if (length(negatives) > 0L) {
    cli::cli_warn(c(
      "Negative values were clamped to zero in {length(negatives)} column{?s}.",
      i = "Saturation curves are defined for non-negative media.",
      i = "Affected: {.val {utils::head(negatives, 5)}}.",
      i = "A {.fn step_normalize} before {.fn step_saturation} centres media \\
           on zero and sends half of every column negative. Saturate first, \\
           or normalise something else."
    ))
  }
  if (length(unseen) > 0L) {
    shown_unseen <- .mm_show_key(unseen)
    cli::cli_warn(c(
      "{length(unseen)} group{?s} not seen during {.fn prep} use{?s/} the \\
       pooled reference level.",
      i = "Affected: {.val {utils::head(shown_unseen, 5)}}."
    ))
  }
  new_data
}

#' @export
print.step_saturation <- function(x, width = max(20, options()$width - 30), ...) {
  uses <- .mm_sat_params(x$type)
  shown <- vapply(uses, function(p) {
    paste(p, "=", .mm_fmt_param(x[[p]]))
  }, character(1))
  title <- sprintf("Saturation (%s, %s) on ", x$type,
                   paste(shown, collapse = ", "))
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_saturation <- function(x, ...) {
  uses <- .mm_sat_params(x$type)
  # A parameter the curve ignores is reported as NA; reporting the supplied
  # value would suggest it did something.
  half_max <- NA_real_
  shape <- NA_real_
  if ("half_max" %in% uses) half_max <- .mm_param_value(x$half_max)
  if ("shape" %in% uses) shape <- .mm_param_value(x$shape)
  grouped <- .mm_has_by(x$by)

  if (!isTRUE(x$trained)) {
    terms <- .mm_sel2char(x$terms)
    group <- NA_character_
    reference <- NA_real_
  } else if (grouped && !is.null(x$group_refs)) {
    keys <- rownames(x$group_refs)
    terms <- rep(x$columns, times = length(keys))
    group <- rep(.mm_show_key(keys), each = length(x$columns))
    reference <- as.vector(t(x$group_refs))
  } else {
    terms <- x$columns
    group <- NA_character_
    reference <- unname(x$refs)
  }
  n <- length(terms)
  cols <- list(terms = terms)
  if (grouped) cols$group <- rep_len(group, n)
  cols <- c(cols, list(
    type = rep(x$type, n), half_max = rep(half_max, n),
    shape = rep(shape, n), reference = rep_len(reference, n),
    id = rep(x$id, n)
  ))
  do.call(.mm_tibble, cols)
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_saturation <- function(x, ...) "mediamix"

#' @exportS3Method generics::tunable
tunable.step_saturation <- function(x, ...) {
  # Only the parameters the chosen curve uses are offered. `shape` means
  # different things to different curves: for "power" it is the exponent,
  # valid only up to 1, so advertising the Hill range would hand tune_grid()
  # values that abort mid-resample.
  uses <- .mm_sat_params(x$type)
  info <- list(
    half_max = list(pkg = "mediamix", fun = "saturation_half_max"),
    shape = if (identical(x$type, "power")) {
      list(pkg = "mediamix", fun = "saturation_shape", range = c(0.1, 1))
    } else {
      list(pkg = "mediamix", fun = "saturation_shape")
    }
  )
  data.frame(
    name = uses,
    call_info = I(unname(info[uses])),
    source = "recipe",
    component = "step_saturation",
    component_id = x$id,
    stringsAsFactors = FALSE
  )
}

# ---- internals ---------------------------------------------------------------

# The parameters each curve uses.
#' @keywords internal
#' @noRd
.mm_sat_params <- function(type) {
  switch(type,
         hill = c("half_max", "shape"),
         exponential = ,
         michaelis_menten = "half_max",
         power = "shape")
}

#' @keywords internal
#' @noRd
.mm_sat_ref <- function(v, ref) {
  v <- as.numeric(v)
  v <- v[is.finite(v)]
  if (length(v) == 0L) return(NA_real_)
  switch(ref,
         max = max(v),
         q99 = unname(stats::quantile(v, 0.99)),
         q95 = unname(stats::quantile(v, 0.95)),
         mean = mean(v))
}

# A reference level has to be positive. Two different things make it fail: a
# column with no positive values at all, and a quantile or mean that lands on
# zero although the column has positive values, as for a channel launched near
# the end of the training period.
#' @keywords internal
#' @noRd
.mm_check_refs <- function(refs, data, ref, call = parent.frame()) {
  bad <- names(refs)[!is.finite(refs) | refs <= 0]
  if (length(bad) == 0L) return(invisible(NULL))
  positive <- vapply(bad, function(cn) {
    any(as.numeric(data[[cn]]) > 0, na.rm = TRUE)
  }, logical(1))
  low <- bad[positive]
  if (length(low) > 0L) {
    cli::cli_abort(c(
      "The {.val {ref}} reference level is not positive for \\
       {length(low)} column{?s} with positive values: {.val {low}}.",
      i = "Most of {cli::qty(length(low))}{?its/their} training values are \\
           zero, as for a channel that launched late in the training period.",
      i = 'Use {.code ref = "max"}, or train on a period when the channel ran.'
    ), call = call)
  }
  cli::cli_abort(c(
    "{length(bad)} column{?s} ha{?s/ve} no positive values in the training \\
     data: {.val {bad}}.",
    i = "A column that is zero throughout cannot be saturated relative to \\
         its own scale. Drop it, or use an absolute {.fn saturate_hill} call."
  ), call = call)
}

# Apply the curve with the half-saturation point scaled by a reference level.
#' @keywords internal
#' @noRd
.mm_saturate_rel <- function(v, ref, type, half_max, shape) {
  switch(
    type,
    hill = saturate_hill(v, half_max = half_max * ref, shape = shape),
    michaelis_menten = saturate_michaelis_menten(v, vmax = 1,
                                                 km = half_max * ref),
    exponential = saturate_exponential(v, rate = log(2) / (half_max * ref)),
    power = saturate_power(v / ref, exponent = shape)
  )
}
