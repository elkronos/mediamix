#' Saturation transformation as a recipe step
#'
#' Applies a diminishing-returns curve to media columns inside a \pkg{recipes}
#' pipeline, with the half-saturation point expressed relative to each column's
#' own scale so that one tunable parameter works across channels.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions choosing the media columns.
#' @param type Which curve: `"hill"` (the default), `"exponential"`,
#'   `"michaelis_menten"` or `"power"`.
#' @param half_max Half-saturation point, expressed as a *fraction* of the
#'   column's reference level, in `(0, 2]`. `0.5` means the curve reaches half
#'   its ceiling at half the reference spend. Ignored when `type = "power"`,
#'   which has no half-saturation point. Tunable, over `[0.05, 1]` by default.
#' @param shape Means different things to different curves, and is validated
#'   accordingly. For `"hill"` it is the Hill exponent, any positive number:
#'   `1` gives a concave curve and values above `1` an S-shape with a
#'   threshold. For `"power"` it is the exponent itself and must lie in
#'   `(0, 1]`, since a power above 1 is not saturating at all. Ignored by
#'   `"exponential"` and `"michaelis_menten"`. Tunable; the advertised range
#'   narrows to `(0.1, 1]` when `type = "power"`, so a joint search cannot
#'   propose a value the curve would reject.
#' @param ref How to set each column's reference level from the training data:
#'   `"max"` (the default), `"q99"`, `"q95"` or `"mean"`. Using a high quantile
#'   rather than the maximum makes the reference robust to a single outlying
#'   week.
#' @param role Not used by this step, since new columns are not created.
#' @param trained Has the step been prepared?
#' @param columns,refs Populated by `prep()`; not set directly.
#'
#' @section Negative and missing values:
#' Saturation curves are defined for non-negative media. Missing values are
#' treated as zero spend, and negative values are clamped to zero with a
#' warning. The commonest way to produce negatives is a `step_normalize()`
#' earlier in the recipe, which centres each column on its mean and therefore
#' sends about half of every column below zero. Saturate first, or normalise
#' something other than the media.
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
#' `half_max = 0.4` means "half the ceiling is reached at 40% of this channel's
#' peak spend" and reads the same way for every channel, so a single tunable
#' parameter covers them all. `prep()` stores one number per column, learned
#' from the training data only, and `bake()` applies it unchanged. A test-set
#' spend above the training reference is not clipped: it simply lands further
#' up the curve.
#'
#' Under the hood, `half_max` is converted to whichever native parameter the
#' chosen curve uses -- the Hill and Michaelis--Menten half-points directly, and
#' \eqn{\log 2 / x_{1/2}} for the exponential rate -- so the fraction means the
#' same thing across all three bounded curves.
#'
#' @section Order:
#' Put this step *after* `step_adstock()`. Saturating first caps each period's
#' spend in isolation and then lets carryover accumulate the capped values past
#' the ceiling. See [media_transform()].
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
#' @export
step_saturation <- function(recipe, ...,
                            type = c("hill", "exponential",
                                     "michaelis_menten", "power"),
                            half_max = 0.5,
                            shape = 1,
                            ref = c("max", "q99", "q95", "mean"),
                            role = NA,
                            trained = FALSE,
                            columns = NULL,
                            refs = NULL,
                            skip = FALSE,
                            id = .mm_rand_id("saturation")) {
  .mm_require_recipes()
  type <- match.arg(type)
  ref <- match.arg(ref)
  # Validate concrete values, but stand aside for `tune()` placeholders --
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
    shape = shape, ref = ref, role = role, trained = trained,
    columns = columns, refs = refs, skip = skip, id = id
  ))
}

#' @keywords internal
#' @noRd
.mm_step_saturation_new <- function(terms, type, half_max, shape, ref, role,
                                    trained, columns, refs, skip, id) {
  recipes::step(
    subclass = "saturation", terms = terms, type = type, half_max = half_max,
    shape = shape, ref = ref, role = role, trained = trained,
    columns = columns, refs = refs, skip = skip, id = id
  )
}

#' @exportS3Method recipes::prep
prep.step_saturation <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names, drop = FALSE],
                      types = c("double", "integer"))

  refs <- vapply(col_names, function(cn) {
    v <- as.numeric(training[[cn]])
    v <- v[is.finite(v)]
    if (length(v) == 0L) return(NA_real_)
    switch(x$ref,
           max = max(v),
           q99 = unname(stats::quantile(v, 0.99)),
           q95 = unname(stats::quantile(v, 0.95)),
           mean = mean(v))
  }, numeric(1))

  bad <- names(refs)[!is.finite(refs) | refs <= 0]
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{length(bad)} column{?s} ha{?s/ve} a non-positive reference level.",
      x = "Affected: {.val {bad}}.",
      i = "A column that is entirely zero cannot be saturated relative to \\
           its own scale. Drop it, or use an absolute {.fn saturate_hill} call."
    ))
  }

  .mm_step_saturation_new(
    terms = x$terms, type = x$type, half_max = x$half_max, shape = x$shape,
    ref = x$ref, role = x$role, trained = TRUE, columns = col_names,
    refs = refs, skip = x$skip, id = x$id
  )
}

#' @exportS3Method recipes::bake
bake.step_saturation <- function(object, new_data, ...) {
  recipes::check_new_data(object$columns, object, new_data)
  if (nrow(new_data) == 0L) return(new_data)

  negatives <- character(0)
  for (cn in object$columns) {
    v <- as.numeric(new_data[[cn]])
    v[!is.finite(v)] <- 0
    if (any(v < 0)) {
      negatives <- c(negatives, cn)
      v[v < 0] <- 0
    }
    hm <- object$half_max * object$refs[[cn]]
    new_data[[cn]] <- switch(
      object$type,
      hill = saturate_hill(v, half_max = hm, shape = object$shape),
      michaelis_menten = saturate_michaelis_menten(v, vmax = 1, km = hm),
      exponential = saturate_exponential(v, rate = log(2) / hm),
      power = saturate_power(v / object$refs[[cn]], exponent = object$shape)
    )
  }
  if (length(negatives) > 0L) {
    cli::cli_warn(c(
      "Negative values were clamped to zero in {length(negatives)} column{?s}.",
      i = "Saturation curves are defined for non-negative media.",
      i = "Affected: {.val {utils::head(negatives, 5)}}.",
      i = "A {.fn step_normalize} before {.fn step_saturation} centres media \\
           on zero and sends half of every column negative -- saturate first, \\
           or normalise something else."
    ))
  }
  new_data
}

#' @export
print.step_saturation <- function(x, width = max(20, options()$width - 30), ...) {
  title <- sprintf("Saturation (%s, half_max = %s) on ", x$type,
                   format(x$half_max))
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @exportS3Method generics::tidy
tidy.step_saturation <- function(x, ...) {
  if (!x$trained) {
    return(data.frame(terms = .mm_sel2char(x$terms), type = x$type,
                      half_max = x$half_max, shape = x$shape,
                      reference = NA_real_, id = x$id,
                      stringsAsFactors = FALSE))
  }
  data.frame(terms = x$columns, type = x$type,
             # "power" has no half-saturation point; reporting the supplied
             # value would suggest it did something.
             half_max = if (x$type == "power") NA_real_ else x$half_max,
             shape = x$shape, reference = unname(x$refs), id = x$id,
             stringsAsFactors = FALSE)
}

#' @exportS3Method generics::required_pkgs
required_pkgs.step_saturation <- function(x, ...) "mediamix"

#' @exportS3Method generics::tunable
tunable.step_saturation <- function(x, ...) {
  # `shape` means different things to different curves. For "power" it is the
  # exponent, valid only on (0, 1], so advertising the Hill range would hand
  # tune_grid() values that abort mid-resample. "power" also ignores half_max
  # entirely, so there is nothing to tune there.
  if (identical(x$type, "power")) {
    return(data.frame(
      name = "shape",
      call_info = I(list(
        list(pkg = "mediamix", fun = "saturation_shape",
             range = c(0.1, 1), inclusive = c(FALSE, TRUE))
      )),
      source = "recipe",
      component = "step_saturation",
      component_id = x$id,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    name = c("half_max", "shape"),
    call_info = I(list(
      list(pkg = "mediamix", fun = "saturation_half_max"),
      list(pkg = "mediamix", fun = "saturation_shape")
    )),
    source = "recipe",
    component = "step_saturation",
    component_id = x$id,
    stringsAsFactors = FALSE
  )
}
