#' Transform every channel of a media plan
#'
#' Applies [media_transform()] to each channel column of a plan, each with its
#' own carryover and saturation, and returns the plan with those columns
#' replaced. It is the step between a table of spend and the regressors a
#' model is fitted on, and the step [simulate_plan()] and [allocate_budget()]
#' repeat for every plan they evaluate.
#'
#' @param plan A data frame with one row per period and a numeric column of
#'   spend for each channel. Other columns, such as a date or a geography, may
#'   be present.
#' @param spec A list with one entry per channel, named after the channel's
#'   column in `plan`. Each entry is a list with elements `adstock` and
#'   `saturation`, in the form [media_transform()] takes them, and optionally
#'   `order`. An element left out takes [media_transform()]'s default, so an
#'   entry without `adstock` gets geometric carryover at `decay = 0.5`; write
#'   `adstock = NULL` for none.
#' @param by Optional grouping, so that carryover runs within each group, such
#'   as a geography, and never from one into the next: column names of
#'   `plan`, or a vector or data frame of vectors with one element or row per
#'   row of `plan`, as for [media_transform()].
#'
#' @return `plan`, with each column named in `spec` replaced by its transformed
#'   series. The rows keep their order and every other column is unchanged.
#'
#' @details
#' A spec entry may hold only `adstock`, `saturation` and `order`. Any other
#' element name is an error, so a misspelt `saturaton` cannot quietly leave a
#' channel unsaturated. The grouping is set once, by `by`, for every channel,
#' and a `by` inside an entry's `adstock` list is an error.
#'
#' A character `by` whose elements all name columns of `plan` is read as
#' those columns; any other `by` is a grouping vector, or a data frame of
#' them. [simulate_plan()], [reallocate()] and [allocate_budget()] read `by`
#' the same way.
#'
#' An error or warning from one channel's transform is reported with the
#' channel's name, followed by the message [media_transform()] gave.
#'
#' @seealso [media_transform()], [simulate_plan()], [allocate_budget()],
#'   [marginal_roi()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' truth <- attr(mm_weekly, "truth")
#' channels <- c("tv", "video", "search", "social", "display")
#'
#' # One entry per channel, here with the parameters the data was simulated
#' # from.
#' spec <- lapply(stats::setNames(channels, channels), function(ch) list(
#'   adstock = list(decay = truth$decay[[ch]]),
#'   saturation = list(half_max = truth$half_max[[ch]],
#'                     shape = truth$shape[[ch]])
#' ))
#'
#' media <- transform_plan(north, spec)
#' round(head(media[channels], 4), 4)
#'
#' # A panel: carryover runs within each geography
#' panel <- transform_plan(mm_weekly, spec, by = "geo")
#' nrow(panel) == nrow(mm_weekly)
#' @export
transform_plan <- function(plan, spec, by = NULL) {
  here <- environment()
  .mm_check_plan(plan)
  .mm_note_dplyr_groups(plan, by, "transform_plan", arg = "plan")
  channels <- .mm_check_spec(spec)
  .mm_check_channels(plan, channels)
  key <- .mm_plan_by(by, plan, channels)
  out <- lapply(stats::setNames(channels, channels), function(ch) {
    .mm_transform_channel(plan[[ch]], spec[[ch]], key, ch, call = here)
  })
  .mm_set_cols(plan, out)
}

#' Predicted media response of a plan
#'
#' Runs each channel of a spend plan through its transform and multiplies the
#' result by the channel's coefficient, giving the response the model
#' predicts from each channel. With `baseline`, it also reports the change
#' from another plan, which is how a proposed reallocation is checked through
#' the curves rather than by multiplying by a marginal return.
#'
#' @inheritParams transform_plan
#' @param plan A data frame with one row per period and a numeric column of
#'   spend for each channel in `spec`. Spend must be complete, finite and
#'   non-negative. Other columns may be present.
#' @param coefficients A numeric vector of the channels' fitted coefficients,
#'   named by channel, such as `coef(fit)`. Every channel in `spec` needs
#'   one; other names, such as the intercept and the controls, are ignored.
#' @param baseline Optional plan to compare with, such as the current plan
#'   when `plan` is a proposed one. It needs the channel columns of `spec`.
#'   When `by` is given as a grouping vector, which applies to both plans,
#'   `baseline` needs as many rows as `plan`; column names are looked up in
#'   each plan.
#' @param extend Number of zero-spend periods appended to the end of the series
#'   (to each group, when `by` is supplied) before response is summed, as for
#'   [marginal_roi()]. The default `0` counts response only inside the plan's
#'   periods.
#'
#' @return A data frame with one row per channel in `spec`, in its order, and
#'   columns `channel`, `spend` (the channel's total spend in `plan`) and
#'   `response` (its coefficient times the sum of its transformed series).
#'   With `baseline`, two more columns give `plan` minus `baseline`:
#'   `spend_change` and `response_change`.
#'
#' @details
#' A channel's response is the quantity [marginal_roi()] divides by spend: its
#' coefficient times the sum of its transformed series over every row of the
#' plan and any `extend` periods after it. It leaves out the intercept and the
#' controls, which a change of media plan does not move, so the difference in
#' response between two plans is the difference in the KPI the model predicts
#' for them. The sum of `response_change` is the total gain or loss.
#'
#' Two plans of different lengths can be compared, since each response is a
#' total; compare plans over the same periods for a like-for-like answer.
#'
#' @seealso [reallocate()] to build a plan that moves spend between channels,
#'   [allocate_budget()], [marginal_roi()], [transform_plan()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#' truth <- attr(mm_weekly, "truth")
#' channels <- c("tv", "video", "search", "social", "display")
#' spec <- lapply(stats::setNames(channels, channels), function(ch) list(
#'   adstock = list(decay = truth$decay[[ch]]),
#'   saturation = list(half_max = truth$half_max[[ch]],
#'                     shape = truth$shape[[ch]])
#' ))
#'
#' simulate_plan(north, spec, truth$beta)
#'
#' # Move a tenth of display's budget into search, keeping both channels'
#' # flighting, and compare the two plans
#' moved <- reallocate(north, from = "display", to = "search",
#'                     amount = 0.1 * sum(north$display))
#' change <- simulate_plan(moved, spec, truth$beta, baseline = north)
#' change
#' sum(change$response_change)
#' @export
simulate_plan <- function(plan, spec, coefficients, by = NULL,
                          baseline = NULL, extend = 0L) {
  here <- environment()
  .mm_check_plan(plan)
  .mm_note_dplyr_groups(plan, by, "simulate_plan", arg = "plan")
  channels <- .mm_check_spec(spec)
  .mm_check_channels(plan, channels, values = TRUE)
  cf <- .mm_plan_coefficients(coefficients, channels)
  extend <- .mm_check_bounded_count(extend, "extend", min = 0L, max = 1e6)
  key <- .mm_plan_by(by, plan, channels)
  base_key <- NULL
  if (!is.null(baseline)) {
    .mm_check_plan(baseline, "baseline")
    .mm_check_channels(baseline, channels, "baseline", values = TRUE)
    base_key <- .mm_baseline_by(by, plan, baseline, channels, key)
  }

  totals <- function(p, k) {
    vapply(channels, function(ch) {
      .mm_total_response(as.numeric(p[[ch]]), cf[[ch]],
                         .mm_channel_transformer(spec[[ch]], ch, here), k,
                         extend)
    }, numeric(1))
  }
  spent <- function(p) {
    vapply(channels, function(ch) sum(as.numeric(p[[ch]])), numeric(1))
  }
  out <- data.frame(channel = channels, spend = unname(spent(plan)),
                    response = unname(totals(plan, key)),
                    stringsAsFactors = FALSE)
  if (!is.null(baseline)) {
    out$spend_change <- out$spend - unname(spent(baseline))
    out$response_change <- out$response - unname(totals(baseline, base_key))
  }
  rownames(out) <- NULL
  out
}

#' Move spend between two channels of a plan
#'
#' Takes `amount` from one channel and gives it to another, keeping each
#' channel's flighting: `from` is scaled down by the same proportion in every
#' period, and `to` gains `amount` in proportion to its spend in each period.
#' The result is a new plan for [simulate_plan()] to compare with the old one.
#'
#' @param plan A data frame with one row per period and a numeric column of
#'   spend per channel. The `from` and `to` columns must be complete, finite
#'   and non-negative.
#' @param from,to Names of the columns to move spend from and to.
#' @param amount Spend to move, non-negative. Without `by`, one number. With
#'   `by`, one number moved within every group, or a vector named by group
#'   that moves each named group its own amount and leaves the others as
#'   they are.
#' @param by Optional grouping, as for [transform_plan()]: column names of
#'   `plan`, or a vector or data frame of vectors with one element or row per
#'   row of `plan`. Group names for `amount` are the group's values, joined
#'   with `" / "` when there is more than one grouping column.
#'
#' @return `plan` with the `from` and `to` columns changed and everything else
#'   as it was.
#'
#' @details
#' Without `by`, `amount` is moved across the whole plan, so each group keeps
#' its share of both channels. With `by`, it is moved within each group, so
#' the total moved is `amount` times the number of groups, or the sum of a
#' named `amount`. Moving an amount in one region only is how a reallocation
#' is set up as a geo test.
#'
#' A move larger than `from`'s spend (in the group) is an error, and so is a
#' move into a channel with no spend (in the group), since there is no
#' flighting to spread the amount over. To start a dark channel, give it small
#' weights in the periods it should run, for example `plan$video <- 1e-6` for
#' an even spread, and move the amount into it.
#'
#' @seealso [simulate_plan()], [allocate_budget()]
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#'
#' moved <- reallocate(north, from = "display", to = "social", amount = 2000)
#' c(display = sum(moved$display) - sum(north$display),
#'   social = sum(moved$social) - sum(north$social))
#'
#' # Display's weeks off stay off, and social keeps its own pattern
#' all(moved$display[north$display == 0] == 0)
#' range(moved$social / north$social, na.rm = TRUE)
#'
#' # A test in one region: only the north moves
#' test <- reallocate(mm_weekly, "display", "social",
#'                    amount = c(north = 2000), by = "geo")
#' tapply(test$social - mm_weekly$social, mm_weekly$geo, sum)
#' @export
reallocate <- function(plan, from, to, amount, by = NULL) {
  .mm_check_plan(plan)
  .mm_note_dplyr_groups(plan, by, "reallocate", arg = "plan")
  for (arg in c("from", "to")) {
    v <- get(arg, inherits = FALSE)
    if (!.mm_is_string(v)) {
      cli::cli_abort("{.arg {arg}} must be one column name, not \\
                      {.obj_type_friendly {v}}.")
    }
  }
  if (identical(from, to)) {
    cli::cli_abort("{.arg from} and {.arg to} are the same column, \\
                    {.val {from}}.")
  }
  .mm_check_channels(plan, c(from, to), values = TRUE, source = NULL)
  key <- .mm_plan_by(by, plan, c(from, to))
  if (!is.numeric(amount) || length(amount) == 0L || anyNA(amount) ||
      any(is.infinite(amount))) {
    cli::cli_abort("{.arg amount} must be a finite number, not \\
                    {.obj_type_friendly {amount}}.")
  }
  if (any(amount < 0)) {
    cli::cli_abort(c(
      "{.arg amount} must not be negative.",
      i = "To move spend the other way, swap {.arg from} and {.arg to}."
    ))
  }

  n <- nrow(plan)
  if (is.null(key)) {
    if (length(amount) != 1L) {
      cli::cli_abort(c(
        "Without {.arg by}, {.arg amount} must be one number, not \\
         {length(amount)}.",
        i = "Supply {.arg by} to move a different amount in each group."
      ))
    }
    if (!is.null(names(amount))) {
      cli::cli_abort(c(
        "{.arg amount} is named, but there is no {.arg by} to match the \\
         names to.",
        i = "Supply {.arg by}, or pass an unnamed number."
      ))
    }
    groups <- list(seq_len(n))
    labels <- NULL
    amounts <- unname(amount)
  } else {
    groups <- unname(.mm_group_index(key))
    labels <- .mm_show_key(unique(key))
    amounts <- .mm_group_amounts(amount, labels)
  }

  x_from <- as.numeric(plan[[from]])
  x_to <- as.numeric(plan[[to]])
  for (g in seq_along(groups)) {
    a <- amounts[[g]]
    if (a == 0) next
    rows <- groups[[g]]
    where <- if (is.null(labels)) "" else
      sprintf(" in group \"%s\"", labels[[g]])
    have <- sum(x_from[rows])
    if (a - have > sqrt(.Machine$double.eps) * have) {
      cli::cli_abort(c(
        "{.arg amount} is more than {.val {from}} spends{where}.",
        x = "{.val {from}} spends {signif(have, 6)}{where}; \\
             {.arg amount} is {signif(a, 6)}."
      ))
    }
    room <- sum(x_to[rows])
    if (room == 0) {
      cli::cli_abort(c(
        "{.val {to}} has no spend{where} to spread {.arg amount} over.",
        i = "{.fn reallocate} adds {.arg amount} to each period in \\
             proportion to the spend already there, which keeps the \\
             channel's flighting.",
        i = "Give {.val {to}} a pattern first: small weights in the periods \\
             it should run, for example {.code plan${to} <- 1e-6} for an \\
             even spread."
      ))
    }
    x_from[rows] <- x_from[rows] * max(0, 1 - a / have)
    x_to[rows] <- x_to[rows] + a * x_to[rows] / room
  }
  .mm_set_cols(plan, stats::setNames(list(x_from, x_to), c(from, to)))
}

# ---- shared plan helpers -------------------------------------------------------

# The elements a spec entry may hold: media_transform()'s arguments other than
# `x`, which is the channel's column, and `by`, which is set for the plan.
#' @keywords internal
#' @noRd
.mm_spec_fields <- function() c("adstock", "saturation", "order")

#' @keywords internal
#' @noRd
.mm_check_plan <- function(plan, arg = "plan", call = parent.frame()) {
  if (!is.data.frame(plan)) {
    cli::cli_abort("{.arg {arg}} must be a data frame with one row per \\
                    period, not {.obj_type_friendly {plan}}.", call = call)
  }
  if (nrow(plan) == 0L) {
    cli::cli_abort("{.arg {arg}} has no rows.", call = call)
  }
  invisible(plan)
}

# The channel names of a spec, after checking its shape: a list named by
# channel, each entry a list of media_transform() arguments.
#' @keywords internal
#' @noRd
.mm_check_spec <- function(spec, call = parent.frame()) {
  if (!is.list(spec) || is.data.frame(spec)) {
    cli::cli_abort(c(
      "{.arg spec} must be a list with one entry per channel, not \\
       {.obj_type_friendly {spec}}.",
      i = "Name each entry after its column, for example \\
           {.code list(tv = list(adstock = list(decay = 0.8)))}."
    ), call = call)
  }
  if (length(spec) == 0L) {
    cli::cli_abort("{.arg spec} has no channels.", call = call)
  }
  nms <- names(spec)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    cli::cli_abort("Every entry of {.arg spec} must be named after its \\
                    channel's column.", call = call)
  }
  dup <- unique(nms[duplicated(nms)])
  if (length(dup) > 0L) {
    cli::cli_abort("{.arg spec} names {.val {dup}} more than once.",
                   call = call)
  }
  fields <- .mm_spec_fields()
  for (ch in nms) {
    entry <- spec[[ch]]
    where <- paste0("spec$", ch)
    if (!is.list(entry) || is.data.frame(entry)) {
      cli::cli_abort(c(
        "{.code {where}} must be a list, not {.obj_type_friendly {entry}}.",
        i = "Give it {.field adstock} and {.field saturation} in the form \\
             {.fn media_transform} takes."
      ), call = call)
    }
    if (length(entry) == 0L) next
    en <- names(entry)
    if (is.null(en) || anyNA(en) || any(!nzchar(en))) {
      cli::cli_abort("Every element of {.code {where}} must be named.",
                     call = call)
    }
    bad <- setdiff(en, fields)
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "{.code {where}} has {cli::qty(length(bad))}{?an unknown \\
         element/unknown elements}: {.val {bad}}.",
        i = "A spec entry takes {.field adstock}, {.field saturation} and \\
             {.field order}."
      ), call = call)
    }
    dup <- unique(en[duplicated(en)])
    if (length(dup) > 0L) {
      cli::cli_abort("{.code {where}} gives {.field {dup}} more than once.",
                     call = call)
    }
    ad <- entry[["adstock"]]
    if (is.list(ad) && "by" %in% names(ad)) {
      cli::cli_abort(c(
        "{.code {where}$adstock} must not contain {.code by}.",
        i = "Group the rows with the {.arg by} argument, which applies to \\
             every channel."
      ), call = call)
    }
  }
  nms
}

# Channel columns must exist and be numeric. With `values = TRUE` they must
# also be complete, finite and non-negative, since totals and rescaling mean
# nothing otherwise. `source` names the argument the channels came from.
#' @keywords internal
#' @noRd
.mm_check_channels <- function(plan, channels, arg = "plan", values = FALSE,
                               source = "spec", call = parent.frame()) {
  miss <- setdiff(channels, names(plan))
  if (length(miss) > 0L) {
    cli::cli_abort(c(
      "{.arg {arg}} has no column{?s} {.val {miss}}.",
      i = if (!is.null(source)) {
        "Each channel in {.arg {source}} is matched to the column of that \\
         name."
      }
    ), call = call)
  }
  for (ch in channels) {
    v <- plan[[ch]]
    if (!is.numeric(v) || !is.null(dim(v))) {
      cli::cli_abort("Column {.val {ch}} of {.arg {arg}} must be numeric, \\
                      not {.obj_type_friendly {v}}.", call = call)
    }
    if (!values) next
    n_na <- sum(is.na(v))
    if (n_na > 0L) {
      cli::cli_abort(c(
        "Column {.val {ch}} of {.arg {arg}} has {n_na} missing value{?s}.",
        i = "Fill them, with 0 where nothing was spent, or drop those \\
             periods."
      ), call = call)
    }
    if (any(is.infinite(v))) {
      cli::cli_abort("Column {.val {ch}} of {.arg {arg}} has infinite \\
                      values.", call = call)
    }
    n_neg <- sum(v < 0)
    if (n_neg > 0L) {
      cli::cli_abort("Column {.val {ch}} of {.arg {arg}} has {n_neg} \\
                      negative value{?s}; spend must be non-negative.",
                     call = call)
    }
  }
  invisible(channels)
}

# `by` read as column names of the plan when it is a character vector whose
# every element names a column; NULL otherwise.
#' @keywords internal
#' @noRd
.mm_by_columns <- function(by, plan) {
  if (is.character(by) && !is.object(by) && length(by) > 0L && !anyNA(by) &&
      all(by %in% names(plan))) {
    return(unique(by))
  }
  NULL
}

# A plan's grouping as one key per row, or NULL: from columns of the plan,
# or from a vector or data frame of vectors with one element or row per row,
# as media_transform() takes it.
#' @keywords internal
#' @noRd
.mm_plan_by <- function(by, plan, channels, arg = "plan",
                        call = parent.frame()) {
  if (is.null(by)) return(NULL)
  n <- nrow(plan)
  cols <- .mm_by_columns(by, plan)
  if (!is.null(cols)) {
    both <- intersect(cols, channels)
    if (length(both) > 0L) {
      cli::cli_abort(c(
        "{.arg by} names {cli::qty(length(both))}{?a channel column/channel \\
         columns}: {.val {both}}.",
        i = "Group by columns that label the series, such as a geography."
      ), call = call)
    }
    return(.mm_check_by(.mm_plain_cols(plan, cols), n, call = call))
  }
  len <- if (is.data.frame(by)) nrow(by) else length(by)
  if (len != n) {
    absent <- if (is.character(by) && !is.object(by)) {
      setdiff(by, names(plan))
    }
    cli::cli_abort(c(
      "{.arg by} must name columns of {.arg {arg}}, or have one element per \\
       row of it ({n}), not {len}.",
      x = if (length(absent) > 0L && len < n) {
        "Not columns of {.arg {arg}}: {.val {absent}}."
      }
    ), call = call)
  }
  .mm_check_by(by, n, call = call)
}

# The baseline's grouping for simulate_plan(): grouping columns are looked up
# in the baseline itself; a grouping vector applies to both plans, so the
# baseline then needs the plan's number of rows.
#' @keywords internal
#' @noRd
.mm_baseline_by <- function(by, plan, baseline, channels, key,
                            call = parent.frame()) {
  if (is.null(by)) return(NULL)
  cols <- .mm_by_columns(by, plan)
  if (!is.null(cols)) {
    return(.mm_plan_by(cols, baseline, channels, "baseline", call = call))
  }
  if (nrow(baseline) != nrow(plan)) {
    cli::cli_abort(c(
      "{.arg baseline} has {nrow(baseline)} row{?s} and {.arg plan} has \\
       {nrow(plan)}, but a grouping vector in {.arg by} applies to both.",
      i = "Give {.arg by} as column names, which are looked up in each plan."
    ), call = call)
  }
  key
}

# Each channel's coefficient, by name. Other names, such as the intercept and
# the controls in coef(fit), are ignored, and the backticks a formula puts
# around a name such as `TV Spend` are dropped, as contributions() does.
#' @keywords internal
#' @noRd
.mm_plan_coefficients <- function(coefficients, channels,
                                  arg = "coefficients",
                                  call = parent.frame()) {
  if (!is.numeric(coefficients) || is.null(names(coefficients))) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a numeric vector named by channel, not \\
       {.obj_type_friendly {coefficients}}.",
      i = "{.code coef(fit)} gives one for a fitted model."
    ), call = call)
  }
  names(coefficients) <- gsub("`", "", names(coefficients), fixed = TRUE)
  miss <- setdiff(channels, names(coefficients))
  if (length(miss) > 0L) {
    cli::cli_abort("{.arg {arg}} has no value for {length(miss)} \\
                    channel{?s}: {.val {miss}}.", call = call)
  }
  cf <- as.numeric(coefficients[match(channels, names(coefficients))])
  bad <- channels[!is.finite(cf)]
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "The coefficient for {.val {bad}} is missing or infinite.",
      i = "A model drops a coefficient when its column is an exact linear \\
           combination of the others, so that channel's effect is not \\
           separately identified."
    ), call = call)
  }
  stats::setNames(cf, channels)
}

# Replace columns of a plan without changing the caller's object. A
# data.table is copied and changed with set(), which keeps it valid for a
# later `:=`; `[[<-` would leave it flagged as an invalid copy.
#' @keywords internal
#' @noRd
.mm_set_cols <- function(plan, values) {
  if (data.table::is.data.table(plan)) {
    out <- data.table::copy(plan)
    for (nm in names(values)) data.table::set(out, j = nm, value = values[[nm]])
    return(out)
  }
  for (nm in names(values)) plan[[nm]] <- values[[nm]]
  plan
}

# One channel's transformed series. An error or warning from the transform is
# reported under the channel's name, with the original as its cause. The
# arguments are bound to symbols so that the call stays short.
#' @keywords internal
#' @noRd
.mm_transform_channel <- function(x, entry, by, channel,
                                  call = parent.frame()) {
  args <- c(list(x = x), entry, list(by = by))
  withCallingHandlers(
    .mm_call_by_name("media_transform", args),
    error = function(e) {
      cli::cli_abort("Could not transform channel {.val {channel}}.",
                     parent = e, call = call)
    },
    warning = function(w) {
      cli::cli_warn("Transforming channel {.val {channel}} gave a warning.",
                    parent = w)
      invokeRestart("muffleWarning")
    }
  )
}

# A function of the series and its grouping keys that transforms one channel.
#' @keywords internal
#' @noRd
.mm_channel_transformer <- function(entry, channel, call) {
  force(entry)
  force(channel)
  force(call)
  function(x, by) .mm_transform_channel(x, entry, by, channel, call = call)
}

# Append `extend` zero-spend periods to the end of the series, or to the end
# of each group's series when there are keys, so that carryover from late
# spend can play out before response is summed.
#' @keywords internal
#' @noRd
.mm_pad_series <- function(x, by, extend) {
  if (extend == 0L) return(list(x = x, by = by))
  if (is.null(by)) return(list(x = c(x, numeric(extend)), by = NULL))
  idx <- split(seq_along(x), factor(by, levels = unique(by)))
  xs <- unlist(lapply(idx, function(i) c(x[i], numeric(extend))),
               use.names = FALSE)
  bs <- rep(names(idx), lengths(idx) + extend)
  list(x = xs, by = bs)
}

# A channel's total response: its coefficient times the sum of its
# transformed series, after `extend` periods of padding.
#' @keywords internal
#' @noRd
.mm_total_response <- function(x, coefficient, transform, by, extend) {
  p <- .mm_pad_series(x, by, extend)
  coefficient * sum(transform(p$x, p$by))
}

# One amount per group, in the order of `labels`: a single unnamed number for
# every group, or a vector named by group label with 0 for groups not named.
#' @keywords internal
#' @noRd
.mm_group_amounts <- function(amount, labels, call = parent.frame()) {
  nms <- names(amount)
  if (is.null(nms)) {
    if (length(amount) != 1L) {
      cli::cli_abort(c(
        "With {.arg by}, {.arg amount} must be one number or a vector named \\
         by group.",
        i = "Groups: {.val {utils::head(labels, 6)}}."
      ), call = call)
    }
    return(rep(amount, length(labels)))
  }
  if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms) > 0L) {
    cli::cli_abort("The names of {.arg amount} must be distinct group \\
                    names.", call = call)
  }
  unknown <- setdiff(nms, labels)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{.arg amount} names {cli::qty(length(unknown))}{?a group/groups} \\
       that {.arg by} does not have: {.val {unknown}}.",
      i = "Groups: {.val {utils::head(labels, 6)}}."
    ), call = call)
  }
  out <- numeric(length(labels))
  out[match(nms, labels)] <- unname(amount)
  out
}
