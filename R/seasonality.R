#' Seasonality controls for a model frame
#'
#' Calendar regressors for a marketing mix model: Fourier terms for a smooth
#' yearly cycle, and flags for the periods that contain a holiday. Both return
#' a data frame with one row per element of `index`, in the same order, ready
#' to [cbind()] to a model frame or to pass as `controls` to
#' [tune_carryover()].
#'
#' @param index The time of each row. For `fourier_terms()`, a `Date` vector,
#'   or a numeric vector with `cycle` given. For `holiday_flags()`, a `Date`
#'   vector. It need not be sorted and may repeat dates, as a panel does. A
#'   missing value gives a row of `NA`.
#' @param harmonics Number of sine and cosine pairs, a whole number of at
#'   least 1. Harmonic `k` repeats `k` times per cycle, so `k` times a year
#'   for a `Date` index.
#' @param cycle Length of one seasonal cycle in the units of a numeric
#'   `index`, such as `52` for a week number or `12` for a month number.
#'   Required for a numeric `index`. A `Date` index always uses the calendar
#'   year, so `cycle` must then be left out.
#' @param prefix Start of every column name, a single string.
#' @param holidays The holiday dates: a `Date` vector for a single flag, or a
#'   named list of `Date` vectors for one flag per element. Dates may repeat
#'   and need not be sorted, but must not be missing.
#' @param period Length of each row's period: a single positive number of
#'   days, or a `difftime`. `NULL` (the default) takes the median step between
#'   consecutive distinct dates in `index`, which is 7 for weekly data and 1
#'   for daily data.
#' @param align Where each date sits in its period: `"start"` (the default)
#'   when dates mark the first day of the period, as week-commencing dates
#'   do, or `"end"` when they mark the last day, as week-ending dates do.
#'
#' @return A data frame with one row per element of `index`, in the same
#'   order, and default row names.
#'
#'   `fourier_terms()` returns `2 * harmonics` numeric columns, a sine and a
#'   cosine for each harmonic in turn: `<prefix>_sin1`, `<prefix>_cos1`,
#'   `<prefix>_sin2`, `<prefix>_cos2` and so on.
#'
#'   `holiday_flags()` returns integer columns holding 1 for a period that
#'   contains a holiday and 0 otherwise. A `Date` vector of holidays gives one
#'   column named `prefix`; a named list gives one column per element, named
#'   `<prefix>_<element name>`, in the order of the list.
#'
#' @details
#' **Fourier terms.** For a `Date`, the time of year is `t = (d - 1) / 365.25`,
#' where `d` is the day of the year, 1 for 1 January. For a numeric index,
#' `t = index / cycle`. Harmonic `k` contributes `sin(2 * pi * k * t)` and
#' `cos(2 * pi * k * t)`. Every year starts at `t = 0`, so a calendar date has
#' the same terms every year, except that from 29 February a leap year is one
#' day further along: its 31 December reaches `t = 365 / 365.25`. Each
#' harmonic adds two columns, and so two coefficients to the model; the first
#' few describe a smooth seasonal shape, and later ones sharper peaks.
#'
#' **Holiday flags.** Each date in `index` marks one end of a period of
#' `period` days. With `align = "start"` the period runs from the date up to,
#' but not including, the date plus `period`; with `align = "end"` it runs
#' from the day after the date minus `period` up to and including the date. A
#' row is flagged when at least one holiday falls inside its period, so with
#' daily data a row is flagged on the holiday itself. A holiday that falls in
#' no row's period, such as one in a missing week, is not flagged anywhere.
#'
#' Every period has the same length, which suits daily and weekly data.
#' When `period` is inferred and some dates are closer together than it, the
#' periods overlap, a holiday can be flagged in two rows, and a warning says
#' so. That happens with monthly data, whose months run from 28 to 31 days,
#' and with a panel whose series start their weeks on different days. Give
#' `period` for the panel, and flag monthly data by each holiday's month.
#'
#' @seealso [tune_carryover()] and [tune_carryover_joint()], which take
#'   control columns as `controls`; [mm_weekly], whose `seasonality` and
#'   `holiday` columns are simulated stand-ins for these controls.
#'
#' @examples
#' data(mm_weekly)
#' north <- mm_weekly[mm_weekly$geo == "north", ]
#'
#' # Two harmonics of the yearly cycle, from week-commencing dates
#' season <- fourier_terms(north$date, harmonics = 2)
#' head(round(season, 3))
#'
#' # A numeric index needs the length of its cycle
#' head(round(fourier_terms(1:104, harmonics = 1, cycle = 52), 3))
#'
#' # Flag the weeks that contain Christmas Day or Black Friday
#' holidays <- list(
#'   christmas = as.Date(c("2023-12-25", "2024-12-25", "2025-12-25")),
#'   black_friday = as.Date(c("2023-11-24", "2024-11-29", "2025-11-28"))
#' )
#' flags <- holiday_flags(north$date, holidays)
#' north$date[flags$holiday_christmas == 1]
#'
#' # Week-ending dates flag the same weeks with align = "end"
#' week_ending <- north$date + 6
#' identical(holiday_flags(week_ending, holidays, align = "end"), flags)
#'
#' # A table with one row per holiday becomes the list through split()
#' calendar <- data.frame(date = as.Date(c("2024-12-25", "2024-11-29")),
#'                        name = c("christmas", "black_friday"))
#' flags_2024 <- holiday_flags(north$date, split(calendar$date, calendar$name))
#' colSums(flags_2024)
#'
#' # Ready to bind to a model frame
#' model_data <- cbind(north[c("date", "revenue")], season, flags)
#' head(model_data)
#' @name seasonality
NULL

#' @rdname seasonality
#' @export
fourier_terms <- function(index, harmonics = 2, cycle = NULL,
                          prefix = "season") {
  harmonics <- .mm_check_count(harmonics, "harmonics", min = 1L)
  .mm_check_prefix(prefix)
  t <- .mm_time_of_cycle(index, cycle)
  cols <- vector("list", 2L * harmonics)
  for (k in seq_len(harmonics)) {
    cols[[2L * k - 1L]] <- sin(2 * pi * k * t)
    cols[[2L * k]] <- cos(2 * pi * k * t)
  }
  names(cols) <- paste0(prefix, "_", rep(c("sin", "cos"), harmonics),
                        rep(seq_len(harmonics), each = 2L))
  as.data.frame(cols, optional = TRUE)
}

#' @rdname seasonality
#' @export
holiday_flags <- function(index, holidays, period = NULL,
                          align = c("start", "end"), prefix = "holiday") {
  align <- match.arg(align)
  .mm_check_prefix(prefix)
  .mm_check_date_index(index)
  sets <- .mm_holiday_sets(holidays, prefix)
  .mm_check_period(period)
  # Whole days: a Date with a fractional part, such as a spreadsheet serial
  # with a time, prints as its day and is flagged as that day.
  day <- floor(as.numeric(index))

  if (is.null(period)) {
    span <- .mm_infer_period(day)
    if (!is.finite(span)) {
      cli::cli_abort(c(
        "{.arg period} cannot be inferred from fewer than two distinct dates.",
        i = "Give the length of each period, for example \\
             {.code period = 7} for weekly data."
      ))
    }
    steps <- diff(sort(unique(day)))
    if (any(steps < span)) {
      shown <- .mm_format_num(span)
      cli::cli_warn(c(
        "Some dates in {.arg index} are closer together than the inferred \\
         period of {shown} days, so their periods overlap.",
        i = "A holiday inside an overlap is flagged in both rows.",
        i = "Give {.arg period} to set the length of every period."
      ))
    }
  } else {
    span <- .mm_period_value(period, index)
  }

  cols <- lapply(sets, function(h) {
    inside <- if (align == "start") {
      findInterval(day + span, h, left.open = TRUE) -
        findInterval(day, h, left.open = TRUE)
    } else {
      findInterval(day, h) - findInterval(day - span, h)
    }
    as.integer(inside > 0L)
  })
  as.data.frame(cols, optional = TRUE)
}

#' @keywords internal
#' @noRd
.mm_check_prefix <- function(prefix, call = parent.frame()) {
  if (!.mm_is_string(prefix) || !nzchar(prefix)) {
    cli::cli_abort("{.arg prefix} must be a single non-empty string.",
                   call = call)
  }
  invisible(prefix)
}

#' @keywords internal
#' @noRd
.mm_check_date_index <- function(index, call = parent.frame()) {
  if (!inherits(index, "Date")) {
    cli::cli_abort(c(
      "{.arg index} must be a {.cls Date} vector, not \\
       {.obj_type_friendly {index}}.",
      i = "Convert it with {.fn as.Date}."
    ), call = call)
  }
  if (length(index) == 0L) {
    cli::cli_abort("{.arg index} must have at least one element.",
                   call = call)
  }
  invisible(index)
}

# The position of each row within its seasonal cycle, as a fraction of the
# cycle: the time of year for a Date, index / cycle for a number.
#' @keywords internal
#' @noRd
.mm_time_of_cycle <- function(index, cycle, call = parent.frame()) {
  if (inherits(index, "Date")) {
    .mm_check_date_index(index, call = call)
    if (!is.null(cycle)) {
      cli::cli_abort(c(
        "{.arg cycle} applies to a numeric {.arg index}.",
        i = "A {.cls Date} index uses the calendar year. Leave {.arg cycle} \\
             out, or pass a numeric index such as a week number."
      ), call = call)
    }
    return(as.POSIXlt(index)$yday / 365.25)
  }
  if (inherits(index, "POSIXt")) {
    cli::cli_abort(c(
      "{.arg index} must be a {.cls Date} or numeric vector, not \\
       {.obj_type_friendly {index}}.",
      i = "The day of the year of a date-time depends on its time zone. \\
           Convert it with {.fn as.Date} first."
    ), call = call)
  }
  index <- .mm_check_numeric(index, "index", call = call)
  if (any(is.infinite(index))) {
    cli::cli_abort("{.arg index} must not contain infinite values.",
                   call = call)
  }
  if (is.null(cycle)) {
    cli::cli_abort(c(
      "A numeric {.arg index} needs {.arg cycle}, the length of one seasonal \\
       cycle in its units.",
      i = "For example {.code cycle = 52} for a week number."
    ), call = call)
  }
  cycle <- .mm_check_scalar(cycle, "cycle", lower = 0,
                            inclusive = c(FALSE, TRUE), call = call)
  index / cycle
}

# Holiday dates as sorted, distinct day numbers, one set per output column,
# named by that column.
#' @keywords internal
#' @noRd
.mm_holiday_sets <- function(holidays, prefix, call = parent.frame()) {
  if (is.data.frame(holidays)) {
    cli::cli_abort(c(
      "{.arg holidays} must be a {.cls Date} vector or a named list of them, \\
       not a data frame.",
      i = "For a table with one row per holiday, {.code split(dates, names)} \\
           gives the list."
    ), call = call)
  }
  if (inherits(holidays, "Date")) {
    sets <- list(holidays)
    names(sets) <- prefix
  } else if (is.list(holidays)) {
    nms <- names(holidays)
    if (length(holidays) == 0L) {
      cli::cli_abort("{.arg holidays} must hold at least one set of dates.",
                     call = call)
    }
    if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
      cli::cli_abort(c(
        "Every element of {.arg holidays} must be named.",
        i = "The names label the flag columns."
      ), call = call)
    }
    if (anyDuplicated(nms)) {
      cli::cli_abort(
        "{.arg holidays} repeats the name{?s} \\
         {.val {unique(nms[duplicated(nms)])}}.",
        call = call
      )
    }
    is_date <- vapply(holidays, inherits, logical(1), what = "Date")
    if (!all(is_date)) {
      bad <- nms[!is_date][1L]
      cli::cli_abort(c(
        "Element {.val {bad}} of {.arg holidays} must be a {.cls Date} \\
         vector, not {.obj_type_friendly {holidays[[bad]]}}.",
        i = "Convert it with {.fn as.Date}."
      ), call = call)
    }
    sets <- holidays
    names(sets) <- paste0(prefix, "_", nms)
  } else {
    cli::cli_abort(c(
      "{.arg holidays} must be a {.cls Date} vector or a named list of them, \\
       not {.obj_type_friendly {holidays}}.",
      i = "Convert dates with {.fn as.Date}."
    ), call = call)
  }
  for (i in seq_along(sets)) {
    n_missing <- sum(is.na(sets[[i]]))
    if (n_missing > 0L) {
      where <- if (is.list(holidays)) {
        paste0(" in element \"", names(holidays)[i], "\"")
      } else {
        ""
      }
      cli::cli_abort(c(
        "{.arg holidays} has {n_missing} missing date{?s}{where}.",
        i = "Drop them, or fill them in."
      ), call = call)
    }
  }
  lapply(sets, function(s) sort(unique(floor(as.numeric(s)))))
}
