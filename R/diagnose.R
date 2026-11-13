#' Diagnose media data before modelling
#'
#' Runs the checks that decide whether a marketing mix model can work at all.
#' None of them are about the model; all of them are about whether the data
#' contains the information the model will be asked to find. Run this first.
#'
#' @param data A data frame containing the media columns. A tibble or
#'   data.table is read as a plain data frame.
#' @param media Character vector naming the media columns, each once.
#' @param spend,impressions Optional character vectors of matching spend and
#'   impression columns, in the same order, for the cost-per-mille consistency
#'   check.
#' @param by Optional character vector of grouping columns. Every diagnostic is
#'   computed *within* each group and the worst case reported: the highest
#'   variance inflation factor, the strongest absolute correlation, the lowest
#'   coefficient of variation, the highest share of dark periods. Pooling
#'   geographies that differ mainly in scale would manufacture correlation that
#'   exists in no single series, so `by` is worth supplying whenever the rows
#'   are a panel. Groups set with `dplyr::group_by()` are not used in its place;
#'   a message says so and names the `by` to pass.
#' @param vif_threshold Variance inflation factor above which a channel is
#'   flagged.
#' @param cv_threshold Coefficient of variation below which a channel is flagged
#'   as insufficiently varying.
#' @param decay Optional geometric decay in `[0, 1)` for computing collinearity
#'   on *adstocked* media: a single number for every channel, or a named vector
#'   with one per channel (unnamed channels are left raw). Adstock smooths each
#'   series, and smoothed series are usually more correlated than the raw
#'   spend, so this is the collinearity the model will face. Rows must be in
#'   time order within each group; pass `date` to check that they are. Missing
#'   spend is treated as zero for the adstock, and a message counts the values
#'   affected. Variation and flighting are always reported on the raw spend.
#' @param date Optional name of the time column, as a string. The column must
#'   be numeric, `Date` or `POSIXct`, with no missing values. Each series (each
#'   group of `by`) is checked for rows that come earlier than the row above
#'   them, for periods that appear more than once, and for missing periods,
#'   judged against the series' usual spacing. Adstock treats adjacent rows as
#'   adjacent periods, so each of these changes what a decay means. See
#'   *Time order* below.
#'
#' @return An object of class `mm_diagnosis`: a named list with elements
#'   \describe{
#'     \item{`variation`}{Per channel: `mean` and `sd` averaged across groups,
#'       the lowest group `cv` and `distinct` count, the highest group
#'       `zero_share`, and the flags `low_variation`, `heavily_flighted` and
#'       `usable`.}
#'     \item{`collinearity`}{Per channel: the worst group `vif`, the
#'       `most_correlated_with` partner and its `correlation`, and `identified`:
#'       `TRUE` when `vif` is at most `vif_threshold`, `FALSE` above it, and
#'       `NA` where the factor could not be computed at all. `NA` is not
#'       `TRUE`.}
#'     \item{`correlations`}{Pairwise correlation matrix. With `by` supplied,
#'       each cell is the group value with the largest magnitude, sign
#'       preserved.}
#'     \item{`cpm`}{One row per row of `data` and spend column when `spend`
#'       and `impressions` are supplied, `NULL` otherwise: `channel` (the spend
#'       column), `row` (the row number), the implied `cpm`, the series'
#'       `median_cpm` and the `outlier` flag. With `by`, the median and outlier
#'       rule are computed within each series and a `group` column is added.}
#'     \item{`dates`}{One row per series when `date` is supplied, `NULL`
#'       otherwise: `group` (with `by`), `rows`, the `start` and `end` of the
#'       series, its `period`, and the counts `out_of_order` (rows that come
#'       earlier than the row above them), `duplicated` (rows that repeat a
#'       period already in the series), `missing` (whole periods absent
#'       between `start` and `end`) and `irregular` (steps between consecutive
#'       time points that differ from `period` by more than a quarter of it).
#'       `period` is the median step between the series' distinct time points,
#'       in the units of `date`: days for a `Date`, seconds for a `POSIXct`.}
#'     \item{`flags`}{Character vector of the problems found, in the order they
#'       should be dealt with. Empty when nothing was found.}
#'     \item{`n_obs`, `n_groups`, `grouped`, `adstocked`}{Rows examined,
#'       series examined, whether `by` was supplied, and whether collinearity
#'       was measured on adstocked media.}
#'   }
#'
#'   `as.data.frame()` and `tidy()` return the per-channel table: one row per
#'   channel, with the `variation` and `collinearity` columns side by side.
#'
#' @details
#' Four things sink marketing mix projects, and all four are visible before a
#' model is fitted.
#'
#' **Channel collinearity** is common, because channels are usually planned
#' together. When two channels move together the model cannot tell their
#' effects apart. It will still produce coefficients, and those coefficients
#' can flip sign on small changes to the specification or the sample. A
#' variance inflation factor above 5 deserves attention and above 10 means the
#' split between those channels is not identified, however tight the overall
#' fit looks.
#'
#' **Insufficient variation** is the quieter version of the same problem. A
#' channel spending nearly the same amount every week carries almost no
#' information about what different amounts would do. No adstock or saturation
#' transform can recover an effect that the data never varied enough to reveal.
#'
#' **Zero inflation and flighting** matter because a channel that is dark most
#' of the time has far less effective sample than its row count suggests, and
#' because carryover across dark periods is where transform bugs hide.
#'
#' **Spend and impression inconsistency**, a wildly varying implied cost per
#' mille, usually means a data-join error upstream rather than a real change
#' in media pricing. It is worth catching before it becomes a modelling
#' puzzle. A period is flagged when its CPM is more than five median absolute
#' deviations from its series' median. When more than half of a series' CPMs
#' are identical the median absolute deviation is zero, and a period is
#' flagged instead when its CPM is at least twice or at most half the median.
#'
#' @section Time order:
#' Adstock treats adjacent rows as adjacent periods. [adstock_geometric()] and
#' [media_transform()] have no time index, so they take each series' rows in
#' the order given: a row out of order mixes later spend into earlier weeks, a
#' period that appears twice is carried over twice, and a missing week lets
#' the decay act once across what were two weeks. None of these raises an
#' error. With `date`, each series is checked for all three, and the result is
#' in `dates`.
#'
#' The usual spacing of a series is the median step between its distinct time
#' points, as in [step_adstock()]; a series with a single time point takes the
#' median of the other series' spacings. A step counts as irregular when it
#' differs from that spacing by more than a quarter of it, so calendar months
#' of 28 to 31 days are regular. A gap of about two periods counts one
#' missing period, of about three periods two, and so on. Rows out of order
#' are counted in the order of `data`, one for each row whose time point is
#' earlier than that of the row above it in the same series.
#'
#' @seealso [plot.mm_diagnosis()], [adstock_geometric()], [contributions()]
#'
#' @examples
#' data(mm_weekly)
#' channels <- c("tv", "video", "search", "social", "display")
#'
#' # Always pass `by` on panel data: pooling geographies that differ in scale
#' # invents correlation that is not in any single series.
#' d <- diagnose_media(mm_weekly, media = channels, by = "geo")
#' d
#'
#' d$variation
#' d$collinearity
#'
#' # Collinearity after adstocking is what the model sees
#' diagnose_media(mm_weekly, media = channels, by = "geo",
#'                decay = attr(mm_weekly, "truth")$decay)$collinearity
#'
#' # A deliberately collinear pair is caught
#' fake <- mm_weekly[mm_weekly$geo == "north", ]
#' fake$twin <- fake$tv * 1.02 + 5
#' diagnose_media(fake, media = c("tv", "twin", "search"))$collinearity
#'
#' # Adstock treats adjacent rows as adjacent periods, so check the dates too.
#' # Here one week of north is dropped and two others swap places.
#' messy <- mm_weekly[-10, ]
#' messy[2:3, ] <- messy[3:2, ]
#' checked <- diagnose_media(messy, media = channels, by = "geo",
#'                           date = "date")
#' checked$dates
#' checked$flags
#' @export
diagnose_media <- function(data, media, spend = NULL, impressions = NULL,
                           by = NULL, vif_threshold = 5, cv_threshold = 0.15,
                           decay = NULL, date = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!is.character(media) || length(media) == 0L) {
    cli::cli_abort("{.arg media} must be a non-empty character vector of \\
                    column names.")
  }
  if (anyDuplicated(media)) {
    cli::cli_abort(c(
      "{.arg media} must name each column once.",
      x = "Repeated: {.val {unique(media[duplicated(media)])}}."
    ))
  }
  if (!is.null(date) && !.mm_is_string(date)) {
    cli::cli_abort("{.arg date} must be a single column name given as a \\
                    string.")
  }
  .mm_assert_cols(data, c(media, spend, impressions, by, date))
  vif_threshold <- .mm_check_scalar(vif_threshold, "vif_threshold", lower = 1)
  cv_threshold <- .mm_check_scalar(cv_threshold, "cv_threshold", lower = 0)
  .mm_note_dplyr_groups(data, by, "diagnose_media")
  # A data.table reads a character index as a join, and a grouped tibble
  # carries groups this function does not use; a plain data frame has neither.
  data <- as.data.frame(data)

  grp <- .mm_step_groups(data, by)
  grouped <- .mm_has_by(by)
  dates <- if (is.null(date)) NULL else {
    .mm_date_table(data[[date]], grp, grouped, date)
  }
  here <- environment()
  m <- as.data.frame(lapply(stats::setNames(media, media), function(cn) {
    .mm_as_number(data[[cn]], cn, call = here)
  }), check.names = FALSE)

  # A column with no usable values cannot take part in any regression, and
  # leaving it in would make `complete.cases()` empty out every other column's
  # design matrix, which would turn one dead channel into an all-clear report.
  usable <- vapply(m, function(z) sum(is.finite(z)) > 0L, logical(1))
  dead <- media[!usable]

  variation <- do.call(rbind, lapply(media, function(cn) {
    per_group <- vapply(grp, function(rows) {
      v <- m[[cn]][rows]
      v <- v[is.finite(v)]
      if (length(v) == 0L) return(c(mean = NA_real_, sd = NA_real_, cv = NA_real_,
                                    zero = NA_real_, distinct = NA_real_))
      mu <- mean(v)
      c(mean = mu, sd = stats::sd(v),
        cv = if (mu > 0) stats::sd(v) / mu else NA_real_,
        zero = mean(v == 0), distinct = length(unique(v)))
    }, numeric(5))
    safe <- function(f, row) {
      vals <- per_group[row, ]
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0L) NA_real_ else f(vals)
    }
    data.frame(
      channel = cn,
      mean = safe(mean, "mean"),
      sd = safe(mean, "sd"),
      cv = safe(min, "cv"),
      distinct = safe(min, "distinct"),
      zero_share = safe(max, "zero"),
      stringsAsFactors = FALSE
    )
  }))
  variation$low_variation <- !is.na(variation$cv) & variation$cv < cv_threshold
  variation$heavily_flighted <- !is.na(variation$zero_share) &
    variation$zero_share > 0.5
  variation$usable <- usable[variation$channel]
  rownames(variation) <- NULL

  # Collinearity is computed WITHIN each group and the worst case reported, as
  # documented. Pooling geographies that differ mainly in scale manufactures
  # correlation that exists in no single series.
  live <- m[, usable, drop = FALSE]
  decays <- .mm_diag_decays(decay, media)
  if (!is.null(decays)) {
    adstocked <- intersect(names(decays), names(live))
    gaps <- vapply(live[adstocked], function(z) sum(is.na(z)), integer(1))
    if (any(gaps > 0L)) {
      gaps <- gaps[gaps > 0L]
      by_channel <- paste0(names(gaps), " (", gaps, ")", collapse = ", ")
      cli::cli_inform(c(
        "Missing spend was treated as zero when adstocking for the \\
         collinearity check.",
        i = "Missing values by channel: {by_channel}."
      ))
    }
    for (cn in adstocked) {
      for (rows in grp) {
        v <- live[[cn]][rows]
        live[[cn]][rows] <- adstock_geometric(v, decay = decays[[cn]],
                                              na_action = "zero")
      }
    }
  }
  per_group_stats <- lapply(grp, function(rows) {
    sub <- live[rows, , drop = FALSE]
    keep <- vapply(sub, function(z) {
      z <- z[is.finite(z)]
      length(z) > 1L && stats::sd(z) > 0
    }, logical(1))
    sub <- sub[, keep, drop = FALSE]
    if (ncol(sub) == 0L || nrow(sub) <= ncol(sub) + 1L) return(NULL)
    list(cor = suppressWarnings(stats::cor(sub, use = "pairwise.complete.obs")),
         vif = .mm_vif(sub))
  })
  per_group_stats <- Filter(Negate(is.null), per_group_stats)

  cors <- .mm_worst_cor(per_group_stats, media)
  collinearity <- .mm_collinearity_table(per_group_stats, media, cors,
                                         vif_threshold)
  cpm <- .mm_cpm_table(data, spend, impressions, grp, grouped = grouped,
                       call = environment())

  # Problems with the time index come first: sorting, aggregating or filling
  # the rows changes every other diagnostic.
  flags <- .mm_date_flags(dates)
  if (length(dead) > 0L) {
    flags <- c(flags, sprintf(
      "No usable values: %s contain no finite observations and were excluded from every diagnostic.",
      paste(dead, collapse = ", ")))
  }
  if (length(per_group_stats) == 0L && length(media) > 1L) {
    flags <- c(flags, paste(
      "Collinearity could not be assessed: no series has enough rows with",
      "variation in every channel. The variance inflation factors are NA,",
      "which is not the same as identified."))
  }
  hi <- collinearity$channel[!is.na(collinearity$vif) &
                               collinearity$vif > vif_threshold]
  if (length(hi) > 0L) {
    flags <- c(flags, sprintf(
      "Collinearity: %s exceed VIF %g. Their individual coefficients are not identified.",
      paste(hi, collapse = ", "), vif_threshold))
  }
  lo <- variation$channel[variation$low_variation]
  if (length(lo) > 0L) {
    flags <- c(flags, sprintf(
      "Low variation: %s vary by less than %g%% of their mean. No transform recovers an effect the data never varied enough to show.",
      paste(lo, collapse = ", "), cv_threshold * 100))
  }
  fl <- variation$channel[variation$heavily_flighted]
  if (length(fl) > 0L) {
    flags <- c(flags, sprintf(
      "Heavily flighted: %s are dark in more than half of periods. Effective sample is smaller than the row count suggests.",
      paste(fl, collapse = ", ")))
  }
  if (!is.null(cpm) && any(cpm$outlier)) {
    flags <- c(flags, sprintf(
      "Implied CPM outliers in %d period(s). This usually means a join error upstream, not a pricing change.",
      sum(cpm$outlier)))
  }

  structure(
    list(variation = variation, collinearity = collinearity,
         correlations = cors, cpm = cpm, flags = flags,
         n_obs = nrow(data), n_groups = length(grp), grouped = grouped,
         adstocked = !is.null(decays), dates = dates),
    class = "mm_diagnosis"
  )
}

#' @export
print.mm_diagnosis <- function(x, ...) {
  cli::cli_h3("Media diagnostics")
  cli::cli_text("{x$n_obs} observation{?s} across {x$n_groups} series, \\
                 {nrow(x$variation)} channel{?s}")
  if (isTRUE(x$adstocked)) {
    cli::cli_text("{.emph Collinearity measured on adstocked media.}")
  }
  if (!isTRUE(x$grouped) && x$n_groups == 1L) {
    cli::cli_text("{.emph Ungrouped. On panel data, pass {.arg by} so that \\
                   collinearity is measured within series.}")
  }
  dt <- x$dates
  if (is.data.frame(dt) && nrow(dt) > 0L) {
    bad <- sum(dt$out_of_order > 0 | dt$duplicated > 0 | dt$irregular > 0,
               na.rm = TRUE)
    if (bad == 0L) {
      cli::cli_text("{.emph Time index: every series is in order, with one \\
                     row per period and no gaps.}")
    } else {
      cli::cli_text("{.emph Time index: {bad} of {nrow(dt)} series \\
                     {cli::qty(bad)}{?has/have} rows out of order, repeated \\
                     periods, gaps or uneven steps.}")
    }
  }
  if (length(x$flags) == 0L) {
    cli::cli_alert_success("No structural problems found.")
  } else {
    cli::cli_alert_warning("{length(x$flags)} issue{?s} found:")
    # Passed as a value, so braces in a series name are printed, not
    # interpreted.
    for (f in x$flags) cli::cli_bullets(c("*" = "{f}"))
  }
  tables <- c("$variation", "$collinearity", "$correlations",
              if (is.data.frame(dt)) "$dates")
  cli::cli_text("")
  cli::cli_text("{.emph Inspect {paste(tables, collapse = ', ')}.}")
  invisible(x)
}

#' @export
as.data.frame.mm_diagnosis <- function(x, row.names = NULL, optional = FALSE,
                                       ...) {
  v <- x$variation
  co <- x$collinearity
  out <- cbind(v, co[match(v$channel, co$channel),
                     setdiff(names(co), "channel"), drop = FALSE])
  rownames(out) <- NULL
  out
}

#' @exportS3Method generics::tidy
tidy.mm_diagnosis <- function(x, ...) {
  as.data.frame(x)
}

# dplyr groups are not used as `by`: grouping does not survive every route a
# data frame takes, so a panel is named explicitly. A grouped frame passed
# without `by` gets a message naming the `by` to pass.
#' @keywords internal
#' @noRd
.mm_note_dplyr_groups <- function(data, by, fn, arg = "data") {
  if (!is.null(by) || !inherits(data, "grouped_df")) return(invisible(NULL))
  vars <- setdiff(names(attr(data, "groups", exact = TRUE)), ".rows")
  if (length(vars) == 0L) return(invisible(NULL))
  code <- if (length(vars) == 1L) {
    sprintf('by = "%s"', vars)
  } else {
    sprintf("by = c(%s)", paste0('"', vars, '"', collapse = ", "))
  }
  cli::cli_inform(c(
    "{.arg {arg}} is grouped by {.val {vars}}, but {.fn {fn}} does not use \\
     dplyr groups: the rows are treated as one series.",
    i = "Pass {.code {code}} to treat each group as its own series."
  ))
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.mm_diag_decays <- function(decay, media, call = parent.frame()) {
  if (is.null(decay)) return(NULL)
  if (!is.numeric(decay) || anyNA(decay) || any(decay < 0 | decay >= 1)) {
    cli::cli_abort("{.arg decay} must be numeric in [0, 1).", call = call)
  }
  if (length(decay) == 1L && is.null(names(decay))) {
    return(stats::setNames(rep(decay, length(media)), media))
  }
  if (is.null(names(decay)) || !all(names(decay) %in% media)) {
    cli::cli_abort("{.arg decay} must be one number, or a vector named by \\
                    media column.", call = call)
  }
  decay
}

# Variance inflation factors without a modelling dependency: regress each
# channel on the others and read 1 / (1 - R^2).
#' @keywords internal
#' @noRd
.mm_vif <- function(m) {
  nm <- names(m)
  vif <- stats::setNames(rep(NA_real_, length(nm)), nm)
  if (length(nm) < 2L) return(vif)
  for (i in seq_along(nm)) {
    y <- m[[i]]
    X <- as.matrix(m[, -i, drop = FALSE])
    ok <- is.finite(y) & stats::complete.cases(X)
    if (sum(ok) <= ncol(X) + 1L) next
    fit <- try(stats::lm.fit(cbind(1, X[ok, , drop = FALSE]), y[ok]),
               silent = TRUE)
    if (inherits(fit, "try-error")) next
    ss_res <- sum(fit$residuals^2)
    ss_tot <- sum((y[ok] - mean(y[ok]))^2)
    if (ss_tot <= 0) next
    r2 <- 1 - ss_res / ss_tot
    vif[i] <- if (r2 >= 1 - 1e-10) Inf else 1 / (1 - r2)
  }
  vif
}

# Element-wise worst case across groups: the entry with the largest absolute
# correlation, sign preserved.
#' @keywords internal
#' @noRd
.mm_worst_cor <- function(stats_list, nm) {
  out <- matrix(NA_real_, length(nm), length(nm), dimnames = list(nm, nm))
  diag(out) <- 1
  for (st in stats_list) {
    cn <- colnames(st$cor)
    cur <- out[cn, cn, drop = FALSE]
    new <- st$cor
    take <- (is.na(cur) | (!is.na(new) & abs(new) > abs(cur))) & !is.na(new)
    cur[take] <- new[take]
    out[cn, cn] <- cur
  }
  diag(out) <- 1
  out
}

#' @keywords internal
#' @noRd
.mm_collinearity_table <- function(stats_list, media, cors, threshold) {
  worst_vif <- stats::setNames(rep(NA_real_, length(media)), media)
  for (st in stats_list) {
    v <- st$vif
    for (cn in names(v)) {
      if (is.na(v[[cn]])) next
      if (is.na(worst_vif[[cn]]) || v[[cn]] > worst_vif[[cn]]) {
        worst_vif[[cn]] <- v[[cn]]
      }
    }
  }
  partner <- lapply(media, function(cn) {
    others <- setdiff(media, cn)
    if (length(others) == 0L) return(list(name = NA_character_, r = NA_real_))
    r <- as.numeric(cors[cn, others])
    # abs() must come before the sentinel: abs(-Inf) is +Inf, which would make
    # the missing entries win every comparison.
    ar <- abs(r)
    ar[!is.finite(ar)] <- -Inf
    if (all(ar == -Inf)) return(list(name = NA_character_, r = NA_real_))
    j <- which.max(ar)
    list(name = others[j], r = r[j])
  })
  out <- data.frame(
    channel = media,
    vif = unname(worst_vif[media]),
    most_correlated_with = vapply(partner, function(z) z$name, character(1)),
    correlation = vapply(partner, function(z) z$r, numeric(1)),
    stringsAsFactors = FALSE
  )
  # NA means "not assessed", which is not the same as "identified".
  out$identified <- ifelse(is.na(out$vif), NA, out$vif <= threshold)
  out <- out[order(-replace(out$vif, is.na(out$vif), -Inf)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @keywords internal
#' @noRd
.mm_cpm_table <- function(data, spend, impressions, grp = NULL,
                          grouped = FALSE, call = parent.frame()) {
  if (is.null(spend) || is.null(impressions)) return(NULL)
  if (length(spend) != length(impressions)) {
    cli::cli_abort("{.arg spend} and {.arg impressions} must name the same \\
                    number of columns, in matching order.",
                   call = call)
  }
  if (is.null(grp)) grp <- list(".all" = seq_len(nrow(data)))
  # The median and MAD are computed within each series. Pooled across
  # geographies, a region that buys media at a different price would be
  # flagged as a stream of join errors.
  rows <- lapply(seq_along(spend), function(i) {
    s <- .mm_as_number(data[[spend[i]]], spend[i], call = call)
    imp <- .mm_as_number(data[[impressions[i]]], impressions[i], call = call)
    cpm_all <- ifelse(imp > 0, s / imp * 1000, NA_real_)
    do.call(rbind, lapply(names(grp), function(g) {
      r <- grp[[g]]
      cpm <- cpm_all[r]
      med <- stats::median(cpm, na.rm = TRUE)
      madv <- stats::mad(cpm, na.rm = TRUE)
      # When more than half the CPMs are identical the MAD is zero, and a rule
      # in MADs flags nothing however far a period strays. A factor of two
      # either side of the median is the fallback.
      outlier <- if (is.finite(madv) && madv > 0) {
        !is.na(cpm) & abs(cpm - med) > 5 * madv
      } else if (is.finite(med) && med > 0) {
        !is.na(cpm) & (cpm >= 2 * med | cpm <= med / 2)
      } else {
        rep(FALSE, length(cpm))
      }
      out <- data.frame(channel = spend[i], row = r, cpm = cpm,
                        median_cpm = med, outlier = outlier,
                        stringsAsFactors = FALSE)
      if (grouped) out$group <- .mm_show_key(g)
      out
    }))
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$channel, out$row), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# One row per series describing how its rows sit on the time index. The usual
# spacing and the count of irregular steps come from the helpers that
# step_adstock() uses, so the two judge a series the same way.
#' @keywords internal
#' @noRd
.mm_date_table <- function(dates, grp, grouped, col, call = parent.frame()) {
  t <- .mm_index_numeric(dates, "date", call = call)
  .mm_check_index_finite(t, grp, grouped, col, call = call)
  periods <- .mm_group_periods(t, grp)
  out <- do.call(rbind, lapply(seq_along(grp), function(i) {
    rows <- grp[[i]]
    v <- t[rows]
    u <- sort(unique(v))
    p <- periods[[i]]
    steps <- if (length(u) > 1L) diff(u) / p else numeric(0)
    data.frame(
      rows = length(rows),
      start = dates[rows[which.min(v)]],
      end = dates[rows[which.max(v)]],
      period = p,
      out_of_order = sum(diff(v) < 0),
      duplicated = sum(duplicated(v)),
      missing = sum(pmax(round(steps) - 1, 0)),
      irregular = .mm_irregular_steps(u, p),
      stringsAsFactors = FALSE
    )
  }))
  if (grouped) {
    out <- cbind(group = .mm_show_key(names(grp)), out,
                 stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  out
}

# Flags for the time index, one per kind of problem, naming up to three of the
# series affected.
#' @keywords internal
#' @noRd
.mm_date_flags <- function(tab) {
  if (is.null(tab)) return(character(0))
  where <- function(hit) {
    if (is.null(tab$group)) return("")
    n <- sum(hit)
    more <- if (n > 3L) sprintf(" and %d more", n - 3L) else ""
    sprintf(" in %d series (%s%s)", n,
            paste(utils::head(tab$group[hit], 3L), collapse = ", "), more)
  }
  count <- function(v) format(sum(v), big.mark = ",", scientific = FALSE)
  flags <- character(0)
  hit <- tab$out_of_order > 0
  if (any(hit)) {
    flags <- c(flags, sprintf(
      "Out of time order: %s row(s)%s come earlier than the row above them. Adstock treats adjacent rows as adjacent periods, so sort each series by date before transforming it.",
      count(tab$out_of_order), where(hit)))
  }
  hit <- tab$duplicated > 0
  if (any(hit)) {
    flags <- c(flags, sprintf(
      "Repeated periods: %s row(s)%s repeat a period already in the series. Adstock needs one row per period in each series: aggregate the repeats, or name the column that separates the series in `by`.",
      count(tab$duplicated), where(hit)))
  }
  hit <- tab$missing > 0
  if (any(hit)) {
    flags <- c(flags, sprintf(
      "Missing periods: %s period(s)%s are absent, judged against each series' usual spacing. Carryover crosses each gap as if it were one period; fill the missing periods with zero spend.",
      count(tab$missing), where(hit)))
  }
  hit <- tab$irregular > 0 & tab$missing == 0
  if (any(hit)) {
    flags <- c(flags, sprintf(
      "Uneven spacing: %s step(s)%s differ from the series' usual spacing by more than a quarter of a period, which changes what the decay means at those steps.",
      count(tab$irregular[hit]), where(hit)))
  }
  flags
}
