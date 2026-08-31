#' Diagnose media data before modelling
#'
#' Runs the checks that decide whether a marketing mix model can work at all.
#' None of them are about the model; all of them are about whether the data
#' contains the information the model will be asked to find. Run this first.
#'
#' @param data A data frame containing the media columns.
#' @param media Character vector naming the media columns.
#' @param spend,impressions Optional character vectors of matching spend and
#'   impression columns, in the same order, for the cost-per-mille consistency
#'   check.
#' @param by Optional character vector of grouping columns. Every diagnostic is
#'   computed *within* each group and the worst case reported: the highest
#'   variance inflation factor, the strongest absolute correlation, the lowest
#'   coefficient of variation, the highest share of dark periods. Pooling
#'   geographies that differ mainly in scale would manufacture correlation that
#'   exists in no single series, so `by` is worth supplying whenever the rows
#'   are a panel.
#' @param vif_threshold Variance inflation factor above which a channel is
#'   flagged.
#' @param cv_threshold Coefficient of variation below which a channel is flagged
#'   as insufficiently varying.
#'
#' @return An object of class `mm_diagnosis`: a named list with elements
#'   \describe{
#'     \item{`variation`}{Per channel: `mean` and `sd` averaged across groups,
#'       the lowest group `cv` and `distinct` count, the highest group
#'       `zero_share`, and the flags `low_variation`, `heavily_flighted` and
#'       `usable`.}
#'     \item{`collinearity`}{Per channel: the worst group `vif`, the
#'       `most_correlated_with` partner and its `correlation`, and `identified`
#'       -- `TRUE`, `FALSE`, or `NA` where the factor could not be computed at
#'       all. `NA` is not `TRUE`.}
#'     \item{`correlations`}{Pairwise correlation matrix. With `by` supplied,
#'       each cell is the group value with the largest magnitude, sign
#'       preserved.}
#'     \item{`cpm`}{Implied cost per mille per period, with outlier flags, when
#'       `spend` and `impressions` are supplied. `NULL` otherwise.}
#'     \item{`flags`}{Character vector of the problems found, in the order they
#'       should be dealt with. Empty when nothing was found.}
#'     \item{`n_obs`, `n_groups`, `grouped`}{Rows examined, series examined,
#'       and whether `by` was supplied.}
#'   }
#'
#' @details
#' Four things sink marketing mix projects, and all four are visible before a
#' model is fitted.
#'
#' **Channel collinearity** is the biggest practical problem in the field. When
#' two channels move together -- because they were planned together, which is
#' usually the case -- the model cannot tell their effects apart. It will still
#' produce coefficients, and those coefficients will flip sign on small changes
#' to the specification or the sample. A variance inflation factor above 5
#' deserves attention and above 10 means the split between those channels is not
#' identified, however tight the overall fit looks.
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
#' **Spend and impression inconsistency** -- a wildly varying implied cost per
#' mille -- almost always means a data-join error upstream rather than a real
#' change in media pricing. It is worth catching before it becomes a modelling
#' puzzle.
#'
#' @seealso [adstock_geometric()], [contributions()]
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
#' # A deliberately collinear pair is caught
#' fake <- mm_weekly[mm_weekly$geo == "north", ]
#' fake$twin <- fake$tv * 1.02 + 5
#' diagnose_media(fake, media = c("tv", "twin", "search"))$collinearity
#' @export
diagnose_media <- function(data, media, spend = NULL, impressions = NULL,
                           by = NULL, vif_threshold = 5, cv_threshold = 0.15) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!is.character(media) || length(media) == 0L) {
    cli::cli_abort("{.arg media} must be a non-empty character vector of \\
                    column names.")
  }
  .mm_assert_cols(data, c(media, spend, impressions, by))
  vif_threshold <- .mm_check_scalar(vif_threshold, "vif_threshold", lower = 1)
  cv_threshold <- .mm_check_scalar(cv_threshold, "cv_threshold", lower = 0)

  grp <- .mm_step_groups(data, by)
  m <- as.data.frame(lapply(data[media], function(z) .mm_as_number(z, "media")),
                     check.names = FALSE)
  names(m) <- media

  # A column with no usable values cannot take part in any regression, and
  # leaving it in would make `complete.cases()` empty out every other column's
  # design matrix -- turning one dead channel into an all-clear report.
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
  cpm <- .mm_cpm_table(data, spend, impressions)

  flags <- character(0)
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
         n_obs = nrow(data), n_groups = length(grp),
         grouped = !is.null(by) && length(by) > 0L),
    class = "mm_diagnosis"
  )
}

#' @export
print.mm_diagnosis <- function(x, ...) {
  cli::cli_h3("Media diagnostics")
  cli::cli_text("{x$n_obs} observation{?s} across {x$n_groups} series, \\
                 {nrow(x$variation)} channel{?s}")
  if (!isTRUE(x$grouped) && x$n_groups == 1L) {
    cli::cli_text("{.emph Ungrouped. On panel data, pass {.arg by} so that \\
                   collinearity is measured within series.}")
  }
  if (length(x$flags) == 0L) {
    cli::cli_alert_success("No structural problems found.")
  } else {
    cli::cli_alert_warning("{length(x$flags)} issue{?s} found:")
    for (f in x$flags) cli::cli_bullets(c("*" = f))
  }
  cli::cli_text("")
  cli::cli_text("{.emph Inspect $variation, $collinearity, $correlations.}")
  invisible(x)
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
.mm_cpm_table <- function(data, spend, impressions) {
  if (is.null(spend) || is.null(impressions)) return(NULL)
  if (length(spend) != length(impressions)) {
    cli::cli_abort("{.arg spend} and {.arg impressions} must name the same \\
                    number of columns, in matching order.",
                   call = parent.frame(2))
  }
  rows <- lapply(seq_along(spend), function(i) {
    s <- as.numeric(data[[spend[i]]])
    imp <- as.numeric(data[[impressions[i]]])
    cpm <- ifelse(imp > 0, s / imp * 1000, NA_real_)
    med <- stats::median(cpm, na.rm = TRUE)
    madv <- stats::mad(cpm, na.rm = TRUE)
    outlier <- !is.na(cpm) & madv > 0 & abs(cpm - med) > 5 * madv
    data.frame(channel = spend[i], period = seq_along(cpm), cpm = cpm,
               median_cpm = med, outlier = outlier, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
