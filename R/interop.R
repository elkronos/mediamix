#' Export journeys in ChannelAttribution's format
#'
#' Collapses an `mm_paths` object into the aggregated `"a > b > c"` table that
#' \pkg{ChannelAttribution} consumes. This package builds the journeys;
#' \pkg{ChannelAttribution} computes Markov removal effects on them in C++.
#' Neither needs to do the other's job.
#'
#' @param paths An `mm_paths` object from [build_paths()].
#' @param sep Separator between channels, `" > "` by default. A channel name
#'   that contains `sep` is an error, since its path strings could not be
#'   split back into channels. \pkg{ChannelAttribution}'s `markov_model()` has
#'   a `sep` argument of its own; give both the same value, as the example
#'   does.
#'
#' @return A data frame with columns `path`, `total_conversions`,
#'   `total_null` and, when the journey table carries values,
#'   `total_conversion_value`. One row per distinct channel sequence, ordered
#'   by descending conversions, then descending null journeys, then path in
#'   the C locale.
#'
#' @details
#' The `total_null` column is why [build_paths()] keeps non-converting journeys
#' by default. Markov removal effects compare the probability of reaching
#' conversion with the probability of reaching the null state. Without null
#' paths the transition matrix has no absorbing failure state, the baseline
#' conversion probability is 1, and each channel's removal effect moves up or
#' down depending on how the missing non-converting journeys differ from the
#' converting ones.
#'
#' @seealso [build_paths()], [attribute()]
#'
#' @examples
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion",
#'                      value = "value")
#'
#' ca <- as_channel_paths(paths)
#' head(ca)
#'
#' # Hand off to ChannelAttribution, if it is installed, with the same
#' # separator on both sides
#' if (requireNamespace("ChannelAttribution", quietly = TRUE)) {
#'   ca_in <- as_channel_paths(paths, sep = ">")
#'   ChannelAttribution::markov_model(
#'     ca_in, var_path = "path",
#'     var_conv = "total_conversions", var_null = "total_null", sep = ">"
#'   )
#' }
#' @export
as_channel_paths <- function(paths, sep = " > ") {
  paths <- .mm_check_paths(paths)
  .mm_check_sep(sep)
  if (nrow(paths) == 0L) {
    return(data.frame(path = character(0), total_conversions = numeric(0),
                      total_null = numeric(0), stringsAsFactors = FALSE))
  }
  strs <- .mm_path_strings(paths, sep)
  has_value <- !all(is.na(paths$conversion_value))

  agg <- stats::aggregate(
    list(total_conversions = as.numeric(strs$converted),
         total_null = as.numeric(!strs$converted)),
    by = list(path = strs$path), FUN = sum
  )
  if (has_value) {
    av <- stats::aggregate(list(total_conversion_value = strs$value),
                           by = list(path = strs$path), FUN = sum)
    agg <- merge(agg, av, by = "path", sort = FALSE)
  }
  agg <- agg[order(-agg$total_conversions, -agg$total_null, agg$path,
                   method = "radix"), , drop = FALSE]
  rownames(agg) <- NULL
  agg
}
