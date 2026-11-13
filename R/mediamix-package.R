#' mediamix: media transforms and attribution path construction
#'
#' Feature engineering for marketing mix modelling and multi-touch attribution.
#' mediamix turns raw marketing data into model-ready features: media spend into
#' carryover- and saturation-adjusted regressors, and raw event logs into
#' attributed customer journeys.
#'
#' It is a preprocessing and reporting package and does not fit models;
#' `lm()`, `glmnet` and `brms` do that better than a marketing package would.
#' It computes order-1 Markov removal effects natively; for higher-order chains
#' [as_channel_paths()] hands your journeys to \pkg{ChannelAttribution}.
#'
#' Full documentation is at <https://elkronos.github.io/mediamix/>, including
#' an end-to-end budget walkthrough, an index from tasks to functions, answers
#' to common questions, a guide to moving a Robyn model across, and the
#' methods behind each function.
#'
#' @section Where to start:
#' Five vignettes, in the order most people need them:
#'
#' \describe{
#'   \item{`vignette("mediamix")`}{Getting started: raw weekly spend through
#'     diagnostics, transforms and a model to contributions and ROI.}
#'   \item{`vignette("journeys")`}{Building journeys from event logs, and what
#'     a naive `group_by()` and `paste()` gets wrong.}
#'   \item{`vignette("carryover")`}{Choosing decay and lag by cross-validation
#'     against your KPI rather than by eye.}
#'   \item{`vignette("tidymodels")`}{Tuning carryover, saturation and model
#'     penalty jointly in one search.}
#'   \item{`vignette("spine")`}{Why carryover and credit are the same idea.}
#' }
#'
#' @section The media half:
#' \describe{
#'   \item{Carryover}{[adstock_geometric()] for the standard geometric kernel,
#'     [adstock_weibull()] and [adstock_delayed()] when the response peaks
#'     after the spend, [adstock_filter()] for a kernel of your own.
#'     [adstock_weights()], [adstock_weights_weibull()] and
#'     [adstock_weights_delayed()] expose the kernels themselves, and
#'     [adstock_state()] carries a filter across a boundary.}
#'   \item{Saturation}{[saturate_hill()], [saturate_exponential()],
#'     [saturate_michaelis_menten()], [saturate_power()], and the
#'     [saturate()] dispatcher.}
#'   \item{Composing them}{[media_transform()] applies carryover then
#'     saturation, in that order, and will not do it backwards quietly.
#'     [transform_plan()] does the same for every channel of a plan, each with
#'     its own parameters, and [from_robyn()] converts Robyn's
#'     hyperparameters to these arguments.}
#'   \item{Choosing parameters}{[decay_from_half_life()], [half_life()] and
#'     [effective_window()] translate between half-lives and decay
#'     coefficients. [tune_carryover()] and [tune_carryover_joint()] select
#'     them by cross-validation against an actual KPI;
#'     [adstock_steady_state()] removes start-up bias.}
#'   \item{Controls}{[fourier_terms()] and [holiday_flags()] build seasonality
#'     terms for the model and for the tuners' `controls`.}
#' }
#'
#' @section The attribution half:
#' \describe{
#'   \item{Journeys}{[build_paths()] turns an event log into journeys,
#'     handling the seven things that go wrong on the way, and can carry other
#'     event columns such as device or campaign. [bind_paths()] combines
#'     journey tables.}
#'   \item{Credit}{[credit_linear()], [credit_first()], [credit_last()],
#'     [credit_position()], [credit_time_decay()] and [credit_custom()], and
#'     the data-driven [markov_removal()]. [attribute()] runs several at once,
#'     by segment and with resampled intervals if asked, and
#'     [attribution_spread()] reports how much the answer depends on which
#'     rule you picked.}
#'   \item{Diagnostics}{[path_summary()], [path_lengths()],
#'     [channel_positions()], [assisted_conversions()], [top_paths()] and
#'     [conversion_lag()], or [path_diagnostics()] for all of them.}
#'   \item{Interop}{[as_channel_paths()] exports to
#'     \pkg{ChannelAttribution}'s format.}
#' }
#'
#' @section Reporting, planning and tidymodels:
#' [diagnose_media()] checks whether the data can support a model at all, so
#' run it first. [contributions()], [roi()], [marginal_roi()], [mroi()],
#' [response_curve()] and [spend_for()] turn a fitted model into a
#' deliverable, and [block_bootstrap()] puts intervals on it.
#' [simulate_plan()], [reallocate()] and [allocate_budget()] compare and split
#' budgets through the same transforms. Contributions, response curves,
#' bootstrap intervals, tuning results, attribution shares, budget allocations
#' and diagnostics each have a `plot()` method; [mm_palette()] exposes their
#' colours. [step_adstock()], [step_adstock_channels()] and
#' [step_saturation()] are \pkg{recipes} steps that carry filter state across
#' the train/test boundary, with [carryover_decay()] and friends as their
#' \pkg{dials} parameters.
#'
#' @section Data:
#' [mm_weekly] is a synthetic weekly panel generated from known parameters, so
#' a workflow can be checked against the truth it is trying to recover.
#' [mm_events] is a synthetic touchpoint log containing, on purpose, every case
#' that breaks a naive journey pipeline.
#'
#' @section Dependencies:
#' The core transforms need only base R and `stats`, plus `cli` and `rlang` for
#' error messages. `data.table` is used by the attribution half, where event
#' logs are large. \pkg{recipes}, \pkg{dials} and the rest of tidymodels are
#' optional and registered conditionally, so the transforms work equally well
#' from a Stan or \pkg{brms} workflow that will never touch them.
#'
#' @examples
#' # The media half: spend in, model-ready regressor out
#' spend <- c(0, 500, 800, 300, 0, 0, 1200, 400)
#' media_transform(
#'   spend,
#'   adstock = list(decay = decay_from_half_life(2)),
#'   saturation = list(half_max = 300)
#' )
#'
#' # The attribution half: event log in, journeys out
#' data(mm_events)
#' paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
#'                      timestamp = "timestamp", conversion = "conversion")
#' path_summary(paths)
#'
#' @keywords internal
#' @importFrom data.table := .N .I .SD
"_PACKAGE"

# Required so that data.table's `[` and `:=` work from inside this namespace.
.datatable.aware <- TRUE

# Quiet R CMD check about data.table's non-standard evaluation and about
# columns referred to by name inside data.table expressions.
utils::globalVariables(c(
  ".", ".N", ".I", ".SD", ".row_id", "..keep",
  "path_id", "touch_rank", "touch_n", "channel", "converted",
  "conversion_value", "timestamp", "credit", "conv_time", "journey",
  "gap_flag", "new_journey", "rank_in_path", "id", "conversion",
  "time_to_conversion", "prev_channel", "n_touch", "value", "keep_row",
  ".is_direct", ".t", "conv_t", "anchor", "N"
))
