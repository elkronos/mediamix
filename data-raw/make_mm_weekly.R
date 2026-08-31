# Generates data/mm_weekly.rda from known adstock and saturation parameters,
# so that vignettes can demonstrate recovery of the truth.
library(mediamix)
set.seed(20260826)

channels <- c("tv", "video", "search", "social", "display")

truth <- list(
  decay      = c(tv = 0.85, video = 0.70, search = 0.15, social = 0.45, display = 0.55),
  half_max   = c(tv = 45000, video = 12000, search = 9000, social = 7000, display = 6000),
  shape      = c(tv = 1.6, video = 1.0, search = 1.0, social = 1.0, display = 1.0),
  beta       = c(tv = 5200, video = 2400, search = 4100, social = 1800, display = 900),
  baseline   = 18000,
  price_beta = -2600,
  trend      = 22,
  noise_sd   = 900
)

geos <- c("north", "central", "south")
geo_scale <- c(north = 1.00, central = 0.72, south = 1.35)
n_weeks <- 156L
dates <- seq(as.Date("2023-01-02"), by = "week", length.out = n_weeks)

flight <- function(n, on_prob, mean_spend, sd_spend, burst = FALSE) {
  on <- stats::rbinom(n, 1L, on_prob)
  if (burst) {
    # Bursty channels run in blocks rather than independent weeks
    on <- as.integer(stats::filter(on, rep(1, 3), sides = 1) > 0)
    on[is.na(on)] <- 0L
  }
  amt <- pmax(0, stats::rnorm(n, mean_spend, sd_spend))
  round(on * amt, 0)
}

rows <- lapply(geos, function(g) {
  s <- geo_scale[[g]]
  wk <- seq_len(n_weeks)
  season <- 1 + 0.22 * sin(2 * pi * (wk - 6) / 52) + 0.10 * sin(4 * pi * wk / 52)
  holiday <- as.integer(format(dates, "%m") == "12")

  spend <- data.frame(
    tv      = flight(n_weeks, 0.45, 38000 * s, 12000 * s, burst = TRUE),
    video   = flight(n_weeks, 0.60, 11000 * s,  4000 * s),
    search  = round(pmax(0, stats::rnorm(n_weeks,  9500 * s, 2200 * s))),
    social  = flight(n_weeks, 0.80,  6500 * s,  2400 * s),
    display = flight(n_weeks, 0.70,  5200 * s,  1900 * s)
  )

  price <- round(stats::rnorm(n_weeks, 24.5, 1.4) -
                   0.9 * holiday + 0.004 * wk, 2)

  contrib <- vapply(channels, function(ch) {
    z <- adstock_geometric(spend[[ch]], decay = truth$decay[[ch]])
    truth$beta[[ch]] * s * saturate_hill(z, half_max = truth$half_max[[ch]] * s,
                                         shape = truth$shape[[ch]])
  }, numeric(n_weeks))

  kpi <- truth$baseline * s +
    truth$trend * wk * s +
    rowSums(contrib) +
    truth$price_beta * s * (price - mean(price)) +
    (truth$baseline * s * 0.35) * (season - 1) +
    2500 * s * holiday +
    stats::rnorm(n_weeks, 0, truth$noise_sd * sqrt(s))

  data.frame(
    date = dates,
    geo = g,
    spend,
    price = price,
    seasonality = round(season, 4),
    holiday = holiday,
    revenue = round(kpi, 0),
    stringsAsFactors = FALSE
  )
})

mm_weekly <- do.call(rbind, rows)
rownames(mm_weekly) <- NULL
attr(mm_weekly, "truth") <- truth

stopifnot(nrow(mm_weekly) == n_weeks * length(geos), all(mm_weekly$revenue > 0))
save(mm_weekly, file = "data/mm_weekly.rda", compress = "xz", version = 2)
cat("mm_weekly:", nrow(mm_weekly), "rows\n")
