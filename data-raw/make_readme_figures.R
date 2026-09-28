# Regenerates the figures embedded in README.md. Run from the package root
# after any change to the plot methods or the example data.
library(mediamix)

png_fig <- function(file, width = 1400, height = 860) {
  grDevices::png(file.path("man", "figures", file), width = width,
                 height = height, res = 170)
}

data(mm_events)
paths <- build_paths(mm_events, id = "customer_id", channel = "channel",
                     timestamp = "timestamp", conversion = "conversion",
                     value = "value", lookback = 30,
                     split_on = c("conversion", "gap"), gap = 30,
                     direct = "label")
png_fig("README-attribution.png")
plot(attribute(paths), n = 6)
grDevices::dev.off()

data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
channels <- c("tv", "video", "search", "social", "display")
truth <- attr(mm_weekly, "truth")
media <- as.data.frame(Map(function(x, d, h, s) media_transform(
  x, adstock = list(decay = d), saturation = list(half_max = h, shape = s)),
  north[channels], truth$decay[channels], truth$half_max[channels],
  truth$shape[channels]))
controls <- data.frame(week = seq_len(nrow(north)), price = north$price,
                       seasonality = north$seasonality,
                       holiday = north$holiday)
fit <- lm(north$revenue ~ ., data = cbind(media, controls))
png_fig("README-contributions.png")
plot(contributions(media, fit, index = north$date))
grDevices::dev.off()
