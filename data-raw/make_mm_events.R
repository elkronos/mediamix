# Generates data/mm_events.rda: a synthetic touchpoint log that contains, on
# purpose, every case that breaks a naive journey pipeline.
set.seed(20260826)

channels <- c("paid_search", "organic_search", "social", "display",
              "email", "affiliate")

# Channels play different roles in a journey. Display and social open journeys;
# paid search and email close them. Without this structure, first-touch and
# last-touch attribution would agree, which is not how real logs behave and
# would make the comparison diagnostics look pointless.
opening_weights <- c(paid_search = 0.10, organic_search = 0.14, social = 0.26,
                     display = 0.32, email = 0.06, affiliate = 0.12)
closing_weights <- c(paid_search = 0.34, organic_search = 0.22, social = 0.09,
                     display = 0.05, email = 0.22, affiliate = 0.08)

# Linear blend from opening to closing mix across the journey.
position_weights <- function(pos, n) {
  w <- if (n == 1L) 0.5 else (pos - 1L) / (n - 1L)
  mix <- (1 - w) * opening_weights + w * closing_weights
  mix / sum(mix)
}
direct_labels <- c("direct", "(none)", "")

n_customers <- 5400L
start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")

make_customer <- function(i) {
  # Most customers never convert; some convert repeatedly.
  n_conv <- sample(0:3, 1L, prob = c(0.62, 0.28, 0.07, 0.03))
  n_bursts <- max(1L, n_conv)
  out <- vector("list", n_bursts)
  clock <- start + stats::runif(1L, 0, 300 * 86400)

  for (b in seq_len(n_bursts)) {
    n_touch <- 1L + stats::rpois(1L, 2.4)
    ch <- vapply(seq_len(n_touch), function(j) {
      sample(channels, 1L, prob = position_weights(j, n_touch))
    }, character(1))

    # Consecutive duplicates: a real log records the same channel repeatedly.
    if (n_touch > 1L && stats::runif(1L) < 0.35) {
      j <- sample(seq_len(n_touch - 1L), 1L)
      ch[j + 1L] <- ch[j]
    }
    # Direct / none / missing labels.
    if (stats::runif(1L) < 0.22) {
      k <- sample(seq_len(n_touch), 1L)
      ch[k] <- sample(direct_labels, 1L)
    }
    if (stats::runif(1L) < 0.04) ch[sample(seq_len(n_touch), 1L)] <- NA_character_

    # Timestamps, with deliberate ties at second resolution.
    offs <- cumsum(c(0, stats::rexp(n_touch - 1L, rate = 1 / (2.5 * 86400))))
    if (n_touch > 2L && stats::runif(1L) < 0.15) offs[3L] <- offs[2L]
    ts <- clock + offs

    converted <- b <= n_conv
    conv <- integer(n_touch)
    val <- rep(NA_real_, n_touch)
    if (converted) {
      conv[n_touch] <- 1L
      val[n_touch] <- round(stats::rgamma(1L, shape = 3, scale = 42), 2)
    }

    out[[b]] <- data.frame(
      customer_id = sprintf("cust_%05d", i),
      channel = ch,
      timestamp = ts,
      conversion = conv,
      value = val,
      stringsAsFactors = FALSE
    )
    # Next burst begins after a long dormant gap.
    clock <- ts[n_touch] + stats::runif(1L, 20, 140) * 86400
  }
  do.call(rbind, out)
}

mm_events <- do.call(rbind, lapply(seq_len(n_customers), make_customer))
# Shuffle: a real export is not sorted, and ordering must not depend on it.
mm_events <- mm_events[sample(nrow(mm_events)), ]
rownames(mm_events) <- NULL

stopifnot(anyNA(mm_events$channel), any(mm_events$conversion == 1L))
save(mm_events, file = "data/mm_events.rda", compress = "xz", version = 2)
cat("mm_events:", nrow(mm_events), "events,",
    length(unique(mm_events$customer_id)), "customers,",
    sum(mm_events$conversion), "conversions\n")
