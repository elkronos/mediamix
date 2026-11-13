# Weibull adstock weights

A two-parameter kernel whose `"pdf"` form, unlike the geometric kernel,
can place its peak *after* the period in which the money was spent.
Television, cinema and out-of-home often behave this way: the response
builds for a week or two before it turns over. A geometric kernel cannot
represent that shape at any decay rate. The default `"cdf"` form decays
monotonically.

## Usage

``` r
adstock_weights_weibull(
  max_lag,
  shape,
  scale,
  type = c("cdf", "pdf"),
  normalise = TRUE
)
```

## Arguments

- max_lag:

  Number of periods the kernel spans, including the current period.
  Unlike the geometric kernel, Weibull adstock has no infinite form:
  `max_lag` is required and must be finite.

- shape:

  Weibull shape parameter, positive. In the `"pdf"` form, `shape <= 1`
  produces a monotonically decaying kernel, and `shape > 1` allows a
  delayed peak without guaranteeing one: see *Details* for when the peak
  moves past the period of spend. In the `"cdf"` form, larger values
  produce a flatter plateau followed by a sharper drop.

- scale:

  Weibull scale parameter, positive, measured in periods. Larger values
  stretch the kernel over more periods.

- type:

  Either `"cdf"` (default) or `"pdf"`. See *Details*.

- normalise:

  Should the weights sum to 1? Defaults to `TRUE`. When `FALSE`, weights
  are scaled so the largest is 1, matching the unnormalised geometric
  convention.

## Value

A numeric vector of length `max_lag`, ordered from the current period to
the most distant lag.

## Details

The two forms answer different questions.

The `"cdf"` form builds the kernel as a cumulative product of Weibull
survival values. Numbering lags from 1 so that \\w_1\\ is the period of
spend, \\w_i = \prod\_{j\<i} (1 - F(j))\\, where \\F\\ is the Weibull
distribution function, so \\w_1 = 1\\ and each later weight is the
previous one times \\1 - F(i - 1)\\. Read \\1 - F(j)\\ as a time-varying
retention rate: it plays the role that the constant \\\theta\\ plays in
the geometric kernel, which is the sense in which this form
*generalises* geometric decay. It is always monotonically decreasing,
but the rate of decay can itself change over time. The kernel is the
running product of these values rather than the Weibull survival
function \\1 - F(i)\\, and its tail falls away much faster. The
construction is the one Robyn calls `"weibull_cdf"`. Robyn expresses
`scale` as a quantile of the series length rather than in periods;
[`from_robyn()`](https://elkronos.github.io/mediamix/reference/from_robyn.md)
converts it.

The `"pdf"` form uses the Weibull density directly, \\w_i \propto
f(i)\\. This is the form that permits a delayed peak, and it is the
reason to reach for Weibull adstock at all. With `shape <= 1` it
collapses back to a monotone decay. Lags are numbered from 1 here too,
so the period of spend carries \\f(1)\\ and the next period \\f(2)\\,
and the kernel peaks after the period of spend only when \\f(2) \>
f(1)\\. That needs `shape > 1` and a Weibull mode,
`scale * ((shape - 1) / shape)^(1 / shape)`, of roughly 1.5 or more
(1.47 at `shape = 2`, rising towards 2 as `shape` grows). With
`shape = 2`, `scale = 2` puts the mode at 1.41 and the kernel still
peaks in the period of spend; `scale = 3` moves the peak one period
later. A density that is numerically zero at every lag, with all its
mass beyond `max_lag` or before the first lag, is an error.

## See also

[`adstock_weibull()`](https://elkronos.github.io/mediamix/reference/adstock_weibull.md),
[`adstock_weights()`](https://elkronos.github.io/mediamix/reference/adstock_weights.md)

## Examples

``` r
# Monotone decay, a flexible generalisation of geometric
round(adstock_weights_weibull(8, shape = 2, scale = 3, type = "cdf"), 4)
#> [1] 0.3680 0.3293 0.2111 0.0777 0.0131 0.0008 0.0000 0.0000

# Delayed peak: the largest single weight lands one period after the spend,
# not in the period of spend itself. No geometric decay rate can do this.
w <- adstock_weights_weibull(8, shape = 2, scale = 3, type = "pdf")
round(w, 4)
#> [1] 0.2027 0.2905 0.2500 0.1531 0.0704 0.0249 0.0069 0.0015
which.max(w)
#> [1] 2
```
