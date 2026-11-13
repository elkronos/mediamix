# Plot a response curve

Two stacked panels on a shared axis of adstocked spend, the saturation
curve's input (see
[`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md)):
the response and its marginal return. With `observed`, levels below or
above its range are shaded, because there the curve is extrapolation.
The point where the marginal return is largest in size is marked as the
steepest, unless it is the first or last point.

## Usage

``` r
# S3 method for class 'mm_response_curve'
plot(x, observed = NULL, ...)
```

## Arguments

- x:

  A data frame from
  [`response_curve()`](https://elkronos.github.io/mediamix/reference/response_curve.md).

- observed:

  Optional numeric vector of the levels seen in the data, normally the
  channel's adstocked series; its range is left unshaded.

- ...:

  Graphical arguments: `main` and `ylab` replace the upper panel's title
  and axis label, `xlab` the spend axis label, and `col` the line
  colour. Others are ignored.

## Value

`x`, invisibly.

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]
rc <- response_curve(seq(0, 6000, by = 100), coefficient = 6000,
                     half_max = 2250, shape = 1.6)

# Shade the levels outside the range of television's adstocked series
plot(rc, observed = adstock_geometric(north$tv, decay = 0.85))
```
