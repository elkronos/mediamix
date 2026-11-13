# Plot a carryover cross-validation profile

The cross-validated error across the decay grid, one line per `max_lag`,
with the minimum (filled) and the one-standard-error choice (open)
marked on the line they belong to. A flat profile is the finding: the
data does not pin carryover down, however precise `best` looks.

## Usage

``` r
# S3 method for class 'mm_carryover'
plot(x, truth = NULL, ...)

# S3 method for class 'mm_carryover_joint'
plot(x, truth = NULL, ...)
```

## Arguments

- x:

  An object from
  [`tune_carryover()`](https://elkronos.github.io/mediamix/reference/tune_carryover.md)
  or
  [`tune_carryover_joint()`](https://elkronos.github.io/mediamix/reference/tune_carryover_joint.md).

- truth:

  Optional known decay (a number, or for the joint version a vector
  named by channel), drawn as a muted reference line. Useful on
  simulated data.

- ...:

  Graphical arguments: `main`, `xlab` and `ylab` replace the titles, and
  `col` gives the line colours, one per `max_lag` in legend order. In
  the joint version `main` is the overall title and `xlab` and `ylab`
  label every panel. Others are ignored.

## Value

`x`, invisibly.

## Details

With more than eight `max_lag` values, the seven with the lowest error
keep a colour each and the others are drawn as thin grey lines, with a
message.

## Examples

``` r
set.seed(42)
spend <- pmax(0, rnorm(120, 500, 250))
kpi <- 200 + 0.5 * adstock_geometric(spend, decay = 0.7) + rnorm(120, sd = 30)
tuned <- tune_carryover(spend, kpi, decays = seq(0.05, 0.95, by = 0.05))
plot(tuned, truth = 0.7)
```
