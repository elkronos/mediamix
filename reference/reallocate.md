# Move spend between two channels of a plan

Takes `amount` from one channel and gives it to another, keeping each
channel's flighting: `from` is scaled down by the same proportion in
every period, and `to` gains `amount` in proportion to its spend in each
period. The result is a new plan for
[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md)
to compare with the old one.

## Usage

``` r
reallocate(plan, from, to, amount, by = NULL)
```

## Arguments

- plan:

  A data frame with one row per period and a numeric column of spend per
  channel. The `from` and `to` columns must be complete, finite and
  non-negative.

- from, to:

  Names of the columns to move spend from and to.

- amount:

  Spend to move, non-negative. Without `by`, one number. With `by`, one
  number moved within every group, or a vector named by group that moves
  each named group its own amount and leaves the others as they are.

- by:

  Optional grouping, as for
  [`transform_plan()`](https://elkronos.github.io/mediamix/reference/transform_plan.md):
  column names of `plan`, or a vector or data frame of vectors with one
  element or row per row of `plan`. Group names for `amount` are the
  group's values, joined with `" / "` when there is more than one
  grouping column.

## Value

`plan` with the `from` and `to` columns changed and everything else as
it was.

## Details

Without `by`, `amount` is moved across the whole plan, so each group
keeps its share of both channels. With `by`, it is moved within each
group, so the total moved is `amount` times the number of groups, or the
sum of a named `amount`. Moving an amount in one region only is how a
reallocation is set up as a geo test.

A move larger than `from`'s spend (in the group) is an error, and so is
a move into a channel with no spend (in the group), since there is no
flighting to spread the amount over. To start a dark channel, give it
small weights in the periods it should run, for example
`plan$video <- 1e-6` for an even spread, and move the amount into it.

## See also

[`simulate_plan()`](https://elkronos.github.io/mediamix/reference/simulate_plan.md),
[`allocate_budget()`](https://elkronos.github.io/mediamix/reference/allocate_budget.md)

## Examples

``` r
data(mm_weekly)
north <- mm_weekly[mm_weekly$geo == "north", ]

moved <- reallocate(north, from = "display", to = "social", amount = 2000)
c(display = sum(moved$display) - sum(north$display),
  social = sum(moved$social) - sum(north$social))
#> display  social 
#>   -2000    2000 

# Display's weeks off stay off, and social keeps its own pattern
all(moved$display[north$display == 0] == 0)
#> [1] TRUE
range(moved$social / north$social, na.rm = TRUE)
#> [1] 1.054472 1.054472

# A test in one region: only the north moves
test <- reallocate(mm_weekly, "display", "social",
                   amount = c(north = 2000), by = "geo")
tapply(test$social - mm_weekly$social, mm_weekly$geo, sum)
#> central   north   south 
#>       0    2000       0 
```
