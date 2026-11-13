# Diagnose media data before modelling

Runs the checks that decide whether a marketing mix model can work at
all. None of them are about the model; all of them are about whether the
data contains the information the model will be asked to find. Run this
first.

## Usage

``` r
diagnose_media(
  data,
  media,
  spend = NULL,
  impressions = NULL,
  by = NULL,
  vif_threshold = 5,
  cv_threshold = 0.15,
  decay = NULL,
  date = NULL
)
```

## Arguments

- data:

  A data frame containing the media columns. A tibble or data.table is
  read as a plain data frame.

- media:

  Character vector naming the media columns, each once.

- spend, impressions:

  Optional character vectors of matching spend and impression columns,
  in the same order, for the cost-per-mille consistency check.

- by:

  Optional character vector of grouping columns. Every diagnostic is
  computed *within* each group and the worst case reported: the highest
  variance inflation factor, the strongest absolute correlation, the
  lowest coefficient of variation, the highest share of dark periods.
  Pooling geographies that differ mainly in scale would manufacture
  correlation that exists in no single series, so `by` is worth
  supplying whenever the rows are a panel. Groups set with
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  are not used in its place; a message says so and names the `by` to
  pass.

- vif_threshold:

  Variance inflation factor above which a channel is flagged.

- cv_threshold:

  Coefficient of variation below which a channel is flagged as
  insufficiently varying.

- decay:

  Optional geometric decay in `[0, 1)` for computing collinearity on
  *adstocked* media: a single number for every channel, or a named
  vector with one per channel (unnamed channels are left raw). Adstock
  smooths each series, and smoothed series are usually more correlated
  than the raw spend, so this is the collinearity the model will face.
  Rows must be in time order within each group; pass `date` to check
  that they are. Missing spend is treated as zero for the adstock, and a
  message counts the values affected. Variation and flighting are always
  reported on the raw spend.

- date:

  Optional name of the time column, as a string. The column must be
  numeric, `Date` or `POSIXct`, with no missing values. Each series
  (each group of `by`) is checked for rows that come earlier than the
  row above them, for periods that appear more than once, and for
  missing periods, judged against the series' usual spacing. Adstock
  treats adjacent rows as adjacent periods, so each of these changes
  what a decay means. See *Time order* below.

## Value

An object of class `mm_diagnosis`: a named list with elements

- `variation`:

  Per channel: `mean` and `sd` averaged across groups, the lowest group
  `cv` and `distinct` count, the highest group `zero_share`, and the
  flags `low_variation`, `heavily_flighted` and `usable`.

- `collinearity`:

  Per channel: the worst group `vif`, the `most_correlated_with` partner
  and its `correlation`, and `identified`: `TRUE` when `vif` is at most
  `vif_threshold`, `FALSE` above it, and `NA` where the factor could not
  be computed at all. `NA` is not `TRUE`.

- `correlations`:

  Pairwise correlation matrix. With `by` supplied, each cell is the
  group value with the largest magnitude, sign preserved.

- `cpm`:

  One row per row of `data` and spend column when `spend` and
  `impressions` are supplied, `NULL` otherwise: `channel` (the spend
  column), `row` (the row number), the implied `cpm`, the series'
  `median_cpm` and the `outlier` flag. With `by`, the median and outlier
  rule are computed within each series and a `group` column is added.

- `dates`:

  One row per series when `date` is supplied, `NULL` otherwise: `group`
  (with `by`), `rows`, the `start` and `end` of the series, its
  `period`, and the counts `out_of_order` (rows that come earlier than
  the row above them), `duplicated` (rows that repeat a period already
  in the series), `missing` (whole periods absent between `start` and
  `end`) and `irregular` (steps between consecutive time points that
  differ from `period` by more than a quarter of it). `period` is the
  median step between the series' distinct time points, in the units of
  `date`: days for a `Date`, seconds for a `POSIXct`.

- `flags`:

  Character vector of the problems found, in the order they should be
  dealt with. Empty when nothing was found.

- `n_obs`, `n_groups`, `grouped`, `adstocked`:

  Rows examined, series examined, whether `by` was supplied, and whether
  collinearity was measured on adstocked media.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
`tidy()` return the per-channel table: one row per channel, with the
`variation` and `collinearity` columns side by side.

## Details

Four things sink marketing mix projects, and all four are visible before
a model is fitted.

**Channel collinearity** is common, because channels are usually planned
together. When two channels move together the model cannot tell their
effects apart. It will still produce coefficients, and those
coefficients can flip sign on small changes to the specification or the
sample. A variance inflation factor above 5 deserves attention and above
10 means the split between those channels is not identified, however
tight the overall fit looks.

**Insufficient variation** is the quieter version of the same problem. A
channel spending nearly the same amount every week carries almost no
information about what different amounts would do. No adstock or
saturation transform can recover an effect that the data never varied
enough to reveal.

**Zero inflation and flighting** matter because a channel that is dark
most of the time has far less effective sample than its row count
suggests, and because carryover across dark periods is where transform
bugs hide.

**Spend and impression inconsistency**, a wildly varying implied cost
per mille, usually means a data-join error upstream rather than a real
change in media pricing. It is worth catching before it becomes a
modelling puzzle. A period is flagged when its CPM is more than five
median absolute deviations from its series' median. When more than half
of a series' CPMs are identical the median absolute deviation is zero,
and a period is flagged instead when its CPM is at least twice or at
most half the median.

## Time order

Adstock treats adjacent rows as adjacent periods.
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md)
and
[`media_transform()`](https://elkronos.github.io/mediamix/reference/media_transform.md)
have no time index, so they take each series' rows in the order given: a
row out of order mixes later spend into earlier weeks, a period that
appears twice is carried over twice, and a missing week lets the decay
act once across what were two weeks. None of these raises an error. With
`date`, each series is checked for all three, and the result is in
`dates`.

The usual spacing of a series is the median step between its distinct
time points, as in
[`step_adstock()`](https://elkronos.github.io/mediamix/reference/step_adstock.md);
a series with a single time point takes the median of the other series'
spacings. A step counts as irregular when it differs from that spacing
by more than a quarter of it, so calendar months of 28 to 31 days are
regular. A gap of about two periods counts one missing period, of about
three periods two, and so on. Rows out of order are counted in the order
of `data`, one for each row whose time point is earlier than that of the
row above it in the same series.

## See also

[`plot.mm_diagnosis()`](https://elkronos.github.io/mediamix/reference/plot.mm_diagnosis.md),
[`adstock_geometric()`](https://elkronos.github.io/mediamix/reference/adstock_geometric.md),
[`contributions()`](https://elkronos.github.io/mediamix/reference/contributions.md)

## Examples

``` r
data(mm_weekly)
channels <- c("tv", "video", "search", "social", "display")

# Always pass `by` on panel data: pooling geographies that differ in scale
# invents correlation that is not in any single series.
d <- diagnose_media(mm_weekly, media = channels, by = "geo")
d
#> 
#> ── Media diagnostics 
#> 468 observations across 3 series, 5 channels
#> ✔ No structural problems found.
#> 
#> Inspect $variation, $collinearity, $correlations.

d$variation
#>   channel      mean       sd        cv distinct zero_share low_variation
#> 1      tv 1700.5021 881.6773 0.5096251      131  0.1474359         FALSE
#> 2   video  336.2778 308.5805 0.8938791       84  0.4166667         FALSE
#> 3  search  487.1432 115.5527 0.2286063      118  0.0000000         FALSE
#> 4  social  251.1923 174.8592 0.6612980      101  0.2756410         FALSE
#> 5 display  188.0641 149.3027 0.7584458       88  0.3333333         FALSE
#>   heavily_flighted usable
#> 1            FALSE   TRUE
#> 2            FALSE   TRUE
#> 3            FALSE   TRUE
#> 4            FALSE   TRUE
#> 5            FALSE   TRUE
d$collinearity
#>   channel      vif most_correlated_with correlation identified
#> 1   video 1.098660               social   0.2246800       TRUE
#> 2  social 1.093837                video   0.2246800       TRUE
#> 3      tv 1.066918                video   0.1589877       TRUE
#> 4  search 1.052593                video  -0.2063315       TRUE
#> 5 display 1.045907               social  -0.1879395       TRUE

# Collinearity after adstocking is what the model sees
diagnose_media(mm_weekly, media = channels, by = "geo",
               decay = attr(mm_weekly, "truth")$decay)$collinearity
#>   channel      vif most_correlated_with correlation identified
#> 1      tv 1.122759                video   0.3245009       TRUE
#> 2   video 1.122127                   tv   0.3245009       TRUE
#> 3  social 1.111494              display   0.2368121       TRUE
#> 4 display 1.100370               social   0.2368121       TRUE
#> 5  search 1.028554                   tv   0.1534401       TRUE

# A deliberately collinear pair is caught
fake <- mm_weekly[mm_weekly$geo == "north", ]
fake$twin <- fake$tv * 1.02 + 5
diagnose_media(fake, media = c("tv", "twin", "search"))$collinearity
#>   channel      vif most_correlated_with correlation identified
#> 1      tv      Inf                 twin  1.00000000      FALSE
#> 2    twin      Inf                   tv  1.00000000      FALSE
#> 3  search 1.000823                 twin  0.02868335       TRUE

# Adstock treats adjacent rows as adjacent periods, so check the dates too.
# Here one week of north is dropped and two others swap places.
messy <- mm_weekly[-10, ]
messy[2:3, ] <- messy[3:2, ]
checked <- diagnose_media(messy, media = channels, by = "geo",
                          date = "date")
checked$dates
#>     group rows      start        end period out_of_order duplicated missing
#> 1   north  155 2023-01-02 2025-12-22      7            1          0       1
#> 2 central  156 2023-01-02 2025-12-22      7            0          0       0
#> 3   south  156 2023-01-02 2025-12-22      7            0          0       0
#>   irregular
#> 1         1
#> 2         0
#> 3         0
checked$flags
#> [1] "Out of time order: 1 row(s) in 1 series (north) come earlier than the row above them. Adstock treats adjacent rows as adjacent periods, so sort each series by date before transforming it."           
#> [2] "Missing periods: 1 period(s) in 1 series (north) are absent, judged against each series' usual spacing. Carryover crosses each gap as if it were one period; fill the missing periods with zero spend."
```
