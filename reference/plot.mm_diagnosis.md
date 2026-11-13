# Plot media diagnostics

One row per channel, in three panels: the worst variance inflation
factor on a log scale, the lowest coefficient of variation, and the
highest share of periods with no spend. What
[`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)
flagged is drawn as a red triangle. When spend and impressions were
supplied, a fourth panel counts the periods whose implied cost per mille
is an outlier, by spend column.

## Usage

``` r
# S3 method for class 'mm_diagnosis'
plot(x, ...)
```

## Arguments

- x:

  An `mm_diagnosis` object from
  [`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md).

- ...:

  Graphical arguments: `main` replaces the overall title and `col` gives
  two colours, for unflagged and flagged values. Others are ignored.

## Value

`x`, invisibly.

## Details

Dashed lines mark variance inflation factors of 5 and 10, the usual
levels for attention and for a split between collinear channels that is
not identified, and the 50% share of dark periods behind the flighting
flag. The thresholds a run used are not stored with it, so the markers,
not the lines, show what was flagged. A channel whose variance inflation
factor could not be computed has no marker in the first panel, which is
not the same as identified.

## See also

[`diagnose_media()`](https://elkronos.github.io/mediamix/reference/diagnose_media.md)

## Examples

``` r
data(mm_weekly)
d <- diagnose_media(mm_weekly,
                    media = c("tv", "video", "search", "social", "display"),
                    by = "geo")
plot(d)
```
