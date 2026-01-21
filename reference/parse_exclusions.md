# Helper function to get a mask based on a list of vectors with identifiers. This function is used to gather exclusions in the `run_nca()` function.

Helper function to get a mask based on a list of vectors with
identifiers. This function is used to gather exclusions in the
[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
function.

## Usage

``` r
parse_exclusions(data, exclusions)
```

## Arguments

- data:

  data.frame

- exclusions:

  a named list of lists, with the names corresponding to columns in the
  dataset. Each sub-list should contain vectors `id` and `reason`. E.g.

  `list( SAMPLEID = list( id = c("017011111-20120908T120000", "017011028-20130719T010000", "017011033-20140318T040000"), reason = c("unexplained anomaly", "manual lambda-z check", "assay error") ) )`

## Value

a list with: the mask (vector of T/F) that indicates the rows in the
data to exclude, and a vector of reasons for exclusions (NA if not
excluded)
