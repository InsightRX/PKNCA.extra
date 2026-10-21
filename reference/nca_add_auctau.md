# Add AUCtau to the NCA output, if multi-dose PK data

Will copy `auclast` (or the column specified in `auc_col`) to `auctau`
in the NCA output, for those intervals where the end of the interval is
\<Inf.
[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md)
uses `aucint.last.dose` instead of `auclast` for intervals without a
concentration measured exactly at the end of the interval.

## Usage

``` r
nca_add_auctau(output, dictionary, verbose = FALSE, auc_col = "auclast")
```

## Arguments

- output:

  PKNCA output object

- dictionary:

  data dictionary

- verbose:

  verbose output?

- auc_col:

  name of the column with the AUC to use for `auctau`.

## Value

NCA output data with `auctau` added.

## Details

It will first check if the NCA was indeed multi-dose, as this is not
relevant variable for single-dose data.
