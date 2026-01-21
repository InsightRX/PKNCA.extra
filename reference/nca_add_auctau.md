# Add AUCtau to the NCA output, if multi-dose PK data

Will copy `auclast` to `auctau` in the NCA output, for those intervals
where the end of the interval is \<Inf.

## Usage

``` r
nca_add_auctau(output, dictionary, verbose = FALSE)
```

## Arguments

- output:

  PKNCA output object

- dictionary:

  data dictionary

- verbose:

  verbose output?

## Value

NCA output data with `auctau` added.

## Details

It will first check if the NCA was indeed multi-dose, as this is not
relevant variable for single-dose data.
