# Function to export output from PKNCA to CDISC- or other mappings

This function could be temporary in this package, PKNCA may support
remapping in the future, at least to CDISC format.

## Usage

``` r
remap_nca_output(data, mapping = NULL, exclude_unmapped = TRUE, keep = c())
```

## Arguments

- data:

  output object from
  [`PKNCA::pk.nca()`](http://humanpred.github.io/pknca/reference/pk.nca.md)

- mapping:

  a list mapping PKNCA parameters to custom parameter names, e.g.
  `list(CMAX_custom = "CMAX", "auc_last = "AUCLAST")`. PKNCA parameter
  names are list values, new parameter names are list names.

- exclude_unmapped:

  exclude unmapped variables?

- keep:

  parameters to keep when remapping and excluding unmapped parameters.

## Value

a data.frame with column names matching the desired mapping

## Examples

``` r
if (FALSE) { # \dontrun{
nca <- run_nca(...)
nca_cdisc <- remap_nca_output(nca, "cdisc")
} # }
```
