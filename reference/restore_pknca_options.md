# Restore a previously saved set of PKNCA options

[`PKNCA::PKNCA.options()`](https://humanpred.github.io/pknca/reference/PKNCA.options.html)
is global, session-wide state. Functions that change it temporarily
(such as
[`run_nca()`](https://insightrx.github.io/PKNCA.extra/reference/run_nca.md))
should save the options on entry with
[`PKNCA::PKNCA.options()`](https://humanpred.github.io/pknca/reference/PKNCA.options.html)
and call this function on exit, so that the options seen by any later
code are not affected by the analysis.

## Usage

``` r
restore_pknca_options(options)
```

## Arguments

- options:

  list of PKNCA options, as returned by
  [`PKNCA::PKNCA.options()`](https://humanpred.github.io/pknca/reference/PKNCA.options.html).

## Value

The restored PKNCA options (invisibly).

## Details

The options are first reset to the PKNCA defaults, after which all
options that differ from the defaults are set in a single call to
[`PKNCA::PKNCA.options()`](https://humanpred.github.io/pknca/reference/PKNCA.options.html).
PKNCA validates all values before storing any of them, so the restore is
atomic: either all options are restored, or (if any value is rejected)
the options are left at the PKNCA defaults. Options whose saved value is
`NULL` are restored through the reset to defaults, since
[`PKNCA::PKNCA.options()`](https://humanpred.github.io/pknca/reference/PKNCA.options.html)
cannot set an option to `NULL`.

PKNCA re-validates the saved values when they are set. Any warnings from
that validation (e.g. `min.span.ratio` below 2) were already shown when
the options were originally set, and are therefore suppressed. A warning
is given if the restored options are not identical to the saved options.
