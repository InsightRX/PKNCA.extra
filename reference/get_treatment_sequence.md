# Function to generate a sequence variable for e.g. a crossover trial design

Function to generate a sequence variable for e.g. a crossover trial
design

## Usage

``` r
get_treatment_sequence(data, dictionary, variable = "treatment", groups = NULL)
```

## Arguments

- data:

  dataset in pre-parse format, preferrably created using
  `create_nca_data()`. Can also be a NONMEM-style dataset with EVID=0\|1
  column indicating PK an dosing data.

- dictionary:

  abridged data dictionary for the input dataset, specified as list
  object. Requires at least entries for `subject_id`, `time`, `conc`,
  `dose`. Optional entries are: `visit`. If required entries are
  specified only partially, then default values (i.e. CDISC)
  nomenclature will be imputed for missing identifiers.

- variable:

  variable name to base the sequence detection on

- groups:

  optional grouping variable, e.g. `ACTARM`.

## Value

a data frame with the subject_id, sequence number, and full sequence
description.
