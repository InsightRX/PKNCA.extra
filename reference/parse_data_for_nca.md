# Parse data into dataset for NCA

The actual time since last dose (`ATSLD`) is simulated from the nominal
time (`PCTPTNUM`) by adding log-normally distributed noise (7% CV). To
make the output reproducible, the noise is generated using a fixed
random seed (argument `seed`). The global random number generator state
of the session is not affected.

## Usage

``` r
parse_data_for_nca(data, seed = 12345)
```

## Arguments

- data:

  list containing SDTM tables as data.frames

- seed:

  integer random seed used to simulate `ATSLD`. Default is `12345`,
  which makes the output reproducible across calls. If `NULL`, the
  current random number generator state of the session is used (and
  advanced), so results will differ between calls unless
  [`set.seed()`](https://rdrr.io/r/base/Random.html) is called
  beforehand.

## Value

data.frame with data for NCA package
