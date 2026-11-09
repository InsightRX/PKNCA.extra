# Draw from a normal distribution using a specific seed

Draws random numbers with
[`stats::rnorm()`](https://rdrr.io/r/stats/Normal.html) using the given
`seed` and R's default random number generators, and restores the global
random number generator state and kind afterwards, so that the session's
random number stream is not affected.

## Usage

``` r
rnorm_with_seed(n, mean = 0, sd = 1, seed = NULL)
```

## Arguments

- n:

  number of draws

- mean:

  mean of the normal distribution

- sd:

  standard deviation of the normal distribution

- seed:

  integer random seed. If `NULL`, draws from the current random number
  generator state without restoring it.

## Value

numeric vector of length `n`
