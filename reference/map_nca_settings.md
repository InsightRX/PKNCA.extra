# Map user-provided NCA settings to NCA engine (e.g. PKNCA)

Map user-provided NCA settings to NCA engine (e.g. PKNCA)

## Usage

``` r
map_nca_settings(settings = NULL, mapping = "pknca")
```

## Arguments

- settings:

  list of settings, potentially to be remapped

- mapping:

  name of mapping to use, default is `pknca`. Mappings are stored within
  package in folder `md/nca`.

## Value

A list of settings.
