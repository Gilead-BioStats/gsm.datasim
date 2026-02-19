# Generate analytics layers from raw data

Convenience wrapper that executes the analytics pipeline and returns
outputs into site/country/study layers (per snapshot when applicable).

## Usage

``` r
generate_analytics_layers(raw_data, config, verbose = FALSE)
```

## Arguments

- raw_data:

  Raw study data (single or multi-snapshot list)

- config:

  Study configuration object

- verbose:

  Whether to print progress/output messages

## Value

Raw analytics pipeline results
