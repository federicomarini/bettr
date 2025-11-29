# Import bettr data from JSON format

Import bettr data from a standardized JSON format and create a
SummarizedExperiment object using `assembleSE`.

## Usage

``` r
bettrFromJSON(file = NULL, json = NULL)
```

## Arguments

- file:

  Character scalar, path to the JSON file to import.

- json:

  Character scalar, JSON string to parse. If provided, `file` is
  ignored.

## Value

A `SummarizedExperiment` object that can be passed to `bettr`.

## Author

Daniel Incicau

## Examples

``` r
df <- data.frame(Method = c("M1", "M2", "M3"),
                 metric1 = c(1, 2, 3),
                 metric2 = c(3, 1, 2))
bettrSE <- assembleSE(df = df)
json_file <- tempfile(fileext = ".json")
bettrToJSON(bettrSE, file = json_file)
bettrSE_reload <- bettrFromJSON(file = json_file)
```
