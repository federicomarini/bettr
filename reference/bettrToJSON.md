# Export SummarizedExperiment to bettr JSON format

Export a bettr SummarizedExperiment object to a standardized JSON format
that can be re-imported using `bettrSEFromJSON`.

## Usage

``` r
bettrToJSON(bettrSE, file = NULL, pretty = TRUE)
```

## Arguments

- bettrSE:

  A `SummarizedExperiment` object created by `assembleSE`.

- file:

  Character scalar, path to the output JSON file. If NULL, returns the
  JSON string without writing to file.

- pretty:

  Logical scalar, whether to format the JSON output with indentation for
  readability (default: TRUE).

## Value

If `file` is NULL, returns a JSON string. Otherwise, writes to file and
returns the file path invisibly.

## Author

Daniel Incicau

## Examples

``` r
df <- data.frame(Method = c("M1", "M2", "M3"),
                 metric1 = c(1, 2, 3),
                 metric2 = c(3, 1, 2))
bettrSE <- assembleSE(df = df)
json_str <- bettrToJSON(bettrSE)
```
