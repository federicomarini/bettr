# Create a summary heatmap

Create a summary heatmap. The input arguments for this functions are
typically generated using
[`bettrGetReady`](https://federicomarini.github.io/bettr/reference/bettrGetReady.md),
which ensures that all required columns are available.

## Usage

``` r
makeHeatmap(
  bettrList = NULL,
  plotdata,
  scoredata,
  idCol,
  metricCol = "Metric",
  valueCol = "ScaledValue",
  weightCol = "Weight",
  scoreCol = "Score",
  metricGroupCol = "metricGroup",
  metricInfo,
  metricColors,
  idInfo,
  idColors,
  metricCollapseGroup = FALSE,
  metricGrouping = "---",
  labelSize = 10,
  showRowNames = TRUE,
  plotType = "Heatmap",
  rownamewidth_cm = 6,
  colnameheight_cm = 6
)
```

## Arguments

- bettrList:

  A `list`, the output object from `prepData`. If `bettrList` is
  provided, arguments `plotdata`, `scoredata`, `idCol`, `metricCol`,
  `valueCol`, `weightCol`, `scoreCol`, `metricGroupCol`, `metricInfo`,
  `metricColors`, `idInfo`, `idColors`, `metricCollapseGroup`,
  `metricGrouping` and `methods` will be ignored and the corresponding
  values will be extracted from `bettrList`. This is the recommended way
  of calling the plotting functions, as it ensures compatibility of all
  components.

- plotdata:

  A `data.frame` with columns representing methods, metrics, scores, and
  weights. Typically obtained as `prepData$plotdata`, where `prepData`
  is the output from `bettrGetReady`.

- scoredata:

  A `data.frame` with columns representing methods, aggregated scores,
  and any other method annotations. Typically obtained as
  `prepData$scoredata`, where `prepData` is the output from
  `bettrGetReady`.

- idCol:

  Character scalar indicating which column of `plotdata` and `scoredata`
  contains the method IDs.

- metricCol:

  Character scalar indicating which column of `plotdata` contains the
  metric IDs. Typically, `"Metric"`.

- valueCol:

  Character scalar indicating which column of `plotdata` contains the
  metric values. Typically, `"ScaledValue"`.

- weightCol:

  Character scalar indicating which column of `plotdata` contains the
  weight values. Typically, `"Weight"`.

- scoreCol:

  Character scalar indicating which column of `scoredata` contains the
  aggregated score values. Typically, `"Score"`.

- metricGroupCol:

  Character scalar indicating which column of `plotdata` contains the
  information about the metric group. Typically, `"metricGroup"`.

- metricInfo:

  `data.frame` with annotations for metrics. Typically obtained as
  `prepData$metricInfo`, where `prepData` is the output from
  `bettrGetReady`.

- metricColors:

  Named list with colors used for the metrics and any other metric
  annotations. Typically obtained as `prepData$metricColors`, where
  `prepData` is the output from `bettrGetReady`.

- idInfo:

  `data.frame` with annotations for entities. Typically obtained as
  `prepData$idInfo`, where `prepData` is the output from
  `bettrGetReady`.

- idColors:

  Named list with colors used for methods and any other method
  annotations. Typically obtained as `prepData$idColors`, where
  `prepData` is the output from `bettrGetReady`.

- metricCollapseGroup:

  Logical scalar indicating whether metrics should be collapsed by the
  group variable provided by `metricGrouping`. Typically obtained as
  `prepData$metricCollapseGroup`, where `prepData` is the output from
  `bettrGetReady`.

- metricGrouping:

  Character scalar indicating the column of `metricInfo` that was used
  to group metrics. Typically obtained as `prepData$metricGrouping`,
  where `prepData` is the output from `bettrGetReady`.

- labelSize:

  Numeric scalar providing the size of the labels in the plot.

- showRowNames:

  Logical scalar indicating whether to show row (method) names in the
  heatmap.

- plotType:

  Either `"Heatmap"` or `"Dot plot"` indicating the type of plot to
  construct.

- rownamewidth_cm, colnameheight_cm:

  Numeric scalars defining the width of row names and height of column
  names, in cm.

## Value

A
[`HeatmapList`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapList.html)
object.

## Author

Charlotte Soneson

## Examples

``` r
## Generate example data
df <- data.frame(Method = c("M1", "M2", "M3"),
                 metric1 = c(1, 2, 3),
                 metric2 = c(3, 1, 2))
metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
                         Group = c("G1", "G2", "G2"))
idInfo <- data.frame(Method = c("M1", "M2", "M3"),
                     Type = c("T1", "T1", "T2"))
prepData <- bettrGetReady(df = df, idCol = "Method",
                          metricInfo = metricInfo, idInfo = idInfo)
makeHeatmap(bettrList = prepData, plotType = "Heatmap")

makeHeatmap(bettrList = prepData, plotType = "Dot plot")

```
