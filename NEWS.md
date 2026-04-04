# bettr 1.7.2

* Move the "Highlight method" UI inside the parallel coordinates plot tab

# bettr 1.6.0

* Add `serverMode` to enable JSON uploads directly in the browser
* Introduce `bettrToJSON()` / `bettrFromJSON()` for JSON serialization of `SummarizedExperiment` objects
* Support `jsonUrl` and `jsonFile` URL parameters for automatic data loading
* Add browser localStorage caching with cache versioning
* Add new vignette on server mode

# bettr 1.5.3

* Expand vignette with more details about the app

# bettr 1.5.2

* Add possibility to specify the default weight for metrics

# bettr 1.5.1

* Minor fixes to tests to be more robust to possible future changes in ggplot2

# bettr 0.99.3

* Add button to close app and return data

# bettr 0.99.2

* Update installation instructions
* Expand vignette

# bettr 0.99.1

* Add R (\>= 4.4.0) to Depends

# bettr 0.99.0

* Ready for submission to Bioconductor
* Add `assembleSE()` function to create a summary SE

# bettr 0.3.0

* Add `bettrGetReady()` function to prepare data for plotting independently of the shiny app
* Export plot functions

# bettr 0.2.3

* Add option to show heatmap as dot plot

# bettr 0.2.2

* Bug fix in the data table
* Add more methods for collapsing metrics

# bettr 0.2.1

* Add table view

# bettr 0.2.0

* New layout with collapsible side panel
* Allow showing only the top N methods
* Add option to filter methods and metrics
* Add option to change space allocated to row/column names in heatmap
* Add more score aggregation methods (weighted median, weighted fraction that the method is the best one)

# bettr 0.1.2

* Bugfix to allow working with a single metric

# bettr 0.1.1

* Remove grid lines in heatmap
* Allow not displaying row names in heatmap
