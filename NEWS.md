# bettr 1.6.0

-   **Server mode for JSON uploads**: The `bettr()` function now supports a `serverMode` parameter that enables users to upload JSON-formatted benchmark data files directly in the browser, without needing to provide data programmatically via R objects
-   **JSON serialization**: New `bettrToJSON()` and `bettrFromJSON()` functions enable serializing and deserializing `SummarizedExperiment` objects to/from JSON format
-   **URL parameters for programmatic loading**: Supports `jsonUrl` and `jsonFile` query parameters to automatically load JSON data on app startup, enabling direct links to visualizations and integration with external tools
-   **Browser localStorage caching**: Automatic client-side caching of uploaded data and application state
-   **Cache versioning**: Administrators of the server can force cache invalidation when deploying updates

# bettr 1.5.3

-   Expand vignette with more details about the app

# bettr 1.5.2

-   Add possibility to specify the default weight for metrics

# bettr 1.5.1

-   Minor fixes to tests to be more robust to possible future changes in ggplot2

# bettr 0.99.3

-   Add button to close app and return data

# bettr 0.99.2

-   Update installation instructions
-   Expand vignette

# bettr 0.99.1

-   Add R (\>= 4.4.0) to Depends

# bettr 0.99.0

-   Ready for submission to Bioconductor
-   Add `assembleSE()` function to create a summary SE

# bettr 0.3.0

-   Add `bettrGetReady()` function to prepare data for plotting independently of the shiny app
-   Export plot functions

# bettr 0.2.3

-   Add option to show heatmap as dot plot

# bettr 0.2.2

-   Bug fix in the data table
-   Add more methods for collapsing metrics

# bettr 0.2.1

-   Add table view

# bettr 0.2.0

-   New layout with collapsible side panel
-   Allow showing only the top N methods
-   Add option to filter methods and metrics
-   Add option to change space allocated to row/column names in heatmap
-   Add more score aggregation methods (weighted median, weighted fraction that the method is the best one)

# bettr 0.1.2

-   Bugfix to allow working with a single metric

# bettr 0.1.1

-   Remove grid lines in heatmap
-   Allow not displaying row names in heatmap
