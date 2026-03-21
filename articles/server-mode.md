# Server Mode Guide for bettr

**Version 1.6.0 and later**

### Overview

Version 1.6.0 introduces **server mode**, a new way to deploy and use
bettr that enables browser-based JSON file uploads, automatic caching,
and programmatic data loading via URLs. This makes bettr suitable for:

- **Multi-user server deployments** where different users need to
  visualize their own data
- **Web-based workflows** without requiring R programming knowledge
- **Data sharing** through portable JSON files and direct links

------------------------------------------------------------------------

### What’s New in Version 1.6.0

#### Server Mode for JSON Uploads

The
[`bettr()`](https://federicomarini.github.io/bettr/reference/bettr.md)
function now supports a `serverMode` parameter that launches the app in
a special mode where users upload JSON files through the browser.

**Benefits:**

- No R programming required for end users
- Multiple users can use the same server instance with their own data
- Data is isolated per user session
- Automatic browser caching for quick reload

#### JSON Serialization

New functions for converting bettr data to/from JSON format:

- [`bettrToJSON()`](https://federicomarini.github.io/bettr/reference/bettrToJSON.md) -
  Export `SummarizedExperiment` to JSON
- [`bettrFromJSON()`](https://federicomarini.github.io/bettr/reference/bettrFromJSON.md) -
  Import JSON back to `SummarizedExperiment`

**Benefits:**

- Portable data format (language-agnostic)
- Easy data sharing between users and systems
- Preserves all metadata, colors, weights, and transformations
- Enables web-based workflows

#### Browser localStorage Caching

Automatic client-side caching of uploaded JSON data in server mode.

**What gets cached:**

- Uploaded JSON data

**Benefits:**

- Page refresh doesn’t lose your work
- Survives browser restarts
- No manual saving required
- Automatic caching after upload

#### Cache Versioning

Administrators can force cache invalidation when deploying updates.

**Use cases:**

- Deploy schema changes that require fresh data
- Force all users to re-upload after breaking changes
- Manage cache in multi-tenant environments
- Troubleshooting cache-related issues

#### URL Parameters for Programmatic Loading

Load JSON data automatically via URL query parameters.

**Benefits:**

- Create direct links to specific visualizations
- Integrate with external tools and pipelines
- Automate opening visualizations after benchmark runs
- Share analysis results with a simple URL

------------------------------------------------------------------------

### Server Mode Basics

#### Starting a Server

``` r
library(bettr)

# Basic server mode
bettr(serverMode = TRUE)

# With cache versioning
bettr(serverMode = TRUE, cacheVersion = "v1.0")

# Custom theme
bettr(serverMode = TRUE, bstheme = "flatly")
```

#### What Happens When You Start Server Mode

1.  The app starts listening on a local port (e.g.,
    `http://127.0.0.1:4567`)
2.  Users navigate to this URL in their browser
3.  The upload interface is displayed
4.  Users can:
    - Upload JSON files manually
    - Have data loaded automatically via URL parameters (if provided)
5.  Once data is uploaded, the full bettr interface appears
6.  All interactions are automatically cached in the browser

#### User Interface

**Upload Mode Interface:**

- Clean, simple upload area
- “Choose JSON File” button
- Information about browser caching
- No “Close app” button (for multi-user safety)

**After Upload:**

- Full bettr visualization interface
- All standard features available
- “Load Data” panel for uploading new files
- “Clear Cached Data” button

------------------------------------------------------------------------

### JSON Serialization

#### Creating JSON Files

##### From Data Frames

``` r
library(bettr)

# Create benchmark data
benchmark_data <- data.frame(
    Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC"),
    Accuracy = c(0.95, 0.92, 0.89),
    Speed = c(120, 150, 100),
    Memory = c(256, 512, 128)
)

# Add metric metadata
metric_info <- data.frame(
    Metric = c("Accuracy", "Speed", "Memory"),
    Type = c("Quality", "Performance", "Resource"),
    Unit = c("Proportion", "ms", "MB")
)

# Define initial transformations
transforms <- list(
    Speed = list(flip = TRUE, transform = "[0,1]"),
    Memory = list(flip = TRUE, transform = "[0,1]")
)

# Create SummarizedExperiment
bettrSE <- assembleSE(
    df = benchmark_data,
    idCol = "Method",
    metricInfo = metric_info,
    initialTransforms = transforms
)

# Export to JSON
json_file <- tempfile(fileext = ".json")
bettrToJSON(bettrSE, file = json_file)
```

**Click to view example JSON structure**

The exported JSON file has the following structure:

``` json
{
  "idCol": "Method",
  "data": [
    {
      "Method": "AlgorithmA",
      "Accuracy": 0.95,
      "Speed": 120,
      "Memory": 256,
      "_row": "AlgorithmA"
    },
    {
      "Method": "AlgorithmB",
      "Accuracy": 0.92,
      "Speed": 150,
      "Memory": 512,
      "_row": "AlgorithmB"
    },
    {
      "Method": "AlgorithmC",
      "Accuracy": 0.89,
      "Speed": 100,
      "Memory": 128,
      "_row": "AlgorithmC"
    }
  ],
  "metricInfo": [
    {
      "Metric": "Accuracy",
      "Type": "Quality",
      "Unit": "Proportion"
    },
    {
      "Metric": "Speed",
      "Type": "Performance",
      "Unit": "ms"
    },
    {
      "Metric": "Memory",
      "Type": "Resource",
      "Unit": "MB"
    }
  ],
  "idInfo": {},
  "initialWeights": {},
  "initialTransforms": {
    "Speed": {
      "flip": true,
      "transform": "[0,1]"
    },
    "Memory": {
      "flip": true,
      "transform": "[0,1]"
    }
  }
}
```

**Key components:**

- `idCol`: Name of the identifier column
- `data`: Array of objects, one per method with all metric values
- `metricInfo`: Metadata about each metric (optional classifications,
  units, etc.)
- `idInfo`: Additional metadata about methods (optional)
- `initialWeights`: Default metric weights (optional)
- `initialTransforms`: Transformations to apply to metrics (flip for
  “lower is better”, transform for normalization)

##### From Existing SummarizedExperiment

``` r
# If you already have a SummarizedExperiment
json_file <- tempfile(fileext = ".json")
bettrToJSON(my_existing_SE, file = json_file)

# Get JSON as string (for programmatic use)
json_string <- bettrToJSON(my_existing_SE, file = NULL)
```

#### Reading JSON Files

``` r
# Read from file
bettrSE <- bettrFromJSON(file = "benchmark_results.json")

# Read from JSON string
bettrSE <- bettrFromJSON(json = json_string)

# Use in non-server mode
bettr(bettrSE = bettrSE)
```

------------------------------------------------------------------------

### URL Parameters for Programmatic Loading

#### Overview

URL query parameters allow you to automatically load JSON data when
users open the app.

#### Two Loading Methods

##### Load from URL (`jsonUrl`)

    http://localhost:4567/?jsonUrl=https://example.com/data.json

**Use cases:**

- Data hosted on web servers
- GitHub raw file URLs
- Cloud storage (S3, Google Cloud Storage, etc.)
- Internal company servers

**Example:**

``` r
# Start server
bettr(serverMode = TRUE)

# Construct URL
base_url <- "http://localhost:4567"
data_url <- "https://raw.githubusercontent.com/user/repo/main/results.json"
full_url <- paste0(base_url, "/?jsonUrl=", URLencode(data_url, reserved = TRUE))

# Open in browser
browseURL(full_url)
```

##### Load from File Path (`jsonFile`)

    http://localhost:4567/?jsonFile=/absolute/path/to/data.json

**Use cases:**

- Local server deployments
- Internal network file shares
- Automated pipelines on same machine
- Development and testing

**Example:**

``` r
# Start server
bettr(serverMode = TRUE)

# Construct URL
base_url <- "http://localhost:4567"
file_path <- "/Users/username/benchmarks/latest_results.json"
full_url <- paste0(base_url, "/?jsonFile=", URLencode(file_path, reserved = TRUE))

# Open in browser
browseURL(full_url)
```

**Using the included example data:**

The bettr package includes a pre-exported JSON file from the
DuoClustering2018 dataset that can be used for testing and examples.

``` r
# Start server
bettr(serverMode = TRUE)

# Get path to included example JSON file
json_path <- system.file("extdata", "duo2018_bettr.json", package = "bettr")

# Construct URL
base_url <- "http://localhost:4567"
full_url <- paste0(base_url, "/?jsonFile=", URLencode(json_path, reserved = TRUE))

# Open in browser
browseURL(full_url)
```

------------------------------------------------------------------------

### Browser Cache Management

#### How Caching Works

1.  **Automatic Save:**
    - When you upload a JSON file, it’s saved to browser localStorage
    - Happens immediately after successful upload
    - No manual action required
2.  **Automatic Restore:**
    - When you reload the page, cached data is automatically restored
    - A notification confirms the restoration
3.  **Cache Storage:**
    - Stored in browser’s localStorage (typically 5-10MB limit)
    - Specific to your browser and domain
    - Persists across browser restarts
    - Not shared across different browsers or devices

#### Manual Cache Control

##### Clear Cache Button

Located in the “Load Data” accordion panel:

    Load Data
    ├── Choose JSON File
    └── Clear Cached Data (button)

Click to:

- Remove all cached data
- Start fresh with a new dataset
- Free up browser storage

#### Cache Versioning (Administrators)

##### Setting Up Versioning

``` r
# Deploy version 1.0
bettr(serverMode = TRUE, cacheVersion = "v1.0")

# Later, deploy version 1.1 with breaking changes
bettr(serverMode = TRUE, cacheVersion = "v1.1")
```

##### Version String Format

Any string works - choose what makes sense for your workflow:

``` r
# Semantic versioning
cacheVersion = "v1.2.3"

# Date-based
cacheVersion = "2025-10-07"

# Build numbers
cacheVersion = "build-456"

# Custom
cacheVersion = "production-release-oct2025"
```

##### User Experience

When version changes:

1.  User opens the app
2.  Browser detects version mismatch
3.  Cache is automatically cleared
4.  User sees: **“Cache was cleared due to version update. Please upload
    your data.”**
5.  User uploads their JSON file again
6.  New cache is created with current version

## Session info

``` r
sessionInfo()
#> R Under development (unstable) (2026-03-20 r89666)
#> Platform: aarch64-apple-darwin23
#> Running under: macOS Sequoia 15.7.4
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRblas.0.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: UTC
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] BiocStyle_2.39.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39       desc_1.4.3          R6_2.6.1           
#>  [4] bookdown_0.46       fastmap_1.2.0       xfun_0.57          
#>  [7] cachem_1.1.0        knitr_1.51          htmltools_0.5.9    
#> [10] rmarkdown_2.30      lifecycle_1.0.5     cli_3.6.5          
#> [13] sass_0.4.10         pkgdown_2.2.0.9000  textshaping_1.0.5  
#> [16] jquerylib_0.1.4     systemfonts_1.3.2   compiler_4.6.0     
#> [19] tools_4.6.0         ragg_1.5.1          bslib_0.10.0       
#> [22] evaluate_1.0.5      yaml_2.3.12         BiocManager_1.30.27
#> [25] otel_0.2.0          jsonlite_2.0.0      rlang_1.1.7        
#> [28] fs_1.6.7            htmlwidgets_1.6.4
```
