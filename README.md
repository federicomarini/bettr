<img src="man/figures/bettr.png" align="right" alt="bettr" width="150"/>

<br>

# `bettr`: a better way to explore what is best

<br>

<!-- badges: start -->
[![R-CMD-check](https://github.com/federicomarini/bettr/workflows/R-CMD-check/badge.svg)](https://github.com/federicomarini/bettr/actions)
[![Codecov.io coverage status](https://codecov.io/github/federicomarini/bettr/coverage.svg?branch=devel)](https://codecov.io/github/federicomarini/bettr)
<!-- badges: end -->

As new (computational) methods come along, it becomes essential to compare 
their performance to existing ones via objective and fair benchmarking.

In a benchmarking study, typically many different performance metrics are
calculated and used to reflect different aspects of performance.
These performance metrics can then be combined into one "overall" ranking. 
However, not all aspects are equally important to everyone, and thus there is
more than one "right" way of aggregating metrics in order to rank methods. 

`bettr` is our proposal to perform this aggregation in an interactive way, 
allowing the user to focus on the aspects that are most important to them, 
and use different types of visualization approaches, enhancing the final 
overview of the benchmarking process.

The screenshots below display two of the representations provided by 
`bettr`, using data from the benchmark of single-cell clustering methods 
performed by [Duo et al (2018)](https://f1000research.com/articles/7-1141).

<img src="vignettes/bettr-screenshot-heatmap.png" alt="Heatmap" width="800"/>
<img src="vignettes/bettr-screenshot-polar.png" alt="Polar plot" width="800"/>

## Installation

The development version of `bettr` can be installed from GitHub: 

``` r
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("federicomarini/bettr")
```

## Usage

For more details on how to use `bettr`, we refer to the 
[vignette](https://federicomarini.github.io/bettr/articles/bettr.html).
If you have questions or run into problems, feel free to open an 
[issue](https://github.com/federicomarini/bettr/issues).
