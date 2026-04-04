---
title: 'Interactive exploration of benchmarking results with _bettr_'
tags:
  - R
  - benchmarking
  - visualization
  - interactive
authors:
  - name: Charlotte Soneson
    orcid: 0000-0003-3833-2169
    affiliation: "1, 2"
    corresponding: true
  - name: Federico Marini
    orcid: 0000-0003-3252-7758
    affiliation: "3, 4"
  - name: Daniel Incicau
    orcid: 0009-0001-1748-6145
    affiliation: "2, 5"
  - name: Anthony Sonrel
    orcid: 0000-0002-2414-715X
    affiliation: "2, 5"
  - name: Almut Lütge
    orcid: 0000-0003-1104-727X
    affiliation: 6
  - name: Reto Gerber
    orcid: 0000-0001-5414-8906
    affiliation: "2, 5"
  - name: Ben Carrillo
    orcid: 0009-0003-5704-4151
    affiliation: "2, 5"
  - name: Izaskun Mallona
    orcid: 0000-0002-2853-7526
    affiliation: "2, 5"
  - name: Mark D Robinson
    orcid: 0000-0002-3048-5518
    affiliation: "2, 5"
affiliations:
  - name: Friedrich Miescher Institute for Biomedical Research, Basel, Switzerland
    index: 1
    ror: 01bmjkv45
  - name: SIB Swiss Institute of Bioinformatics, Basel, Switzerland
    index: 2
    ror: 002n09z45
  - name: Institute of Medical Biostatistics, Epidemiology and Informatics (IMBEI), University Medical Center of the Johannes Gutenberg-University Mainz, Mainz, Germany
    index: 3
    ror: 00q1fsf04
  - name: Research Center for Immunotherapy (FZI) Mainz, Mainz, Germany
    index: 4
  - name: Department of Molecular Life Sciences, University of Zurich, Zurich, Switzerland
    index: 5
    ror: 02crff812 
  - name: Swiss Data Science Centre, Zurich, Switzerland
    index: 6
    ror: 02hdt9m26
date: 12 December 2025
bibliography: paper.bib
---

# Summary

Method benchmarking is a core part of many research fields and aims to establish best practices in method selection and application as well as help identifying gaps and possibilities for improvement in existing methods. 
A typical benchmarking study scores a set of methods using a variety of different metrics intended to capture different aspects of performance and usability (e.g., area under the receiver operating characteristic (ROC) curve for a classification task, or execution time). 
For practical purposes, e.g., when a user needs to select a method for their own analysis, it is often the case that not all of the evaluated metrics are equally important. 
For example, the ability of a statistical method to handle arbitrarily complex experimental designs may not be relevant if the user only needs to perform a two-group comparison, while memory frugality may be of high importance if the analysis will be performed on a system with limited computational resources. 
Hence, having the ability to easily and adaptively create a weighted summary score by which the methods can be ranked, or alternatively to create a visual summary of only the desired metrics, would make it possible for users to tailor their method choice based on the aspects that are most relevant to them. 

Inspired by the [OECD 'Better Life Index'](https://www.oecd.org/en/data/tools/oecd-better-life-index.html), which allows users to score OECD countries by a customizable weighted average of various factors contributing to human well-being, we developed the _bettr_ R/Bioconductor package to provide similar support for consumers of benchmarking studies. 
Given a table with values of evaluation metrics across the evaluated methods, as well as optional method annotations and metric groupings, _bettr_ allows users to visualize performance summaries emphasizing the aspects and evaluation metrics that are most important to them (Figure 1). 
_bettr_ can be used interactively as an R/Shiny application [@chang2025-shiny], or programmatically by calling the underlying functions directly for full reproducibility and integration into analytical pipelines (see the package vignettes for more details). 

![Screenshot of a _bettr_ application illustrating the benchmarking results from @soneson2018-bias. 
The size and color of the circles indicate the evaluation score for each method (rows) and metric (columns), with higher values being more favorable. 
The bars above the plot indicate the current weights for the metrics (these are controlled by the sliders in the bottom of the left sidebar), and the bars to the right of the plot represent the aggregated scores for the methods (in this case, the weighted mean across metrics). 
The colored bars to the left of the plot represent categorical annotations and characterizations of the methods, which are not taken into account when determining the ranking. 
In addition to the displayed dot plot, the results can be visualized in several other ways, including a heatmap, polar plots, and using parallel coordinates. 
Moreover, the set of methods and metrics to include in the display can be controlled by the user via the 'Filter methods/metrics' tab.](bettr-screenshot-heatmap.png)


# Statement of Need

Method benchmarking is a prolific area of research across different fields of application. 
However, in many cases the conclusions presented in benchmarking papers are hard for a reader to explore further, since the results (e.g., performance metrics) are often not readily available or provided in an explorable format [@Sonrel2023-metaanalysis].
As a consequence, much of the interpretation is left solely to the benchmarker. 
_bettr_ aims to address this asymmetry and enable both authors and readers of benchmarking studies to explore the results more easily and flexibly.

For the reader of a benchmarking study, the interactive nature of _bettr_ makes it particularly suitable for such visual, exploratory analysis since they can interactively modify the importance associated with the different evaluation metrics and immediately see how it affects the ranking of the methods. 
For authors of benchmarking studies, it is straightforward to deploy an instance of _bettr_ using, e.g., a local Shiny server or a commercial option such as [https://shinyapps.io](https://shinyapps.io), to allow readers to easily explore their results.
Moreover, _bettr_ accepts input in multiple formats, including a collection of data frames, a SummarizedExperiment object [@morgan2025-se] or a JSON file, and contains functions for converting one input format to another. 
This increases the flexibility of the application and makes it easy to combine with a variety of workflows. 
For example, a user working locally in R may find it more convenient to generate a set of data frames, while an automated benchmarking workflow could export all necessary components in a single, platform-independent JSON file. 
_bettr_ can also be deployed in server mode, allowing users to upload their own input data (in JSON format) to a running app. 


# State of the Field

Complementary functionality for creating summary representations (e.g., heatmap-like visualizations) of benchmarking results is provided e.g. by the funkyheatmap package [@cannoodt2025-funkyheatmap], and benchmarking platforms such as OpenEBench [@capella2017-openebench] and OpenProblems [@Luecken2025-openproblems] also produce result visualizations for the included benchmarks. 
However, existing tools typically lack flexibility in either input format or means of deployment, or the ability to explore results interactively, and are not intended to support user-specific metric weighting.
To the best of our knowledge, _bettr_ is the first generic tool for benchmark visualization that combines interactivity and flexibility in metric weighting with ease of use and a transparent, programmatic interface. 

# Software Design

The design philosophy behind _bettr_ focuses on flexibility, accessibility, and reproducibility. 
By supporting several input formats, from collections of data frames via a single R-based object to an all-encompassing JSON file, _bettr_ can be used for downstream analysis and visualization of results generated using a wide range of benchmark setups and programming languages. 
_bettr_ is fully open source and easily installable as an R package, mainly distributed via Bioconductor with the most recent development version also available on GitHub.
In addition, it is available via [_r-universe_](https://bioc.r-universe.dev/bettr), which among other things provides binaries for several platforms, including WebAssembly.
As a complement to the interactive interface, full reproducibility is enabled by the equivalent programmatic interface to the functionality, as well as the ability to export the processed data as either a shareable csv file containing the metric values as well as the final score, or an R list that can be used directly as the input for further analysis and visualization.

# Research Impact Statement

In 2025, _bettr_ was [downloaded](https://bioconductor.org/packages/stats/bioc/bettr/) almost 3,000 times (by 1,593 unique IPs) from Bioconductor alone. 
Adoption of _bettr_ is expected to increase further in the near future, as it has become the first supported integration for automated metrics reporting within the [Omnibenchmark](https://www.omnibenchmark.org) project. 
Omnibenchmark [@mallona2026-omnibenchmark] is a benchmarking system that automates and standardizes routine aspects of benchmarking through standardization and formalization of benchmarking plans. 
During execution, Omnibenchmark collects computational performance metrics (e.g., peak memory usage, CPU utilization, run time, etc), as well as, where applicable, algorithmic performance metrics (e.g., F1 scores, ARIs, etc). 
These results are then exported via a command-line interface in a JSON format compatible with _bettr_, enabling automated reporting. 
This integration lowers the barrier for benchmark authors to produce rich, interactive summaries of complex benchmarking studies without requiring custom visualization pipelines. 
Hence, broader adoption of Omnibenchmark as a benchmarking framework is expected to further drive the use of _bettr_ as an interactive platform for exploring and interpreting benchmark results. 

# Availability and Examples

_bettr_ is available via [Bioconductor](https://www.bioconductor.org/packages/bettr/) and on [GitHub](https://github.com/federicomarini/bettr). 
A collection of example data sets and _bettr_ configurations are available from [https://github.com/csoneson/bettr-examples](https://github.com/csoneson/bettr-examples). 
In addition, an example instance, using data from @soneson2018-bias, is deployed on [https://csoneson.shinyapps.io/soneson2018de/](https://csoneson.shinyapps.io/soneson2018de/). 

# AI Usage Disclosure

The majority of the _bettr_ code base was developed over several years, in a public GitHub repository, without the use of generative AI. 
For one pull request (#25), adding the capabilities to support JSON files as input and to cache the state of the app, Claude Sonnet 4.5 (Anthropic) was used to  assist with drafting implementation of new functions and UI components based on predefined design documents, while maintaining existing functionality as is.
At least two of the package developers reviewed the code carefully, edited it where necessary, and verified that the new code performed as intended and did not introduce regressions in existing functionality (which was further verified using the comprehensive set of existing unit tests).

# Acknowledgements

CS is supported by the Novartis Research Foundation. 
The work of FM is supported by the Deutsche Forschungsgemeinschaft (DFG, German Research Foundation) Projektnummer 318346496. 
MDR acknowledges funding from the Swiss National Science Foundation (grants 200021_212940 and 310030_204869) as well as support from swissuniversities P5 Phase B funding (project 23-36_14). 
The funders did not have any role in the design of the study, the collection, analysis and interpretation of data, or in writing the manuscript.

# References
