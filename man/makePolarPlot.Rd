% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makePolarPlot.R
\name{makePolarPlot}
\alias{makePolarPlot}
\title{Create a polar plot}
\usage{
makePolarPlot(
  bettrList = NULL,
  plotdata,
  idCol,
  metricCol = "Metric",
  valueCol = "ScaledValue",
  metricGroupCol = "metricGroup",
  metricColors,
  metricCollapseGroup = FALSE,
  metricGrouping = "---",
  labelSize = 10
)
}
\arguments{
\item{bettrList}{A \code{list}, the output object from \code{prepData}.
If \code{bettrList} is provided, arguments \code{plotdata},
\code{scoredata}, \code{idCol}, \code{metricCol}, \code{valueCol},
\code{weightCol}, \code{scoreCol}, \code{metricGroupCol},
\code{metricInfo}, \code{metricColors}, \code{idInfo}, \code{idColors},
\code{metricCollapseGroup}, \code{metricGrouping} and \code{methods}
will be ignored and the corresponding values will be extracted from
\code{bettrList}. This is the recommended way of calling the plotting
functions, as it ensures compatibility of all components.}

\item{plotdata}{A \code{data.frame} with columns representing methods,
metrics, scores, and weights. Typically obtained as
\code{prepData$plotdata}, where \code{prepData} is the output from
\code{bettrGetReady}.}

\item{idCol}{Character scalar indicating which column of \code{plotdata} and
\code{scoredata} contains the method IDs.}

\item{metricCol}{Character scalar indicating which column of \code{plotdata}
contains the metric IDs. Typically, \code{"Metric"}.}

\item{valueCol}{Character scalar indicating which column of \code{plotdata}
contains the metric values. Typically, \code{"ScaledValue"}.}

\item{metricGroupCol}{Character scalar indicating which column of
\code{plotdata} contains the information about the metric group.
Typically, \code{"metricGroup"}.}

\item{metricColors}{Named list with colors used for the metrics and
any other metric annotations. Typically obtained as
\code{prepData$metricColors}, where \code{prepData} is the output from
\code{bettrGetReady}.}

\item{metricCollapseGroup}{Logical scalar indicating whether metrics
should be collapsed by the group variable provided by
\code{metricGrouping}. Typically obtained as
\code{prepData$metricCollapseGroup}, where \code{prepData} is the
output from \code{bettrGetReady}.}

\item{metricGrouping}{Character scalar indicating the column of
\code{metricInfo} that was used to group metrics. Typically obtained as
\code{prepData$metricGrouping}, where \code{prepData} is the output
from \code{bettrGetReady}.}

\item{labelSize}{Numeric scalar providing the size of the labels in the plot.}
}
\value{
A \code{ggplot} object.
}
\description{
Create a polar plot. The input arguments for this functions are
typically generated using \code{\link{bettrGetReady}}, which ensures that
all required columns are available.
}
\examples{
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
makePolarPlot(bettrList = prepData)
                 
}
\author{
Charlotte Soneson
}
