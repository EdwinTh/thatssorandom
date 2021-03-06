% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_bin.R
\name{gg_outlier_bin}
\alias{gg_outlier_bin}
\title{Create a Histogram with Outlier Bins}
\usage{
gg_outlier_bin(x, var_name, cut_off_floor, cut_off_ceiling, col = "black",
  fill = "cornflowerblue", fill_outlier_bins = "forestgreen",
  binwidth = NULL)
}
\arguments{
\item{x}{a dataframe.}

\item{var_name}{the name of the variable to visualize.}

\item{cut_off_floor}{the floor of the histogram, all values below it
will be put in the same bin. If `NA`` no floor bin is created.}

\item{cut_off_ceiling}{the ceiling of the histogram, all values above it
will be put in the same bin. If `NA`` no ceiling bin is created.}

\item{col}{the color of the histogram.}

\item{fill}{the fill of the regular histogram bins.}

\item{fill_outlier_bins}{the fill of the outlier bins, make them a separate
color to stress there different nature.}

\item{binwidth}{the binwidth of the histogram.}
}
\description{
Make a ggplot with `geom_histogram` that contains outlier bins. Bins can
be created for both floor and ceiling outliers. Code is experimental
so expect some deviations from normal plotting, like messed-up grid lines.
}
\examples{
library(tidyverse)
data_frame(x = c(runif(100, 0, 1000), rnorm(1000, 500, 3))) \%>\%
 gg_outlier_bin("x", 520, 480)
}
\seealso{
\url{https://edwinth.github.io/blog/outlier-bins/}
}
