% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_quantogram.R
\name{calculate_quantogram}
\alias{calculate_quantogram}
\title{Calculate Kendall's cosine quantogram}
\usage{
calculate_quantogram(x, params = getOption("CONSTANTS_QUANTOGRAM"))
}
\arguments{
\item{x}{numeric vector of measurement values}

\item{params}{cosine quantogram parameters including "RNG_START", "RNG_END", "STEP", "Q_MIN"}
}
\description{
Calculate Kendall's cosine quantogram
}
