% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{gate_programmatic}
\alias{gate_programmatic}
\title{Programmatically gate data with pre-recorded lasso selection coordinates}
\usage{
gate_programmatic(x, y, programmatic_gates)
}
\arguments{
\item{x}{A vector representing the X dimension.}

\item{y}{A vector representing the Y dimension.}

\item{programmatic_gates}{A `data.frame` of the gate brush data, as saved in 
`tidygate_env$gates`. The column `x` records X coordinates, the column `y` records Y 
coordinates and the column `.gate` records the gate number.}
}
\value{
A vector of strings, of the gates each X and Y coordinate pair is within.
}
\description{
A helpful way to repeat previous interactive lasso selections to enable reproducibility. 
Programmatic gating is based on the package [gatepoints](https://github.com/wjawaid/gatepoints)
by Wajid Jawaid.
}
