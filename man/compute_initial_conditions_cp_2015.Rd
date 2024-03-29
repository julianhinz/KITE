% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_caliendo_parro_2015.R
\name{compute_initial_conditions_cp_2015}
\alias{compute_initial_conditions_cp_2015}
\title{Compute initial conditions for Caliendo and Parro (2015)-type model}
\usage{
compute_initial_conditions_cp_2015(input, settings)
}
\arguments{
\item{input}{List of prepared initial conditions.}

\item{settings}{List of various settings and model dimensions.}
}
\value{
List of data.tables with initial_conditions.
}
\description{
`compute_initial_conditions_cp_2015()` updates value added through wages to play nice with rest of the data.
}
