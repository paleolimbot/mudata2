% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mudata_helpers.R
\name{mutate_data}
\alias{mutate_data}
\alias{mutate_params}
\alias{mutate_locations}
\alias{mutate_datasets}
\alias{mutate_columns}
\alias{mutate_tbl}
\alias{mutate_tbl.default}
\title{Modify mudata tables}
\usage{
mutate_data(x, ...)

mutate_params(x, ...)

mutate_locations(x, ...)

mutate_datasets(x, ...)

mutate_columns(x, ...)

mutate_tbl(x, ...)

\method{mutate_tbl}{default}(x, tbl, ...)
}
\arguments{
\item{x}{A mudata object}

\item{...}{Passed to \link[dplyr:mutate]{mutate}}

\item{tbl}{The table name to modify}
}
\value{
A modified mudata object
}
\description{
Modify mudata tables
}
\examples{
library(lubridate)
second_lake_temp \%>\%
  mutate_data(datetime = with_tz(datetime, "America/Halifax"))

}
