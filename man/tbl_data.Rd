% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mudata_helpers.R
\name{tbl_data}
\alias{tbl_data}
\alias{tbl_data.default}
\alias{tbl_data_wide}
\alias{tbl_data_wide.default}
\alias{tbl_params}
\alias{tbl_params.default}
\alias{tbl_locations}
\alias{tbl_locations.default}
\alias{tbl_datasets}
\alias{tbl_datasets.default}
\alias{tbl_columns}
\alias{tbl.mudata}
\alias{x_columns}
\alias{x_columns.default}
\title{Access components of a mudata object}
\usage{
tbl_data(x)

\method{tbl_data}{default}(x)

tbl_data_wide(x, ...)

\method{tbl_data_wide}{default}(x, key = "param", value = "value", ...)

tbl_params(x)

\method{tbl_params}{default}(x)

tbl_locations(x)

\method{tbl_locations}{default}(x)

tbl_datasets(x)

\method{tbl_datasets}{default}(x)

tbl_columns(x)

tbl_columns(x)

\method{tbl}{mudata}(src, which, ...)

x_columns(x)

\method{x_columns}{default}(x)
}
\arguments{
\item{x, src}{A mudata object}

\item{...}{Passed to other methods}

\item{key, value}{Passed to \link[tidyr:spread]{spread}}

\item{which}{Which tbl to extract}
}
\value{
The appropriate component
}
\description{
Access components of a mudata object
}
\examples{
tbl_data(kentvillegreenwood)
}
