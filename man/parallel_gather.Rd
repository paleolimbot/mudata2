% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parallel_gather}
\alias{parallel_gather}
\title{Melt multiple sets of columns in parallel}
\usage{
parallel_gather(x, key, ..., convert = FALSE, factor_key = FALSE)
}
\arguments{
\item{x}{A data.frame}

\item{key}{Column name to use to store variables, which are the column names
of the first gather operation.}

\item{...}{Named arguments in the form \code{new_col_name = c(old, col, names)}. All
named arguments must have the same length (i.e., gather the same number of columns).}

\item{convert}{Convert types (see \link[tidyr:gather]{gather})}

\item{factor_key}{Control whether the key column is a factor or character vector.}
}
\value{
A gathered data frame.
}
\description{
Essentially this is a wrapper around \link[tidyr:gather]{gather} that
is able to \link[dplyr:bind_cols]{bind_cols} with several gather operations. This is useful when a wide
data frame contains uncertainty or flag information in paired columns.
}
\examples{
# gather paired value/error columns using
# parallel_gather
parallel_gather(pocmajsum,
  key = "param",
  value = c(Ca, Ti, V),
  sd = c(Ca_sd, Ti_sd, V_sd)
)

# identical result using only tidyverse functions
library(dplyr)
library(tidyr)
gathered_values <- pocmajsum \%>\%
  select(core, depth, Ca, Ti, V) \%>\%
  gather(Ca, Ti, V,
    key = "param", value = "value"
  )
gathered_sds <- pocmajsum \%>\%
  select(core, depth, Ca_sd, Ti_sd, V_sd) \%>\%
  gather(Ca_sd, Ti_sd, V_sd,
    key = "param_sd", value = "sd"
  )

bind_cols(
  gathered_values,
  gathered_sds \%>\% select(sd)
)

}
\seealso{
\link[tidyr:gather]{gather}
}
