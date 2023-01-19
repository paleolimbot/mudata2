
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mudata2

<!-- badges: start -->

[![](https://cranlogs.r-pkg.org/badges/mudata2)](https://cran.r-project.org/package=mudata2)
[![DOI](https://img.shields.io/static/v1?label=DOI&message=10.1139%2Ffacets-2017-0026&color=blue)](https://doi.org/10.1139/facets-2017-0026)
[![R-CMD-check](https://github.com/paleolimbot/mudata2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paleolimbot/mudata2/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/mudata2/branch/master/graph/badge.svg)](https://app.codecov.io/gh/paleolimbot/mudata2?branch=master)
<!-- badges: end -->

The **mudata2** package provides tools to read, write, and document
multi-parameter spatiotemporal data.

## Installation

You can install **mudata2** from CRAN with:

``` r
install.packages("mudata2")
```

Or alternatively, you can the development version from github with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/mudata")
```

## What is mudata?

The [mudata
format](https://www.facetsjournal.com/doi/10.1139/facets-2017-0026) is a
data table in a specific form, with data dictionaries for locations,
parameters, and datasets contained in the data table. The term “mudata”
refers to a (mostly) universal data structure; the term “multi-parameter
spatiotemporal data” is a mouthful, but just means that you measured a
bunch of things (parameters) in a bunch of places (locations) at a bunch
of different times. The best example of this is historical climate data,
because it is usually set up in such a way that there are climate
stations (locations) that measure some things (parameters, like
temperature, precipitation, wind, etc.) at various points in time. This
package is designed primarily for climate data and sediment core data,
however the format can be applied to many other types of data where
parameters are measured along one or more common axes (time, depth,
etc.).

A few examples from this package can be found in the `ns_climate`,
`second_lake_temp`, and `alta_lake` datasets:

``` r
ns_climate
#> A mudata object aligned along "date"
#>   distinct_datasets():  "ecclimate_monthly"
#>   distinct_locations(): "ANNAPOLIS ROYAL 6289", "BADDECK 6297" ... and 13 more
#>   distinct_params():    "dir_of_max_gust", "extr_max_temp" ... and 9 more
#>   src_tbls():           "data", "locations" ... and 3 more
#> 
#> tbl_data() %>% head():
#> # A tibble: 6 × 7
#>   dataset           location          param       date       value flag  flag_…¹
#>   <chr>             <chr>             <chr>       <date>     <dbl> <chr> <chr>  
#> 1 ecclimate_monthly SABLE ISLAND 6454 mean_max_t… 1897-01-01    NA M     Missing
#> 2 ecclimate_monthly SABLE ISLAND 6454 mean_max_t… 1897-02-01    NA M     Missing
#> 3 ecclimate_monthly SABLE ISLAND 6454 mean_max_t… 1897-03-01    NA M     Missing
#> 4 ecclimate_monthly SABLE ISLAND 6454 mean_max_t… 1897-04-01    NA M     Missing
#> 5 ecclimate_monthly SABLE ISLAND 6454 mean_max_t… 1897-05-01    NA M     Missing
#> 6 ecclimate_monthly SABLE ISLAND 6454 mean_max_t… 1897-06-01    NA M     Missing
#> # … with abbreviated variable name ¹​flag_text
```

``` r
second_lake_temp
#> A mudata object aligned along "datetime", "depth"
#>   distinct_datasets():  "second_lake_temp"
#>   distinct_locations(): "Second Lake"
#>   distinct_params():    "temp"
#>   src_tbls():           "data", "locations" ... and 3 more
#> 
#> tbl_data() %>% head():
#> # A tibble: 6 × 6
#>   dataset          location    param datetime            depth value
#>   <chr>            <chr>       <chr> <dttm>              <dbl> <dbl>
#> 1 second_lake_temp Second Lake temp  2013-07-10 07:14:56     0  23.9
#> 2 second_lake_temp Second Lake temp  2013-07-10 08:14:56     0  23.8
#> 3 second_lake_temp Second Lake temp  2013-07-10 09:29:56     0  23.7
#> 4 second_lake_temp Second Lake temp  2013-07-10 09:44:56     0  23.8
#> 5 second_lake_temp Second Lake temp  2013-07-10 10:14:56     0  23.7
#> 6 second_lake_temp Second Lake temp  2013-07-10 11:29:56     0  23.9
```

``` r
alta_lake
#> A mudata object aligned along "depth", "age"
#>   distinct_datasets():  "alta_lake16"
#>   distinct_locations(): "ALGC2"
#>   distinct_params():    "As", "C", "C/N" ... and 11 more
#>   src_tbls():           "data", "locations" ... and 3 more
#> 
#> tbl_data() %>% head():
#> # A tibble: 6 × 10
#>   dataset     location param depth   age value stdev units     n zone  
#>   <chr>       <chr>    <chr> <dbl> <dbl> <dbl> <dbl> <chr> <int> <chr> 
#> 1 alta_lake16 ALGC2    As     0.25 2015.  23    NA   ppm       1 Zone 3
#> 2 alta_lake16 ALGC2    As     0.75 2011.  24.3  23.0 ppm       3 Zone 3
#> 3 alta_lake16 ALGC2    As     1.25 2008.  60    NA   ppm       1 Zone 3
#> 4 alta_lake16 ALGC2    As     1.75 2003.  60    NA   ppm       1 Zone 3
#> 5 alta_lake16 ALGC2    As     2.5  1998.  43.4  NA   ppm       1 Zone 3
#> 6 alta_lake16 ALGC2    As     3.5  1982.  42    NA   ppm       1 Zone 3
```

For examples of using and creating `mudata` objects, see
`vignette("mudata2", package = "mudata2")` and
`vignette("mudata_create", package = "mudata2")`.

## Why do I need it?

There are plenty of ways to store “multi-parameter spatiotemporal data”,
but few of them are good at keeping metadata like what method was used
to measure a parameter, the latitude and longitude of a sample location,
or the level of uncertainty of a parameter measurement. These metadata
are rarely used directly in analyses, but are invaluable to correctly
interpret the results (and to correctly choose the analysis).

## When do I need it?

We have used the mudata format in the following situations:

- **Data from two or more sources need to be combined**: Rather than
  choose the format of one source or the other, we convert both sources
  to a mudata object, then combine them. The mudata format can handle
  almost any kind of data with minimal metadata loss, so not only is the
  data combined, but any contextual information provided with the data
  is kept. One of the projects that inspired continuing development on
  this package was collecting 10 years drinking water quality data from
  tens of communities around Atlantic Canada. Because no community ever
  provided the same information in the same format, we converted each
  community’s data to mudata, then combined them prior to analysis.

- **Data need to be archived**: One of the first uses of this package
  was to respond to a request along the lines of “send me all your data
  in addition to the report”. We had collected tens of sediment cores,
  sectioned them in to many slices, and measured 30-40 elemental
  concentrations for each. We needed a way to report the data, how we
  measured the elemental concentrations, and from where we had collected
  the cores. The mudata format is able to do all of these things, with
  the added bonus that our clients could read our data from both R and
  Excel.

- **Somebody sends you data in mudata format**: This probably only
  applies so far to the people to whom we have sent data, but if you
  happen to be one of them, you can use `read_mudata()`, the various
  subsetting functions (like `select_locations()`), and
  `tbl_data()`/`tbl_data_wide()` to extract the data you need and be on
  your way.

## More information

For more examples of mudata usage, see the package vignettes:
`vignette("mudata2", package = "mudata2")` and
`vignette("mudata_create", package = "mudata2")`

## References

Dunnington DW and Spooner IS (2018). “Using a linked table-based
structure to encode self-describing multiparameter spatiotemporal data”.
FACETS.
[doi:10.1139/facets-2017-0026](https://doi.org/10.1139/facets-2017-0026)
