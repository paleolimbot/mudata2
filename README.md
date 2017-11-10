
<!-- README.md is generated from README.Rmd. Please edit that file -->
mudata2
=======

[![](http://cranlogs.r-pkg.org/badges/mudata2)](https://cran.r-project.org/package=mudata2) [![Travis-CI Build Status](https://travis-ci.org/paleolimbot/mudata.svg?branch=master)](https://travis-ci.org/paleolimbot/mudata) [![Coverage Status](https://img.shields.io/codecov/c/github/paleolimbot/mudata/master.svg)](https://codecov.io/github/paleolimbot/mudata?branch=master)

The **mudata2** package provides tools to read, write, and document multi-parameter spatiotemporal data.

Installation
------------

You can install **mudata2** from github with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/mudata")
```

What is mudata?
---------------

The mudata format is a data table in a specific form, with data dictionaries for locations, parameters, and datasets contained in the data table. The term "mudata" refers to a (mostly) universal data structure; the term "multi-parameter spatiotemporal data" is a mouthful, but just means that you measured a bunch of things (parameters) in a bunch of places (locations) at a bunch of different times. The best example of this is historical climate data, because it is usually set up in such a way that there are climate stations (locations) that measure some things (parameters, like temperature, precipitation, wind, etc.) at various points in time. This package is designed primarily for climate data and sediment core data, however the format can be applied to many other types of data where parameters are measured along one or more common axes (time, depth, etc.).

Why do I need it?
-----------------

There are plenty of ways to store "multi-parameter spatiotemporal data", but few of them are good at keeping metadata like what method was used to measure a parameter, the latitude and longitude of a sample location, or the level of uncertainty of a parameter measurement. These metadata are rarely used directly in analyses, but are invaluable to correctly interpret the results (and to correctly choose the analysis).

When do I need it?
------------------

We have used the mudata format in the following situations:

-   **Data from two or more sources need to be combined**: Rather than choose the format of one source or the other, we convert both sources to a mudata object, then combine them. The mudata format can handle almost any kind of data with minimal metadata loss, so not only is the data combined, but any contextual information provided with the data is kept. One of the projects that inspired continuing development on this package was collecting 10 years drinking water quality data from tens of communities around Atlantic Canada. Because no community ever provided the same information in the same format, we converted each community's data to mudata, then combined them prior to analysis.

-   **Data need to be archived**: One of the first uses of this package was to respond to a request along the lines of "send me all your data in addition to the report". We had collected tens of sediment cores, sectioned them in to many slices, and measured 30-40 elemental concentrations for each. We needed a way to report the data, how we measured the elemental concentrations, and from where we had collected the cores. The mudata format is able to do all of these things, with the added bonus that our clients could read our data from both R and Excel.

-   **Somebody sends you data in mudata format**: This probably only applies so far to the people to whom we have sent data, but if you happen to be one of them, you can use `read_mudata()`, the various subsetting functions (like `select_locations()`), and `tbl_data()`/`tbl_data_wide()` to extract the data you need and be on your way.

More information
----------------

For more examples of mudata usage, see the package vignettes: `vignette("mudata", package = "mudata2")`, and `vignette("mudata_create", package = "mudata2")`

Why mudata*2*?
--------------

The first mudata went on CRAN before I learned about [unit testing](https://github.com/r-lib/testthat), before I learned about the [tidyverse](https://www.tidyverse.org/), and before the journal article describing the format went through peer review. This led to important changes that couldn't be backward-compabible (but don't worry, all those files I sent you before September 2017 can still be read no problem).
