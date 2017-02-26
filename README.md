R package 'mudata'
================
Dewey Dunnington
February 26, 2017

The 'mudata' package for R is a set of tools to create, manipulate, and visualize multi-parameter, spatiotemporal data. Data of this type includes all data where multiple parameters (e.g. wind speed, precipitation, temperature) are measured along a common axis (e.g. time, depth) at discrete locations (e.g. climate stations). These data include long-term climate data collected from climate stations, paleolimnological data, ice core data, and ocean core data among many others. Data of this type is often voluminous and difficult to organize given its multi-dimensional nature. The (mostly) universal data (mudata) format is an attempt to organize these data in a common way to facilitate their documentation and comparison.

Installation
------------

The mudata package can be installed using `devtools` and [github](http://github.com/paleolimbot/mudata):

``` r
# install.packages("devtools") # if devtools is not installed
devtools::install_github("paleolimbot/mudata")
```

The MUData format
-----------------

The (mostly) universal data format is a collection of five (or more) tables, one of which contains the data in a molten form (see [reshape2](https://cran.r-project.org/package=reshape2)). The easiest way to visualize a mudata object is to inspect the `kentvillegreenwood` dataset within the package. This object is a collection of daily observations from Kentville, Nova Scotia, and Greenwood, Nova Scotia from July and August, 1999.

``` r
library(mudata)
data("kentvillegreenwood")
plot(kentvillegreenwood)
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

The default `plot()` function for a `mudata` object uses [ggplot2](https://cran.r-project.org/package=ggplot2) to produce a faceted plot comparing locations and datasets by parameter. However, if we take a look at the structure of the object, we can see that it is just a `list` of 5 `data.frame` objects.

``` r
str(kentvillegreenwood)
```

    ## List of 5
    ##  $ data     :'data.frame':   1364 obs. of  7 variables:
    ##   ..$ dataset    : chr [1:1364] "ecclimate" "ecclimate" "ecclimate" "ecclimate" ...
    ##   ..$ location   : chr [1:1364] "KENTVILLE CDA CS" "KENTVILLE CDA CS" "KENTVILLE CDA CS" "KENTVILLE CDA CS" ...
    ##   ..$ param      : chr [1:1364] "maxtemp" "maxtemp" "maxtemp" "maxtemp" ...
    ##   ..$ x          : POSIXct[1:1364], format: "1999-07-01" ...
    ##   ..$ value      : num [1:1364] 28.5 30.7 26.4 28.6 26 25.3 28.6 23.9 22.8 20.9 ...
    ##   ..$ dataquality: logi [1:1364] NA NA NA NA NA NA ...
    ##   ..$ flags      : chr [1:1364] "" "" "" "" ...
    ##  $ locations:'data.frame':   2 obs. of  21 variables:
    ##   ..$ dataset     : chr [1:2] "ecclimate" "ecclimate"
    ##   ..$ location    : chr [1:2] "KENTVILLE CDA CS" "GREENWOOD A"
    ##   ..$ name        : chr [1:2] "KENTVILLE CDA CS" "GREENWOOD A"
    ##   ..$ province    : chr [1:2] "NOVA SCOTIA" "NOVA SCOTIA"
    ##   ..$ climateid   : chr [1:2] "8202810" "8202000"
    ##   ..$ stationid   : int [1:2] 27141 6354
    ##   ..$ wmoid       : int [1:2] 71671 71397
    ##   ..$ tcid        : chr [1:2] "XKT" "YZX"
    ##   ..$ latitude    : num [1:2] 45.1 45
    ##   ..$ longitude   : num [1:2] -64.5 -64.9
    ##   ..$ latitude.1  : num [1:2] 45.1 45
    ##   ..$ longitude.1 : num [1:2] -64.5 -64.9
    ##   ..$ elevation   : num [1:2] 48.7 28
    ##   ..$ firstyear   : int [1:2] 1996 1942
    ##   ..$ lastyear    : int [1:2] 2016 2016
    ##   ..$ hlyfirstyear: int [1:2] 1999 1953
    ##   ..$ hlylastyear : int [1:2] 2016 2016
    ##   ..$ dlyfirstyear: int [1:2] 1996 1942
    ##   ..$ dlylastyear : int [1:2] 2016 2016
    ##   ..$ mlyfirstyear: int [1:2] 1996 1942
    ##   ..$ mlylastyear : int [1:2] 2007 2013
    ##  $ params   :'data.frame':   11 obs. of  3 variables:
    ##   ..$ dataset: chr [1:11] "ecclimate" "ecclimate" "ecclimate" "ecclimate" ...
    ##   ..$ param  : chr [1:11] "maxtemp" "mintemp" "meantemp" "heatdegdays" ...
    ##   ..$ label  : chr [1:11] "Max Temp (C)" "Min Temp (C)" "Mean Temp (C)" "Heat Deg Days (C)" ...
    ##  $ datasets :'data.frame':   1 obs. of  1 variable:
    ##   ..$ dataset: chr "ecclimate"
    ##  $ columns  :'data.frame':   23 obs. of  4 variables:
    ##   ..$ dataset: chr [1:23] "ecclimate" "ecclimate" "ecclimate" "ecclimate" ...
    ##   ..$ table  : chr [1:23] "data" "data" "data" "locations" ...
    ##   ..$ column : chr [1:23] "x" "value" "flags" "name" ...
    ##   ..$ type   : chr [1:23] "POSIXct" "numeric" "character" "character" ...
    ##  - attr(*, "class")= chr [1:2] "mudata" "list"

The most important of these is the `$data` table, which contains the actual parameter measurements. All of the other tables are ways to document the information contained in the data table, such as the coordinates of each location (in this case there is also quite a bit more information about each location, such as the various identifiers used by climate organizations), or the units in which each parameter is measured. The first few rows of this table are as follows:

| dataset   | location         | param   | x          |  value| dataquality | flags |
|:----------|:-----------------|:--------|:-----------|------:|:------------|:------|
| ecclimate | KENTVILLE CDA CS | maxtemp | 1999-07-15 |   28.4| NA          |       |
| ecclimate | KENTVILLE CDA CS | maxtemp | 1999-07-16 |   30.9| NA          |       |
| ecclimate | KENTVILLE CDA CS | maxtemp | 1999-07-17 |   34.6| NA          |       |
| ecclimate | KENTVILLE CDA CS | maxtemp | 1999-07-18 |   32.8| NA          |       |
| ecclimate | KENTVILLE CDA CS | maxtemp | 1999-07-19 |   28.6| NA          |       |
| ecclimate | KENTVILLE CDA CS | maxtemp | 1999-07-20 |   23.3| NA          |       |

The package contains functions to `plot()` the data (using the [ggplot2](https://cran.r-project.org/package=ggplot2) framework), produce a `summary()` of the data, `print()` a short description of the object, and `subset()` the object. Combining objects can be accomplished by `rbind()`-ing two `mudata` objects.

``` r
print(kentvillegreenwood)
```

    ## A mudata object with 1 dataset(s), 2 location(s), 11 param(s), and 1364 data points
    ## ... datasets: ecclimate
    ## ... locations: KENTVILLE CDA CS, GREENWOOD A
    ## ... x: from 1999-07-01 to 1999-08-31
    ## ... params:
    ## ... ...  cooldegdays  from   0.0  to   8.6
    ## ... ...  dirofmaxgust  from   1.0  to  33.0
    ## ... ...  heatdegdays  from   0.0  to   3.7
    ## ... ...  maxtemp  from  18.2  to  34.6
    ## ... ...  meantemp  from  14.3  to  26.6
    ## ... ...  mintemp  from   7.3  to  21.8
    ## ... ...  snowongrnd  from   0.0  to   0.0
    ## ... ...  spdofmaxgust  from  32.0  to  74.0
    ## ... ...  totalprecip  from   0.0  to  65.1
    ## ... ...  totalrain  from   0.0  to  64.7
    ## ... ...  totalsnow  from   0.0  to   0.0

``` r
head(summary(kentvillegreenwood, digits=2))
```

    ##     dataset    location        param  Min Median  Mean  Max  n NAs
    ## 1 ecclimate GREENWOOD A  cooldegdays  0.0   1.65  2.31  8.2 62   0
    ## 2 ecclimate GREENWOOD A dirofmaxgust  1.0  23.00 22.76 33.0 62  33
    ## 3 ecclimate GREENWOOD A  heatdegdays  0.0   0.00  0.45  3.7 62   0
    ## 4 ecclimate GREENWOOD A      maxtemp 18.8  26.65 26.13 34.4 62   0
    ## 5 ecclimate GREENWOOD A     meantemp 14.3  19.65 19.86 26.2 62   0
    ## 6 ecclimate GREENWOOD A      mintemp  7.3  13.55 13.54 21.8 62   0

``` r
kvtemp <- subset(kentvillegreenwood, params=c("mintemp", "maxtemp", "meantemp"))
kvtemp
```

    ## A mudata object with 1 dataset(s), 2 location(s), 3 param(s), and 372 data points
    ## ... datasets: ecclimate
    ## ... locations: KENTVILLE CDA CS, GREENWOOD A
    ## ... x: from 1999-07-01 to 1999-08-31
    ## ... params:
    ## ... ...  mintemp  from   7.3  to  21.8
    ## ... ...  maxtemp  from  18.2  to  34.6
    ## ... ...  meantemp  from  14.3  to  26.6

``` r
kvprecip <- subset(kentvillegreenwood, params="totalprecip")
rbind(kvtemp, kvprecip)
```

    ## A mudata object with 1 dataset(s), 2 location(s), 4 param(s), and 496 data points
    ## ... datasets: ecclimate
    ## ... locations: KENTVILLE CDA CS, GREENWOOD A
    ## ... x: from 1999-07-01 to 1999-08-31
    ## ... params:
    ## ... ...  maxtemp  from  18.2  to  34.6
    ## ... ...  meantemp  from  14.3  to  26.6
    ## ... ...  mintemp  from   7.3  to  21.8
    ## ... ...  totalprecip  from   0.0  to  65.1

``` r
plot(kvtemp)
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
biplot(kvtemp, col="location")
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

Creating a MUData object
------------------------

Creating a `mudata` object from your own data is slightly more difficult, but the package contains a few functions to (hopefully) make things a bit easier. The key is creating the `$data` table in the correct format; the rest can be handled automatically by the `mudata()` constructor.

### Case 1: Wide, summarised data

Probably the most common case, wide, summarised data is the norm in most disciplines. If you've gotten this far, there is a good chance that you have data like this hanging around somewhere:

``` r
data("pocmajsum")
pocmajwide <- pocmajsum[c("core", "depth", "Ca", "V", "Ti")]
```

| core  |  depth|    Ca|    V|    Ti|
|:------|------:|-----:|----:|-----:|
| MAJ-1 |      0|  1885|   78|  2370|
| MAJ-1 |      1|  1418|   70|  2409|
| MAJ-1 |      2|  1550|   70|  2376|
| MAJ-1 |      3|  1448|   64|  2485|
| MAJ-1 |      4|  1247|   57|  2414|
| MAJ-1 |      5|  1412|   81|  1897|
| POC-2 |      0|  1622|   33|  2038|
| POC-2 |      1|  1488|   36|  2016|
| POC-2 |      2|  2416|   79|  3270|
| POC-2 |      3|  2253|   79|  3197|
| POC-2 |      4|  2372|   87|  3536|
| POC-2 |      5|  2635|   87|  3890|

This is a small subset of paleolimnological data for two sediment cores near Halifax, Nova Scotia. The data is a multi-parameter spatiotemporal dataset because it contains multiple parameters (calcium, titanium, and vanadium concentrations) measured along a common axis (depth in the sediment core) at discrete locations (cores named MAJ-1 and POC-2). Currently, our columns are not named properly: for the `mudata` format the terminology is 'location' and 'x', not 'core' and 'depth'. The `rename.cols()` function is intended to make this job simple (this is just a simple wrapper around the `rename()` function in the [plyr](https://cran.r-project.org/package=plyr) package, which does all of the work).

``` r
pocmajwide <- rename.cols(pocmajwide, "core"="location", "depth"="x")
```

Finally, we need to get the data into a "long" format, with a column named "param" and our actual values in a single column (called "value", predictably). This can be done using the `melt()` function in the [reshape2](https://cran.r-project.org/package=reshape2) package.

``` r
library(reshape2)
pocmajlong <- melt(pocmajwide, id.vars=c("location", "x"), variable.name = "param",
                   value.name="value")
```

The (first six rows of the) data now look like this:

| location |    x| param |  value|
|:---------|----:|:------|------:|
| MAJ-1    |    0| Ca    |   1885|
| MAJ-1    |    1| Ca    |   1418|
| MAJ-1    |    2| Ca    |   1550|
| MAJ-1    |    3| Ca    |   1448|
| MAJ-1    |    4| Ca    |   1247|
| MAJ-1    |    5| Ca    |   1412|

Now the data is ready to be put into the `mudata()` constructor. If it isn't, the constructor will throw an error (hopefully) telling you how to fix the data.

``` r
md <- mudata(pocmajlong)
plot(md, yvar="x")
```

![](README_files/figure-markdown_github/unnamed-chunk-16-1.png)

### Case 2: Wide, summarised data with uncertainty

Data is often output in a format similar to the format above, but with uncertainty information in paired columns. Data from an ICP-MS, for example is often in this format, with the concentration and a +/- column next to it. One of the advantages of a long format is the ability to include this information in a way that makes plotting with error bars easy. The `pocmajsum` dataset is a version of the dataset described above, but with standard deviation values in paired columns with the value itself.

``` r
data("pocmajsum")
```

| location |    x|    Ca|    V|    Ti|
|:---------|----:|-----:|----:|-----:|
| MAJ-1    |    0|  1885|   78|  2370|
| MAJ-1    |    1|  1418|   70|  2409|
| MAJ-1    |    2|  1550|   70|  2376|
| MAJ-1    |    3|  1448|   64|  2485|
| MAJ-1    |    4|  1247|   57|  2414|
| MAJ-1    |    5|  1412|   81|  1897|
| POC-2    |    0|  1622|   33|  2038|
| POC-2    |    1|  1488|   36|  2016|
| POC-2    |    2|  2416|   79|  3270|
| POC-2    |    3|  2253|   79|  3197|
| POC-2    |    4|  2372|   87|  3536|
| POC-2    |    5|  2635|   87|  3890|

As above, we need to rename the "core" and "depth" columns to "location" and "x" using the `rename.cols()` function.

``` r
pocmajwide <- rename.cols(pocmajsum, "core"="location", "depth"="x")
```

Then (also as above), we need to "melt" the data to get it into long form. Because we have paired columns, this is handled by a different function (from the mudata package) called `parallel.melt()`.

``` r
pocmajlong <- parallel.melt(pocmajwide, id.vars=c("location", "x"), 
                            value=c("Ca", "Ti", "V"), 
                            sd=c("Ca_sd", "Ti_sd", "V_sd"),
                            variable.name = "param")
```

| location |    x| param |  value|   sd|
|:---------|----:|:------|------:|----:|
| MAJ-1    |    0| Ca    |   1885|  452|
| MAJ-1    |    1| Ca    |   1418|   NA|
| MAJ-1    |    2| Ca    |   1550|   NA|
| MAJ-1    |    3| Ca    |   1448|   NA|
| MAJ-1    |    4| Ca    |   1247|   NA|
| MAJ-1    |    5| Ca    |   1412|  126|

The data is now ready to be fed to the `mudata()` constructor, after which we can use the `plot()` function to add error bars based on the `sd` column.

``` r
md <- mudata(pocmajlong)
plot(md, yvar="x", errors="sd")
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)

And more!
---------

There is much more to the `mudata()` package, which we hope will be useful both in its current form and as the project evolves to fit the needs of its users.
