
context("utility functions")

test_that("parallel.melt produces the expected data frame", {
  data(pocmajsum)
  # melt automatically
  pocmajlong <- parallel.melt(pocmajsum, id.vars=c("core", "depth"),
                              value=c("Ca", "Ti", "V"),
                              sd=c("Ca_sd", "Ti_sd", "V_sd"))
  expect_that(names(pocmajlong), equals(c("core", "depth", "param", "value", "sd")))
  
  # melt manually
  ca <- plyr::rename(pocmajsum[c("core", "depth", "Ca", "Ca_sd")], c("Ca"="value", "Ca_sd"="sd"))
  ca$param <- "Ca"
  ti <- plyr::rename(pocmajsum[c("core", "depth", "Ti", "Ti_sd")], c("Ti"="value", "Ti_sd"="sd"))
  ti$param <- "Ti"
  v <- plyr::rename(pocmajsum[c("core", "depth", "V", "V_sd")], c("V"="value", "V_sd"="sd"))
  v$param <- "V"
  pocmajlongman <- rbind(ca, ti, v)[c("core", "depth", "param", "value", "sd")]
  
  expect_true(all(sapply(data.frame(pocmajlong == pocmajlongman), all, na.rm=TRUE)))
})

test_that("unnamed arguments are not allowed in parallel.melt", {
  expect_error(parallel.melt(data.rame(a=1, b=2), id.vars="a", "b"),
               "All arguments must be named")
})

test_that("numeric variables are correctly identified", {
  df <- data.frame(a=factor("a factor"), b="not a factor", c=4,
                   d=4.5, e=Sys.Date(), f=Sys.time())
  expect_that(names(df)[sapply(df, is.numericish)], equals(c("c", "d", "e", "f")))
  expect_that(!sapply(df, is.numericish), equals(sapply(df, ggplot2:::is.discrete)))
})
