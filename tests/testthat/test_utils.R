
context("utility functions")

test_that("parallel_melt produces the expected data frame", {
  data(pocmajsum)
  # melt automatically
  pocmajlong <- parallel_melt(pocmajsum, id.vars=c("core", "depth"),
                              value=c("Ca", "Ti", "V"),
                              sd=c("Ca_sd", "Ti_sd", "V_sd"))
  expect_that(names(pocmajlong), equals(c("core", "depth", "param", "value", "sd")))
  
  # melt manually
  ca <- dplyr::rename(pocmajsum[c("core", "depth", "Ca", "Ca_sd")], value = Ca, sd = Ca_sd)
  ca$param <- "Ca"
  ti <- dplyr::rename(pocmajsum[c("core", "depth", "Ti", "Ti_sd")], value = Ti, sd = Ti_sd)
  ti$param <- "Ti"
  v <- dplyr::rename(pocmajsum[c("core", "depth", "V", "V_sd")], value = V, sd = V_sd)
  v$param <- "V"
  pocmajlongman <- rbind(ca, ti, v)[c("core", "depth", "param", "value", "sd")]
  
  expect_true(all(sapply(data.frame(pocmajlong == pocmajlongman), all, na.rm=TRUE)))
})

test_that("unnamed arguments are not allowed in parallel_melt", {
  expect_error(parallel_melt(data.rame(a=1, b=2), id.vars="a", "b"),
               "All arguments must be named")
})

test_that("parallel_gather produces the expected output", {

  # melt automatically using parallel_melt
  pocmajlong <- parallel_melt(pocmajsum, id.vars=c("core", "depth"),
                              value=c("Ca", "Ti", "V"),
                              sd=c("Ca_sd", "Ti_sd", "V_sd")) %>%
    tibble::as_tibble()
  expect_that(names(pocmajlong), equals(c("core", "depth", "param", "value", "sd")))
  
  # melt automatically using parallel_gather
  pocmaj_gathered <- parallel_gather(pocmajsum, key = "param", 
                                     value = c(Ca, Ti, V),
                                     sd = c(Ca_sd, Ti_sd, V_sd),
                                     factor_key = TRUE) %>%
    tibble::as_tibble()
  
  # expect identical to parallel_melt output
  expect_identical(pocmaj_gathered, pocmajlong)
})

test_that("parallel gather can select variables using dplyr expressions", {
  # expect various ways of selecting variables to work properly
  pm2 <- pocmajsum %>% dplyr::select(core, depth, Ca, Ti, V, dplyr::ends_with("sd"))
  expect_identical(parallel_gather(pm2, key = "param", 
                                   value = Ca:V, sd = dplyr::ends_with("sd")),
                   parallel_gather(pm2, key = "param", 
                                   value = c(Ca, Ti, V), sd = c(Ca_sd, Ti_sd, V_sd)))
})

test_that("parallel gather escape hatch returns correct results", {
  pm2 <- pocmajsum %>% dplyr::select(core, depth, Ca, Ti, V, dplyr::ends_with("sd"))
  expect_identical(parallel_gather_(pm2, key = "param", 
                                   value = c("Ca", "Ti", "V"), sd = c("Ca_sd", "Ti_sd", "V_sd")),
                   parallel_gather(pm2, key = "param", 
                                   value = c(Ca, Ti, V), sd = c(Ca_sd, Ti_sd, V_sd)))
})
