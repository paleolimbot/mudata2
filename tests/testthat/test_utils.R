
context("utility functions")

test_that("parallel_gather produces the expected output", {

  # melt manually using gather() and bind_cols()
  gathered_values <- pocmajsum %>%
    dplyr::select(core, depth, Ca, Ti, V) %>%
    tidyr::gather(Ca, Ti, V, 
                  key = "param", value = "value")
  gathered_sds <- pocmajsum %>%
    dplyr::select(core, depth, Ca_sd, Ti_sd, V_sd) %>%
    tidyr::gather(Ca_sd, Ti_sd, V_sd, 
                  key = "param_sd", value = "sd")
  
  pocmajlong <- dplyr::bind_cols(
    gathered_values,
    gathered_sds %>% dplyr::select(sd)
  ) %>% tibble::as_tibble()
  
  
  # melt automatically using parallel_gather
  pocmaj_gathered <- parallel_gather(pocmajsum, key = "param", 
                                     value = c(Ca, Ti, V),
                                     sd = c(Ca_sd, Ti_sd, V_sd)) %>%
    tibble::as_tibble()
  
  # expect identical to manual parallel gather
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
  values <- c("Ca", "Ti", "V")
  sds <- c("Ca_sd", "Ti_sd", "V_sd")
  expect_identical(parallel_gather_(pm2, key = "param", value = values, sd = sds),
                   parallel_gather(pm2, key = "param", 
                                   value = c(Ca, Ti, V), sd = c(Ca_sd, Ti_sd, V_sd)))
})
