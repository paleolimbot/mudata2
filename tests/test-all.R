
library(testthat)

# expect identical mudata function
expect_equal_mudata <- function(md1, md2) {
  md1 <- dplyr::collect(md1)
  md2 <- dplyr::collect(md2)
  
  # expect names equal
  expect_true(setequal(names(md1), names(md2)))
  
  # expect attributes equal
  expect_identical(attributes(md1), attributes(md2))
  
  # expect all table values equal
  for(tbl_name in names(md1)) {
    tbl1 <- md1[[tbl_name]]
    tbl2 <- md2[[tbl_name]]
    expect_equal(colnames(tbl1), colnames(tbl2))
    for(col_name in colnames(tbl1)) {
      col1 <- tbl1[[col_name]]
      col2 <- tbl2[[col_name]]
      if(is.numeric(col1) && is.numeric(col2)) {
        expect_true(all(na.omit(dplyr::near(col1, col2))))
        expect_equal(is.na(col1), is.na(col2))
      } else {
        expect_equal(col1, col2)
      }
    }
  }
}

expect_silent(expect_equal_mudata(kentvillegreenwood, kentvillegreenwood))

test_check("mudata")