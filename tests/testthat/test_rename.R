
context("renaming functions")

test_that("rename.cols function works", {
  df <- data.frame(a=1, b=2, c=3, d=4)
  expect_that(names(rename.cols(df, a="letter_a", b="letter_b")),
              equals(c("letter_a", "letter_b", "c", "d")))
})

test_that("rename.cols.qtag function renames id.vars/tags/values attributes", {
  data("pocmaj")
  pocmaj$tag <- "a tag!"
  qt <- as.qtag(pocmaj, id.vars=c("core", "depth"), tag.vars="tag")
  rn <- rename.cols(qt, core="thecore", Ca="Calcium", tag="newtag")
  expect_that(id.vars(rn), equals(c("thecore", "depth")))
  expect_that(measure.vars(rn), equals(c("Calcium", "Ti", "V")))
  expect_that(tag.vars(rn), equals("newtag"))
})

test_that("rename.cols outputs a message when no names are found", {
  df <- data.frame(a=1, b=2, c=3, d=4)
  expect_message(rename.cols(df, notacolumn="willnotbeacolumn"), 
                 "The following `from` values were not present in `x`: notacolumn")
})

test_that("rename.cols outputs a warning when duplicate names are created", {
  df <- data.frame(a=1, b=2, c=3, d=4)
  expect_warning(rename.cols(df, b="a"))
})

test_that("rename.values function works", {
  x <- c("fish", "fish", "fish", "whistle")
  expect_that(rename.values(x, fish="newfish"), equals(c("newfish", "newfish", "newfish", "whistle")))
  expect_that(rename.values(x, whistle="newwhistle"), equals(c("fish", "fish", "fish", "newwhistle")))
  expect_that(rename.values(x, fish="newfish", defaultValue="not a fish"), 
              equals(c("newfish", "newfish", "newfish", "not a fish")))
})

test_that("rename.values outputs a message if values are not found", {
  x <- c("fish", "fish", "fish", "whistle")
  expect_message(rename.values(x, notfound="willneverbefound"), 
                 "Not all values were found: notfound")
})

test_that("mudata rename works", {
  data("kentvillegreenwood")
  md2 <- rename.datasets(kentvillegreenwood, ecclimate="avalley")
  expect_that(unique(c(md2$data$dataset, md2$locations$dataset, 
                       md2$params$dataset, md2$datasets$dataset)), equals("avalley"))
  
  md2 <- rename.locations(kentvillegreenwood, "GREENWOOD A"="Greenwood")
  expect_true("Greenwood" %in% unique(c(md2$data$location, md2$locations$location)))
  expect_false("GREENWOOD A" %in% unique(c(md2$data$location, md2$locations$location)))
  
  md2 <- rename.params(kentvillegreenwood, maxtemp="Maximum Temperature")
  expect_true("Maximum Temperature" %in% unique(c(md2$data$param, md2$params$param)))
  expect_false("maxtemp" %in% unique(c(md2$data$param, md2$params$param)))
  
  md2 <- rename.cols(kentvillegreenwood, latitude="lat", longitude="lon")
  expect_true("lon" %in% names(md2$locations))
  expect_false("longitude" %in% names(md2$locations))
  expect_true("lat" %in% md2$columns$column)
  expect_false("latitude" %in% md2$columns$column)
})

