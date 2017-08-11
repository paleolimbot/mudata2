
context("type parsing")

test_that("bare types are parsed correctly", {
  expect_equal(parse_type("a_type")$type, "a_type")
  expect_equal(parse_type("a_type2")$type, "a_type2")
  
  expect_identical(parse_type("a_type")$args, list())
  expect_identical(parse_type("a_type2")$args, list())
})

test_that("invalid bare types are identified", {
  expect_error(parse_type("0type"), "Invalid type specification:.*")
  expect_error(parse_type("_type"), "Invalid type specification:.*")
  expect_error(parse_type("Type"), "Invalid type specification:.*")
  expect_error(parse_type("tYpe"), "Invalid type specification:.*")
})

test_that("types with zero arguments are parsed correctly", {
  expect_identical(parse_type("a_type()"), parse_type("a_type"))
  expect_identical(parse_type("a_type2()"), parse_type("a_type2"))
})

test_that("invalid types with zero arguments are identified", {
  expect_error(parse_type("0type()"), "Invalid type specification:.*")
  expect_error(parse_type("_type()"), "Invalid type specification:.*")
  expect_error(parse_type("Type()"), "Invalid type specification:.*")
  expect_error(parse_type("tYpe()"), "Invalid type specification:.*")
})

test_that("types with one argument are parsed correctly", {
  
  expect_identical(parse_type("type_name(key='value')")$args,
                   list(key = "value"))
  expect_identical(parse_type("type_name(key=\"value\")")$args,
                   list(key = "value"))
  expect_identical(parse_type("type_name(key=1)")$args,
                   list(key = 1))
  expect_identical(parse_type("type_name(key=1.3)")$args,
                   list(key = 1.3))
  expect_identical(parse_type("type_name(key=13.)")$args,
                   list(key = 13))
  expect_identical(parse_type("type_name(key=.13)")$args,
                   list(key = .13))
})

test_that("various whitespace is allowed in type specifications", {
  expect_identical(parse_type("type_name(key ='value')")$args,
                   list(key = "value"))
  expect_identical(parse_type("type_name( key=\"value\")")$args,
                   list(key = "value"))
  expect_identical(parse_type("type_name(key=1 )")$args,
                   list(key = 1))
  expect_identical(parse_type("type_name(key= 1.3)")$args,
                   list(key = 1.3))
  expect_identical(parse_type("type_name(key =13.)")$args,
                   list(key = 13))
  expect_identical(parse_type("type_name(key = .13)")$args,
                   list(key = .13))
})

test_that("types with invalid arguments are identified", {
  # type errors
  expect_error(parse_type("0type(key='value')"), "Invalid type specification:.*")
  expect_error(parse_type("_type(key='value')"), "Invalid type specification:.*")
  expect_error(parse_type("Type(key='value')"), "Invalid type specification:.*")
  expect_error(parse_type("tYpe(key='value')"), "Invalid type specification:.*")
  
  # argument errors
  expect_error(parse_type("type(key='value)"), "Invalid argument string:.*")
  expect_error(parse_type("type(key='value\")"), "Invalid argument string:.*")
  expect_error(parse_type("type(key='value', key2=)"), "Invalid argument string:.*")
  
  # whitespace errors
  expect_error(parse_type("type(key='value' key2='value2')"), "Invalid argument string:.*")
  expect_error(parse_type("type(key=5 key2='value2')"), "Invalid argument string:.*")
  expect_error(parse_type("type(key='value' key2=5)"), "Invalid argument string:.*")

})

test_that("whitespace/commas in argument strings is correctly handled", {
  expect_error(parse_type("type(,)"), "Invalid argument string:.*")
  expect_error(parse_type("type(,a)"), "Invalid argument string:.*")
  expect_error(parse_type("type(,fish = 'thing')"), "Invalid argument string:.*")
  expect_error(parse_type("type(fish = 'thing',)"), "Invalid argument string:.*")
  expect_error(parse_type("type(fish = 'thing' ,)"), "Invalid argument string:.*")
  expect_error(parse_type("type(fish = 'thing' , )"), "Invalid argument string:.*")
  
  expect_silent(parse_type("type(key='value', key2='value2' )"))
  expect_error(parse_type("type(key='value' key2='value2')"), "Invalid argument string:.*")
  expect_silent(parse_type("type(key=5, key2='value2' )"))
  expect_error(parse_type("type(key=5 key2='value2')"), "Invalid argument string:.*")
  expect_silent(parse_type("type(key='value', key2=5 )"))
  expect_error(parse_type("type(key='value' key2='value2')"), "Invalid argument string:.*")
})

test_that("multiple arguments are all extracted", {
  long_type <- "type_name(key='string value', key2=1234.6, key3=1234, key5 = \"dq string val\")"
  result <- parse_type(long_type)
  expect_equal(result$type, "type_name")
  expect_equal(result$args$key, "string value")
  expect_equal(result$args$key2, 1234.6)
  expect_equal(result$args$key3, 1234)
  expect_equal(result$args$key5, "dq string val")
})
