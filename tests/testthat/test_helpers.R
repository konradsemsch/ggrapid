
context("Testing functions from helpers file...")

test_that("number_tikcs function works...",{
  expect_true(is.function(number_ticks()))
})

test_that("apply_theme function works...",{
  expect_true(is.list(apply_theme()))
  expect_error(apply_theme("test"), "no other theme than 'ipsum' is currently supported")
})

test_that("select_palette function works...",{
  expect_true(is.vector(select_palette()))
  expect_error(select_palette("test"), "palette type not supported")
  expect_length(select_palette("awtools"), 8)
  expect_length(select_palette("berlin"), 60)
  expect_length(select_palette("lajolla"), 60)
})
