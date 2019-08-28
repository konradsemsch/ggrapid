
context("Testing functions from tweak file...")

test_that("first_to_lower function works...",{
  expect_identical(first_to_lower(c("Test")), "test")
  expect_identical(first_to_lower(c("Test1", "Test2")), c("test1", "test2"))
  t_in  <- tibble(Test1 = c(1, 2), Test2 = c(3, 4))
  t_out <- tibble(test1 = c(1, 2), test2 = c(3, 4))
  expect_identical(first_to_lower(t_in), t_out)
})

test_that("first_to_upper function works...",{
  expect_identical(first_to_upper(c("test")), "Test")
  expect_identical(first_to_upper(c("test1", "test2")), c("Test1", "Test2"))
  t_in  <- tibble(test1 = c(1, 2), test2 = c(3, 4))
  t_out <- tibble(Test1 = c(1, 2), Test2 = c(3, 4))
  expect_identical(first_to_upper(t_in), t_out)
})
