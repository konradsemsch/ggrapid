
context("Testing functions from calculate file...")

test_that("calculate_decile_table function works...",{

  diamonds_filter <- diamonds %>% filter(cut %in% c("Ideal", "Premium"))

  t_1 <- diamonds_filter %>%
    calculate_decile_table(price, cut, "Ideal")

  t_2 <- diamonds_filter %>%
    calculate_decile_table(binning = price,
                           grouping = cut,
                           top_level = "Ideal",
                           n_bins = 5)

  t_3 <- diamonds_filter %>%
    calculate_decile_table(binning = price,
                           grouping = cut,
                           top_level = "Ideal",
                           n_bins = 5,
                           format = FALSE,
                           color)

  expect_equal(nrow(t_1), 10)
  expect_equal(ncol(t_1), 8)
  expect_named(
    t_1,
    c("decile", "min", "median", "max", "top_level", "total", "bottom_level", "ratio")
    )

  expect_equal(nrow(t_2), 5)
  expect_equal(ncol(t_2), 8)
  expect_named(
    t_2,
    c("decile", "min", "median", "max", "top_level", "total", "bottom_level", "ratio")
  )

  expect_equal(nrow(t_3), 35)
  expect_equal(ncol(t_3), 9)
  expect_named(
    t_3,
    c("color", "decile", "min", "median", "max", "top_level", "total", "bottom_level", "ratio")
  )

})
