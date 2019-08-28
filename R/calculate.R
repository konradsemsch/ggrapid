
# Calculate decile table --------------------------------------------------

#' Calculate a decile breakdown
#'
#' This function calculates a decile table for any combination of numerical and categorical variables.
#'
#' @param df A data frame.
#' @param binning Variable for which binning should be applied.
#' @param grouping A two-level (binary) variable to calculate the ratio in each bin.
#' @param top_level Top level of the grouping variable. Defaults to "1".
#' @param n_bins Provide a number of bins. Defaults to 10.
#' @param format Should table printing be formatted with kable? Defaults to FALSE
#' @param ... Additional grouping columns to calculate deciles
#' @examples
#' library(tidyverse)
#'
#' diamonds_filter <- diamonds %>% filter(cut %in% c("Ideal", "Premium"))
#'
#' diamonds_filter %>%
#'   calculate_decile_table(price, cut, "Ideal")
#'
#' diamonds_filter %>%
#'   calculate_decile_table(binning = price,
#'                          grouping = cut,
#'                          top_level = "Ideal",
#'                          n_bins = 10,
#'                          format = FALSE,
#'                          color)
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom stats median
#' @export
calculate_decile_table <- function(df,
                                   binning,
                                   grouping,
                                   top_level = "1",
                                   n_bins = 10,
                                   format = FALSE,
                                   ...) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(top_level))
    stop("argument must be character")

  if (!is.numeric(n_bins))
    stop("argument must be numeric")

  var_binning  <- rlang::enquo(binning)
  var_grouping <- rlang::enquo(grouping)

  var_extra_grouping <- rlang::enquos(...)

  params <- list(na.rm = TRUE)

  df_out <- df %>%
    tidyr::drop_na(!!var_binning) %>%
    mutate(
      decile = as.factor(ntile(!!var_binning, n_bins)),
      grouping_chr = as.character(!!var_grouping)
    ) %>%
    group_by(!!!var_extra_grouping, .data$decile) %>%
    summarize(
      min          = round(min(!!var_binning, !!!params), 3),
      median       = round(median(!!var_binning, !!!params), 3),
      max          = round(max(!!var_binning, !!!params), 3),
      top_level    = sum(.data$grouping_chr == top_level),
      total        = n(),
      bottom_level = .data$total - .data$top_level,
      ratio        = .data$top_level / .data$total
    ) %>%
    ungroup() %>%
    mutate_at(vars(one_of(c("min", "median", "max"))), round, 2)

  if (format == TRUE) {
    df_out %<>%
      mutate_at(vars(.data$ratio), ~formattable::color_tile("white", "red")(.x))
  }

  return(df_out)

}
