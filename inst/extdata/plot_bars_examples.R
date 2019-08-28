#' library(dplyr)
#'
#' diamonds_sum <- diamonds %>%
#'   group_by(cut) %>%
#'   summarise(mean_carat = mean(carat, na.rm = TRUE))
#'
#' diamonds %>%
#'   plot_bars(x = carat,
#'             x_type = "num",
#'             fill = cut,
#'             facet = color)
#'
#' # Not fully working
#' diamonds %>%
#'   plot_bars(x = carat,
#'             x_type = "num",
#'             fill = cut,
#'             position = "stack",
#'             facet = color,
#'             # binwidth = 50,
#'             vline = 45,
#'             angle = 45,
#'             alpha = .7)
#'
#' # for generating counts
#' diamonds %>%
#'   plot_bars(x = cut,
#'             x_type = "char",
#'             y_prop = FALSE)
#'
#' diamonds %>%
#'   plot_bars(x = cut,
#'             x_type = "char",
#'             position = "dodge",
#'             fill = cut,
#'             facet = color)
#'
#' # for generating proportions
#' diamonds %>%
#'   plot_bars(x = cut,
#'             x_type = "char",
#'             y_prop = TRUE,
#'             position = "fill",
#'             fill = color)
#'
#' diamonds_sum %>%
#'   plot_bars(x = cut,
#'             y = mean_carat,
#'             x_type = "char",
#'             stat = "identity")