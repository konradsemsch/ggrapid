
# Create a decile plot ---------------------------------------------------

#' Plot decile plots of numerical variables
#'
#' This function creates nicely formatted, standardised decile plots. Prior to calling the function
#' the data should only be in a form of a decile table \code{\link{calculate_decile_table}}. Above
#' each bar the median value of the bin is displayed.
#'
#' @param df A data frame.
#' @param x A categorical variable for the x axis groupings. Defaults to 'decile'.
#' @param y A numerical variable for the y axis levels. Defaults to 'ratio'.
#' @param facet Select an additional faceting variable to create facets. Defaults to NULL.
#' @param ticks Select the number of ticks on the x and y axis with the \code{\link{number_ticks}} function. Defaults to 10.
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0.
#' @param title Should the plot title appear automatically. Defaults to TRUE.
#' @param subtitle Text that is displayed on the subtitle. Defaults to NULL.
#' @param caption Text that is displayed on the caption. Defaults to NULL.
#' @param lab_x Text that is displayed on the x axis. Defaults to "Decile".
#' @param lab_y Text that is displayed on the y axis. Defaults to "Percentage".
#' @param legend Should the plot legend appear automatically. Defaults to TRUE.
#' @param alpha Select plot fill transparency. Defaults to 0.7.
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 0.0\%.
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 1.0\%.
#' @param theme_type Select a theme type from themes available in the \code{\link{apply_theme}} function. Defaults to "ipsum".
#' @param palette Select a palette type with the \code{\link{select_palette}} function. Defaults to "awtools".
#' @examples
#' library(tidyverse)
#'
#' diamonds_filter <- diamonds %>% filter(cut %in% c("Ideal", "Premium"))
#'
#' diamonds_filter %>%
#'   calculate_decile_table(price, cut, "Ideal") %>%
#'   plot_deciles()
#'
#' diamonds_filter %>%
#'   calculate_decile_table(binning = price,
#'                          grouping = cut,
#'                          top_level = "Ideal",
#'                          n_bins = 10,
#'                          format = FALSE,
#'                          color) %>%
#'   plot_deciles(facet = color)
#' @importFrom ggplot2 geom_bar geom_text position_dodge scale_fill_gradientn
#' @importFrom rlang .data
#' @export
plot_deciles <- function(df,
                         x = decile,
                         y = ratio,
                         facet = NULL,
                         ticks = 10,
                         angle = 0,
                         title = TRUE,
                         subtitle = NULL,
                         caption = NULL,
                         lab_x = "Decile",
                         lab_y = "Ratio",
                         legend = TRUE,
                         alpha = .7,
                         quantile_low = 0,
                         quantile_high = 1,
                         theme_type = "ipsum",
                         palette = "awtools"
                         ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  var_x     <- rlang::enquo(x)
  var_y     <- rlang::enquo(y)
  var_facet <- rlang::enquo(facet)

  limits_min <- 0
  limits_max <- dplyr::select(df, !!var_y)[[1]] %>% max() + .05

  plot <- df %>%
    ggplot() +
    geom_bar(
      aes(
        x = decile,
        y = ratio,
        fill = ratio
      ),
      stat = "identity",
      width = .8,
      alpha = alpha
    ) +
    geom_text(
      aes(
        x = decile,
        y = ratio + 0.03,
        label = round(median, 2)
      ),
      position = position_dodge(.9),
      size = 3.2,
      check_overlap = TRUE
    ) +
    ggtitle(
      label = if (title == TRUE) {
        glue::glue("Decile plot of {rlang::quo_text(var_y)} by {rlang::quo_text(var_x)}")
      } else if (is.character(title)) {
        title
      } else {
        element_blank()
      }
    ) +
    labs(
      fill = "Ratio",
      x = lab_x,
      y = lab_y) +
    labs(
      subtitle = if (is.null(subtitle)) {element_blank()} else {subtitle}
    ) +
    labs(
      caption = if (is.null(caption)) {element_blank()} else {caption}
    ) +
    scale_y_continuous(
      limits = c(
        limits_min,
        limits_max
      ),
      labels = scales::percent,
      breaks = number_ticks(ticks)
    ) +
    apply_theme(theme_type) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    )

  if (!rlang::quo_is_null(var_facet)) {
    plot <- plot +
      facet_wrap(rlang::quo_text(var_facet), scales = "free_x")
  }

  plot +
    scale_fill_gradientn(colours = select_palette(palette))

}
