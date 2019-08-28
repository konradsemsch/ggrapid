
# Create a boxplot ---------------------------------------------------

#' Plot box-plots of numerical variables
#'
#' This function creates nicely formatted, standardized box-plots.
#'
#' @param df A data frame.
#' @param x A categorical variable for the x axis groupings.
#' @param y A numerical variable for the y axis levels.
#' @param fill Select an additional grouping variable to be used for plotting. Defaults to NULL.
#' @param facet Select an additional faceting variable to create facets. Defaults to NULL.
#' @param ticks Select the number of ticks on the x and y axis with the \code{\link{number_ticks}} function. Defaults to 10.
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0.
#' @param title Should the plot title appear automatically. Defaults to TRUE.
#' @param subtitle Text that is displayed on the subtitle. Defaults to NULL.
#' @param caption Text that is displayed on the caption. Defaults to NULL.
#' @param lab_x Text that is displayed on the x axis. Defaults to "Level".
#' @param lab_y Text that is displayed on the y axis. Defaults to "Value range".
#' @param legend Should the plot legend appear automatically. Defaults to TRUE.
#' @param vline Should any horizontal lines be added to the plot. Defaults to c(NaN).
#' @param alpha Select plot fill transparency. Defaults to 0.7.
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 0.0\%.
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 1.0\%.
#' @param theme_type Select a theme type from themes available in the \code{\link{apply_theme}} function. Defaults to "ipsum".
#' @param palette Select a palette type with the \code{\link{select_palette}} function. Defaults to "awtools".
#' @examples
#' diamonds %>%
#'   plot_boxplot(x = cut,
#'                y = carat)
#'
#' diamonds %>%
#'   plot_boxplot(x = cut,
#'                y = carat,
#'                fill = color)
#'
#' diamonds %>%
#'   plot_boxplot(x = cut,
#'                y = carat,
#'                fill = cut,
#'                facet = color)
#'
#' diamonds %>%
#'   plot_boxplot(x = cut,
#'                y = carat,
#'                fill = cut,
#'                title = "Write your title here",
#'                subtitle = "Write your subtitle here",
#'                caption = "Write your caption here",
#'                lab_x = "Carat",
#'                alpha = .5,
#'                vline = 1)
#' @importFrom ggplot2 geom_boxplot geom_hline
#' @export
plot_boxplot <- function(df,
                         x,
                         y,
                         fill = NULL,
                         facet = NULL,
                         ticks = 10,
                         angle = 0,
                         title = TRUE,
                         subtitle = NULL,
                         caption = NULL,
                         lab_x = "Level",
                         lab_y = "Value range",
                         legend = TRUE,
                         vline = c(NaN),
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
  var_fill  <- rlang::enquo(fill)
  var_facet <- rlang::enquo(facet)

  limits <- df %>%
    dplyr::select(value = !!var_y) %>%
    dplyr::summarise(
      min = stats::quantile(.data$value, quantile_low[[1]], na.rm = TRUE),
      max = stats::quantile(.data$value, quantile_high[[1]], na.rm = TRUE)
    )

  plot <- df %>%
    ggplot() +
    geom_hline(yintercept = vline, linetype = 2, size = 1, color = "#6E7B8B", alpha = .8) +
    ggtitle(
      label = if (title == TRUE) {
        glue::glue("Boxplot plot of {rlang::quo_text(var_y)} by {rlang::quo_text(var_x)}")
      } else if (is.character(title)) {
        title
      } else {
        element_blank()
      }
    ) +
    labs(
      fill = glue::glue("{first_to_upper(rlang::quo_text(var_fill))}:"),
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
        limits$min,
        limits$max
      ),
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

  if (rlang::quo_is_null(var_fill)) {

    plot +
      geom_boxplot(
        aes(
          x = {{ x }},
          y = {{ y }}
        ),
        alpha = alpha,
        fill = "#1d8fd2"
      )

  } else {

    plot +
      geom_boxplot(
        aes(
          x = {{ x }},
          y = {{ y }},
          fill = {{ fill }}
        ),
        alpha = alpha
      ) +
      scale_fill_manual(values = select_palette(palette))
  }
}
