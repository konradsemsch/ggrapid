
# Create a density plot ---------------------------------------------------

#' Plot density of numerical variables
#'
#' This function creates nicely formatted, standardized density plots.
#'
#' @param df A data frame.
#' @param x A numerical variable to plot its density.
#' @param fill Select an additional grouping variable to be used for density plotting. Defaults to NULL.
#' @param facet Select an additional faceting variable to create facets. Defaults to NULL.
#' @param position Select the positional adjustment of the plot.  Possible values are: "identity" (default), "stack" or "fill"
#' @param ticks Select the number of ticks on the x and y axis with the \code{\link{number_ticks}} function. Defaults to 10.
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0.
#' @param title Should the plot title appear automatically. Defaults to TRUE.
#' @param subtitle Text that is displayed on the subtitle. Defaults to NULL.
#' @param caption Text that is displayed on the caption. Defaults to NULL.
#' @param lab_x Text that is displayed on the x axis. Defaults to "Value range".
#' @param lab_y Text that is displayed on the y axis. Defaults to "Density".
#' @param legend Should the plot legend appear automatically. Defaults to TRUE.
#' @param vline Should any vertical lines be added to the plot. Defaults to c(NaN).
#' @param alpha Select plot fill transparency. Defaults to 0.7.
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 0.0\%.
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 1.0\%.
#' @param theme_type Select a theme type from themes available in the \code{\link{apply_theme}} function. Defaults to "ipsum".
#' @param palette Select a palette type with the \code{\link{select_palette}} function. Defaults to "awtools".
#' @examples
#' diamonds %>%
#'   plot_density(x = carat)
#'
#' diamonds %>%
#'   plot_density(x = carat,
#'                fill = cut)
#'
#' diamonds %>%
#'   plot_density(x = carat,
#'                fill = cut,
#'                position = "stack")
#'
#' diamonds %>%
#'   plot_density(x = carat,
#'                fill = cut,
#'                position = "fill")
#'
#' diamonds %>%
#'   plot_density(x = carat,
#'                facet = cut)
#'
#' diamonds %>%
#'   plot_density(x = carat,
#'                fill = cut,
#'                facet = cut,
#'                alpha = .5)
#'
#' diamonds %>%
#'   plot_density(x = carat,
#'                fill = cut,
#'                facet = cut,
#'                title = "Write your title here",
#'                subtitle = "Write your subtitle here",
#'                caption = "Write your caption here",
#'                lab_x = "Carat",
#'                alpha = .5,
#'                vline = 1)
#' @importFrom ggplot2 ggplot geom_vline geom_density ggtitle labs theme
#' @importFrom ggplot2 ggtitle labs theme aes
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 element_blank element_text
#' @importFrom ggplot2 ggplot scale_x_continuous scale_y_continuous scale_fill_manual
#' @importFrom rlang .data
#' @export
plot_density <- function(df,
                         x,
                         fill = NULL,
                         facet = NULL,
                         position = "identity",
                         ticks = 5,
                         angle = 0,
                         title = TRUE,
                         subtitle = NULL,
                         caption = NULL,
                         lab_x = "Value range",
                         lab_y = "Density",
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
  var_fill  <- rlang::enquo(fill)
  var_facet <- rlang::enquo(facet)

  limits <- df %>%
    dplyr::select(value = !!var_x) %>%
    dplyr::summarise(
      min = stats::quantile(.data$value, quantile_low[[1]], na.rm = TRUE),
      max = stats::quantile(.data$value, quantile_high[[1]], na.rm = TRUE)
    )

  plot <- df %>%
    ggplot() +
    geom_vline(xintercept = vline, linetype = 2, size = 1, color = "#6E7B8B", alpha = .8) +
    ggtitle(
      label = if (title == TRUE) {
        glue::glue("Density plot of {rlang::quo_text(var_x)}")
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
    scale_x_continuous(
      limits = c(
        limits$min,
        limits$max
      ),
      breaks = number_ticks(ticks)
    ) +
    scale_y_continuous(
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
      geom_density(
        aes(
          x = {{ x }}
        ),
        position = position,
        alpha = alpha,
        fill = "#019875"
      )

  } else {

    plot +
      geom_density(
        aes(
          x = {{ x }},
          fill = {{ fill }}
        ),
        position = position,
        alpha = alpha
      ) +
      scale_fill_manual(values = select_palette(palette))
  }
}

