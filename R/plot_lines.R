
# Create a line plot ------------------------------------------------------

#' Plot lines of numerical variables. Usefull especially for time-series data
#'
#' This function creates nicely formatted, standardised line plots. Color and group parameters for geom_line and
#' geom_point are automatically inherited from the fill parameter.
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
#' @param lab_x Text that is displayed on the x axis. Defaults to "Value range".
#' @param lab_y Text that is displayed on the y axis. Defaults to "Value range".
#' @param legend Should the plot legend appear automatically. Defaults to TRUE.
#' @param hline Should any horizontal lines be added to the plot. Defaults to c(NaN).
#' @param alpha Select plot fill transparency. Defaults to 0.7.
#' @param limit_min Select lower limit for the y scale. Defaults to NA.
#' @param limit_max Select upper limit for the y scale. Defaults to NA.
#' @param theme_type Select a theme type from themes available in the \code{\link{apply_theme}} function. Defaults to "ipsum".
#' @param palette Select a palette type with the \code{\link{select_palette}} function. Defaults to "awtools".
#' @examples
#' library(tidyverse)
#'
#' tibble(
#'   time = 1:20,
#'   value = rnorm(20, 0.5, 2)
#'   ) %>%
#'   plot_line(
#'     x = time,
#'     y = value,
#'     ticks = 10,
#'     hline = 0.05
#'   )
#'
#' tibble(
#'   time = 1:20,
#'   value = rnorm(20, 0.5, 2)
#'   ) %>%
#'   plot_line(
#'     x = time,
#'     y = value,
#'     ticks = 10,
#'     hline = 0.05,
#'     limit_min = -2,
#'     limit_max = 2
#'   )
#' @import dplyr
#' @importFrom ggplot2 geom_line geom_point coord_cartesian
#' @importFrom rlang .data
#' @export
plot_line <- function(df,
                      x,
                      y,
                      fill = NULL,
                      facet = NULL,
                      ticks = 10,
                      angle = 0,
                      title = TRUE,
                      subtitle = NULL,
                      caption = NULL,
                      lab_x = "Value range",
                      lab_y = "Value range",
                      legend = TRUE,
                      hline = c(NaN),
                      alpha = 0.7,
                      limit_min = NA,
                      limit_max = NA,
                      theme_type = "ipsum",
                      palette = "awtools"
                      ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  var_x     <- rlang::enquo(x)
  var_y     <- rlang::enquo(y)
  var_fill  <- rlang::enquo(fill)
  var_facet <- rlang::enquo(facet)

  true_min <- min(select(df, !!var_y), na.rm = TRUE)
  true_max <- max(select(df, !!var_y), na.rm = TRUE)

  plot <- df %>%
    ggplot() +
    geom_hline(yintercept = hline, linetype = 2, size = 1, color = "#6E7B8B", alpha = .8) +
    ggtitle(
      label = if (title == TRUE) {
        glue::glue("{first_to_upper(rlang::quo_text(var_y))} by {rlang::quo_text(var_x)}")
      } else if (is.character(title)) {
        title
      } else {
        element_blank()
      }
    ) +
    labs(
      color = glue::glue("{first_to_upper(rlang::quo_text(var_fill))}:"),
      x = lab_x,
      y = lab_y) +
    labs(
      subtitle = if (is.null(subtitle)) {element_blank()} else {subtitle}
    ) +
    labs(
      caption = if (is.null(caption)) {element_blank()} else {caption}
    ) +
    coord_cartesian(
      ylim = c(
        ifelse(is.na(limit_min), true_min, limit_min),
        ifelse(is.na(limit_max), true_max, limit_max)
      )
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
      geom_line(
        aes(
          x = {{ x }},
          y = {{ y }},
          group = 1
        ),
        alpha = alpha,
        color = "#019875"
      ) +
      geom_point(
        aes(
          x = {{ x }},
          y = {{ y }},
        ),
        alpha = alpha,
      )

  } else {

    plot +
      geom_line(
        aes(
          x = {{ x }},
          y = {{ y }},
          group = {{ fill }},
          color = {{ fill }}
        ),
        alpha = alpha
      ) +
      geom_point(
        aes(
          x = {{ x }},
          y = {{ y }},
        ),
        alpha = alpha
      ) +
      scale_fill_manual(values = select_palette(palette))
  }
}
