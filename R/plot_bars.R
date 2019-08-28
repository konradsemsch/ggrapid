# Create a barplot ---------------------------------------------------

#' Plot bars of numerical variables
#'
#' This function creates nicely formatted, standardized bar-plots.
#'
#' @param df A data frame.
#' @param x A numeric/ categorical variable to plot the bar plot.
#' @param y A numeric variable which contains summarised y values (before plotting). Use only with stat = "identity".
#' @param y_prop A logical variable to choose between counts/ proportion on the y axis. Defaults to FALSE (proportion).
#' @param x_type Character identifier for type of the variable x defined above: "num" for numeric (plots histogram) and "char" for character (plots bar chart). Defauls to "num".
#' @param fill Select an additional grouping variable to be used for plotting. Defaults to NULL.
#' @param facet Select an additional faceting variable to create facets. Defaults to NULL.
#' @param binwidth Select binwidth, defaults to NULL and lets ggplot select the optimal binwidth.
#' @param position Select the position of the barplot from: "stack" (default), "dodge" or "fill".
#' @param stat Character identifier for whether the data is already grouped ("identity") or if the function needs to aggregate data at the level of x ("count").
#' @param fct_order Should the factors be reordered by their frequency? Defaults to FALSE.
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
#' library(tidyverse)
#'
#' diamonds_sum <- diamonds %>%
#'   group_by(cut) %>%
#'   summarise(mean_carat = mean(carat, na.rm = TRUE))
#'
#' @import dplyr
#' @importFrom ggplot2 geom_histogram xlim
#' @importFrom rlang .data :=
#' @export
plot_bars <- function(df,
                      x,
                      y = NULL,
                      y_prop = FALSE,
                      x_type = "num",
                      fill = NULL,
                      facet = NULL,
                      binwidth = NULL,
                      position = "stack",
                      stat = "count",
                      fct_order = FALSE,
                      angle = 0,
                      title = TRUE,
                      subtitle = NULL,
                      caption = NULL,
                      lab_x = "Value range",
                      lab_y = "Proportion",
                      legend = TRUE,
                      vline = c(NaN),
                      alpha = 0.7,
                      quantile_low = 0,
                      quantile_high = 1,
                      theme_type = "ipsum",
                      palette = "awtools"
                      ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(palette))
    stop("argument must be character")

  var_x     <- rlang::enquo(x)
  var_fill  <- rlang::enquo(fill)
  var_facet <- rlang::enquo(facet)
  var_y     <- rlang::enquo(y)

  if (x_type == "num") {

    plot <- df %>%
      ggplot() +
      geom_vline(xintercept = vline, linetype = 2, size = 1, color = "#6E7B8B", alpha = .8)

    limits <- df %>%
      select(value = !!var_x) %>%
      summarise(
        min = stats::quantile(.data$value, quantile_low[[1]], na.rm = TRUE),
        max = stats::quantile(.data$value, quantile_high[[1]], na.rm = TRUE)
      )

    if (rlang::quo_is_null(var_fill)) {

      plot <- plot +
        geom_histogram(
          aes(
            x = {{ x }}
          ),
          alpha = alpha,
          position = position,
          fill = "#1d8fd2",
          binwidth  = binwidth
        ) +
        xlim(limits$min, limits$max)

    } else {

      plot <- plot +
        geom_histogram(
          aes(
            x = {{ x }},
            fill = {{ fill }}
          ),
          alpha = alpha,
          position = position,
          binwidth = binwidth
        ) +
        xlim(limits$min, limits$max) +
        scale_fill_manual(values = select_palette(palette))

    }
  }

  else if (x_type == "char") {

    var_name <- rlang::quo_name(var_x)

    if (fct_order == TRUE){

      df <- df %>%
        mutate(!!var_name := as.factor(!!var_x) %>%
                 forcats::fct_infreq() %>%
                 forcats::fct_rev())
    } else {

      df <- df %>%
        mutate(!!var_name := as.factor(!!var_x))
    }

    if (rlang::quo_is_null(var_y)) {

      if (y_prop){

        plot <- df %>%
          ggplot(aes(y = stat(count / sum(count))))

      } else {

        plot <- df %>%
          ggplot(aes(y = stat(count)))

      }

    } else {

      if (y_prop) {

        df_tmp <- df %>%
          mutate(prop = (!!var_y)/sum(!!var_y))

        plot <- df_tmp %>%
          ggplot(aes(y = .data$prop))

      } else {

        plot <- df %>%
          ggplot(aes(y = {{ y }}))

      }
    }

    if (rlang::quo_is_null(var_fill)) {

      plot <- plot +
        geom_bar(
          aes({{ x }}),
          alpha = alpha,
          stat = stat,
          fill = "#019875",
          position = position)

    } else {

      plot <- plot +
        geom_bar(
          aes(
            x = {{ x }},
            fill = {{ fill }}
          ),
          alpha = alpha,
          stat = stat,
          position = position
        ) +
        scale_fill_manual(values = select_palette(palette))
    }

    if (y_prop) {
      plot <- plot +
        scale_y_continuous(labels = scales::percent_format())
    }
  }

  if (!rlang::quo_is_null(var_facet)) {
    plot <- plot +
      facet_wrap(rlang::quo_text(var_facet), scales = "free_x")
  }

  if (!y_prop & lab_y == "Proportion") lab_y = "Count"

  plot +
    ggtitle(
      label = if (title == TRUE) {
        glue::glue("Bar plot of {rlang::quo_text(var_x)}")
      } else if (is.character(title)) {
        title
      } else {
        element_blank()
      }
    ) +
    labs(
      fill = glue::glue("{aider::first_to_upper(rlang::quo_text(var_fill))}:"),
      x = lab_x,
      y = lab_y
    ) +
    labs(
      subtitle = if (is.null(subtitle)) {element_blank()} else {subtitle}
    ) +
    labs(
      caption = if (is.null(caption)) {element_blank()} else {caption}
    ) +
    apply_theme(theme_type) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    )
}
