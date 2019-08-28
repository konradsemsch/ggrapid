
# Number ticks ------------------------------------------------------------

#' Specify equal sized axis ticks
#'
#' This function creates equally spaced axis ticks for ggplot graphs. Should be used as input
#' for the "break" argument of scale_continuous function in a ggplot function.
#'
#' @param n Number of desired ticks on an axis. Defaults to 10.
number_ticks <- function(n = 10) {

  if (!is.numeric(n))
    stop("n must be a numeric input")

  function(limits) {
    pretty(limits, n)
  }

}

# Apply theme -------------------------------------------------------------

#' Apply a neat, customized ggplot2 theme
#'
#' This function applies a customized ggplot2 theme (based on the hrbrthemes package)
#' to any ggplot graph in order to create neat and good looking visualizations.
#'
#' @param theme_type Type of a base hrbrthemes theme to use. Currently only "ipsum" is supported.
#' @importFrom ggplot2 theme element_text rel %+replace%
#' @export
apply_theme <- function(theme_type = "ipsum"){

  if (!is.character(theme_type))
    stop("theme_type must be character")

  selected_theme <- if(theme_type == "ipsum"){
    hrbrthemes::theme_ipsum(
      # Adjusting the basic grid color
      grid_col = "#e6e6e6"
    )
  } else {
    stop("no other theme than 'ipsum' is currently supported")
  }

  selected_theme %+replace%
    theme(
      # Sizing adjustments of different plot elements
      plot.title    = element_text(size = rel(1.25)),
      plot.subtitle = element_text(size = rel(1.00)),
      strip.text    = element_text(size = rel(0.95)),
      legend.title  = element_text(size = rel(0.95))
    )

}

# Select palette ----------------------------------------------------------

#' Apply a neat, appealing pallete scheme
#'
#' Palettes are based on the list of available color schemes: \code{\link{https://github.com/EmilHvitfeldt/r-color-palettes}}.
#'
#' @param palette Select a palette. Available options for discrete palettes are: "awtools" (8 discrete colors)
#' and for continuous paletter: "berlin" or "lajolla" (60 continuous colors each). Defaults to "awtools".
#'
#' @export
select_palette <- function(palette = "awtools"){

  if (!is.character(palette))
    stop("argument must be character")

  if (!(palette %in% c("awtools", "binary", "inv_binary", "berlin", "lajolla")))
    stop("palette type not supported")

  ### Discrete palettes

  if (palette == "awtools") {

    paletteer::paletteer_d("awtools", "a_palette")

  } else if (palette == "binary") {

    c("#01b289", "#db685d")

  } else if (palette == "inv_binary") {

    c("#db685d", "#01b289")

  ### Continuous palettes

  } else if (palette == "berlin") {

    paletteer::paletteer_c("scico", "berlin", 60)

  } else if (palette == "lajolla") {

    paletteer::paletteer_c("scico", "lajolla", 60)


  } else {

    NULL

  }

}
