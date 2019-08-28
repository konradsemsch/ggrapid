
# Create a correlation matrix ---------------------------------------------

#' Plot a correlation matrix of numerical variables
#'
#' This function creates a nicely formatted, standardised correlation matrix of numerical variables.
#' Make sure that long variables names are are shortened before using the function for easier interpretation.
#'
#' @param df A data frame.
#' @param method A character string indicating which correlation coefficient (or covariance) should be computed. Options are: "spearman" (default), "pearson" or "kendall".
#' @param order Ordering method of the correlation matrix. Options are: "alphabet" (default) and "hclust".
#' @param label_size Size of the text label. Defaults to 0.7.
#' @param number_size Size of the correlation number. Defaults to 0.9.
#' @examples
#' diamonds %>%
#'   plot_correlation()
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_correlation <- function(df,
                             method = "spearman",
                             order = "alphabet",
                             label_size = 0.7,
                             number_size = 0.9
                             ) {

  ### Testing
  # df <- credit_data
  # method = "spearman"
  # order = "hclust"
  # label_size = 0.7
  ###

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (any(!is.character(method), !is.character(order)))
    stop("arguments must be character")

  if (!is.numeric(label_size))
    stop("argument must be numeric")

  cor_mtx <- df %>%
    select_if(is.numeric) %>%
    stats::cor(use = "pairwise.complete.obs", method = method)

  cor_sig <- corrplot::cor.mtest(cor_mtx, conf.level = .95)

  corrplot::corrplot(
    cor_mtx,
    col = grDevices::colorRampPalette(c("#6666ff","white","#ff4c4c"))(200),
    order = order,
    tl.cex = label_size,
    addCoef.col = "black",
    number.cex = number_size,
    method = "square",
    type = "lower",
    tl.pos = "dt",
    addrect = 3,
    tl.col = "black",
    tl.srt = 45,
    p.mat = if (order == "alphabet") {NULL} else {cor_sig$p},
    insig = "blank",
    diag = FALSE)

}
