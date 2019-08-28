
# First to lower ----------------------------------------------------------

#' Convert strings first letter to lowercase
#'
#' This function converts your data frame column names or character vector elements
#' first letter to lowercase. It can be used in a pipe.
#'
#' @param x A data frame or vector.
#' @examples
#' first_to_lower(c("first", "second"))
#' @export
first_to_lower <- function(x) {

  if (is.data.frame(x)) {
    df_names <- names(x)
    substr(df_names, 1, 1) <- tolower(substr(df_names, 1, 1))
    names(x) <- df_names
    x
  } else if (all(is.vector(x), is.character(x))) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    x
  } else {
    stop("argument must be a data frame or character vector")
  }

}

# First to upper ----------------------------------------------------------

#' Convert strings first letter to uppercase
#'
#' This function converts your data frame column names or character vector elements
#' first letter to uppercase. It can be used in a pipe.
#'
#' @param x A data frame or vector.
#' @examples
#' first_to_upper(c("first", "second"))
#'
#' diamonds %>%
#'    first_to_upper()
#' @export
first_to_upper <- function(x) {

  if (is.data.frame(x)) {
    df_names <- names(x)
    substr(df_names, 1, 1) <- toupper(substr(df_names, 1, 1))
    names(x) <- df_names
    x
  } else if (all(is.vector(x), is.character(x))) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  } else {
    stop("argument must be a data frame or character vector")
  }

}
