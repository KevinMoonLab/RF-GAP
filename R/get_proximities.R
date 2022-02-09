
# TODO: Set up x_test for pbk, rfproxih
# TODO: Make sure rf takes precedence over x and y

#' This function generates random forest proximities of a variety of types.
#'
#' @name get_proximities
#' @import ranger
#' @import FNN
#' @import data.table
#' @param x A dataframe or matrix of data. Rows (n) are observations,
#'   columns (d) are variables.
#' @param y The labels corresponding to x. should be of type factor.
#' @param rf A forest object of s3 class 'ranger'. Note: rf requires
#'   write.forest = TRUE and keep.inbag = TRUE.
#' @param x_test An (optional) test dataset (dataframe or matrix)
#' @param type The type of proximities to be obtained.
#'  Options are rfgap (default), original, oob, pbk, or rfproxih.
#' @param nk Number of terminal node neighbors to consider when calling type =
#'  rfproxih. (default 10).
#' @param ... Additional argument options for ranger(). (If rf is not supplied).
#' @return A matrix of proximity values.
#' @examples
#'
#' x <- iris[, 1:4]
#' y <- iris[, 5]
#'
#' prox <- get_proximities(x, y)
#' @export
get_proximities <- function(x, y = NULL, rf = NULL,
                            x_test = NULL, type = "rfgap", nk = 10, ...) {


  if (is.null(rf) && is.null(y)) {
    stop("Either y or rf must be supplied")
  } else if (is.null(rf) && !is.null(y)) {
    rf <- ranger::ranger(
      x = x, y = y,
      keep.inbag = TRUE,
      write.forest = TRUE,
      ...
    )
  }

  prox <- switch(type,
    "rfgap" = get_rfgap(rf, x, x_test),
    "original" = get_original_proximities(rf, x, x_test),
    "oob" = get_oob_prox(rf, x, x_test),
    "pbk" = get_pbk_proximities(rf, x, ...),
    "rfproxih" = get_rfproxih(rf, x, y, nk, ...),
    stop("Only types rfgap, original, oob, pbk, and rfproxih are supported.
         Please select one of these types")
  )

  prox <- as.rf_proximities(prox)
  return(prox)

}

#--------------------------------------------------------------#
#           Constructor function for rf_proximities
#--------------------------------------------------------------#
#' as.rf_proximities is a constructor for the rf_proximities class
#' @name as.rf_proximities
#' @param x a matrix to be assigned the class rf_proximities
#' @return A matrix of class rf_proximities
#' @export
as.rf_proximities <- function(x) {
  x <- structure(x, class = c("matrix", "rf_proximities"))
  return(x)
}
