#' rf_mds generates a two-dimensional MDS plot given random forest proximities
#' @name rf_mds
#' @param prox A pre-computed rf_proximities object. If provided, x, rf, and
#' type will be ignored
#' @param x A dataframe or matrix of data. Rows (n) are observations,
#'   columns (d) are variables.
#' @param y The labels corresponding to x. Should be of type factor or numeric.
#' @param rf A forest object of s3 class 'ranger'. Note: rf requires
#'   write.forest = TRUE and keep.inbag = TRUE.
#' @param type The type of proximities to be obtained.
#'  Options are rfgap (default), original, oob, pbk, or rfproxih.
#' @param mds_type Type of MDS to be run; 'metric' (default) or 'nonmetric'
#' @return MDS coordinates. A plot will be generated if plot = TRUE
# TODO: Add example.
#' @import MASS
#' @import RColorBrewer
#' @import ggplot2
#' @export
rf_mds <- function(x, y, prox = NULL, rf = NULL, type = 'rfgap',
                     mds_type = 'nonmetric') {

  if (!is.null(prox)) {
    warning('x, and type will be ignored')

  } else if (is.null(prox) && !is.null(rf)) {
    prox <- get_proximities(x = x, y = y, rf = rf, type = type)

  } else {
    prox <- get_proximities(x = x, y = y, type = type)

  }

  prox <- zero_one_scale(prox)
  prox <- make_symmetric(prox)
  prox[prox == 1] <- 0.999999

  if (mds_type == 'metric') {
    mds <- stats::cmdscale(sqrt(1 - prox))
  } else{
    mds <- MASS::isoMDS(sqrt(1 - prox))$points
  }

  mds <- as.rf_mds(mds)

  mds

}

#' This is is a generic plot function for an rf_mds object
#'
#' @name plot.rf_mds
#' @param x an rf_mds object
#' @param y class labels associated with x.
#' @param ... Additional arguments for generic plot function
#' @import ggplot2
#' @export

plot.rf_mds <- function(x, y, ...) {

  x <- as.data.frame(x)
  Class <- y

  g <- ggplot2::ggplot(x, ggplot2::aes(x = V1, y = V2)) +

    {if (is.factor(y))geom_point(aes(shape = Class, color = Class))} +
    {if (is.factor(y))scale_color_brewer(palette = 'Paired')} +
    {if (is.factor(y))scale_shape_manual(values = c(19, 17, 15, 3, 7,
                                                    8, 18, 25, 10, 11,
                                                    4, 9, 2, 12, 14))} +


    {if (!is.factor(y))geom_point(aes(color = y))} +

    {if (!is.factor(y))scale_colour_continuous(low = '#ffffd9',
                                               high = '#0c2c84')} +

    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

  g
}


# A class rf_mds
as.rf_mds <- function(x) {
  structure(x, class = c('rf_mds', 'matrix'))
}
