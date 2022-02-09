#--------------------------------------------------------------#
#                   Detect Outliers
#--------------------------------------------------------------#

#' detect_outliers produces a dataset's outlier measure score based on random-forest
#'   proximities
#' @name rf_outliers
#' @param x A data matrix (rows are observations, columns variables) or a
#'   pre-computed proximity matrix of class rf_proximities.
#' @param y A vector of class labels or a continuous responses
#' @param type The type of random forest proximity to be used.  Options are
#'   'rfgap', 'oob', or 'original'.
#' @param ... Additional options for running ranger::ranger()
#' @param rf (Optional) a pretrained random forest of class ranger.
#' @return A vector of outlier measures of length length(y)
#' @examples
#' x <- iris[, 1:4]
#' y <- iris[, 5]
#' outlier_measures <- rf_outliers(x, y)
#' @export
#'

rf_outliers <- function(x, y, type = "rfgap", rf = NULL, ...) {

  if (!is.factor(y)) {
    stop('rf_outliers is only available for classification problems.  Be
            sure "y" is a factor type.')
  }
  y <- as.factor(y)

  n <- dim(x)[1]

  prox_sum <- rep(0, n)
  outlier_measures <- rep(0, n)


  if (class(x) == "rf_proximities") {

    prox <- x

  } else {
    if (!is.null(rf)) {
      prox <- get_proximities(x, y = y, rf = rf, type = type)
    } else {
      prox <- get_proximities(x, y, rf = rf, type = type, ...)
    }
  }

  prox        <- zero_one_scale(prox)

  raw_measure <- sapply(1:n, function(i) {n / sum(prox[i, y == y[i]]^2)})
  medians     <- sapply(1:n, function(i) {stats::median(raw_measure[y == y[i]])})
  mads        <- sapply(1:n, function(i) {stats::mad(raw_measure[y == y[i]])})

  outlier_measures <- abs(raw_measure - medians) / mads
  outlier_measures[!is.finite(outlier_measures)] <- 0
  return(as.rf_outlier(outlier_measures))
}

# Generic function to impose rf_outlier class
as.rf_outlier <- function(x) {
  x <- structure(x, class = c('rf_outlier', 'numeric'))
}


###############################################################################

#' This is is a generic plot function for an rf_outlier object
#'
#' @name plot.rf_outlier
#' @param outlier an rf_outlier object
#' @param x dataframe associated with the outlier scores
#' @param y class labels associated with x.
#' @param base_size starting point size
#' @param scale_size how much is the outlier score affecting point size
#' @param ... Additional arguments for mds and generic plotting
#' @import ggplot2
#' @export
#'
plot.rf_outlier <- function(outlier, x, y, base_size = 2, scale_size = 3, ...) {

  x <- as.data.frame(x)

  mds <- as.data.frame(rf_mds(x, y, ...))

  Class <- y

  scale = as.numeric(base_size + min_max_scale(outlier) * scale_size)

  g <- ggplot2::ggplot(mds, ggplot2::aes(x = V1, y = V2)) +

    {if (is.factor(y))geom_point(aes(shape = Class, color = Class,
                                     size = scale))} +
    {if (is.factor(y))scale_color_brewer(palette = 'Dark2')} +


    {if (is.factor(y))scale_shape_manual(values = c(19, 17, 15, 3, 7,
                                                    8, 18, 25, 10, 11,
                                                    4, 9, 2, 12, 14))} +


    {if (!is.factor(y))geom_point(aes(color = y,
                                      size  = scale))} +

    {if (!is.factor(y))scale_colour_continuous(low = '#ffffd9',
                                               high = '#0c2c84')} +

    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

  g
}
