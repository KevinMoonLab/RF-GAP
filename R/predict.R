#--------------------------------------------------------------#
#                Proximity Classification
#--------------------------------------------------------------#

# TODO: Build a separate function for comparing predictions
# TODO: Need to be compatible with adding a test set?

#' A function to make predictions using random forest proximities
#'  (rf_proximities class)
#' @name predict.rf_proximities
#' @param object An rf_proximity class matrix
#' @param y A vector of class labels or a continuous responses
#' @param ... Additional arguments for running ranger::ranger
#' @return A list which contains the predictions and prediction error rate
#' @examples
#' x <- iris[, 1:4]
#' y <- iris[, 5]
#' prox <- get_proximities(x, y)
#' preds_list <- predict(prox, y)
#' @export

predict.rf_proximities <- function(object, y, ...) {
  n_classes <- length(unique(as.factor(y)))
  n <- length(y)

  prox <- object

  prox_scaled <- sum_to_one(prox)

  preds <- rep(0, n)
  if (is.factor(y)) {
    classes <- unique(y)

    for (i in 1:n) {
      class_votes <- rep(0, n_classes)

      for (class_idx in 1:n_classes) {
        y_temp <- rep(0, n)
        y_temp[y == classes[class_idx]] <- 1
        class_votes[class_idx] <- prox_scaled[i, ] %*% y_temp
      }

      preds[i] <- classes[which.max(class_votes)]
    }

    correct <- preds == as.double(y)
    error <- 1 - sum(correct) / n
  } else {
    # TODO: Also a helper function and use apply
    for (i in 1:n) {
      preds[i] <- prox_scaled[i, ] %*% y
      error <- sum((preds - y)^2) / n
    }
  }
  return(list("preds" = preds, "error" = error))
}
