#--------------------------------------------------------------#
#                    Imputation
#--------------------------------------------------------------#
#' A function to impute missing values from a dataset.
#' @name rf_impute
#' @param x A dataframe or matrix of data. Rows (n) are observations,
#'   columns (d) are variables.
#' @param y The labels corresponding to x. Should be of type factor.
#' @param type The type of proximities to be obtained. Options are
#'   rfgap (default), original, or oob.
#' @param ... Additional argument options for ranger(). (If rf is not supplied).
#' @param x_true The x data matrix prior to removing values for experimentation.
#'   This is used to compare imputation methods, not used in "real-life"
#'   applications.
#' @param n_iters The number of iterations for imputation.  The default is 1.
#' @return A dataframe with imputed values if x_true is NULL. Else, a list
#'   which contains the imputed dataset and error values (MSE between the
#'   true and imputed data)
#' @examples
#' x <- airquality[, 1:4]
#' y <- airquality[, 5]
#' x_imp <- rf_impute(x, y)
#' @export
#'

rf_impute <- function(x, y, type = "rfgap", n_iters = 1, x_true = NULL, ...) {
  cat_error <- rep(0, n_iters + 1)
  cont_error <- rep(0, n_iters + 1)

  n <- dim(x)[1]
  d <- dim(x)[2]

  # Automatically assigning as classification or regression problem
  if (length(unique(y)) <= 20) {
    y <- as.factor(y)
  }

  # Added as.data.frame to see if this helps
  catCols <- sapply(as.data.frame(x), function(x) {
    is.factor(x)
  })

  x_med <- x
  miss_idx <- which(is.na(x), arr.ind = TRUE)

  # Fill in missing values with median
  for (m in 1:dim(miss_idx)[1]) {
    row <- as.numeric(miss_idx[m, "row"])
    col <- as.numeric(miss_idx[m, "col"])

    # Checking if categorical variable; then replace with mode
    if (catCols[colnames(x)[col]]) {
      x_med[row, col] <- get_mode(x[y[row] == y, col])
    } else {
      x_med[row, col] <- stats::median(x[y[row] == y, col], na.rm = TRUE)
    }

    # Check for values still missing
    if (is.na(x_med[row, col])) {
      if (catCols[colnames(x)[col]]) {
        x_med[row, col] <- get_mode(x[, col])
      } else {
        x_med[row, col] <- stats::median(x[, col], na.rm = TRUE)
      }
    }
  }

  # Checking difference in original vs. imputed data
  if (exists("x_true")) {
    cat_error[1] <- 1 - sum(x_true[catCols] == x_med[catCols]) /
      prod(dim(x[catCols]))

    cont_error[1] <- norm(as.matrix(x_true[!catCols] - x_med[!catCols]), "F")
  }

  # Only return mean if n_iters = 0
  if (n_iters == 0) {
    return(x_med)
  } else {
    x_imp <- x_med
    for (iter in 1:n_iters) {

      rf <- ranger::ranger(
        y = y, x = x_imp,
        write.forest = TRUE,
        keep.inbag = TRUE,
        ...
      )

      prox <- get_proximities(x = x_imp, y = y, rf = rf, type = type)


      # Running Imputation
      miss_vars <- unique(miss_idx[, 2])
      for (j in miss_vars) {
        if (catCols[colnames(x)[j]]) {
          miss_obs <- miss_idx[miss_idx[, 2] == j, 1]
          unique_classes <- unique(x[, j][!is.na(x[, j])])
          votes <- matrix(rep(0, length(miss_obs) * count_unique(x[, j])),
                          nrow = count_unique(x[, j]))
          for (d in 1:count_unique(x[, j])) {
            class <- unique_classes[d]
            if (!is.matrix(prox[miss_obs, (x[-miss_obs, j] == class)])) {
              votes[d, ] <- prox[miss_obs, (x[-miss_obs, j] == class)]
            } else {
              votes[d, ] <- rowSums(prox[miss_obs, (x[-miss_obs, j] == class)])
            }
          }

          x_imp[miss_obs, j] <- apply(votes, 2, function(x) {
            unique_classes[which.max(x)]
          })
        } else {
          miss_obs <- miss_idx[miss_idx[, 2] == j, 1]


          tryCatch(
            {
              sums <- rowSums(prox[miss_obs, -miss_obs])
              sums[which(sums == 0)] <- 1
              imp_vals <- t(x_imp[-miss_obs, j]) %*% t(prox[miss_obs, -miss_obs] / sums)
              x_imp[miss_obs, j] <- as.numeric(imp_vals)
            },
            error = function(e) {
              sums <- sum(prox[miss_obs, -miss_obs])
              imp_vals <- sum(x_imp[-miss_obs, j] * prox[miss_obs, -miss_obs] / sums)
              x_imp[miss_obs, j] <- imp_vals
            }
          )
        } # End else
      } # End for

      # If x_true is provided, this gives the errors between imputed and true vars
      if (!is.null(x_true)) {
        cat_error[iter + 1] <- 1 - sum(x_true[catCols] == x_imp[catCols]) /
          prod(dim(x[catCols]))

        cont_error[iter + 1] <- norm(as.matrix(x_true[!catCols] - x_imp[!catCols]), "F")
      }
    } # End for iter
  } # End else

  if (!is.null(x_true)) {
    return(list("x_imp" = x_imp, "categorical_error" = cat_error, "continuous_error" = cont_error))
  } else {
    return(x_imp)
  }
} # End function
