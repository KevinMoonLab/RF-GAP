get_original_proximities <- function(rf, x, x_test = NULL) {
  leaf_matrix <- stats::predict(rf, x, type = "terminalNodes")$predictions

  # Used if test set is supplied
  if (!is.null(x_test)) {
    leaf_matrix_test <- stats::predict(rf, x_test,
                                       type = "terminalNodes")$predictions

    leaf_matrix <- rbind(leaf_matrix, leaf_matrix_test)
  }

  n <- dim(leaf_matrix)[1]
  num_trees <- dim(leaf_matrix)[2]

  prox <- matrix(rep(0, n * n), nrow = n, ncol = n)

  for (ind in 1:n) {
    prox_vec <- rep(0, n)
    for (t in 1:num_trees) {
      index <- leaf_matrix[ind, t]
      matches <- which(leaf_matrix[, t] == index)
      prox_vec[matches] <- prox_vec[matches] + 1
    }
    prox_vec <- prox_vec / num_trees
    prox[ind, ] <- prox_vec
  }
  return(prox)
}
