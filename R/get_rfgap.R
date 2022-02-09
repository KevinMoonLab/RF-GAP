get_rfgap <- function(rf, x, x_test = NULL) {

  leaf_matrix <- stats::predict(rf, x, type = "terminalNodes")$predictions

  # If test set available and want proximities to test set
  if (!is.null(x_test)) {
    leaf_matrix_test <- stats::predict(rf, x_test,
                                       type = "terminalNodes"
    )$predictions

    leaf_matrix <- rbind(leaf_matrix, leaf_matrix_test)
  }


  n <- dim(leaf_matrix)[1]
  num_trees <- dim(leaf_matrix)[2]

  in_bag_count <- matrix(unlist(rf$inbag.counts), ncol = num_trees)

  # New for test set
  if (!is.null(x_test)) {
    in_bag_count <- rbind(
      in_bag_count,
      matrix(rep(0, num_trees * nrow(x_test)), ncol = num_trees)
    )
  }

  in_bag <- in_bag_count
  in_bag[in_bag > 0] <- 1
  in_bag_leaves <- in_bag * leaf_matrix

  prox <- matrix(rep(0, n * n), nrow = n, ncol = n)

  for (ind in 1:n) {
    prox_vec <- rep(0, n)
    for (t in 1:num_trees) {
      if (in_bag[ind, t] == 1) {
        next
      } else {
        index <- leaf_matrix[ind, t]
        matches <- which(in_bag_leaves[, t] == index)
        k <- length(matches)
        if (k > 0) {
          prox_vec[matches] <- prox_vec[matches] + 1 / k
        }
      }
    }

    s <- sum(in_bag[ind, ] == 0)

    if (s != 0) {
      prox[ind, ] <- prox_vec / s
    } else {
      prox[ind, ] <- rep(0, n)
    }
  }
  return(prox)
}
