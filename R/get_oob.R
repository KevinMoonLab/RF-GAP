get_oob_prox <- function(rf, x, x_test = NULL) {
  leaf_matrix <- stats::predict(rf, x, type = "terminalNodes")$predictions

  # If including a test set
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
  oob <- 1 - in_bag
  oob_leaves <- oob * leaf_matrix

  prox <- matrix(rep(0, n * n), nrow = n, ncol = n)

  for (ind in 1:n) {
    prox_vec <- rep(0, n)
    tree_counts <- rep(0, n)
    for (t in 1:num_trees) {
      if (in_bag[ind, t] == 1) {
        next
      } else {
        index   <- leaf_matrix[ind, t]
        matches <- which(oob_leaves[, t] == index)
        oob_obs <- which(oob[, t] == 1)
        tree_counts[oob_obs] <- tree_counts[oob_obs] + 1
        prox_vec[matches] <- prox_vec[matches] + 1
      }
    }
    prox_vec <- prox_vec / tree_counts
    prox_vec[is.na(prox_vec)] <- 0
    prox_vec[is.infinite(prox_vec)] <- 0
    prox[ind, ] <- prox_vec
  }
  return(prox)
}
