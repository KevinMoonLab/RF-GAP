#------------------------------------------------------------------------------#
#                           PBK Proximities
#------------------------------------------------------------------------------#
# This method comes from the paper
#     "A novel approach to estimate proximity in a random forest:
#      An exploratory study"

#      https://www.sciencedirect.com/science/article/abs/pii/S095741741200810X

#      Code was not provided by the authors and has not been optimized for speed
#      or memory. This has been assembled for comparative purposes only.


#' @import Matrix
#'
# This will return a list of sparse matrices containing the pair-wise node
# distances for each tree in the forest
get_tree_dists <- function(rf, x) {

  # the leaf_matrix contains terminal node indices (# obs by # trees)
  leaf_matrix <- stats::predict(rf, x, type = 'terminalNodes')$predictions

  n         <- dim(leaf_matrix)[1]
  ntrees    <- dim(leaf_matrix)[2]

  # Not very efficient; this will store all pairwise node distances for each tree
  all_dists = list()


  for (t in 1:ntrees) {
    childNodes <- rf[["forest"]][["child.nodeIDs"]][[t]]
    dists = tree_distance(childNodes)
    dist_mat <- Matrix::sparseMatrix(i = as.numeric(unlist(dists[, 1])),
                                     j = as.numeric(unlist(dists[, 2])),
                                     x = as.numeric(unlist(dists[, 3])),
                                     symmetric = TRUE)

    all_dists <- append(all_dists, dist_mat)
  }
  return(all_dists)
}


# Create list of g-matrices
# w is a parameter to suggest how much to incorporate the node distances
get_pbk_proximities <- function(rf, x, w = 1,
                                all_dists = get_tree_dists(rf, x)) {

  leaf_matrix <- stats::predict(rf, x, type = 'terminalNodes')$predictions

  n         <- dim(leaf_matrix)[1]
  ntrees    <- dim(leaf_matrix)[2]


  prox <- matrix(integer(n * n), nrow = n)
  for (t in 1:ntrees) {
    dist_mat <- all_dists[t][[1]]
    leaves   <- leaf_matrix[, t]
    unique_leaves <- unique(leaves)
    for (m in unique(leaves)) {
      gs <- dist_mat[m, ]

      gprox <- 1 / exp(w * gs)

      prox[, which(leaves == m)] <- prox[, which(leaves == m)] + gprox[leaves]
    }
  }
  return(prox / ntrees)
}
