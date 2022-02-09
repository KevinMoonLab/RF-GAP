#------------------------------------------------------------------------------#
#                           RFProxIH Proximities
#------------------------------------------------------------------------------#

# From the paper "https://arxiv.org/pdf/2007.02572.pdf"
# Code was not provided by the authors and has not been optimized for speed
# or memory. This has been assembled for comparative purposes only.

get_rfproxih <- function(rf, x, y, nk = 10) {

  if (is.null(y)) {
    stop('y must be provided for rfproxih')
  }

  if ('data.frame' %!in% class(x)) {
    x <- as.data.frame(x)
  }

  split_vars <- get_split_vars(rf)

  leaf_matrix <- stats::predict(rf, x, type = "terminalNodes")$predictions

  num.trees <- ncol(leaf_matrix)
  n         <- nrow(leaf_matrix)

  s <- matrix(integer(n ^ 2), nrow = n)

  for (t in 1:num.trees) {

    child_node_ids <- rf[['forest']][['child.nodeIDs']][[t]]
    child_node_matrix <- matrix(unlist(child_node_ids), nrow = 2, byrow = TRUE)
    terminal_nodes <- which(child_node_matrix[1, ] == 0) - 1

    nodes <- leaf_matrix[, t]

    for (i in 1:length(terminal_nodes)) {

      node <- terminal_nodes[i]
      matching_nodes <- which(node == nodes)


      included_variables <-split_vars[[t]][[i]]

      xtemp    <- x[matching_nodes,
                    included_variables,
                    drop = FALSE]

      ytemp <- y[matching_nodes]

      inds  <- row.names(xtemp)
      ntemp <- length(inds)
      k     <- max(min(nk, nrow(xtemp) - 1), 1)

      if (k <= 1) {
        next
      }

      kNN   <- FNN::get.knn(xtemp, k = k, algorithm = 'kd_tree')$nn.index
      row.names(kNN) <- inds

      kDN   <- rowSums(matrix(ytemp != t(ytemp[kNN]), nrow = ntemp)) / k

      sp    <- 1 - kDN

      for (j in matching_nodes) {

        s[j, matching_nodes[matching_nodes != j]] <-
          s[j, matching_nodes[matching_nodes != j]] +
          sp[which(matching_nodes != j)]

      }

    }
  }
  make_symmetric(s / num.trees)
}


get_split_vars <- function(rf) {

  num.trees <- rf[["forest"]][["num.trees"]]

  split_vars <- list()

  for (t in 1:num.trees) {

    child_node_ids <- rf[['forest']][['child.nodeIDs']][[t]]
    child_node_matrix <- matrix(unlist(child_node_ids), nrow = 2, byrow = TRUE)
    terminal_nodes <- which(child_node_matrix[1, ] == 0) - 1

    var_ids <- rf[["forest"]][["split.varIDs"]][[t]] + 1
    parents <- matrix(terminal_nodes, byrow = TRUE, nrow = 1)
    vars    <- matrix(integer(length(parents)), nrow = 1, byrow = TRUE)

    while (max(parents) > 0) {

      parents <- sapply(parents, function (i) {
        if (i == 0) {
          0
        } else {
          which(child_node_matrix == i, arr.ind = TRUE)[2] - 1
        }

      })

      vars <- rbind(vars, var_ids[parents + 1])
    }

    vars <- apply(vars[-1, ], 2, unique)
    split_vars[[t]] <- vars
  }

  return(split_vars)
}


'%!in%' <- Negate('%in%')
