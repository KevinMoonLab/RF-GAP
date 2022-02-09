#--------------------------------------------------------------#
#                  Additional helper functions
#--------------------------------------------------------------#

get_mode <- function(v) {
  uniqv <- as.matrix(unique(v[!is.na(v)]))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

count_unique <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  length(uniqv)
}

min_max_scale <- function(x) {
  denom <- (max(x) - min(x))
  if (denom == 0) {
    denom <- 1
  }
  (x - min(x)) / denom
}


# Scales proximity rows to sum up to 1
sum_to_one <- function(x) {
  row_sum <- apply(x, 1, sum)
  x / row_sum
}

# Scale the proximity rows from 0 to 1
zero_one_scale <- function(x) {
  maxs <- apply(abs(x), 1, max)
  x / maxs
}

# Makes a matrix symmetric
make_symmetric <- function(x) {
  (x + t(x)) / 2
}
