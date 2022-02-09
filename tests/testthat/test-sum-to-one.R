test_that("Check RF-GAP rows sum to one", {
  x <- iris[, 1:4]
  y <- iris[, 5]

  prox <- get_proximities(x, y, type = 'rfgap')

  expect_equal(rowSums(prox), rep(1, 150))
})
