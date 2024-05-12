#unit tests for generate partial clique

# Write at least two unit tests for your
# `generate_partial_clique()` function inside a file called `test_generate_partial_clique.R`
# within the `testthat` folder.
#

# from instructions

context("Testing generate_partial_clique")

test_that("generate_partial_clique works", {
  set.seed(10)
  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)

  expect_true(is.list(res))
  expect_true(is.matrix(res$adj_mat))
  expect_true(all(dim(res$adj_mat) == c(10,10)))
})

#passed!

context("Testing the max number of edges in the clique")

test_that("generate_partial_clique results within fully connected range", {
  set.seed(10)
  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 1,
                                 clique_edge_density = 1)

  expect_true(sum(res$adj_mat) <= 100)
})
#passed

# max of the sum would be one hundred 1s in the matrix (but only 45 edges
# because of symmetric and nodes.)
