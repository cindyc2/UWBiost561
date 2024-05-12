#unit test for compute maximum partial clique


# Writing at least five unit tests for your
# `compute_maximal_partial_clique()` function inside a file called `test_compute_maximal_partial_clique.R`
# within the `testthat` folder. Based on the "List of types of unit-tests" page in Lecture 5,
# ensure each of the five unit tests is in a "different category."
#
# Some pointers:
#
# - You should use `generate_partial_clique()` within some of your tests for `compute_maximal_partial_clique()`.
# Since **you** are constructing the random adjacency matrix with a partial clique
# with at least edge density of `clique_edge_density` of size `round(n*clique_fraction)`,
# then you should expect that for "simple" random adjacency matrices, your `compute_maximal_partial_clique()` can recover the partial clique that **you** constructed.




#unit test make sure output is edge

# try with multiple equal size cliques


context("Testing compute_maximal_partial_clique")

#1
test_that("compute_maximal_partial_clique works", {
  set.seed(10)

  res1 <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)
  res2 <- compute_maximal_partial_clique(adj_mat = res1$adj_mat,
                                        alpha = 0.5)

  expect_true(is.list(res2))
})
#passed!


#2
test_that("compute_maximal_partial_clique give an actual edge of a clique", {
  library(matrixcalc)
  set.seed(10)

  res1 <- generate_partial_clique(n = 10,
                                  clique_fraction = 1,
                                  clique_edge_density = 1)
  res2 <- compute_maximal_partial_clique(adj_mat = res1$adj_mat,
                                         alpha = 1)

  expect_true(res1$adj_mat[res2$clique_idx[[1]],  res2$clique_idx[[2]]] == 1)
})
#passed

# 3
test_that("compute_maximal_partial_clique rejects empty matrix", {
  set.seed(10)

  res3 <- as.matrix(0)
  expect_error(compute_maximal_partial_clique(adj_mat = res3,
                                              alpha = 1))
})
#passed!

#4
test_that("compute_maximal_partial_clique won't work with permuted rows", {
  set.seed(10)

  res7 <- generate_partial_clique(n = 10,
                                  clique_fraction = 0.5,
                                  clique_edge_density = 0.6)
  res7 <- res7$adj_mat[sample(1:nrow(res7$adj_mat),nrow(res7$adj_mat)),]


  expect_error(compute_maximal_partial_clique(adj_mat = res7,
                                              alpha = 1))
})
# passed!



#5
test_that("compute_maximal_partial_clique rejects rectangle matrix", {
  set.seed(10)

  res1 <- as.matrix(c(1,2,3,4,5,6))

  expect_error(compute_maximal_partial_clique(adj_mat = res1,
                                              alpha = 1))
})
#passed!


#6
test_that("compute_maximal_partial_clique runs in less than 30 seconds", {
  start_time <- Sys.time()
  res1 <- generate_partial_clique(n = 50,
                                  clique_fraction = 0.5,
                                  clique_edge_density = 0.9)
  res2 <- compute_maximal_partial_clique(adj_mat = res1$adj_mat,
                                         alpha = 1)

  end_time <- Sys.time()

  execution_time <- end_time - start_time
  expect_true(execution_time < 30, "Execution time should be less than 30 seconds")
})
#passed!
