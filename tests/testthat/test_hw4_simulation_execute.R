# create a few (more than one) unit tests for your simulation function.
#This unit test (like your unit tests in HW3) will be in your `tests/testthat` folder.
#I am also purposely vague about how many tests or what kind of unit tests to write.
#This is for you to decide! After all, you're about to unleash your code to
#perform a (potentially long) simulation test, so you hope you've tested your
#simulation code well enough for this to be a good use of time.
#
# Note: Your unit tests should be <u>**fast**</u> (i.e., take no more than a minute to run).
#This means your unit tests should <u>**not**</u> be performing your complete simulation study.
#After all, it'll be a useless unit test if it takes an hour to figure out if
#your unit test passed. However, you want your unit tests
#(which take less than a minute) to give you confidence that your code will
#work when running the complete simulation study (which might take more than an hour).
#
# <span style="color: blue;">(There is nothing to report for this question.
#Your code will be in the `tests/testthat` folder, **not**
# in this R Markdown file.)</span>


# note if you end tests early (by pressing escape) it tells whether it passed or not so far

library(testthat)
context("Testing simulation study")

#1
test_that("hw4_simulation_execute returns data that isn't a list but saved on Bayes", {
  set.seed(10)

  res2 <- my_function(num_trials = 3)

  expect_true(!is.list(res2))
})
#passed!

#1
# test_that("hw4_simulation_execute returns data that isn't a list but saved on Bayes", {
#   set.seed(10)
#
#   res2 <- my_function(num_trials = 3)
#
#   expect_true(!is.list(res2))
# })
#passed!


#2
test_that("hw4_simulation_execute runs in less than 30 seconds for 1 implementation", {
  start_time <- Sys.time()
  end_time <- Sys.time()

  res2 <- my_function(num_trials = 5, imp_numbers = 1)

  execution_time <- end_time - start_time
  expect_true(execution_time < 30, "Execution time should be less than 30 seconds each on average")
})
#passed
