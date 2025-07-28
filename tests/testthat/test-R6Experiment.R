# This file contains tests specifically for the R6Experiment class.
library(future)
library(dplyr)

# Test R6Experiment with set_parameter

test_that("R6Experiment works with set_parameter", {
  experiment <- R6Experiment$new(model)

  experiment$
    set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c("Colonoscopy", "FIT"))$
    set_parameter(parameter_name = "abc", experimental_design = "lhs", min = 1, max = 10)$
    set_design(n_lhs = 2)

  expect_true(is.R6Experiment(experiment))
})

# Test R6Experiment without set_parameter

test_that("R6Experiment works without set_parameter", {
  experiment <- R6Experiment$new(model)
  experiment$set_design()
  expect_true(is.R6Experiment(experiment))
})

# Test R6Experiment with convert to grid = T

experiment <- R6Experiment$new(model)

experiment$
  set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c("Colonoscopy", "FIT"))$
  set_parameter(parameter_name = "abc", experimental_design = "lhs", min = 1, max = 10)$
  set_design(n_lhs = 2, convert_lhs_to_grid = T)

test_that("R6Experiment works with convert to grid = T", {
  expect_true(is.R6Experiment(experiment))
})

# Test R6Experiment with pre-existing design

test_that("R6Experiment works with pre-existing design", {
  experiment <- R6Experiment$new(model)
  # External grid:
  grid_design <- expand.grid(c(1:10), c(10:13))
  # Create an experimental design:
  experiment$set_design(grid_design_df = grid_design)
  expect_true(is.R6Experiment(experiment))
})

# Test R6Experiment runs in parallel using future

test_that("R6Experiment runs in parallel using future", {
  experiment <- R6Experiment$new(model)

  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 3)

  # Set up future for parallel execution
  future::plan(future::multisession, workers = 2)

  # Run in parallel mode
  results <- experiment$run()

  expect_equal(length(unique(results$rep.id)), 3)
  expect_equal(nrow(results), nrow(experiment$policy_design))
  expect_true(all(c("rep.id", "seed") %in% names(results)))

  # Reset future plan to sequential
  future::plan(future::sequential)
})

# Test R6Experiment runs sequentially

test_that("R6Experiment runs sequentially", {
  experiment <- R6Experiment$new(model)

  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 3)

  # Run in sequential mode
  results <- experiment$run()

  expect_equal(length(unique(results$rep.id)), 3)
  expect_equal(nrow(results), nrow(experiment$policy_design))
  expect_true(all(c("rep.id", "seed") %in% names(results)))
})

# Test R6Experiment produces identical results with the same seed

test_that("R6Experiment produces identical results with the same seed", {
  # Set the same seed before creating experiments
  seed_value <- 1234
  set.seed(seed_value)
  experiment1 <- R6Experiment$new(model)
  experiment1$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment1$set_design(n_reps = 3, set_seed = TRUE)

  set.seed(seed_value)
  experiment2 <- R6Experiment$new(model)
  experiment2$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment2$set_design(n_reps = 3, set_seed = TRUE)

  # Run both experiments
  results1 <- experiment1$run()
  results2 <- experiment2$run()

  # Verify that results are identical
  expect_identical(results1, results2)
})
