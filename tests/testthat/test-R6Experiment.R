# This file contains tests specifically for the R6Experiment class.

# Ensure all required libraries are loaded
library(dplyr)
library(tidyr)
library(future)


# Creating a model object -------------------------------------------------


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
  # Ensure all required libraries are loaded for this test
  experiment <- R6Experiment$new(model)

  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 3)

  # Assign the parallel backend function
  backend_fn <- R6Sim:::get_parallel_backend_fn()

  # Use the backend function in the test
  plan(backend_fn)

  # Run in parallel mode with checkpoint_dir
  temp_checkpoint_dir <- tempfile()
  results <- experiment$run(checkpoint_dir = temp_checkpoint_dir)

  expect_equal(length(unique(results$rep.id)), 3)
  expect_equal(nrow(results), nrow(experiment$policy_design))
  expect_true(all(c("rep.id", "seed") %in% names(results)))

  # Reset future plan to sequential
  plan(sequential)

  # Close/delete the temporary checkpoint file
  unlink(temp_checkpoint_dir, recursive = TRUE)
})

# Test R6Experiment runs sequentially

test_that("R6Experiment runs sequentially", {
  experiment <- R6Experiment$new(model)

  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 3)

  # Run in sequential mode with checkpoint_dir
  temp_checkpoint_dir <- tempfile()
  results <- experiment$run(checkpoint_dir = temp_checkpoint_dir)

  expect_equal(length(unique(results$rep.id)), 3)
  expect_equal(nrow(results), nrow(experiment$policy_design))
  expect_true(all(c("rep.id", "seed") %in% names(results)))

  # Close/delete the temporary checkpoint file
  unlink(temp_checkpoint_dir, recursive = TRUE)
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

  # Run both experiments with checkpoint_dir
  temp_checkpoint_dir1 <- tempfile()
  temp_checkpoint_dir2 <- tempfile()
  results1 <- experiment1$run(checkpoint_dir = temp_checkpoint_dir1)
  results2 <- experiment2$run(checkpoint_dir = temp_checkpoint_dir2)

  # Verify that results are identical
  expect_identical(results1, results2)

  # Close/delete the temporary checkpoint files
  unlink(temp_checkpoint_dir1, recursive = TRUE)
  unlink(temp_checkpoint_dir2, recursive = TRUE)
})
