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

# Test graceful error handling functionality

test_that("R6Experiment handles errors gracefully when graceful = TRUE", {
  # Create a mock model that will throw an error
  ErrorModel <- R6::R6Class(
    classname = "ErrorModel",
    inherit = R6Sim,
    public = list(
      should_error = FALSE,

      initialize = function(name) {
        super$initialize(name = name)
        self$set_input("Test1", 1)
      },

      simulate = function() {
        if (self$should_error) {
          stop("Simulated error for testing")
        }
        return(data.frame(result = 42))
      }
    )
  )

  error_model <- ErrorModel$new(name = "error_test")
  error_model$should_error <- TRUE

  experiment <- R6Experiment$new(error_model)
  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 2)

  # Capture warnings
  temp_checkpoint_dir <- tempfile()

  # Test graceful = TRUE
  expect_warning(
    results_graceful <- experiment$run(checkpoint_dir = temp_checkpoint_dir, graceful = TRUE),
    "Error in experiment"
  )

  # Check that results contain error column
  expect_true("error" %in% names(results_graceful))
  expect_true(all(results_graceful$error == "Simulated error for testing"))
  expect_equal(nrow(results_graceful), nrow(experiment$policy_design))

  # Clean up
  unlink(temp_checkpoint_dir, recursive = TRUE)
})

test_that("R6Experiment stops execution when graceful = FALSE and error occurs", {
  # Create a mock model that will throw an error
  ErrorModel <- R6::R6Class(
    classname = "ErrorModel",
    inherit = R6Sim,
    public = list(
      should_error = FALSE,

      initialize = function(name) {
        super$initialize(name = name)
        self$set_input("Test1", 1)
      },

      simulate = function() {
        if (self$should_error) {
          stop("Simulated error for testing")
        }
        return(data.frame(result = 42))
      }
    )
  )

  error_model <- ErrorModel$new(name = "error_test")
  error_model$should_error <- TRUE

  experiment <- R6Experiment$new(error_model)
  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 2)

  temp_checkpoint_dir <- tempfile()

  # Test graceful = FALSE (default behavior)
  expect_error(
    experiment$run(checkpoint_dir = temp_checkpoint_dir, graceful = FALSE),
    "Simulated error for testing"
  )

  # Clean up
  unlink(temp_checkpoint_dir, recursive = TRUE)
})

test_that("R6Experiment mixed success and error scenarios with graceful = TRUE", {
  # Create a model that errors on specific conditions
  ConditionalErrorModel <- R6::R6Class(
    classname = "ConditionalErrorModel",
    inherit = R6Sim,
    public = list(
      initialize = function(name) {
        super$initialize(name = name)
        self$set_input("Test1", 1)
      },

      simulate = function() {
        # Error when Test1 == 2
        if (self$inputs$Test1 == 2) {
          stop("Error when Test1 equals 2")
        }
        return(data.frame(result = self$inputs$Test1 * 10))
      }
    )
  )

  conditional_model <- ConditionalErrorModel$new(name = "conditional_test")

  experiment <- R6Experiment$new(conditional_model)
  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2, 3))
  experiment$set_design(n_reps = 1)

  temp_checkpoint_dir <- tempfile()

  # Test with graceful = TRUE
  expect_warning(
    results <- experiment$run(checkpoint_dir = temp_checkpoint_dir, graceful = TRUE),
    "Error in experiment"
  )

  # Check mixed results
  expect_equal(nrow(results), 3)

  # Check successful runs
  successful_runs <- results[is.na(results$error) | results$error == "", ]
  expect_equal(nrow(successful_runs), 2)
  expect_true(all(successful_runs$result %in% c(10, 30)))

  # Check error runs
  error_runs <- results[!is.na(results$error) & results$error != "", ]
  expect_equal(nrow(error_runs), 1)
  expect_equal(error_runs$error, "Error when Test1 equals 2")

  # Clean up
  unlink(temp_checkpoint_dir, recursive = TRUE)
})
