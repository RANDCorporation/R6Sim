# This file contains tests specifically for the R6Experiment class.

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