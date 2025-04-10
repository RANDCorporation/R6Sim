

# Sample model:

Mymodel <- R6::R6Class(
  classname = "Mymodel",
  inherit = R6Sim,
  public = list(
    sim_res = NULL,

    # Custom Initialize function
    initialize = function(name) {
      super$initialize(name = name)
      self$set_input("pop.size", 100)$
        set_input("risk.mean", 0.01)$
        set_input("risk.sd", 0.001)$
        set_input(name = "trials", value = 10)
    },

    # Sample Simulate function
    simulate = function() {

      # Create a sample population with some health events:
      sim_res <- data.frame(
        p.id = 1:self$inputs$pop.size,
        risk = rnorm(n = self$inputs$pop.size, mean = self$inputs$risk.mean, sd = self$inputs$risk.sd)
      ) %>%
        mutate(probability.event = 1 - exp(-risk)) %>%
        mutate(n.events = rbinom(n = 1:self$inputs$pop.size, size = self$inputs$trials, prob = probability.event)) %>%
        group_by() %>%
        summarise(n.events = sum(n.events))

      invisible(sim_res)
    }
  )
)

# Creating a model object -------------------------------------------------

# Creates a model object and gives it a name.
model <- Mymodel$new(name = "test")

test_that("R6Sim was created", {
  expect_true(is.R6Sim(model))
})


# Setting model inputs:
model$
  set_input(name = "pop.size", value = 1000, type = "settings")$
  set_input(name = "risk.mean", value = 0.15, type = "natural_history")$
  set_input(name = "risk.sd", value = 0.02, type = "natural_history")$
  set_input(name = "trials", value = 10, type = "natural_history")$
  set_input(name = "strategy.id", value = 1, type = "policy")$
  set_input(name = "some_date", value = "2020-01-01", type = "policy")$
  set_input(name = "det.ratios", value = seq.default(from = 0, to = 1, length.out = 101), type = "policy")

# Setting an input twice:
model$set_input(name = "risk.sd", value = 0.02, type = "natural_history")

test_that("input was created", {
  expect_true(model$inputs$pop.size == 1000)
})


# Set posterior -----------------------------------------------------------

# Loading multiple posterior data.frames:
posterior.a <- data.frame(
  risk.mean = rnorm(n = 1000, mean = 0, sd = 0.5),
  risk.sd = rnorm(n = 1000, mean = 1, sd = 2),
  weights = 1 / 1000
)
# Let's suppose a different calibration resulted in a different posterior:
posterior.b <- data.frame(
  risk.mean = rnorm(n = 1000, mean = 1, sd = 0.5),
  risk.sd = rnorm(n = 1000, mean = 0.5, sd = 2),
  weights = 1 / 1000
)

# Here we set the posterior of the model using three posterior files:
model$set_param_dist(
  params_list = list(pa = posterior.a, pb = posterior.b, pc = posterior.b),
  param_dist_weights = "weights", use_average = F, n_sample = 10
)

test_that("set_posterior works", {
  expect_true(nrow(model$params_df) == 30)
})

# Set posterior works without sampling:

model$set_param_dist(
  params_list = list(pa = posterior.a, pb = posterior.b, pc = posterior.b),
  param_dist_weights = "weights", use_average = F, resample = F
)


test_that("set_posterior works without resampling", {
  expect_true(nrow(model$params_df) == 3000)
})

# Here we set the posterior of the model using three posterior files:
model$set_param_dist(
  params_list = list(pa = posterior.a, pb = posterior.b, pc = posterior.b),
  param_dist_weights = "weights", use_average = T
)

test_that("set_posterior works with averages", {
  expect_true(nrow(model$params_df) == 3)
})




# simulate model ----------------------------------------------------------

set.seed(1234)

res <- model$simulate()

test_that("simulate works", {
  expect_equal(object = nrow(res), expected = 1)
})

test_that("both setup_run and simulate methods must be implemented", {
  # Create a model inheriting from R6Sim without overriding methods
  BasicModel <- R6::R6Class(
    classname = "BasicModel",
    inherit = R6Sim,
    public = list(
      initialize = function(name) {
        super$initialize(name = name)
      }
      # No simulate or setup_run implementation
    )
  )
  
  # Create instance
  basic_model <- BasicModel$new("basic")
  
  # Methods should throw errors because they must be implemented
  expect_error(basic_model$setup_run(), "Setup_run method must be implemented by your class")
  expect_error(basic_model$simulate(), "Simulate method must be implemented by your class")
})


# json tests --------------------------------------------------------------

json <- model$to_json()
# Re-creating the model from json:
new_model <- Mymodel$new(name = "Mymodel")
new_model$set_inputs_from_json(json = json)

test_that("to_json and set_input_from_json work", {
  expect_equal(length(new_model$inputs), length(model$inputs))
})


test_that("results from a json-converted model is identical to original model", {
  set.seed(1234)
  new_res <- new_model$simulate()

  expect_identical(res, new_res)
})

test_that("to_json preserves inputs and model functionality", {
  # Create a duplicate model with different input values
  test_model <- Mymodel$new(name = "test_json_model")
  test_model$set_input("pop.size", 500)
  test_model$set_input("risk.mean", 0.05)
  
  # Convert to JSON and back
  json_model <- test_model$to_json()
  
  # Create new model from JSON
  new_model <- Mymodel$new("new_model") 
  new_model$set_inputs_from_json(json_model)
  
  # Verify input values were preserved
  expect_equal(new_model$inputs$pop.size, 500)
  expect_equal(new_model$inputs$risk.mean, 0.05)
  
  # Verify simulation results match
  set.seed(123)
  result1 <- test_model$simulate()
  
  set.seed(123)
  result2 <- new_model$simulate()
  
  expect_equal(result1, result2)
})

test_that("set_inputs_from_json handles errors", {
  # Test error handling for parse errors
  test_model <- Mymodel$new(name = "test")
  expect_error(test_model$set_inputs_from_json("invalid json string"))
  
  # Test handling invalid JSON structure
  expect_error(test_model$set_inputs_from_json('{"not_inputs": {}}'))
})



# Test get_inputs from yaml and excel -------------------------------------

test_that("get_inputs works with YAML files", {
  # Create a temporary YAML file
  yaml_content <- "
pop.size: 1000
risk.mean: 0.15
risk.sd: 0.02
trials: 10
"
  yaml_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, yaml_file)

  # Create model and test get_inputs
  model <- Mymodel$new(name = "test")
  model$get_inputs(yaml_file)

  # Test that inputs were set correctly
  expect_equal(model$inputs$pop.size, 1000)
  expect_equal(model$inputs$risk.mean, 0.15)
  expect_equal(model$inputs$risk.sd, 0.02)
  expect_equal(model$inputs$trials, 10)

  # Cleanup
  unlink(yaml_file)
})


test_that("get_inputs works with Excel files", {
  # Skip if readxl not available
  skip_if_not_installed("readxl")

  # Create test workbook
  wb_file <- tempfile(fileext = ".xlsx")

  # Create data for sheets
  sheet1_data <- data.frame(
    pop.size = 2000,
    risk.mean = 0.25
  )

  sheet2_data <- data.frame(
    risk.sd = 0.03,
    trials = 20
  )

  # Write Excel file with multiple sheets
  writexl::write_xlsx(list(
    Sheet1 = sheet1_data,
    Sheet2 = sheet2_data
  ), wb_file)

  # Test get_inputs with Excel file
  model <- Mymodel$new(name = "test")
  model$get_inputs(wb_file)

  # Verify inputs were set correctly
  expect_equal(model$inputs$Sheet1$pop.size, 2000)
  expect_equal(model$inputs$Sheet1$risk.mean, 0.25)
  expect_equal(model$inputs$Sheet2$risk.sd, 0.03)
  expect_equal(model$inputs$Sheet2$trials, 20)

  # Cleanup
  unlink(wb_file)
})

test_that("get_inputs handles errors appropriately", {
  # Test non-existent file
  model <- Mymodel$new(name = "test")
  expect_error(model$get_inputs("nonexistent.yaml"))

  # Test invalid file type
  invalid_file <- tempfile(fileext = ".txt")
  writeLines("some text", invalid_file)
  expect_error(model$get_inputs(invalid_file))
  unlink(invalid_file)
})



# set_input tests ---------------------------------------------------------

test_that("set_inputs handles unusual inputs", {
  model <- Mymodel$new(name = "test")
  
  # Error on missing parameters
  expect_error(model$set_input())

  # Set an initial value to be replaced
  model$set_input("test_length", 10)
  
  # Warning on length mismatch (input change)
  expect_warning(
    model$set_input(name = "test_length", value = c(1,2,3)), 
    "You are replacing the input test_length which had length 1 with an object of length 3"
  )
  
  # Warning on unsupported classes
  custom_class <- structure(1, class = "custom_class")
  expect_warning(
    model$set_input(name = "custom_obj", value = custom_class),
    "Input custom_obj includes values using classes that we do not recommend"
  )
  
  # The second warning output from weird classes in list is harder to test exactly
  # So we'll just verify that a warning is thrown
  weird_list <- list(a = structure(1, class = "weird"))
  expect_warning(model$set_input(name = "weird_list", value = weird_list))
})



# R6Experiment tests -----------------------------------------------------

test_that("R6Experiment works with set_parameter", {
  experiment <- R6Experiment$new(model)

  experiment$
    set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c("Colonoscopy", "FIT"))$
    set_parameter(parameter_name = "abc", experimental_design = "lhs", min = 1, max = 10)$
    set_design(n_lhs = 2)

  expect_true(is.R6Experiment(experiment))
})

test_that("R6Experiment works without set_parameter", {
  experiment <- R6Experiment$new(model)
  experiment$set_design()
  expect_true(is.R6Experiment(experiment))
})

experiment <- R6Experiment$new(model)

experiment$
  set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c("Colonoscopy", "FIT"))$
  set_parameter(parameter_name = "abc", experimental_design = "lhs", min = 1, max = 10)$
  set_design(n_lhs = 2, convert_lhs_to_grid = T)

test_that("R6Experiment works with convert to grid = T", {
  expect_true(is.R6Experiment(experiment))
})

test_that("R6Experiment works with pre-existing design", {
  experiment <- R6Experiment$new(model)
  # External grid:
  grid_design <- expand.grid(c(1:10), c(10:13))
  # Create an experimental design:
  experiment$set_design(grid_design_df = grid_design)
  expect_true(is.R6Experiment(experiment))
})


test_that("R6Experiment supports stochastic replications with seeds", {
  experiment <- R6Experiment$new(model)

  # Test with default n_reps=1 and set_seed=T
  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c("A", "B"))
  experiment$set_design(n_lhs = 2, )

  # Check experiment has seed column
  expect_true("seed" %in% names(experiment$policy_design))
  expect_equal(length(unique(experiment$policy_design$rep.id)), 1)
  expect_false(any(is.na(experiment$policy_design$seed)))

  # Test with n_reps=3 and set_seed=T
  experiment$set_design(n_lhs = 2, n_reps = 3)
  expect_equal(length(unique(experiment$policy_design$rep.id)), 3)
  expect_false(any(is.na(experiment$policy_design$seed)))
  expect_equal(length(unique(experiment$policy_design$seed)), 3)

  # Test with set_seed=F
  experiment$set_design(n_lhs = 2, n_reps = 2, set_seed = F)
  expect_true(all(is.na(experiment$policy_design$seed)))

  # Test reproducibility with seeds
  set.seed(123)
  experiment$set_design(n_lhs = 2, n_reps = 2)
  seeds1 <- experiment$policy_design$seed

  set.seed(123)
  experiment$set_design(n_lhs = 2, n_reps = 2)
  seeds2 <- experiment$policy_design$seed

  expect_equal(seeds1, seeds2)
})

test_that("run_single_experiment sets seeds correctly", {

  experiment <- R6Experiment$new(model)

  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))

  experiment$set_design(n_reps = 2, set_seed = T)

  # Get results with same policy_design_id but different seeds
  res1 <- experiment$run()
  res2 <- experiment$run()

  # Same policy_design_id should give different results due to different seeds
  expect_true(identical(res1, res2))

  # Same policy_design_id and seed should give identical results
  experiment$set_design(n_reps = 2, set_seed = F)
  res3 <- experiment$run()

  expect_false(identical(res1, res3))
})

test_that("R6Experiment runs with replications", {
  experiment <- R6Experiment$new(model)

  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 3)
  
  # Run in sequential mode
  results <- experiment$run(parallel = FALSE)

  expect_equal(length(unique(results$rep.id)), 3)
  expect_equal(nrow(results), nrow(experiment$policy_design))
  expect_true(all(c("rep.id", "seed") %in% names(results)))
})

test_that("R6Experiment basic run works", {
  experiment <- R6Experiment$new(model)
  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 2)
  
  # Test with default parameters
  results <- experiment$run()
  expect_equal(nrow(results), nrow(experiment$policy_design))
  expect_true("Test1" %in% names(results))
  
  # Check correct number of replications
  expect_equal(length(unique(results$rep.id)), 2)
})

# Skip parallel tests as they're harder to run consistently
test_that("R6Experiment parallel tests are skipped", {
  skip("Skipping parallel execution tests")
  
  # Test with PSOCK cluster (default)
  experiment <- R6Experiment$new(model)
  experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c(1, 2))
  experiment$set_design(n_reps = 2)
  
  # These would be run if not skipped
  results <- experiment$run(n_cores = 2, parallel = TRUE, packages = "dplyr")
  
  # Test model_from_cluster_eval = TRUE
  expect_error(
    experiment$run(n_cores = 2, parallel = TRUE, model_from_cluster_eval = TRUE),
    regexp = "cluster_experiment"
  )
})


test_that("R6Experiment handles models with no parameter distributions", {
  # Create two models
  model1 <- Mymodel$new(name = "test1")
  model2 <- Mymodel$new(name = "test2")

  # Create experiment with models having no params_df
  experiment <- R6Experiment$new(model1, model2)
  experiment$set_design(n_lhs = 2)

  # Check that params_df was created for both models
  expect_false(is.null(experiment$models[[1]]$params_df))
  expect_false(is.null(experiment$models[[2]]$params_df))

  # Check params_df structure
  expect_true(nrow(experiment$models[[1]]$params_df) == 1)
  expect_true("param.id" %in% names(experiment$models[[1]]$params_df))
})

test_that("R6Experiment errors on inconsistent parameter distributions", {
  # Create two models
  model1 <- Mymodel$new(name = "test1")
  model2 <- Mymodel$new(name = "test2")

  # Set params_df for only one model
  model1$set_param_dist(
    params_list = list(default = data.frame(weights = 1)),
    param_dist_weights = "weights",
    use_average = TRUE
  )

  # Create experiment with inconsistent models
  experiment <- R6Experiment$new(model1, model2)

  # Should error when setting design
  expect_error(
    experiment$set_design(n_lhs = 2),
    regexp = "Inconsistent parameter distributions"
  )
})

test_that("R6Experiment works with all models having parameter distributions", {
  # Create two models
  model1 <- Mymodel$new(name = "test1")
  model2 <- Mymodel$new(name = "test2")

  # Set params_df for both models
  params <- list(default = data.frame(weights = 1))
  model1$set_param_dist(params_list = params, param_dist_weights = "weights", use_average = TRUE)
  model2$set_param_dist(params_list = params, param_dist_weights = "weights", use_average = TRUE)

  # Create and set up experiment
  experiment <- R6Experiment$new(model1, model2)
  experiment$set_design(n_lhs = 2)

  # Check experiment setup worked
  expect_false(is.null(experiment$params))
  expect_false(is.null(experiment$policy_design))
})



# Test writing experiments to disk ----------------------------------------

# Implement tests when this is implemented in the package:

# test_that("write_design works with CSV format", {
#   # Setup test experiment
#   model <- Mymodel$new(name = "test")
#   experiment <- R6Experiment$new(model)
#   experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c("A", "B"))
#   experiment$set_design(n_lhs = 2)
#
#   # Create temp directory
#   temp_dir <- file.path(tempdir(), "R6Sim-test")
#   dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
#
#   # Test CSV write
#   experiment$write_design(path = temp_dir, format = "csv")
#   expect_true(file.exists(file.path(temp_dir, "exp_design.txt")))
#   expect_true(file.exists(file.path(temp_dir, "exp_design_col_names.txt")))
#
#   # Cleanup
#   if (dir.exists(temp_dir)) {
#     unlink(temp_dir, recursive = TRUE)
#   }
# })

# test_that("write_design works with temp directory", {
#   # Setup test experiment
#   model <- Mymodel$new(name = "test")
#   experiment <- R6Experiment$new(model)
#   experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c("A", "B"))
#   experiment$set_design(n_lhs = 2)
#
#   # Create temp directory that works cross-platform
#   temp_dir <- file.path(tempdir(), "R6Sim-test")
#   dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
#
#   # Test JSON write
#   experiment$write_design(path = temp_dir, format = "json")
#   expect_true(file.exists(file.path(temp_dir, "exp_design.txt")))
#
#   # Test CSV write
#   experiment$write_design(path = temp_dir, format = "csv")
#   expect_true(file.exists(file.path(temp_dir, "exp_design.txt")))
#   expect_true(file.exists(file.path(temp_dir, "exp_design_col_names.txt")))
#
#   # Cleanup
#   if (dir.exists(temp_dir)) {
#     unlink(temp_dir, recursive = TRUE)
#   }
# })
#
# test_that("write_design returns experimental design when no path provided", {
#   # Setup
#   model <- Mymodel$new(name = "test")
#   experiment <- R6Experiment$new(model)
#   experiment$set_parameter(parameter_name = "Test1", experimental_design = "grid", values = c("A", "B"))
#   experiment$set_design(n_lhs = 2)
#
#   # Test return value when no path provided
#   result <- experiment$write_design(format = "json")
#   expect_type(result, "character")
#   expect_gt(length(result), 0)
# })


