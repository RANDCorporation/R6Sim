

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
      self$sim_res <- data.frame(
        p.id = 1:self$inputs$pop.size,
        risk = rnorm(n = self$inputs$pop.size, mean = self$inputs$risk.mean, sd = self$inputs$risk.sd)
      ) %>%
        mutate(probability.event = 1 - exp(-risk)) %>%
        mutate(n.events = rbinom(n = 1:self$inputs$pop.size, size = self$inputs$trials, prob = probability.event))

      invisible(self)
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

model$simulate()

test_that("simulate works", {
  expect_equal(object = nrow(model$sim_res), expected = model$inputs$pop.size)
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
  new_model$simulate()

  expect_identical(model$sim_res, new_model$sim_res)
})


# set_input tests ---------------------------------------------------------
#
# test_that("set_inputs handles unusual inputs", {
#
#   expect_error(model$set_input())
#
#   # Unusual data-types
#   expect_warning(model$set_input(name = "some_date", value = as.Date("2021-01-01")))
#
#   expect_warning(model$set_input(name = "some_list", value = list(a = as.Date("2021-01-01")) ))
#
#   # objects with different lengths:
#   expect_warning(model$set_input(name = "pop.size", value = c(1,2,3)))
#
#   # lists with nested values:
#
# })



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


# complete tests that involve writing the experiment to disk:
#
# test_that("to_JSON returns a list with the experiment", {
#   json_exp = experiment$write_design()
#   expect_true(length(json_exp) == 2)
# })
#
# test_that("write_design can write to a file", {
#   experiment$write_design(path = "./json-test")
#
#   expect_true(file.exists("./json-test/policy_design.txt"))
#   expect_true(file.exists("./json-test/nh_design.txt"))
#   file.remove("./json-test/policy_design.txt")
#   file.remove("./json-test/nh_design.txt")
#   file.remove("./json-test/")
# })
#
# test_that("write_design also can write to csv", {
#   experiment$write_design(path = "./json-test", design = "policy", format = "csv")
#   expect_true(file.exists("./json-test/policy_design.txt"))
#   expect_true(file.exists("./json-test/policy_design_col_names.txt"))
#   file.remove("./json-test/policy_design.txt")
#   file.remove("./json-test/policy_design_col_names.txt")
#   file.remove("./json-test/")
# })
#
#
