# This file contains tests for JSON-related functionalities of the R6Sim class.

# json tests --------------------------------------------------------------

json <- model$to_json()

# Ensure `res` is defined by running the simulation on the original model
set.seed(1234)
res <- model$simulate()

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