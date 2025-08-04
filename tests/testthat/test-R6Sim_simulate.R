# This file contains tests for the simulation functionalities of the R6Sim class.

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