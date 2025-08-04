test_that("R6Sim_set_input handles replacing inputs with different lengths", {
  sim <- R6Sim$new()
  sim$inputs <- list(existing_input = c(1, 2, 3))
  sim$inputs_table <- data.frame(name = "existing_input", type = "numeric")

  expect_warning(
    sim$set_input("existing_input", c(1, 2), "numeric")
  )
})

test_that("R6Sim_set_input warns for unsupported input types", {
  sim <- R6Sim$new()
  unsupported_value <- structure(1, class = "unsupported_class")

  expect_warning(
    sim$set_input("unsupported_input", unsupported_value, "numeric")
  )
})
