# This file contains tests for input handling functionalities of the R6Sim class.

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