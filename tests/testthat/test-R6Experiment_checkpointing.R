# Test for checkpointing functionality in R6Experiment
library(testthat)
library(future)
library(doFuture)

# Register multisession for parallel processing
registerDoFuture()

# Assign the parallel backend function
backend_fn <- R6Sim:::get_parallel_backend_fn()

# Helper function to create a mock R6Experiment object
create_mock_experiment <- function() {
  R6Experiment$new(model)
}

test_that("Checkpointing saves and resumes correctly", {
  # Create a mock experiment
  experiment <- create_mock_experiment()

  full_design <- data.frame(some_param = 1:100)

  # Mock policy design
  experiment$set_design(grid_design_df = full_design)

  # Temporary directory for checkpoints
  checkpoint_dir <- tempfile()

  length(unique(experiment$policy_design$policy.exp.id))

  # Run experiment with checkpointing
  results <- experiment$run(checkpoint_frequency = 10, checkpoint_dir = checkpoint_dir)

  # Check if checkpoint files are created
  checkpoint_files <- list.files(checkpoint_dir, pattern = "*.rds", full.names = TRUE)
  expect_true(length(checkpoint_files) > 0)

  # Resume from the last checkpoint
  last_checkpoint <- checkpoint_files[length(checkpoint_files)]

  checkpoint_experiment <- readRDS(last_checkpoint)

  # Update the design to add new runs

  checkpoint_experiment$set_design(grid_design_df = full_design)

  # Here, it should start from where we left
  resumed_results <- checkpoint_experiment$run(checkpoint_frequency = 2, checkpoint_dir = checkpoint_dir)

  # Ensure results are consistent
  expect_equal(nrow(resumed_results), nrow(experiment$policy_design))

  # Clean up checkpoint directory
  unlink(checkpoint_dir, recursive = TRUE)
  expect_false(dir.exists(checkpoint_dir))
})
