#------------------------------------------------------------------------------#
# R6Sim: R6-based Simulation Modeling Toolkit
# Copyright (C) 2024 by The RAND Corporation
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# See LICENSE.md and README.md for more information on usage and licensing
#
# Author: Pedro Nascimento de Lima
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Set Experimental Design Function
# Purpose: Defines the Experimental Design table.
# Creation Date: September 2021
#------------------------------------------------------------------------------#

# Functions for Running Experiments ---------------------------------------

# See documentation within the R6Experiment class.
#' @importFrom lhs randomLHS
#' @import dplyr
#' @importFrom stats qunif
#' @importFrom data.table as.data.table data.table
R6Experiment_set_design <- function(self, n_lhs, blocks, grid_design_df, convert_lhs_to_grid, lhs_to_grid_midpoints) {

  # convert LHS parameters to grid if requested:
  if (convert_lhs_to_grid) {
    # Convert all lhs parameters to grid
    self$experimental_parameters <- lapply(X = self$experimental_parameters, FUN = convert_lhs_param_to_grid, lhs_to_grid_midpoints = lhs_to_grid_midpoints)
  }


  ## Getting a Data.Frame of LHS Parameters
  lhs_params <- do.call(rbind.data.frame, Filter(f = function(a) a$experimental_design == "lhs", self$experimental_parameters))

  grid_lhs_params <- Filter(f = function(a) a$experimental_design == "grid_lhs", self$experimental_parameters) %>%
    sapply(., function(a) a[3]) %>%
    expand.grid(.)


  # Only sample lhs if there is one LHS variable:
  if (nrow(lhs_params) > 0) {

    # Obtan an LHS sample with the lhs::randomLHS sample.
    lhs_sample <- lhs::randomLHS(n = n_lhs, k = nrow(lhs_params)) %>%
      as.data.frame()

    names(lhs_sample) <- lhs_params$parameter_name

    lhs_experiments <- list()

    for (param in names(lhs_sample)) {
      # Use a uniform quantile to translate the random LHS to actual values.
      lhs_experiments[[param]] <- qunif(p = lhs_sample[, param], min = lhs_params$min[lhs_params$parameter_name == param], max = lhs_params$max[lhs_params$parameter_name == param])
    }

    lhs_experiments <- lhs_experiments %>%
      as.data.frame(.) %>%
      mutate(lhs.id = row_number())
  } else if (nrow(grid_lhs_params) > 0) {

    # create grid with "Lhs parameters", but using a grid desing. Useful to distinguish uncertainties from policies:
    lhs_experiments <- grid_lhs_params %>%
      mutate(lhs.id = row_number())

    names(lhs_experiments) <- sub(pattern = ".values", replacement = "", x = names(lhs_experiments))
  } else {
    # lhs Experiments is a single experiment with no variable:
    lhs_experiments <- data.frame(lhs.id = 1)
  }


  ## Getting a Data.Frame of Grid Parameters:
  # use the grid_design_df if this data.frame was provided
  if (!missing(grid_design_df)) {
    grid_params <- as.data.frame(grid_design_df)

    # if grid_design_df was not provided, then
  } else {
    grid_params <- Filter(f = function(a) a$experimental_design == "grid", self$experimental_parameters) %>%
      sapply(., function(a) a[3]) %>%
      expand.grid(.)
  }

  # If there are no grid parameters, then there's only one point in the grid.
  if (nrow(grid_params) > 0) {
    grid_params <- grid_params %>%
      mutate(grid.id = row_number())
  } else {
    grid_params <- data.frame(grid.id = 1)
  }

  # Getting Rid of the .values appendix
  names(grid_params) <- sub(pattern = ".values", replacement = "", x = names(grid_params))


  # Obtaining a table for models and their parameters in the params:
  models_df <- data.frame(model.name = sapply(self$models, "[[", "name")) %>%
    dplyr::mutate(model.id = dplyr::row_number())

  # Assign model ids to their params tables:
  for (model_id in models_df$model.id) {
    # Assign the model id to the params table:
    self$models[[model_id]]$params_df$model.id <- model_id
  }

  # This is a very compact way of getting exactly two columns that are within the self$models objects.
  # param.id is the parameter id within each model
  # all.params.id is an id referring to the experiment design. nrow(all_models_params) = max(all.params.id)
  all_models_params <- do.call(rbind, lapply(lapply(self$models, "[[", "params_df"), get_ids_from_params)) %>%
    dplyr::mutate(all.params.id = dplyr::row_number())

  # params Experimental Design:
  # The params experimental design defines the design to be run including only variation in the params distribution of the model.

  params_design <- all_models_params %>%
    dplyr::mutate(params_design.id = row_number())

  # Policy Experimental Design:
  # The Policy design is the combination of all experimental parameters:
  policy_design <- expand.grid(grid_params$grid.id, lhs_experiments$lhs.id, params_design$params_design.id)
  names(policy_design) <- c("grid.id", "lhs.id", "params_design.id")

  policy_design <- policy_design %>%
    dplyr::left_join(params_design, by = "params_design.id")

  # Assert that the Names of Alternative tables don't collide.
  all_collumns <- c(names(params_design), names(lhs_experiments), names(grid_params), "block.id")

  duplicated_names <- all_collumns[duplicated(all_collumns)]
  assertthat::assert_that(
    {
      length(duplicated_names) == 0
    },
    msg = paste0("The names of these parameters are duplicated: ", duplicated_names)
  )

  # Setting all params object:
  self$params <- all_models_params
  self$grid <- grid_params
  self$lhs <- lhs_experiments
  self$blocks <- blocks

  # Defining the full experimental design table (it doesn't include parameters in the params because those can be different by model)
  policy_design <- policy_design %>%
    left_join(grid_params, by = "grid.id") %>%
    left_join(lhs_experiments, by = "lhs.id") %>%
    mutate(policy.exp.id = row_number())

  # Save Experimental Design as a data.tables and json objects:

  # For the Natural history design:
  self$params_design <- data.table::as.data.table(params_design)

  # For the policy design
  self$policy_design <- data.table::as.data.table(policy_design)

  invisible(self)
}


# Auxiliary functions -----------------------------------------------------

# converts an lhs-based design to a grid-based design.
convert_lhs_param_to_grid <- function(parameter, lhs_to_grid_midpoints) {
  if (parameter$experimental_design == "lhs") {
    parameter$experimental_design <- "grid_lhs"
    parameter$values <- seq.default(from = parameter$min, to = parameter$max, length.out = 2 + lhs_to_grid_midpoints)
    # Clearing min and max values after using them
    parameter$min <- NULL
    parameter$max <- NULL
  }
  parameter
}

# Creating a table with all models params and their parameters ids.
# This auxiliary function selects two columns from the params:
get_ids_from_params <- function(params) {
  params[, c("param.id", "model.id")]
}
