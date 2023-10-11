

#------------------------------------------------------------------------------#
#
# R6Sim: R6-based Simulation Modeling Toolkit
#
# Author: Pedro Nascimento de Lima
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# R6Experiment Class
# Purpose: The R6Experiment contains one or more models...
#------------------------------------------------------------------------------#

#' R6 Class Representing a `R6Experiment`
#'
#' @description
#' This class implements a `R6Experiment`.
#'
#' @import R6
#' @export
R6Experiment <- R6::R6Class(
  classname = "R6Experiment",
  # Use public to expose methods of this class:
  public = list(

    #' @field models is a list containing R6Sim objects.
    models = NULL,

    #' @field blocks number of population blocks for cases when we want to paralellize individual-level simulations.
    blocks = NULL,

    #' @field exp_design is a data.frame containing one row per experiment to be run.
    exp_design = NULL,

    #' @field grid is a data.frame containing one row per point in the grid experimental design.
    grid = NULL,

    #' @field lhs is a table containing one row per point in the Latin Hypercube experimental design.
    lhs = NULL,

    #' @field params is a data.frame containing one row per parameter set defined in the params object of each model included in the experiment.
    params = NULL,

    # Note: params design is the old natural history design, and policy design is the policy desing.
    #' @field params_design is a data.frame containing one row per parameter set defined in the params object of each model included in the experiment.
    params_design = NULL,

    #' @field policy_design is a data.frame containing one row per policy experiment.
    policy_design = NULL,

    #' @field experimental_parameters is a list containing details about each experimental parameter. Experimental parameters can be either policy levers or uncertainties. Defining this distinction is up to the user.
    experimental_parameters = list(),

    #' @description
    #' This function is used to initialize a `R6Experiment` object. This object represents an experiment that will be run and can encompass multiple models.
    #' @param ... set of R6Sim to be included in the experiment. One `R6Experiment` can contain multiple models of the `c19model` class.
    #' @return a new `R6Experiment` object.
    initialize = function(...) {
      self$models <- list(...)
    },

    #' @description
    #' Set Experimental Parameter for the Experiment
    #'
    #' @details
    #' This function constructs the experimental_parameter object, and appends experimental parameters that will be visible inside the model in the future.
    #' Experimental parameters can be either uncertainties or decision levers.
    #' Every parameter defined in this function can be accessed within the model by using \code{experimental_parameters$param_name}.
    #'
    #' @param parameter_name character string defining the parameter name.
    #' @param experimental_design Either "grid" or "lhs" Use lhs if you want to create a Latin Hypercube Sample within the min and max bounds you provided. Use Grid
    #' @param values use when experimental_design = "grid". This should be a vector including the values to be included in a grid experimental design. Please use parameters and values that can be converted to strings without any issues.
    #' @param min use when experimental_design = "lhs". This should be a numeric value indicating the minimum bound in the Latin Hypercube sample.
    #' @param max use when experimental_design = "lhs". This should be a numeric value indicating the minimum bound in the Latin Hypercube sample.
    set_parameter = function(parameter_name, experimental_design, values, min, max) {
      R6Experiment_set_parameter(self = self, parameter_name = parameter_name, experimental_design = experimental_design, values = values, min = min, max = max)
    },

    #' @description
    #' Set Experimental Design
    #'
    #' @details
    #' Creates two data.frames that represent the experimental design" the `exp_design` for natural history experiments and the `policy_design` for policy experiments. These experimental designs are created based on the parameters defined by the set_parameter functions. The experimental design created by this function is useful to run a typical RDM analysis where each policy is evaluated across a LHS of deep uncertainties. To achieve that, define each policy lever as a grid parameter, and each uncertainty as an "lhs" uncertainty. Natural history uncertainties are often already defined in the model's posterior file and are also considered.
    #' The natural history design will have `n_posterior` runs for each model in the experimental design.
    #' The policy experimental design will have `blocks` \* `n_lhs` \* `n_grid_points` \* `n_posterior` for each model in the experimental design.
    #'
    #' @param n_lhs The number of points in the Latin Hypercube Sample to be created.
    #' @param blocks is the number of population blocks to use to parallelize the runs across nodes.
    #' @param grid_design_df a data.frame containing a pre-existing experimental design to be used. This function will use this experimental design in lieu of parameters defined in the grid, so this effectively replaces any set of parameters that are part of a grid design.
    #' @param convert_lhs_to_grid Default is FALSE. If TRUE, this function convert the LHS parameters to "grid" parameters. This is useful when one needs to test the "corners" of the experimental design before performing a full LHS run.
    #' @param lhs_to_grid_midpoints Only relevant when convert_to_lhs = T. Default value is 0. This should be an integer determining how many points within the grid hypercube should be created for the parameters being converted from LHS to a GRID design. For example, if convert_lhs_to_grid = T and lhs_to_grid_midpoints = 0, this function will create a full factorial design of the LHS parameters with 2^n points. If one wants to use one midpoint, then the design will have 3^n points, and so on. This parameter does not affect parameters orignally defined as part of a grid design because their values have already been set.
    set_design = function(n_lhs, blocks = 1, grid_design_df, convert_lhs_to_grid = F, lhs_to_grid_midpoints = 0) {
      R6Experiment_set_design(self = self, n_lhs = n_lhs, blocks = blocks, grid_design_df = grid_design_df, convert_lhs_to_grid = convert_lhs_to_grid, lhs_to_grid_midpoints = lhs_to_grid_midpoints)
    },

    #' @description
    #' Write Experimental design to disk
    #'
    #' @details
    #' Creates a data.frame in which each row represents a single experiment. The json object included in each row contains all information that the models need to re-create themselves in the server-side in a HPC workflow.
    #'
    #' @param path folder where json experimental designs should be saved. Do not specify a file name. If missing, the function will return the design specified below.
    #' @param write_inputs if TRUE (default), writes model inputs to json. Might be unnecessary when inputs are set in the model run script.
    #' @param format "json" or "csv". the natural history design must be written to json, whereas the policy design can be written to json or csv.
    write_design = function(path, write_inputs = T, format = c("json", "csv")) {

      # Checking arguments, selecting defaults:
      format <- match.arg(format)

      dir.create(path, showWarnings = F)

      file_name <- paste0(path, "/exp_design.txt")

      if (format == "json") {
        json_exp_design <- R6Experiment_to_json(
          self = self,
          experimental_design = self$exp_design,
          write_inputs = write_inputs
        )

        message(paste0(
          "Writing Experimental Design JSON File with ",
          nrow(json_exp_design), " json rows."
        ))

        write.table(
          x = json_exp_design,
          file = file_name,
          row.names = F, col.names = F, quote = F
        )
      }

      if (format == "csv") {
        csv_exp_design <- R6Experiment_to_csv(
          self = self,
          experimental_design = self$exp_design,
          write_inputs = write_inputs
        ) %>%
          mutate(across(where(is.logical), .fns = ~ as.numeric(.x)))

        message(paste0(
          "Writing Experimental Design CSV File with ",
          nrow(csv_exp_design),
          " rows."
        ))

        # Write file:
        write.table(
          x = csv_exp_design,
          file = file_name,
          row.names = F, col.names = F, append = F, sep = ","
        )

        # Write column names:
        write.table(
          x = names(csv_exp_design),
          file = paste0(path, "/exp_design_col_names.txt"),
          row.names = F, col.names = F, append = F, sep = ","
        )
      }
    },

    run = function(n_cores = 3, parallel = F, cluster_eval_script, model_from_cluster_eval = T) {
      R6Experiment_run(self = self, n_cores = n_cores, parallel = parallel, cluster_eval_script = cluster_eval_script, model_from_cluster_eval = model_from_cluster_eval)
    }
  ),

  # Use private to hold data that will not be accessed by the user directly.
  private = list(
    # Private objects are not documented and exported as R6 fields:
    # character vector with names of data.frame objects. This is used when converting objects to and from json.
    df_objects = c("inputs_table"),
    # character vector of list of objects to convert to and from
    json_objects = c("inputs", "inputs_table")
  ),
  # Use active binding functions to get and set the private data
  active = list()
  # Initialize function is used to initialize the object:
)


#' Checks if object is a `R6Experiment`.
#'
#' @param x the object
#'
#' @return TRUE if object is a `R6Experiment`
#' @export
is.R6Experiment <- function(x) {
  "R6Experiment" %in% class(x)
}
