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
# R6Experiment Class
# Purpose: The R6Experiment contains one or more models...
#------------------------------------------------------------------------------#

#' R6 Class Representing an `R6Experiment`
#'
#' @description
#' Manages experimental designs and execution for R6Sim models.
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

    #' @field set_seed is a T if the experiment will be controlling and setting seeds.
    set_seed = T,

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
    #' The policy experimental design will have `blocks` \* `n_lhs` \* `n_grid_points` \* `n_posterior` \* `n_reps` for each model in the experimental design.
    #'
    #' @param n_lhs The number of points in the Latin Hypercube Sample to be created.
    #' @param blocks is the number of population blocks to use to parallelize the runs across nodes.
    #' @param grid_design_df a data.frame containing a pre-existing experimental design to be used. This function will use this experimental design in lieu of parameters defined in the grid, so this effectively replaces any set of parameters that are part of a grid design.
    #' @param convert_lhs_to_grid Default is FALSE. If TRUE, this function convert the LHS parameters to "grid" parameters. This is useful when one needs to test the "corners" of the experimental design before performing a full LHS run.
    #' @param lhs_to_grid_midpoints Only relevant when convert_to_lhs = T. Default value is 0. This should be an integer determining how many points within the grid hypercube should be created for the parameters being converted from LHS to a GRID design. For example, if convert_lhs_to_grid = T and lhs_to_grid_midpoints = 0, this function will create a full factorial design of the LHS parameters with 2^n points. If one wants to use one midpoint, then the design will have 3^n points, and so on. This parameter does not affect parameters orignally defined as part of a grid design because their values have already been set.
    #' @param n_reps Number of stochastic replications for each experimental design point. Default is 1.
    #' @param set_seed Whether to set random seeds for reproducibility. If TRUE, generates unique seeds for each replication. Default is TRUE.
    set_design = function(n_lhs, blocks = 1, grid_design_df, convert_lhs_to_grid = F, lhs_to_grid_midpoints = 0, n_reps = 1, set_seed = T) {
      R6Experiment_set_design(self = self, n_lhs = n_lhs, blocks = blocks, grid_design_df = grid_design_df, convert_lhs_to_grid = convert_lhs_to_grid, lhs_to_grid_midpoints = lhs_to_grid_midpoints, n_reps = n_reps, set_seed = set_seed)
    },

    #' @description
    #' Run Experiment
    #'
    #' @param n_cores number of cores to use
    #' @param parallel whether to evaluate run in parallel
    #' @param cluster_eval_script Optional path to R script that is sourced once in each parallel process before running experiments.
    #'        Useful for model setup that should happen once per process, like:
    #'        - Loading required packages
    #'        - Compiling models (e.g. odin models)
    #'        - Setting up simulation parameters/data
    #'        - Creating model instances for use across runs
    #' @param model_from_cluster_eval If TRUE, expects model instances to be created in cluster_eval_script.
    #'        Set TRUE when model compilation is needed (like with odin).
    #' @param packages character vector of packages to be loaded before running the model in parallel.
    #' @param ... additional parameters passed to model simulation
    run = function(n_cores = 3, parallel = F, cluster_eval_script = NULL,
                   model_from_cluster_eval = F, packages = NULL, ...) {
      R6Experiment_run(self = self, n_cores = n_cores, parallel = parallel,
                       cluster_eval_script = cluster_eval_script,
                       model_from_cluster_eval = model_from_cluster_eval, packages = packages, ...)
    }),

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
