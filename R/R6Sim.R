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
# R6Sim class
# Purpose: The R6Sim class defines the structure of all R6Sim models.
# To make this file as small as possible, we only add definitions and
# documentation to the class. longer functions shall be within
# R6Sim_function_name.R files
# Creation Date: August 2022
#------------------------------------------------------------------------------#

#' R6 Class for a Simulation Model
#'
#' @description
#' Base class for building simulation models with R6. Provides methods for managing inputs,
#' parameters, and simulation execution.
#'
#' @details
#' The R6Sim class includes functionality for:
#' * Input and parameter management
#' * JSON serialization
#' * Parallel execution
#' * Parameter sampling
#'
#' @examples
#' # Create simulation model
#' MyModel <- R6::R6Class(
#'   "MyModel",
#'   inherit = R6Sim,
#'   public = list(
#'     initialize = function(name) {
#'       super$initialize(name)
#'       self$set_input("population", 1000)
#'       self$set_input("growth_rate", 0.05)
#'     },
#'     simulate = function(...) {
#'       pop <- self$inputs$population
#'       growth <- self$inputs$growth_rate
#'       results <- pop * (1 + growth)^(1:10)
#'       return(data.frame(year = 1:10, population = results))
#'     }
#'   )
#' )
#'
#' model <- MyModel$new("pop_model")
#' results <- model$simulate()
#'
#' @export
R6Sim <- R6::R6Class(

  # Intentionally no indentation to avoid losing screen real state.
  classname = "R6Sim",
  # Use public to expose methods of this class:
  public = list(

    #' @field name is a character string representing the model name.
    name = NULL,

    #' @field inputs is a list of model inputs. One can add inputs to the object with the set_input function.
    inputs = NULL,

    #' @field inputs_table is a data.frame listing all inputs added to the model.
    inputs_table = NULL,

    #' @field params_df is a data.frame containing parameters to run the model. Not to be confused with the params vector withi the inputs list, which is what should be used by the simulation model. Each column is a parameter, each row is a parameter set. The set_posterior distribution will be used to set the posterior distribution from a bayesian calibration.
    params_df = NULL,

    #' @field sim_results is the object that will receive results from the simulation function.
    sim_results = NULL,

    #' @field simulate_fn is a function that will take as parameters self and ..., simulate the natural history of crc and returns invisible(self).
    simulate_fn = NULL,

    #' @description
    #' Create a new `R6Sim` object.
    #' @param name name of the model to be created.
    #' @return s new `R6Sim` object.
    initialize = function(name = "model name") {
      stopifnot(is.character(name), length(name) == 1)
      self$name <- name
      # Initializing inputs objects:
      self$inputs <- list()
      self$inputs_table <- data.frame(name = character(), type = character())
    },

    #' @description
    #' Set Input
    #'
    #' @param name Character string defining the input name
    #' @param value Input value. Can be a single value, list or vector.
    #' @param type Optional character string defining input type.
    #'
    #' @details
    #' Validates input types and maintains input registry.
    #' Accepts numeric, character, logical, data.frame and list inputs.
    #' Type tags enable selective JSON export.
    #'
    #' @examples
    #' model$set_input("population", 1000, type = "parameter")
    #' model$set_input("growth_rates", c(0.01, 0.02), type = "scenario")
    #' model$set_input("settings", list(iterations = 100), type = "config")
    #'
    #' @export
    set_input = function(name, value, type = NA_character_) {
      R6Sim_set_input(self = self, name = name, value = value, type = type)
    },


    #' @description
    #' Get Inputs
    #'
    #' @details
    #' Use this function to get model inputs from a spreadsheet or yaml file
    #'
    #' @param source either a path to a xlsx or yaml file, or a function that returns a named list of new inputs to be added to the model.
    #' @importFrom yaml read_yaml
    #' @importFrom readxl read_xlsx
    #' @importFrom readxl excel_sheets
    get_inputs = function(source) {
      stopifnot(is.character(source))
      stopifnot(length(source) == 1)
      stopifnot(file.exists(source))

      if (is.character(source)) {

        # if using a spreadsheet:
        if (any(sapply(c("xlsx", "xls"), grepl, x = source))) {
          source_type <- "xlsx"
          sheets <- excel_sheets(source)
          new_inputs <- lapply(sheets, function(x) read_xlsx(source, sheet = x))
          names(new_inputs) <- sheets
        }

        # if using an yaml file:
        if (any(sapply(c("yaml", "yml"), grepl, x = source))) {
          source_type <- "yaml"
          new_inputs <- read_yaml(source)
        }
      }

      stopifnot(is.list(new_inputs))
      # assign each input
      for (i in names(new_inputs)) {
        self$set_input(name = i, value = new_inputs[[i]], type = source_type)
      }

      invisible(self)
    },

    #' @description
    #' Converts a `R6Sim` to a JSON string
    #'
    #' @param input_types vector of types of input to include in the json object.
    #'
    #' @return a JSON string containing the R6Sim objects that should be exported
    to_json = function(input_types) {
      R6Sim_to_json(self = self, private = private, types = input_types)
    },

    #' @description
    #' Set model Inputs from JSON string
    #'
    #' @details
    #' Use this function to set model inputs from a JSON string. Note that the posterior distribution is not included in the json strong of model inputs.
    #'
    #' @param json a JSON string generated by the model_to_json function
    set_inputs_from_json = function(json) {
      R6Sim_set_inputs_from_json(self = self, json = json)
    },

    #' @description
    #' Set distribution of model parameters
    #'
    #' @details
    #' Use this function to set a distribution of model parameters to the model. This is useful when we want to sample from a posterior distribution of model parameters.
    #'
    #' @param params_list named list of one more more data.frames containing a distribution of model parameters.
    #' @param param_dist_weights character indicating the name of the column that contain weights to be used when sampling from the posterior
    #' @param cols_to_ignore character vector of columns name to ignore. This is useful when the posterior files contain columns that are not parameters and are not inputs to the model.
    #' @param n_sample the size of the sample to take from each posterior file.
    #' @param use_average T if one wants to use the average value of all parameters rather than the mean
    #' @param seed random seed to use when sampling from the posterior
    #' @param resample if T, samples from the posterior using the posterior weights. Otherwise, returns the full posterior, and preservers the weights.
    set_param_dist = function(params_list, param_dist_weights, cols_to_ignore = NULL, n_sample = 1000, use_average = F, seed = 12345678, resample = T) {
      R6Sim_set_param_dist(self = self, param_dists_list = params_list, param_dist_weights = param_dist_weights, cols_to_ignore = cols_to_ignore, use_average = use_average, n_sample = n_sample, seed = seed, resample = resample)
    },

    #' @description
    #' Runs the simulation model for a single parameter set.
    #'
    #' @details
    #' The simulate method should be used to simulate a model run. All inputs used by the model should already have been defined before this function is called.
    #' @param ... any set of parameters passed to this function will be passed along to the model simulate function.
    simulate = function(...) {
      stop("Simulate method must be implemented by your class.")
    },

    #' @description
    #' Sets parameters for a current run.
    #'
    #' @details
    #' This function is a wrapper around the user natural history function. It passes the `self` object and any other parameters provided to the function.
    #' @param ... any set of parameters passed to this function will be passed along to the user natural history function.
    setup_run = function(...) {
      stop("Setup_run method must be implemented by your class.")
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

#' Checks if object is a `R6Sim`.
#'
#' @param x the model object
#'
#' @return TRUE if object is a R6Sim
#' @export
is.R6Sim <- function(x) {
  "R6Sim" %in% class(x)
}
