


#------------------------------------------------------------------------------#
#
# R6Sim: R6-based Simulation Modeling Toolkit
#
# Author: Pedro Nascimento de Lima
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# R6Sim class
# Purpose: The R6Sim class defines the structure of all R6Sim models.
# To make this file as small as possible, we only add definitions and
# documentation to the class. longer functions shall be within
# R6Sim_function_name.R files
# Creation Date: August 2022
#------------------------------------------------------------------------------#

#' R6 Class Representing an `R6Sim`
#'
#' @description
#' This class implements an `R6Sim` model.
#'
#' @import R6
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
    initialize = function(name) {
      stopifnot(is.character(name), length(name) == 1)
      self$name <- name
      # Initializing inputs objects:
      self$inputs = list()
      self$inputs_table = data.frame(name = character(), type = character())
    },

    #' @description
    #' Set Input
    #'
    #' @details
    #' Use this function to add a new input to a R6Sim object.
    #' Model inputs should be added or modified through this function. Inputs can be vectors or lists of strings, numeric or integers, to guarantee that they can be translated to and from JSON without any issues.
    #'
    #' @param name character string defining the input name
    #' @param value input value. Can be a single value, a list or a vector.
    #' @param description sentence describing the meaning of this input.
    #' @param type optional character string defining the type of input. Useful when one wants to only write inputs of a certain type to json.
    set_input = function(name, value, type = NA){
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
      stopifnot(length(source)==1)
      stopifnot(file.exists(source))

      if(is.character(source)) {

        # if using a spreadsheet:
        if(any(sapply(c("xlsx", "xls"), grepl, x = source))) {
          source_type <- "xlsx"
          sheets <- excel_sheets(source)
          new_inputs <- lapply(sheets, function(x) read_xlsx(source, sheet = x))
          names(new_inputs) <- sheets
        }

        # if using an yaml file:
        if(any(sapply(c("yaml", "yml"), grepl, x = source))) {
          source_type <- "yaml"
          new_inputs <- read_yaml(source)
        }

      }

      stopifnot(is.list(new_inputs))
      # assign each input
      for(i in names(new_inputs)) {
        self$set_input(name = i, value = new_inputs[[i]], type = source_type)
      }

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
    set_inputs_from_json = function(json){
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
    set_param_dist = function(params_list, param_dist_weights, cols_to_ignore = NULL, n_sample = 1000, use_average = F, seed = 12345678, resample = T){
      R6Sim_set_param_dist(self = self, param_dists_list = params_list, param_dist_weights = param_dist_weights, cols_to_ignore = cols_to_ignore, use_average = use_average, n_sample = n_sample, seed = seed, resample = resample)
    },

    #' @description
    #' Runs the simulation model for a single parameter set.
    #'
    #' @details
    #' This function is a wrapper around the user natural history function. It passes the `self` object and any other parameters provided to the function.
    #' @param ... any set of parameters passed to this function will be passed along to the user natural history function.
    simulate = function(...){
      stop("Simulate method must be implemented by your class.")
    },

    #' @description
    #' make priors for calibration (using the IMABC format)
    #'
    #' return an imabc priors object
    #'
    #' @param priors_file csv file name where priors should be saved, if any
    make_priors = function(priors_file){

      priors = self$inputs$priors %>%
        # clean up fixed parameters
        mutate(sd = ifelse(dist_base_name == "fixed", NA, sd),
               min = ifelse(dist_base_name == "fixed", NA, min),
               max = ifelse(dist_base_name == "fixed", NA, max),
               a = ifelse(dist_base_name == "fixed", NA, a),
               b = ifelse(dist_base_name == "fixed", NA, b)
        ) %>%
        # clean up uniform parameters:
        mutate(mean = ifelse(dist_base_name == "unif", NA,mean)) %>%
        # add a and b parameters for the truncated normal:
        mutate(a = ifelse(dist_base_name == "truncnorm", min,NA)) %>%
        mutate(b = ifelse(dist_base_name == "truncnorm", max,NA)) %>%
        # clean up additional columns:
        select(-any_of(c("ideal_dist_base_name", "reported_prior"))) %>%
        imabc::as.priors()

      if (!missing(priors_file)) {
        write.csv(as.data.frame(priors), file = priors_file)
      }

      return(priors)

    },

    #' @description
    #' make targets for calibration
    #'
    #' return an imabc targets object
    #'
    #' @param targets_file csv file name where targets should be saved, if any
    #' @importFrom imabc as.priors
    #' @importFrom imabc as.targets
    #' @importFrom imabc imabc
    make_targets = function(targets_file){

      # this name can't change.
      time_variable = "week"
      target_variables = names(self$inputs$t_tol)[-c(1,2)]

      targets_df = get_long_target_df(self$inputs$t_series, target_variables)

      tolerances_df = get_long_target_df(self$inputs$t_tol, target_variables)

      init_tolerances_df = get_long_target_df(self$inputs$t_ini_tol, target_variables)

      # create imabc target object:
      imabc_targets = data.frame(
        target_names = targets_df$target_names,
        target_groups = targets_df$target_groups,
        targets = targets_df$value,
        current_lower_bounds = targets_df$value - init_tolerances_df$value,
        current_upper_bounds = targets_df$value + init_tolerances_df$value,
        stopping_lower_bounds = targets_df$value - tolerances_df$value,
        stopping_upper_bounds = targets_df$value + tolerances_df$value
      ) %>%
        imabc::as.targets(.)

      # Write csv data for imabc:
      if (!missing(targets_file)) {
        write.csv(x = as.data.frame(imabc_targets), file = targets_file, row.names = F)
      }

      return(imabc_targets)

    },

    #' @description
    #' Simulate Model Posterior Distribution of parameters
    #'
    #' @details
    #' This function is used to simulate the model over the posterior distribution
    #' Before running this function, the user should run the set_posterior.
    #' @seealso simulate
    #' @param parallel if T, the model will run in parallel
    #' @param n_cores number of CPUs to use in the simulation. defaults to detectCores() - 2
    #' be passed along to the user natural history function.
    simulate_posterior = function(parallel = T, n_cores = detectCores() - 2){

      # Simulate posterior distribution for a specific model
      R6Sim_simulate_posterior(self = self, parallel = parallel, n_cores = n_cores)

    }

  ),

  # Use private to hold data that will not be accessed by the user directly.
  private = list(
    # Private objects are not documented and exported as R6 fields:
    #character vector with names of data.frame objects. This is used when converting objects to and from json.
    df_objects = c("inputs_table"),
    #character vector of list of objects to convert to and from
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
is.R6Sim = function(x){
  "R6Sim" %in% class(x)
}
