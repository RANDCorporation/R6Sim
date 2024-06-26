

#------------------------------------------------------------------------------#
#
# R6Sim: R6-based Simulation Modeling Toolkit
# Copyright (C) 2024 by The RAND Corporation
# Author: Pedro Nascimento de Lima
# See LICENSE.md and README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Set Experimental Parameter
# Purpose: Defines an Experimental Parameter to be used in an
# experimental design.
# Creation Date: Sept 2022
#------------------------------------------------------------------------------#

# Set Parameter
# See set_parameter documentation within the R6Experiment class.
R6Experiment_set_parameter <- function(self, parameter_name, experimental_design, values, min, max) {

  # Checking if Parameters make sense.
  assertthat::assert_that(is.character(parameter_name), msg = "Parameter name should be a character string.")

  assertthat::assert_that(experimental_design %in% c("lhs", "grid"), msg = "Experimental design should either be lhs or grid.")

  if (experimental_design == "lhs") {
    assertthat::assert_that(!missing(min) & !missing(max), msg = "min and max parameters should be provided in an lhs sample.")

    assertthat::assert_that(missing(values), msg = "Values parameter should not be provided for an LHS sample.")

    assertthat::assert_that(min < max, msg = "Minimum value should lower than the maximum value. Use grid to simulate a single value.")
  } else if (experimental_design == "grid") {
    assertthat::assert_that(missing("min") & missing(max), msg = "min and max parameters should not be provided in a grid sample.")

    assertthat::assert_that(!missing(values), msg = "Values parameter should be provided in a grid experimental design.")
  }

  # The experimental design object is created here as a list aiming to allow us to pass any length of values in parameters that have a "grid" experimental design.
  if (experimental_design == "lhs") {
    self$experimental_parameters[[parameter_name]] <- list(
      parameter_name = parameter_name,
      experimental_design = experimental_design,
      min = min,
      max = max
    )
  }

  if (experimental_design == "grid") {
    self$experimental_parameters[[parameter_name]] <- list(
      parameter_name = parameter_name,
      experimental_design = experimental_design,
      values = values
    )
  }

  return(invisible(self))
}
