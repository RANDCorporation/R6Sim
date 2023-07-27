
#' R6Sim: R6-based Simulation Modeling Toolkit
#'
#' This package implements the \code{\link{R6Sim}} and the \code{\link{R6Experiment}} R6 classes, providing an encapsulated object-oriented framework for simulation modeling studies.
#'
#'
#' @docType package
#' @name R6Sim.package
NULL



# dplyr is used in this package; this fixes some of the issues created by it:
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887

#' @name globalvariables definitions
#' @noRd
#' @importFrom utils globalVariables
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
utils::globalVariables(c(".", "where", "i"))
