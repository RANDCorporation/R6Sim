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
