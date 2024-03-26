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
# Set Input method
# Purpose: Sets inputs objects for models based on the R6Sim class.
# Creation Date: Sept 2022
#------------------------------------------------------------------------------#

# Set input for vision_models
# See documentation within the model file.
R6Sim_set_input <- function(self, name, value, type) {

  # if input already exists, then update the table:

  if (name %in% self$inputs_table$name) {
    if (length(self$inputs[[name]]) != length(value)) {
      warning(paste0("You are replacing the input ", name, " which had length ", length(self$inputs[[name]]), " with an object of length ", length(value)))
    }
  }

  # Check if input type is supported.
  recommended_classes <- c("numeric", "integer", "logical", "character", "data.frame", "list", "grouped_df", "tbl_df", "tbl", "matrix", "array", "Date", "POSIXct", "POSIXt")

  if (!all(class(value) %in% recommended_classes)) {
    warning(paste0("Input ", name, " includes values using classes that we do not recommend because they may cause issues when we translate model inputs to and from strings. Use only integers, numerics, logical or character objects."))
  }

  # If this is a list or dataframe, also check if there aren't any weird classes within the first level of the object.
  if (any(class(value) %in% c("list", "data.frame"))) {
    if (!all(unlist(sapply(value, class)) %in% recommended_classes)) {
      warning(paste0("Input ", name, " includes values using classes that we do not recommend because they may cause issues when we translate model inputs to and from strings. Use only integers, numerics, logicals, characters or dates:"))
      warning(unlist(sapply(value, class)))
    }
  }

  # Add or update the row:
  self$inputs_table <- self$inputs_table %>%
    rows_upsert(data.frame(name = name, type = type), by = "name")

  # Update inputs values
  self$inputs[[name]] <- value

  # return invisible self because we call this function for its side effects:
  invisible(self)
}
