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
# Run an R6Experiment
# Purpose: Runs an R6Experiment in parallel, assuming each input assigned
# maps to one model input
#------------------------------------------------------------------------------#

#' Runs R6Experiment in parallel
#'
#'
#' This function is most useful to simulate the posterior distribution for a single model in parallel in one machine. This function is not used when calibrating the model and not useful for parallelization across multiple nodes.
#'
#' @param self experiment object
#' @param n_cores number of cores to use
#' @param parallel whether to evaluate run in parallel
#' @param cluster_eval_script path to script that instantiates necessary functions. this will often mean sourcing functions external to the package and loading dependencies for the model. needed if parallel = T
#' @param model_from_cluster_eval T if model is instantiated in the cluter eval scripts, F otherwise. Use T if using models that need compilation (like odin) and F otherwise.
#' @param cluster_type either "FORK" or "PSOCK".
#' @param ... additional parameters to be passed to the model simulation function.
#' @return results data.frame from all simulations in parallel
#'
#' @import parallel
R6Experiment_run <- function(self, n_cores, parallel, cluster_eval_script, model_from_cluster_eval, cluster_type = "PSOCK", ...) {

  # If we are running the experiment in parallel, we need to instantiate the models explicitly within each "node".
  # if the model is self-contained, that should be enough.
  if (parallel) {

    # Make cluster and evaluate the cluster eval script.
    # With parallel
    cl <- parallel::makeCluster(n_cores, type = cluster_type)
    parallel::clusterExport(cl, "cluster_eval_script", envir = environment())
    parallel::clusterEvalQ(cl, source(cluster_eval_script))

  }

  # progress bar ------------------------------------------------------------
  # Progress bar setup adapted from:
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  # pb <- progress_bar$new(
  #   format = "R6Sim: experiment = :experiment [:bar] :elapsed | eta: :eta",
  #   total = nrow(self$policy_design), # 100
  #   width = 80
  # )

  # allowing progress bar to be used in foreach -----------------------------
  # progress <- function(n) {
  #   pb$tick(tokens = list(experiment = n))
  # }

  # opts <- list(progress = progress)

  # foreach loop ------------------------------------------------------------

  #browser()

  if (parallel) {

    # Using parLapply and not foreach due to licensing issues.
    results <- parLapply(cl = cl, X = 1:nrow(self$policy_design), fun = run_single_experiment, model_from_cluster_eval = model_from_cluster_eval, self = self, parallel = parallel, ...)

    parallel::stopCluster(cl)
  } else {
    results <- lapply(X = 1:nrow(self$policy_design), FUN = run_single_experiment, model_from_cluster_eval = model_from_cluster_eval, self = self, parallel = parallel, ...)
  }

  return(do.call(rbind, results))
}

run_single_experiment <- function(policy_design_id, self, model_from_cluster_eval, parallel, ...) {

  if (!model_from_cluster_eval | !parallel) {
    model <- self$models[[self$policy_design$model.id[policy_design_id]]]
  } else {
    stopifnot("cluster_experiment object not defined. Create an R6Experiment object called cluster_experiment in your cluster_eval_script file, containing the models used in this analysis." = exists("cluster_experiment"))

    stopifnot("cluster_experiment object is not an R6Experiment. Make sure to use R6Experiment to create the cluster_experiment object." = is.R6Experiment(cluster_experiment))

    model <- cluster_experiment$models[[self$policy_design$model.id[policy_design_id]]]
  }

  id_cols <- c("grid.id", "lhs.id", "params_design.id", "param.id", "model.id", "all.params.id", "policy.exp.id")

  scenario_inputs <- self$policy_design[policy_design_id, ] %>%
    select(-any_of(id_cols)) %>%
    as.data.frame()

  # Set each input
  for (var in names(scenario_inputs)) {
    model$set_input(var, scenario_inputs[, var])
  }

  res <- model$simulate(...)


  #browser()
  # This cbind might be the issue, it might not be handling the policy design appropriately.
  return(cbind(self$policy_design[policy_design_id, ], res))

}

