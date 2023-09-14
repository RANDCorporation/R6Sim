

#------------------------------------------------------------------------------#
#
# R6Sim: R6-based Simulation Modeling Toolkit
#
# Author: Pedro Nascimento de Lima
# See LICENSE.txt and README.txt for information on usage and licensing
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
#'
#' @return results data.frame from all simulations in parallel
#'
#' @import parallel
#' @import doSNOW
#' @import foreach
#' @import progress
R6Experiment_run <- function(self, n_cores, parallel, cluster_eval_script) {

  # TODO: Continue here.
  # browser()

  if (parallel) {
    # PNL note: Decide whether to use snow:: or parallel:: makeCluster

    browser()

    cl <- parallel::makeCluster()

    cl <- snow::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    #snow::clusterExport(cl, "cluster_eval_script")
    #snow::clusterEvalQ(cl, source(cluster_eval_script))
    snow::clusterEvalQ(cl, source("./R/scripts/cluster_eval.R"))
  }

  # progress bar ------------------------------------------------------------
  # Progress bar setup adapted from:
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- progress_bar$new(
    format = "R6Sim: experiment = :experiment [:bar] :elapsed | eta: :eta",
    total = nrow(self$policy_design), # 100
    width = 80
  )

  # allowing progress bar to be used in foreach -----------------------------
  progress <- function(n) {
    pb$tick(tokens = list(experiment = n))
  }

  opts <- list(progress = progress)

  # foreach loop ------------------------------------------------------------
  results <- foreach(i = 1:nrow(self$policy_design), .combine = rbind, .options.snow = opts) %dopar% {

    # Assign model inputs:
    model <- self$models[[self$policy_design$model.id[i]]]

    id_cols <- c("grid.id", "lhs.id", "params_design.id", "param.id", "model.id", "all.params.id", "policy.exp.id")

    scenario_inputs <- self$policy_design[i,] %>%
      select(-any_of(id_cols)) %>%
      as.data.frame()

    # Set each input
    for(var in names(scenario_inputs)) {
      model$set_input(var, scenario_inputs[,var])
    }

    res <- model$simulate()

    return(cbind(self$policy_design[i,],res))

  }

  if (parallel) {
    stopCluster(cl)
  }

  return(results)
}
