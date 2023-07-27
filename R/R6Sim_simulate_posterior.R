
# loading parallel and doSNOW package and creating cluster ----------------

#' Simulates Posterior Distribution for a single model
#'
#'
#' This function is most useful to simulate the posterior distribution for a single model in parallel in one machine. This function is not used when calibrating the model and not useful for parallelization across multiple nodes.
#'
#' @param self model object
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
R6Sim_simulate_posterior = function (self, n_cores, parallel, cluster_eval_script) {

  if(parallel) {
    # PNL note: Decide whether to use snow:: or parallel:: makeCluster
    cl <- snow::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    clusterEvalQ(cl, source(cluster_eval_script))
  }

  # progress bar ------------------------------------------------------------
  # Progress bar setup adapted from:
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- progress_bar$new(
    format = "R6Sim: simulating parm set = :ode_sim [:bar] :elapsed | eta: :eta",
    total = nrow(self$posterior_params),    # 100
    width = 80)

  # allowing progress bar to be used in foreach -----------------------------
  progress <- function(n){
    pb$tick(tokens = list(ode_sim = n))
  }

  opts <- list(progress = progress)

  # foreach loop ------------------------------------------------------------
  results = foreach(i = 1:nrow(self$posterior_params), .combine = rbind, .options.snow = opts) %dopar% {
    self$inputs$params = as.numeric(self$posterior_params[i,self$inputs$priors$parameter_name])
    names(self$inputs$params) = self$inputs$priors$parameter_name
    res = self$simulate()
    res$param.id = i
    return(res)
  }

  if (parallel){
    stopCluster(cl)
  }

  return(results)

}
