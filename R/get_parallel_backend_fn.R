# Helper function to get the parallel backend function
get_parallel_backend_fn <- function() {
  backend <- getOption("parallel_backend", "multicore")
  if (backend == "multicore") {
    future::multicore
  } else if (backend == "multisession") {
    future::multisession
  } else if (backend == "sequential") {
    future::sequential
  } else {
    stop("Unsupported parallel backend: ", backend)
  }
}