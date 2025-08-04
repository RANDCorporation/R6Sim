library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(truncnorm)


# ---- Function: Construct Prior Table ----
construct_priors <- function(..., .list = NULL) {
  exprs <- c(list(...), .list)
  stopifnot(length(exprs) > 0)

  parse_prior <- function(expr) {
    if (!inherits(expr, "formula")) stop("Each prior must be a formula like x ~ dist(...)")

    lhs <- as_string(expr[[2]])
    call <- expr[[3]]
    dist <- as_string(call[[1]])
    args <- as.list(call[-1])

    tibble(
      name = lhs,
      dist = dist,
      params = list(args)
    )
  }

  map_dfr(exprs, parse_prior)
}

# ---- Function: Sample Priors with Constraints ----
sample_priors <- function(param_specs, n, constraints = NULL, max_attempts = 10) {
  stopifnot(is.data.frame(param_specs))
  required_cols <- c("name", "dist", "params")
  stopifnot(all(required_cols %in% names(param_specs)))

  # Sampling dispatch function
  draw_samples <- function(dist, args, n) {
    switch(dist,
           uniform = do.call(runif, c(list(n = n), args)),
           normal  = do.call(rnorm, c(list(n = n), args)),
           beta    = do.call(rbeta, c(list(n = n), args)),
           truncnorm = do.call(truncnorm::rtruncnorm, c(list(n = n), args)),
           stop(paste("Unsupported distribution:", dist))
    )
  }

  samples <- tibble()
  attempts <- 0
  while (nrow(samples) < n && attempts < max_attempts) {
    raw_samples <- param_specs %>%
      mutate(sample = map2(dist, params, ~ draw_samples(.x, .y, n))) %>%
      select(name, sample) %>%
      unnest_wider(sample, names_sep = "_") %>%
      pivot_longer(cols = starts_with("sample_"), names_to = "draw", values_to = "value") %>%
      mutate(draw = as.integer(gsub("sample_", "", draw))) %>%
      pivot_wider(names_from = name, values_from = value)

    if (!is.null(constraints)) {
      constraint_exprs <- parse_exprs(constraints)
      raw_samples <- raw_samples %>% filter(!!!constraint_exprs)
    }

    samples <- bind_rows(samples, raw_samples)
    attempts <- attempts + 1
  }

  if (nrow(samples) < n) {
    warning(paste("Only", nrow(samples), "samples returned after", max_attempts, "attempts."))
  }

  samples %>% slice_head(n = n)
}


# Construct priors
priors <- construct_priors(
  x ~ uniform(min = 2, max = 44),
  y ~ normal(mean = 5, sd = 2),
  z ~ truncnorm(a = 0, b = 10, mean = 5, sd = 1),
  w ~ beta(shape1 = 1, shape2 = 2)
)

# Sample with constraints
set.seed(42)
samples <- sample_priors(priors, n = 1000, constraints = c("x + y < 1000", "z + 1000 > x"))

print(head(samples))


