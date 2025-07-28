# This helper file contains shared setup code for R6Sim tests.
library(dplyr)
library(tidyr)

# Sample model:
Mymodel <- R6::R6Class(
  classname = "Mymodel",
  inherit = R6Sim,
  public = list(
    sim_res = NULL,

    # Custom Initialize function
    initialize = function(name) {
      super$initialize(name = name)
      self$set_input("pop.size", 100)$
        set_input("risk.mean", 0.01)$
        set_input("risk.sd", 0.001)$
        set_input(name = "trials", value = 10)
    },

    # Sample Simulate function
    simulate = function() {

      # Create a sample population with some health events:
      sim_res <- data.frame(
        p.id = 1:self$inputs$pop.size,
        risk = rnorm(n = self$inputs$pop.size, mean = self$inputs$risk.mean, sd = self$inputs$risk.sd)
      ) %>%
        mutate(probability.event = 1 - exp(-risk)) %>%
        mutate(n.events = rbinom(n = 1:self$inputs$pop.size, size = self$inputs$trials, prob = probability.event)) %>%
        group_by() %>%
        summarise(n.events = sum(n.events))

      invisible(sim_res)
    }
  )
)

# Creating a model object -------------------------------------------------

# Creates a model object and gives it a name.
model <- Mymodel$new(name = "test")
