
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R6Sim

<!-- badges: start -->

[![R-CMD-check](https://github.com/RANDCorporation/R6Sim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RANDCorporation/R6Sim/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`R6Sim` provides an `R6` base class for simulation modeling analyses
using the structure of encapsulated object-oriented programming provided
by `R6`. This package allows one to create models that inherit `R6Sim`,
and specify models and experimental designs that can use multiple models
using the `R6Experiment` class.

# Why `R6Sim`

Simulation modeling projects performed in R often rely on
poorly-designed code bases, where the modeler and others don’t have
visibility into what the model does and all the inputs the model needs
to run. Often, there is a tight dependence on inputs that may live in
the global environment, *or* the modeler adds an large set of function
parameters that percolate through the code and are passed through
several layers of functions.

My opinion is that one of the reasons for those problems is the overuse
of the functional programming paradigm, whereas an encapsulated
object-oriented paradigm could be helpful. In particular, I want my
simulation models to be self-contained, and I want to be able to create
simulation studies where I can extend the model to other use-cases and
create experiments with multiple versions of the same model. After doing
modeling projects in R for several years, I arrived at this simple
pattern where the model is an R6 class, and there’s another class (which
I call an experiment), that can include one *or more* models, and can
run those models.

Each model can have its own inputs, and one ore more joint probability
distributions of model parameters (sometimes obtained via bayesian
calibration). Then, all I want is to create an experiment and run it in
parallel. And I don’t want to re-create that code architecture in every
project. `R6Sim` is my current solution for this type of problem.
`R6Sim` is a generalization of `crcrdm`, which I had created for cancer
screening models.

## Installation

You can install the development version of `R6Sim` like so:

``` r
# install the remotes package if you do not have it
# install.packages("remotes")
# install from github:
remotes::install_github("randcorporation/r6sim")
```

## Usage

This package contains two `R6` classes that can be inherited in your
analysis: `R6Sim` and `R6Experiment`. Here I provide a brief
introduction of how they work. A more complete example of their use can
be seen in repositories that build models and analyses based on `R6Sim`,
such as the [value of environmental
surveillance](github.com/randcorporation/value-of-surveillance) project.

### The `R6Sim` model class

The `R6Sim` class is meant to represent an R6 simulation model. An
`R6Sim` model should be self-contained model, and once instantiated,
should immediately know how to simulate itself with the
`R6Sim$simulate()` function. Here is an example `R6Sim` model:

``` r
library(dplyr)
library(R6Sim)

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
           set_input(name = "trials",value = 10)
      
      # Can also add a parameter distribution 
      self$set_param_dist(params_list = list(param_dist_a = data.frame(sample_param = 1, weights = 1)), use_average = T, param_dist_weights = "weights")

    },

    # Sample Simulate function
    simulate = function() {

      # Create a sample population with some health events:
      self$sim_res = data.frame(p.id = 1:self$inputs$pop.size,
                           risk = rnorm(n = self$inputs$pop.size, mean = self$inputs$risk.mean, sd = self$inputs$risk.sd)) %>%
        mutate(probability.event = 1-exp(-risk)) %>%
        mutate(n.events = rbinom(n = 1:self$inputs$pop.size, size = self$inputs$trials, prob = probability.event))
      
      # Let's return the number of events as the key model input
      return(data.frame(events = sum(self$sim_res$n.events)))
    }
  )
)

model <- Mymodel$new("test model")

model$simulate()
#>   events
#> 1     11
```

Once your model is created, you can set inputs and run the model again:

``` r
# you can chain methods
model$set_input("risk.mean", 0.05)$
  simulate()
#>   events
#> 1     57
```

### The `R6Experiment` class

An `R6Experiment` can contain multiple `R6Sim` models. That class can be
used to define experiments and run them in parallel. Your experiment can
have multiple models.

``` r
experiment = R6Experiment$new(model)

experiment$
  set_parameter(parameter_name = "risk.mean",experimental_design = "grid",values = seq.default(from = 0.01, to = 0.05, by = 0.01))$
  set_design()

results <- experiment$run(parallel = F, model_from_cluster_eval = F)

results %>%
  select(risk.mean, events)
#>    risk.mean events
#>        <num>  <int>
#> 1:      0.01      9
#> 2:      0.02     15
#> 3:      0.03     25
#> 4:      0.04     32
#> 5:      0.05     43
```

Once you experiment runs, now you can iterate and add new inputs, or new
parameters to your experiment. To see a real use case with parallel
runs, see the
[value-of-surveillance](github.com/randcorporation/value-of-surveillance)
project.

## Contact

Reach out to [Pedro Nascimento de
Lima](https://www.rand.org/about/people/l/lima_pedro_nascimento_de.html)
for questions related to this repository.

## License

Copyright (C) 2023 by The [RAND Corporation](https://www.rand.org). This
repository is released as open-source software under an MIT + license.
See the LICENSE.md file.
