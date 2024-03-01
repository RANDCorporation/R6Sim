
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
remotes::install_github("randcorporation/r6sim",build_vignettes = T)
```

## Usage

See the introductory vignette:

``` r
vignette("introduction", package = "R6Sim")
```

## Contact

Reach out to [Pedro Nascimento de
Lima](https://www.rand.org/about/people/l/lima_pedro_nascimento_de.html)
for questions related to this repository.

## License

Copyright (C) 2023 by The [RAND Corporation](https://www.rand.org). This
repository is released as open-source software under an MIT + license.
See the LICENSE.md file.
