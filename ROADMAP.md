# R6Sim Roadmap

This is a list of features I want to have in R6Sim. This document outlines an organized plan to implement these features in the package, create tests for each feature, and establish an implementation timeline.

## Sample Models and Start-up Projects

- The package should include sample simulation models from all major model formalisms
- Tools to help users create these sample models and initialize an R6Sim project:
  - Systems dynamics / ODEs
  - Discrete event simulations
  - Markov models
  - Microsimulation
  - Regression-based Microsimulation
  - Python-based models

## Model Calibration

- Integration with Bayesian model calibration tools:
  - Make integration with Bayesian calibration tools (including IMABC) easy for users
  - R6Sim should facilitate specification of priors and calibration targets used by ABC approaches
  - Base package can include integrators with IMABC and other tools, allowing users to set up priors and calibration targets that run automatically

## Parallelization & Integration with HPC Workflows

- Implement three execution modes:
  - **Sequential**: Single-threaded execution on one computer
  - **Parallel**: Multi-threaded execution on one computer/node
  - **HPC**: Distributed execution across a computing cluster
- Include pre-made workflows for running models on clusters
- Support for simple parallel experiments using PBS or Slurm batch jobs seamlessly
- Include EMEWS workflows (or other workflow solutions) for complex parallel tasks like calibration with minimal setup overhead

## More Flexible Experiments and Experimental Designs

- Create flexible "experiments" that can include both calibration and policy simulation
- Support calibration "experiments" where multiple model versions are calibrated to the same data
- Implement specialized classes for different experiment types:
  - `CalibrationExperiment` that returns one or multiple posteriors
  - `PolicyExperiment` similar to the current experiment implementation

## Integration with Search Methods

- Make experimental design more flexible beyond just combining grid designs with LHS designs
- Support search algorithms (single-objective and many-objective)
- Implement a general interface to search methods allowing iterative addition of new methods
- Support both R and Python-based search methods:
  - Start with basic search methods available in base R
  - Expand to more specialized algorithms over time

## Python Integration

- Simplify integration of experimental design with Python algorithms
- Create easier R6Sim wrappers for Python-based models with built-in package support