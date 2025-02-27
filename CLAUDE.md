# R6Sim Development Guide

## Commands
- `devtools::load_all()` - Load package for development
- `devtools::document()` - Update documentation
- `devtools::build_manual()` - Build package manual
- `attachment::att_amend_desc()` - Fix dependencies
- `testthat::test_file("tests/testthat/test-test_R6Sim.R")` - Run specific test file
- `devtools::test()` - Run all tests

## Code Style
- **Classes**: PascalCase (R6Sim, R6Experiment)
- **Methods/Functions**: snake_case (set_input, run_simulation)
- **Parameters/Variables**: dot.case (pop.size, risk.mean, param.id)
- **Imports**: Listed in DESCRIPTION, managed via attachment package
- **Method Chaining**: Object methods use chainable syntax with `$` operator
- **Data Manipulation**: Use dplyr pipes (`%>%`) and functions (bind_rows, bind_cols)

## Dependencies
- Core: R6, assertthat, data.table, dplyr, jsonlite, magrittr, purrr
- Testing: testthat (>= 3.0.0)