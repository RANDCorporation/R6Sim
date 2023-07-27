

#------------------------------------------------------------------------------#
#
# CRCRDM: Robust Decision Making tools for Colorectal Cancer Screening Models
#
# Author: Pedro Nascimento de Lima
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Set param_dist Method
# Purpose: Samples from multiple param_dist files and defines parameters
# from the param_dist distribution.
# Creation Date: July 2021
#------------------------------------------------------------------------------#

# See documentation in the R6Sim class.
R6Sim_set_param_dist = function(self, param_dists_list, param_dist_weights, cols_to_ignore, use_average, n_sample, seed, resample) {

  # Setting a seed for reproducibility because this function will create a sample:
  if(!missing(seed)){
    set.seed(seed = seed)
  }

  # Checking Inputs:

  # param_dist is a list:
  assertthat::assert_that(is.list(param_dists_list),msg = "param_dists_list object should be a list.")

  # it only contains data.frames:
  assertthat::assert_that(all(sapply(param_dists_list, is.data.frame)),msg = "param_dists_list object should be a list.")

  # every data.frame contains exactly the same columns:
  names_list = lapply(param_dists_list, names)

  # all names of the data.frames are exactly the same:
  assertthat::assert_that(all(sapply(names_list, identical, names_list[[1]])), msg = "All dataframes in the param_dist_lists should have the same parameters. each parameter data.frame must have the same names.")
  assertthat::assert_that(all(sapply(names_list, base::setequal, names_list[[1]])), msg = "All dataframes in the param_dist_lists should have the same parameters. each parameter data.frame must have the same names.")

  # param_dist_weights must be provided:
  assertthat::assert_that(!missing(param_dist_weights), msg = "param_dist_weights parameter must be provided.")

  # param_dist_weights needs to be a character:
  assertthat::assert_that(is.character(param_dist_weights), msg = "param_dist_weights should be a character defining the collumn name of the weights to use to sample from the param_dist.")

  # It should be a single character
  assertthat::assert_that(length(param_dist_weights) == 1, msg = "param_dist_weights should be a single character")

  # it also needs to exist in the param_dist file - we already checked all of them are the same, so we can just check the first one.
  assertthat::assert_that(param_dist_weights %in% names_list[[1]])

  # Defining the names of the param_dists and unselecting undesired columns:
  for(param_dist_id in 1:length(names(param_dists_list)) ) {
    param_dists_list[[param_dist_id]]$param_dist.df.id = param_dist_id
    param_dists_list[[param_dist_id]]$param_dist.df.name = names(param_dists_list)[param_dist_id]
    # Ignoring unwanted variables:
    param_dists_list[[param_dist_id]] = param_dists_list[[param_dist_id]] %>%
      dplyr::select(-dplyr::any_of(cols_to_ignore))

  }

  # If we want to use the average, we can do so:
  if(use_average) {

    # calculate weighted averages for every param_dist in the param_dists_list:
    params_df <- purrr::map_dfr(.x = param_dists_list, .f = calculate_weighted_averages, param_dist_weights = param_dist_weights) %>%
      dplyr::mutate(param.id = row_number())

    # If not using weighted averages, sample from the param_dist using its weights:
  } else {

    # n_sample is smaller or equal to the size of the dataframe.
    assertthat::assert_that(all(sapply(param_dists_list, nrow) >= n_sample), msg = "Number of rows in each param_dist data.frame must be greater or equal than n_sample.")

    # n_sample is not missing
    assertthat::assert_that(!missing(n_sample), msg = "n_sample parameter must be provided.")

    # Use the purrr::map_dfr function to sample from the param_dist distribution for each param_dist file provided.
    params_df <- purrr::map_dfr(.x = param_dists_list, .f = sample_from_param_dist, n_sample = n_sample, param_dist_weights = param_dist_weights, resample = resample) %>%
      dplyr::mutate(param.id = row_number())

    row.names(params_df) = NULL
  }

  # The main result of this function is the params_df data.frame, which we assign to self:
  self$params_df = params_df

  if (resample) {
    self$params_df <- self$params_df %>%
      dplyr::select(-any_of(param_dist_weights))
  }

  # Return the model object:
  invisible(self)

}



# Auxiliary Functions -----------------------------------------------------

# Sample from param_dist ------------------------------------------

# This internal function samples from the param_dist distribution. It is defined here because it is only used by the set_param_dist function.
sample_from_param_dist = function(param_dist_data_frame, n_sample, param_dist_weights, resample) {

  # Assign an id to the original param_dist table:
  param_dist_data_frame = param_dist_data_frame %>%
    mutate(param_dist.orig.row.id = dplyr::row_number())

  # Next, sample from the param_dist with replacement:
  if(resample){
    ids_to_select = sample(x = param_dist_data_frame$param_dist.orig.row.id, size = n_sample, replace = T, prob = param_dist_data_frame[,param_dist_weights])
    # Or don't resample at all and return the param_dist directly
  } else {
    ids_to_select = param_dist_data_frame$param_dist.orig.row.id
  }

  # Selects these rows from the data.frame:
  param_dist_sample = param_dist_data_frame[ids_to_select,] %>%
    dplyr::mutate(param_dist.df.row.id = dplyr::row_number())

  return(param_dist_sample)

}


# Calculate weighted averages ------------------------------------

# This private function is used to calculate weighted averages if the user wants them.
calculate_weighted_averages = function(df, param_dist_weights) {
  # Calculate a normalized_weights variable to ensure that the weighted average will be correct
  # And the weights add up to one.

  # ensure this is a vanilla data.frame
  df = as.data.frame(df)

  df$normalized_weights = df[,param_dist_weights] / sum(df[,param_dist_weights])

  # Calculate the weighted average for every numeric variable:
  df %>%
    # Multiply value by the normalized weights:
    mutate(across(where(is.numeric) & !c(.data$param_dist.df.id, .data$param_dist.df.name), ~ .x * df$normalized_weights)) %>%
    select(-.data$normalized_weights) %>%
    # Add them up:
    group_by(.data$param_dist.df.id, .data$param_dist.df.name) %>%
    summarise(across(where(is.numeric),sum), .groups = "drop") %>%
    as.data.frame()
}


