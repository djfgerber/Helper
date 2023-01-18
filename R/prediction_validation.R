#' Get the mean squared error
#'
#' @describeIn get_errors
#'
#' @return A single double
#' @export
#'
#' @examples
#' gambia_prediction %>%
#'   get_mse("estimate", "prevalence")
get_mse <- function(data_predicted, name_mean, name_prevalence){
    get_errors(data_predicted, name_mean, name_prevalence)^2 %>%
    mean(na.rm = TRUE)
}

#' Get the root mean squared error
#'
#' @describeIn get_errors
#'
#' @return A single double
#' @export
#'
#' @examples
#' gambia_prediction %>%
#'   get_rmse("estimate", "prevalence")
get_rmse <- function(data_predicted, name_mean, name_prevalence){
  sqrt(get_mse(data_predicted, name_mean, name_prevalence))
}

#' Get the mean absolute error
#'
#' @describeIn get_errors
#' @return A single double
#' @export
#'
#' @examples
#' gambia_prediction %>%
#'   get_mae("estimate", "prevalence")
get_mae <- function(data_predicted, name_mean, name_prevalence){
  get_errors(data_predicted, name_mean, name_prevalence) %>%
    abs() %>%
    mean(na.rm = TRUE)
}

#' Get errors
#'
#' @param data_predicted Prediction data (a tibble)
#' @param name_mean Name of the mean
#' @param name_prevalence Name of the prevalence
#'
#' @return A vector of differences between name_mean nad name_prevalence
#'
#' @examples
#' #gambia_prediction %>%
#' #   get_errors("estimate", "prevalence")
get_errors <- function(data_predicted, name_mean, name_prevalence){
  is_data(data_predicted)
  is_covariates(data_predicted, c(name_mean, name_prevalence))
  data_predicted[[name_mean]] - data_predicted[[name_prevalence]]
}

#' Get the proportion of estimates falling within a p-BCI
#'
#' @param data_predicted Prediction data (a tibble)
#' @param name_prevalence Name of the prevalence
#' @param quantile_interval_probability The probability p of the BCI
#'
#' @return A single double
#' @export
#'
#' @examples
#' gambia_prediction %>%
#'   get_correct_within_quantile_interval("estimate", .7)
get_correct_within_quantile_interval <-
  function(data_predicted,
           name_prevalence,
           quantile_interval_probability) {
    is_data(data_predicted)
    is_probability(quantile_interval_probability)
    var_name <- paste0("quantile_interval", quantile_interval_probability)
    var_name_low <- paste0(var_name, "_low")
    var_name_high <- paste0(var_name, "_high")
    is_covariates(data_predicted, c(name_prevalence, var_name_high, var_name_low))
    data_predicted %>%
      dplyr::mutate(in_quantile_interval = .data[[name_prevalence]] <= .data[[var_name_high]] &
               .data[[name_prevalence]] >= .data[[var_name_low]]) %>%
      dplyr::pull(in_quantile_interval) %>%
      mean(na.rm = TRUE)
  }

#' Get the length of the BCI
#' @describeIn get_correct_within_quantile_interval
#'
#' @return A single double
#' @export
#'
#' @examples
#' gambia_prediction %>% get_quantile_interval_length(.7)
get_quantile_interval_length <-
  function(data_predicted,
           quantile_interval_probability) {
    is_probability(quantile_interval_probability)
    var_name <- paste0("quantile_interval", quantile_interval_probability)
    var_name_low <- paste0(var_name, "_low")
    var_name_high <- paste0(var_name, "_high")
    is_covariates(data_predicted, c(var_name_high, var_name_low))
    data_predicted %>%
      dplyr::mutate(length_quantile_interval = .data[[var_name_high]] - .data[[var_name_low]]) %>%
      dplyr::pull(length_quantile_interval) %>%
      mean(na.rm = TRUE)
  }
