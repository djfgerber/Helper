is_matrix <- function(x, is_fn) {
  is_fn(x) & length(dim(x)) == 2
}

is_matrix_square <- function(x, is_fn) {
  is_fn(x) && length(dim(x)) == 2 && nrow(x) == ncol(x)
}

is_data <- function(data) {
  if (!inherits(data, c("tbl_df", "tbl", "data.frame"))) {
    stop("Data must inherit from tbl_df, tbl and data.frame.")
  }
}

is_sf_data <- function(data) {
  if (!inherits(data, c("sf", "tbl_df", "tbl", "data.frame"))) {
    stop("Data must inherit from sf, tbl_df, tbl and data.frame.")
  }
}

is_SpatRaster_data <- function(data) {
  if (!inherits(data, c("SpatRaster"))) {
    stop("Data must inherit from SpatRaster (package:terra).",
         .call = FALSE)
  }
}

is_binomial_response <- function(data, response) {
  if (length(response) != 2) {
    stop(paste0("Response variable `", substitute(response),
                "` must be a character vector containing the count and the total."))
  }
  is_response(data, response)
  if(!is.integer(data[[response[1]]])){
    stop(paste0("Response variable `", substitute(response),
                "` count in data must be a column of type integer."))
  }
  if(!is.integer(data[[response[2]]])){
    stop(paste0("Response variable `", substitute(response),
                "` total in data must be a column of type integer."))
  }
}

is_negative_binomial_response <- function(data, response) {
  if (length(response) != 1) {
    stop(paste0("Response variable `", substitute(response),
                "` must be a character vector containing the count."))
  }
  is_response(data, response)
  if(!is.integer(data[[response[1]]])){
    stop(paste0("Response variable `", substitute(response),
                "` count in data must be a column of type integer."))
  }
}

is_response <- function(data, response){
  if (!is.character(response)) {
    stop(paste0("Response variable `", substitute(response), "` must be a character vector."))
  }
  if (any(is.na(response))) {
    stop(paste0("Response variable `", substitute(response), "` must not be NA."))
  }
  if (any(!response %in% colnames(data))) {
    stop(paste0("Response variable `", substitute(response), "` must be a column of data."))
  }
}

is_group_var <- function(data, group_var){
  if (length(group_var) != 1) {
    stop(paste0("Grouping variable `", substitute(group_var),
                "` must be a character vector of length 1."))
  }
  if (!is.character(group_var)) {
    stop(paste0("Grouping variable variable `", substitute(group_var), "` must be a character vector."))
  }
  if (any(is.na(group_var))) {
    stop(paste0("Grouping variable `", substitute(group_var), "` must not be NA."))
  }
  if (any(!group_var %in% colnames(data))) {
    stop(paste0("Grouping variable `", substitute(group_var), "` must be a column of data."))
  }
}

is_flag <- function(flag) {
  if (!purrr::is_scalar_logical(flag)) {
    stop(paste0("Variable `", substitute(flag), "` must be a flag."))
  }
  if (is.na(flag))
    stop(paste0("Variable `", substitute(flag), "` must not be NA."))
}

is_covariates <- function(data, covariates) {
  if (!length(covariates) == 0) {
    if (!is.character(covariates)) {
      stop("Covariates must be a character vector.")
    }
    if (any(is.na(covariates))) {
      stop("Covariates must not be NA")
    }
    if (any(!covariates %in% colnames(data))) {
      stop("Covariates must be columns of data.")
    }
    if (data %>%
        dplyr::select(dplyr::all_of(covariates)) %>%
        drop_geometry_if_exists() %>%
        dplyr::select(tidyselect:::where(is.character)) %>%
        ncol() > 0) {
      stop("Covariates must not be of type character.")
    }
  }
  TRUE
}

is_probability <- function(x){
  if (!purrr::is_scalar_double(x)){
    stop(paste0("Variable `", substitute(x), "` must be scalar double."))
  }
  if(is.na(x)){
    stop(paste0("Variable `", substitute(x), "` must be not be NA."))
  }
  if(x > 1 | x < 0){
    stop(paste0("Variable `", substitute(x), "` must be in [0,1]."))
  }
}

is_probabilities <- function(x){
  x %>% purrr::walk(is_probability)
}


