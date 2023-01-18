#' Scale covariates
#'
#' `r lifecycle::badge('superseded')` Use `scale_vars`instead.
#' Scale covariates and retain the used means and sds for later scaling of the predictor covariates.
#'
#' @param covariate_matrix The covariate matrix, usually a tibble.
#' @param means A double vector of means of length equal to ncol of the covariate matrix
#' @param sds A double vector of standard deviations of length equal to ncol of the covariate matrix
#'
#' @return list of three components $covariates_scaled (the scaled covariates), $means and $sds the means and standard deviations used for scaling.
#' @export
#'
#' @examples
#' set.seed(3)
#' train <- tibble::tibble(cov1 = rnorm(10, 10, 1), cov2 = rnorm(10, 5, 1))
#' (scaled_a <- scale_covariates(train))
#' test <- tibble::tibble(cov1 = rnorm(5, 10, 1), cov2 = rnorm(5, 5, 1))
#' scale_covariates(test, scaled_a$means, scaled_a$sds)
scale_covariates <-
  function(covariate_matrix,
           means = numeric(0L),
           sds = numeric(0L)) {
    stopifnot(
      purrr::is_double(means),
      purrr::is_double(sds),
      length(means) == 0 & length(sds) == 0 |
        length(means) == ncol(covariate_matrix) &
        length(sds)  == ncol(covariate_matrix)
    )
    if (length(means) == 0 & length(sds) == 0) {
      # If estimation
      means <- covariate_matrix %>%
        dplyr::summarize(dplyr::across(dplyr::everything(), mean)) %>%
        unlist %>%
        unname
      sds <- covariate_matrix %>%
        dplyr::summarize(dplyr::across(dplyr::everything(), sd)) %>%
        unlist %>%
        unname
    }

    for (i in 1:ncol(covariate_matrix)) {
      covariate_matrix[, i] <- (covariate_matrix[, i] - means[i]) / sds[i]
    }
    list("covariates_scaled" = covariate_matrix,
         "means" = means,
         "sds" = sds)
  }


#' Scale variables
#'
#' @description `r lifecycle::badge('experimental')`
#' @param data_estimation Data for estimation
#' @param covariates The covariates to scale
#' @param data_prediction Data for prediction
#'
#' @return A list with $data_estimation and $data_prediction containing the scaled data sets.
#' @export
#'
#' @examples
#' \dontrun{
#' tibble(a = c(1,2,3),
#' b = c(10,11,15), x = c(1,1,1), y = c(1,2,3)) %>%
#' sf::st_as_sf(coords = c("x", "y")) %>%
#' scale_vars(c("a", "b"),
#' tibble(b = c(10,11,15), a = c(1,2,3), x = c(1,1,1), y = c(1,2,3)) %>%
#'  sf::st_as_sf(coords = c("x", "y")))
#' scale_vars(sf_gambia, NULL, NULL)
#' scale_vars(sf_gambia, c("green", "netuse"), NULL)
#' scale_vars(sf_gambia[1:50,], c("green", "netuse"), sf_gambia[51:65,8:1])
#' scale_vars(sf_gambia[1:50,], c("green", "netuse"), sf_gambia[51:65,1:8])
#' }
scale_vars <- function(data_estimation,
                       covariates,
                       data_prediction) {
  is_sf_data(data_estimation)
  is_covariates(data_estimation, covariates)
  if (!is.null(data_prediction)) {
    is_sf_data(data_prediction)
    is_covariates(data_prediction, covariates)
  }

  c_names <- colnames(data_estimation)
  numeric_names <- c_names[purrr::map_lgl(c_names,
                                          ~ data_estimation[[.x]] %>%
                                            is.numeric())] %>%
    intersect(covariates)
  means <- data_estimation %>%
    drop_geometry_if_exists() %>%
    dplyr::summarize(dplyr::across(dplyr::all_of(numeric_names),
                                   ~ .x %>% mean(na.rm = TRUE))) %>%
    unlist() %>%
    unname()
  sds <- data_estimation %>%
    drop_geometry_if_exists() %>%
    dplyr::summarize(dplyr::across(dplyr::all_of(numeric_names),
                                   ~ .x %>% sd(na.rm = TRUE))) %>%
    unlist() %>%
    unname()
  for (i in seq_along(numeric_names)) {
    data_estimation <- data_estimation %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(numeric_names[i]), ~ (.x - means[i]) /
                                    sds[i]))
    if(!is.null(data_prediction)){
      data_prediction <- data_prediction %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(numeric_names[i]), ~ (.x - means[i]) /
                                      sds[i]))
    }
  }

  list("data_estimation" = data_estimation,
       "data_prediction" = data_prediction)
}



#' Transform factors to design matrix
#' @description `r lifecycle::badge('experimental')`
#' @param data A sf data object.
#' @param covariates The covariates to use.
#'
#' @return A list with $data and new $covariates names.
#' @export
#'
#' @examples
resolve_factors <- function(data_estimation, covariates, data_prediction = NULL){
  is_sf_data(data_estimation)
  is_covariates(data_estimation, covariates)
  resolved_factors_estimation <- data_estimation %>%
    resolve_factors_internal(covariates)
  resolved_factors_prediction <- if(is.null(data_prediction)){
    NULL
  }else{
    is_sf_data(data_prediction)
    is_covariates(data_prediction, covariates)
    resolve_factors_internal(data_prediction,covariates)$data
  }
  list("data_estimation" = resolved_factors_estimation$data,
       "data_prediction" = resolved_factors_prediction,
       "covariates" = resolved_factors_estimation$covariates)
}

resolve_factors_internal <- function(data, covariates){
  fct_names <- data %>%
    drop_geometry_if_exists() %>%
    dplyr::select(dplyr::all_of(covariates)) %>%
    dplyr::select_if(is.factor) %>%
    colnames()

  for(i in seq_along(fct_names)){
    ff <- data %>% dplyr::pull(dplyr::all_of(fct_names[i]))
    ff <- ff %>% as_dummy(fct_names[i])
    data <- data %>%
      dplyr::bind_cols(ff) %>%
      dplyr::select(-dplyr::all_of(fct_names[i]))
    covariates <- c(covariates[-which(covariates == fct_names[i])],
                    colnames(ff))
  }
  list("data" = data,
       "covariates" = covariates)
}




