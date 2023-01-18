
#' Bayesian variable selction with INLA
#' `r lifecycle::badge("experimental")`
#' @param covariates A character vector of covariates contained in the data_estimation
#' @param probs A numeric vector for probabilities for covariates which are fitted as quantiles.
#' @param workers A scalar integer indicating the number of parallel sessions to run.
#' @inheritParams fit_inla2
#'
#' @details Parallel computation requires the installation of `future` and `furrr`.
#'
#' @return A tibble with fitted covariates, dic, waic and if the model fit threw an error or not.
#' @export
bvs_inla <- function(covariates,
                     probs = c(.33,.66),
                     data_estimation,
                     response,
                     family = "binomial",
                     flag_intercept,
                     mesh = NULL,
                     alpha = 1.5,
                     flag_spatial_re = FALSE,
                     flag_non_spatial_re = FALSE,
                     prior_pc_range = c(0.3, 0.5),
                     prior_pc_sigma = c(10, 0.01),
                     prior_iid = c(0.01, 0.01),
                     workers = 1L) {
  is_data(data_estimation)
  is_covariates(data_estimation, covariates)
  if(!purrr::is_scalar_integer(workers)){
    stop("`bvs_inla` argument `workers` needs to be a scalar integer.")
  }


# clean data_estimation ---------------------------------------------------
  # tictoc::tic("clean data_estimation")
  # keep only relevant variables
  data_covariates <- data_estimation %>%
    drop_geometry_if_exists() %>%
    dplyr::select(all_of(c(covariates)))
  # add quantiles variables where necessary
  numeric_covariates <- names(data_covariates)[purrr::map_lgl(data_covariates,is.numeric)]
  data_covariates <- data_covariates %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_covariates),
                                ~ .x %>%
                                  as_quantile_factor(probs = probs),
                                .names = "{.col}_quantile" ))
  # patch together
  data_estimation <- data_estimation %>%
    dplyr::select(all_of(c(response))) %>%
    dplyr::bind_cols(data_covariates)
  # tictoc::toc()
  cat("Dim data_estimation:", dim(data_estimation), "\n")
# create covariates combinations list -------------------------------------
  # tictoc::tic("create covariates combinations list")
  covariates_combinations <- purrr::map_dbl(covariates,
                 ~ ifelse(data_covariates[[.x]] %>%
                            is.numeric(), 3, 2)) %>%
    purrr::map(~1:.x) %>%
    expand.grid()
  for(i in 1:length(covariates)){
    covariates_combinations[,i] <- ifelse(covariates_combinations[,i] == 1,
                                          "excluded",
                                          ifelse(covariates_combinations[,i] == 2,
                                                 covariates[i],
                                                 paste0(covariates[i],"_quantile")))
  }
  covariates_list <- list()
  for(i in 1:nrow(covariates_combinations)){
    covariates_line <- NULL
    for(j in 1:ncol(covariates_combinations)){
      if(covariates_combinations[i,j] != "excluded"){
        covariates_line <- c(covariates_line, covariates_combinations[i,j])
      }
    }
    covariates_list[[i]] <- covariates_line
  }
  n_models <- length(covariates_list)
  cat("There are", n_models, "models to evaluate.\n")

  if(workers > n_models){
    workers <- n_models
  }
  # tictoc::toc()


# fit inla ----------------------------------------------------------------
  # tictoc::tic("fit inla")
  if(workers == 1 ||
     !requireNamespace("future", quietly = TRUE) ||
     !requireNamespace("furrr", quietly = TRUE)){
    cat("Models are estimated using 1 worker.\n")
    covariates_list %>%
      purrr::map_dfr(function(x){
        fit <- try(fit_inla2(data_estimation = data_estimation,
                             response = response,
                             covariates = x,
                             flag_intercept = flag_intercept,
                             mesh = mesh,
                             alpha = alpha,
                             family =family,
                             flag_spatial_re = flag_spatial_re,
                             flag_non_spatial_re = flag_non_spatial_re,
                             prior_pc_range = prior_pc_range,
                             prior_pc_sigma = prior_pc_sigma,
                             prior_iid = prior_iid))
        covariates_names <- paste0(x, collapse = ", ")
        if(class(fit)[1] == "try-error"){
          tibble::tibble(
            "covariates" = covariates_names,
            "dic" = NA_real_,
            "waic" = NA_real_,
            "error" = TRUE
          )
        }else{
          tibble::tibble(
            "covariates" = covariates_names,
            "dic" = fit$fit$dic$dic,
            "waic" = fit$fit$waic$waic,
            "error" = FALSE
          )
        }
      })
  }else{
    cat("Models are estimated using", workers, "workers.\n")
    future::plan(future::multisession, workers = workers)
    covariates_list %>%
      furrr::future_map_dfr(function(x){
        fit <- try(fit_inla2(data_estimation = data_estimation,
                             response = response,
                             covariates = x,
                             flag_intercept = flag_intercept,
                             mesh = mesh,
                             alpha = alpha,
                             family =family,
                             flag_spatial_re = flag_spatial_re,
                             flag_non_spatial_re = flag_non_spatial_re,
                             prior_pc_range = prior_pc_range,
                             prior_pc_sigma = prior_pc_sigma,
                             prior_iid = prior_iid))
        covariates_names <- paste0(x, collapse = ", ")
        if(class(fit)[1] == "try-error"){
          tibble::tibble(
            "covariates" = covariates_names,
            "dic" = NA_real_,
            "waic" = NA_real_,
            "error" = TRUE
          )
        }else{
          tibble::tibble(
            "covariates" = covariates_names,
            "dic" = fit$fit$dic$dic,
            "waic" = fit$fit$waic$waic,
            "error" = FALSE
          )
        }
      },
      .progress = TRUE,
      .options = furrr::furrr_options(seed = TRUE)
      )
  }
  # tictoc::toc()

}
