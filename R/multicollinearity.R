
#' Stepwise covariate elimination through VIF
#' @description `r lifecycle::badge("experimental")` Stepwise covariate elimination
#' through eliminating the covariate with the maximum vif until a certain threshold.
#' The goal is to reduce the multicollinearity between the covariates. This function
#' is yet only programmed for a bivariate binomial logit regression.
#' @param data A tibble containing all the covariates and the positives and the total cases.
#' @param covariate_names The covariate names
#' @param positives_name The positives name
#' @param total_name The total name
#' @param threshold A threshold
#'
#' @return Returns a list of 2: A tibble of stepwise eliminated covariates and a vector of
#' covariates to keep.
#' @export
#'
#' @examples
#' mtcars %>%
#'   tibble::as_tibble() %>%
#'   dplyr::mutate("total" = 5) %>%
#'   vif_step_elimination(c("mpg", "cyl", "disp", "hp", "drat"),
#'   "gear",
#'   "total")
#' mtcars %>%
#'   tibble::as_tibble() %>%
#'   dplyr::mutate("total" = 5) %>%
#'   vif_step_elimination(c("mpg", "cyl", "disp", "hp", "drat"),
#'   "gear",
#'   "total",
#'   threshold = 10)
vif_step_elimination <-
  function(data,
           covariate_names,
           positives_name,
           total_name,
           threshold = 2) {
    stopifnot(purrr::is_scalar_double(threshold),
              threshold > 0,
              purrr::is_scalar_character(positives_name),
              purrr::is_scalar_character(total_name),
              purrr::is_character(covariate_names),
              tibble::is_tibble(data),
              nrow(data) > 5,
              length(covariate_names) > 0,
              all(covariate_names %in% colnames(data)),
              positives_name %in% colnames(data),
              total_name %in% colnames(data))

    result <- tibble::tibble(var_name = character(),
                     vif = double())
    while (length(covariate_names) > 1) {
      vif_model <- paste("cbind(",
                         positives_name,", ",
                         total_name,"-", positives_name, ")~",
                         paste(covariate_names, collapse = "+")) %>%
        stats::as.formula() %>%
        stats::glm(family = stats::binomial(link = "logit"), data = data) %>%
        car::vif()
      max_vif <- max(vif_model)

      if (max_vif < threshold) {
        break()
      }
      remove <- which(vif_model == max(max_vif))

      result <- result %>%
        dplyr::add_row(tibble::tibble(var_name = names(vif_model)[remove],
                                      vif = vif_model[remove]))
      covariate_names <- names(vif_model)[-remove]

    }
    list(eliminate = result,
         keep = covariate_names)
  }


#' Stepwise covariate elimination through correlation cut off
#'
#' @description `r lifecycle::badge("experimental")` Stepwise covariate elimination
#' through eliminating the covariates with a correlation above the threshold. For each
#' candidate correlation the two covariates are examined for their correlation with the
#' response and the one with the lower correlation with the response is dropped. The
#' goal is to reduce the multicollinearity between the covariates. Other methods for
#' choosing between a candidate pair could be thought of.
#'
#' @param data A tibble containing all the covariates and the response variable.
#' @param covariate_names The covariate names
#' @param response The response name
#' @param threshold A threshold 0 < threshold < 1
#' @param method A method for which of the two candidates to eliminate, possible = "response".
#'
#' @return Returns a list of 2: A tibble of stepwise eliminated covariates and a vector of
#' covariates to keep.
#' @export
#'
#' @examples
#' mtcars %>%
#'   tibble::as_tibble() %>%
#'   cor_step_elimination(c("mpg", "cyl", "disp", "hp", "drat"),
#'   "gear")
#' mtcars %>%
#'   tibble::as_tibble() %>%
#'   cor_step_elimination(c("mpg", "cyl", "disp", "hp", "drat"),
#'   "gear",
#'   threshold = 0.5)
cor_step_elimination <- function(data,
                                 covariate_names,
                                 response,
                                 threshold = 0.8,
                                 method = "response"){
  stopifnot(purrr::is_scalar_double(threshold),
            threshold <= 1,
            threshold > 0,
            purrr::is_scalar_character(method),
            method %in% c("response"),
            purrr::is_scalar_character(response),
            tibble::is_tibble(data),
            nrow(data) > 5,
            purrr::is_character(covariate_names),
            length(covariate_names) > 0,
            all(covariate_names %in% colnames(data)),
            response %in% colnames(data))
  data_response <- data %>%
    dplyr::pull(response)
  result <- tibble::tibble(var_name = character(),
                           cor = double(),
                           cor_with = character(),
                           cor_resp = double())
  while(length(covariate_names) > 1){
    data_covariates <- data %>%
      dplyr::select(dplyr::all_of(covariate_names))
    corr_covariates <- data_covariates %>%
      stats::cor()
    corr_covariates_abs <- abs(corr_covariates)
    corr_covariates_abs[upper.tri(corr_covariates_abs, diag = TRUE)] <- 0
    max_corr <- corr_covariates_abs %>% max()

    if(max_corr < threshold){
      break()
    }
    inds <- which(corr_covariates_abs == max_corr, arr.ind = TRUE)[1,]
    removal_candidates <- colnames(corr_covariates)[inds]

    if (method == "response") {
      corr_response <- stats::cor(data[, removal_candidates], data[, response])
      corr_response_abs <- abs(corr_response)
      removal_ind <- which(corr_response_abs == corr_response_abs %>% min())
      removal_var <- removal_candidates[removal_ind]
      result <- result %>%
        dplyr::add_row(
          tibble::tibble(
            var_name = removal_var,
            cor = corr_covariates[inds[1], inds[2]],
            cor_with = removal_candidates[-removal_ind],
            cor_resp = corr_response[removal_ind,]
          )
        )
      covariate_names <- covariate_names[-which(covariate_names == removal_var)]
    }
  }

  list(eliminate = result,
       keep = covariate_names)
}
