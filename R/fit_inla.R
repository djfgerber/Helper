#' Fit a Bayesian model with INLA
#'
#' @param data_estimation The data for estimation, either a tibble or a sf
#' @param response Character vector of the name of the response in `data_estimation`. In the binomial case a vector of 2 containing 1) the response and 2) the number of trials
#' @param covariates Name of the covariates
#' @param family One of `binomial`, `negative_binomial`, `zero_inflated_binomial`, or `zero_inflated_negative_binomial`
#' @param flag_intercept Set true if a intercept should be included
#' @param flag_scale_vars Set true if variables should be scaled bevore analysis
#' @param flag_non_spatial_re Set true if a non spatial random effect should be included
#' @param mesh Specify a `inla.mesh` if `flag_spatial_re` is true.
#' @param alpha Specify if `flag_spatial_re` is true, alpha = 1.5 is exponential spatial correlation.
#' @param prior.iid Set the prior as values for $1/sigma^2 ~ Gamma(`prior.iid[1]`, `prior.iid[2]`)$ if `flag_non_spatial_re` is true.
#' @param data_prediction The data for prediction, either a tibble or a sf
#' @param n_sample A integer defining a desired number of draws from the posterior distribution
#' @param hdpi_probabilities
#' @param zi_covariates
#' @param flag_spatial_re
#' @param prior_pc_range
#' @param prior_pc_sigma
#' @param prior_tau_gamma
#' @param prior_decay_gamma
#' @param flag_zi_intercept Set true if a intercept should be included in the zero inflated part of a zero inflated model
#'
#' @return
#' @export
#'
#' @examples
fit_inla <- function(data_estimation ,
                     response,
                     covariates,
                     zi_covariates = NULL,
                     family = "binomial",
                     flag_intercept,
                     flag_zi_intercept,
                     flag_scale_vars,
                     flag_spatial_re,
                     flag_non_spatial_re,
                     mesh = NULL,
                     alpha = 1.5,
                     prior_pc_range = c(0.3, 0.5),
                     prior_pc_sigma = c(10, 0.01),
                     prior_tau_gamma = c(2.01, 1.01),
                     prior_decay_gamma = c(2.01, 1.01),
                     prior.iid = c(0.01, 0.01),
                     data_prediction = NULL,
                     n_sample = 0L,
                     hdpi_probabilities = NULL) {

  is_sf_data(data_estimation)
  is_covariates(data_estimation, covariates)
  if (!is.null(data_prediction)) {
    is_sf_data(data_prediction)
    is_covariates(data_prediction, covariates)
    if (!identical(sf::st_crs(data_estimation), sf::st_crs(data_estimation))) {
      stop("CRS for estimation and projection must be the same.")
    }
    if (!is.null(hdpi_probabilities)){
      is_probabilities(hdpi_probabilities)
    }
  }
  stopifnot(family %in% c("binomial",
                          "negative_binomial"#,
                          # "zero_inflated_negative_binomial",
                          # "zero_inflated_binomial"
                          ))
  if(family %>% stringr::str_detect("negative_binomial")){
    is_negative_binomial_response(data_estimation, response)
  }else{
    is_binomial_response(data_estimation, response)
  }
  if((!family %in% c("zero_inflated_negative_binomial", "zero_inflated_binomial")) & !is.null(zi_covariates)){
    stop("It is not a zero-inflated model. Do not specify zero inflated covariates `zi_covariates`.")
  }else{
    if(!is.null(zi_covariates)){
      is_covariates(data_estimation, zi_covariates)
    }
  }
  stopifnot(purrr::is_scalar_integer(n_sample),
            n_sample > -1)
  purrr::walk(c(flag_intercept, flag_zi_intercept, flag_scale_vars,
                flag_spatial_re, flag_non_spatial_re), is_flag)
  if(!family %in% c("zero_inflated_negative_binomial", "zero_inflated_binomial") &&
     flag_zi_intercept){
    stop("`zi_intercept` == TRUE only makes sens for zero inflated models.")
  }
  if(flag_spatial_re){
    if(is.null(mesh)){
      stop("Provide a mesh for spatial random effect or turn flag_spatial_re off.")
    }
    if(class(mesh) != "inla.mesh"){
      stop("Provide a mesh of class inla.mesh.")
    }
    stopifnot(all(is.na(prior_pc_range), is.na(prior_pc_sigma)) ||
              all(is.na(prior_decay_gamma), is.na(prior_tau_gamma)) ||
              all(is.na(prior_decay_gamma),is.na(prior_tau_gamma),is.na(prior_pc_range),is.na(prior_pc_sigma)))
  }

  flag_zero_inflated <- family %>% stringr::str_detect("zero_inflated")

  covariates_info <- get_covariates_design_matrices(data_estimation = data_estimation,
                                                    data_prediction = data_prediction,
                                                    covariates = covariates,
                                                    zi_covariates = zi_covariates,
                                                    flag_scale_vars = flag_scale_vars,
                                                    flag_intercept = flag_intercept,
                                                    flag_zi_intercept = flag_zi_intercept,
                                                    flag_non_spatial_re = flag_non_spatial_re,
                                                    flag_zero_inflated = flag_zero_inflated)
  covariates_estimation <- covariates_info$covariates_estimation
  covariates <- covariates_info$covariates
  covariates_prediction <- covariates_info$covariates_prediction

  if(flag_non_spatial_re){
    formula_non_spatial_re <-
      paste0(
        "+ f(location_id, model = 'iid', hyper=list(prec=list(param=c(",
        prior.iid[1], "," , prior.iid[2],
        "),prior='loggamma')))"
      )
  }else{
    formula_non_spatial_re <- ""
  }

  if(flag_spatial_re){
    if(any(is.na(prior_pc_range)) & any(is.na(prior_decay_gamma))){
      spde <- INLA::inla.spde2.matern(mesh = mesh,
                                      alpha = alpha)
      formula_spatial_re <- "+ f(spatial.field, model=spde)"
    }else if(any(is.na(prior_pc_range))){
      spde <- INLA::inla.spde2.matern(mesh = mesh,
                                      alpha = alpha,
                                      B.tau = matrix(c(-0.5*log(4*pi),0.5,-0.5),1,3),
                                      B.kappa =  matrix(c(0,0,1),1,3))
      formula_spatial_re <- paste("+ f(spatial.field, model=spde,
    hyper = list(theta1 = list(param = c(",prior_tau_gamma[1],
                                  ",",prior_tau_gamma[2],"), prior = 'loggamma'),
                 theta2 = list(param = c(",prior_decay_gamma[1],
                                  ",",prior_decay_gamma[2],"), prior = 'loggamma')))")
    }else{
      spde <- INLA::inla.spde2.pcmatern(
        mesh = mesh,
        alpha = alpha,
        prior.range = prior_pc_range,
        prior.sigma = prior_pc_sigma
      )
      formula_spatial_re <- "+ f(spatial.field, model=spde)"
    }
    s.index <- INLA::inla.spde.make.index("spatial.field", spde$n.spde) #spatial index
  }else{
    formula_spatial_re <- ""
  }

  stack <- INLA::inla.stack(
    data = switch(ifelse(family %>% stringr::str_detect("zero_inflated_") &&
                           is.null(zi_covariates) &&
                           !flag_zi_intercept &&
                           !flag_non_spatial_re,
                         family %>% stringr::str_replace("zero_inflated_",""),
                         family),
                  "binomial" = data_estimation %>%
                    dplyr::select(all_of(response)) %>%
                    dplyr::rename(y = .data[[response[1]]],
                                  Ntrials = .data[[response[2]]]) %>%
                    drop_geometry_if_exists(),
                  "negative_binomial" = data_estimation %>%
                    dplyr::select(all_of(response)) %>%
                    dplyr::rename(y = .data[[response[1]]]) %>%
                    drop_geometry_if_exists(),
                  "zero_inflated_binomial" = list(
                    "y" = rbind(
                      cbind(ifelse(data_estimation[[response[1]]]==0, 1, 0),
                            NA_real_),
                      cbind(NA_real_,
                            data_estimation[[response[1]]])
                    ),
                    "Ntrials" = c(rep(1, nrow(data_estimation)),
                                  data_estimation  %>% dplyr::pull(response[2]))),
                  "zero_inflated_negative_binomial" = list(
                    "y" = rbind(
                      cbind(ifelse(data_estimation[[response[1]]]==0, 1, 0),
                            NA_real_),
                      cbind(NA_real_,
                            data_estimation[[response[1]]])
                    ))),
    A = construct_inla_A(data_estimation,
                         family,
                         zi_covariates,
                         flag_zi_intercept,
                         flag_spatial_re,
                         flag_non_spatial_re,
                         response,
                         mesh),
    effects = if(flag_spatial_re){
      list("spatial.field" = s.index,
           covariates_estimation)
      }else{
        list(covariates_estimation)
      },
    tag = "estimation"
  )
  if (!is.null(data_prediction)) {
    stack_prediction <- INLA::inla.stack(
      data = switch(
        ifelse(family %>%  stringr::str_detect("zero_inflated_") &&
                 is.null(zi_covariates) &&
                 !flag_zi_intercept &&
                 !flag_non_spatial_re,
               family %>%  stringr::str_replace("zero_inflated_",""),
               family),
        "binomial" = list("y" = NA,
                          "Ntrials" = NA),
        "negative_binomial" = list("y" = NA),
        "zero_inflated_binomial" = list(
          "y" = matrix(NA, nrow = 2 * nrow(data_prediction), ncol = 2),
          "Ntrials" = rep(NA, 2 * nrow(data_prediction))
        ),
        "zero_inflated_negative_binomial" = list("y" = matrix(
          NA, nrow = 2 * nrow(data_prediction), ncol = 2
        ))
      ),
      A = construct_inla_A(data_prediction,
                           family,
                           zi_covariates,
                           flag_zi_intercept,
                           flag_spatial_re,
                           flag_non_spatial_re,
                           response,
                           mesh),
      effects = if(flag_spatial_re){
        list("spatial.field" = s.index,
                     covariates_prediction)
      }else{
        list(covariates_prediction)
      },
      tag = "prediction"
    )
    stack <- stack %>%
      INLA::inla.stack.join(stack_prediction)
  }

  formula_fixed <- paste0("y ~ 0 +",
           paste0(covariates, collapse = "+"))
  formula_str <- paste0(formula_fixed, formula_spatial_re, formula_non_spatial_re)
  formula = as.formula(formula_str)

  inla_family <- function(family, zi_covariates){
    if(family == "binomial"){
      "binomial"
    }else if(family == "negative_binomial"){
      "nbinomial"
    }else if(family == "zero_inflated_binomial" &&
             is.null(zi_covariates) &&
             !flag_zi_intercept &&
             !flag_non_spatial_re){
      "zeroinflatedbinomial1"
    }else if(family == "zero_inflated_binomial" &&
             (!is.null(zi_covariates) |
             flag_zi_intercept |
             flag_non_spatial_re)){
      c("binomial", "zeroinflatedbinomial1")
    }else if(family == "zero_inflated_negative_binomial" && is.null(zi_covariates)){
      "zeroinflatednbinomial1"
    }else{
      c("binomial", "zeroinflatednbinomial1")
    }
  }

  zi_n_sample <-
    if (!is.null(data_prediction) &&
        family == "zero_inflated_binomial") {
      250L
    } else{
      0L
    }

  fitted_model <- INLA::inla(
    formula,
    data = INLA::inla.stack.data(stack),
    family = inla_family(family, zi_covariates),
    Ntrials = INLA::inla.stack.data(stack)$Ntrials,
    control.compute = list(
      dic = TRUE,
      cpo = TRUE,
      waic = TRUE,
      config = n_sample > 0 | zi_n_sample > 0
    ),
    control.predictor = list(
      A = INLA::inla.stack.A(stack),
      compute = TRUE,
      link = 1
    ),
    control.family = switch(ifelse(family %>%
                                     stringr::str_detect("zero_inflated_") &&
                                     is.null(zi_covariates) &&
                                     !flag_zi_intercept &&
                                     !flag_non_spatial_re,
                                   family %>%
                                     stringr::str_replace("zero_inflated_",""),
                                   family),
                            "binomial" = list(link = "logit"),
                            "negative_binomial" = list(variant=0),
                            "zero_inflated_binomial" = list(
                              list(link = c("logit")),
                              list(link = c("logit"))),
                            "zero_inflated_negative_binomial" = list(
                              list(link = c("logit")),
                              list(variant=0, link = c("log")))),
    # control.results = list(
    #   return.marginals.predictor = !is.null(hdpi_probabilities)
    # ),
    verbose = FALSE
  )

  summary_fixed <- get_summary_fixed(fitted_model = fitted_model,
                                     model_name = family,
                                     family = family)
  summary_hyperpar <- get_summary_hyperpar(
    fitted_model = fitted_model,
    spde = spde,
    flag_non_spatial_re = flag_non_spatial_re,
    flag_spatial_re = flag_spatial_re,
    family = family,
    model_name = family,
    zi_covariates = zi_covariates,
    flag_zi_intercept = flag_zi_intercept
  )

  prediction <- if (!is.null(data_prediction)) {
    index.pred <- INLA::inla.stack.index(stack, "prediction")$data
    prediction <-
      fitted_model$summary.fitted.values[index.pred,] %>%
      tibble::as_tibble() %>%
      rename_inla_coef() %>%
      dplyr::mutate(coef_var = se / estimate,
                    half_iqr = (q0_975 - q0_025) / 2)
    if(!is.null(hdpi_probabilities)){
      hdpis <- purrr::map_dfc(hdpi_probabilities,
                              function(y) {
                                fitted_model$marginals.fitted.values[index.pred] %>%
                                  purrr::map_dfr(
                                    ~ INLA::inla.hpdmarginal(y, .x) %>%
                                      tibble::as_tibble() %>%
                                      dplyr::rename(
                                        !!paste0("hdpi", y, "_high") := "high",
                                        !!paste0("hdpi", y, "_low") := "low"
                                      )
                                  )
                              })
      prediction <- prediction %>%
        dplyr::bind_cols(hdpis)
    }

    if (n_sample > 0 || zi_n_sample > 0) {
      prediction <- if(family %in% c("negative_binomial", "zero_inflated_negative_binomial")){
        stop("Reimplement")
        prediction %>%
          dplyr::bind_cols(
            INLA::inla.posterior.sample(n_sample, fitted_model) %>%
              purrr::map_dfc( ~ .x$latent[index.pred] %>%
                                exp()) %>% # this is probably wrong for negative_binomial and zero_inflated_negative_binomial
              purrr::set_names(paste0(
                "count_sample_", seq.int(n_sample)
              ))
          )
      }else{
        prediction %>%
          dplyr::bind_cols(
            INLA::inla.posterior.sample(max(n_sample, zi_n_sample), fitted_model) %>%
              purrr::imap_dfc( ~ tibble::tibble(!!paste0("prevalence_sample_",.y) := .x$latent[index.pred] %>%
                                inverse_logit()))
          )
      }
    }
    if (flag_zero_inflated &&
        (!is.null(zi_covariates) |
        flag_zi_intercept |
        flag_non_spatial_re)) {
      prediction <-
        dplyr::bind_cols(prediction[1:nrow(data_prediction),] %>%
                           dplyr::rename_with(~ paste0("zero_", .x)),
                         prediction[-c(1:nrow(data_prediction)),]%>%
                           dplyr::rename_with(~ paste0("binomial_", .x)))
      p_zero <- prediction %>%
        tibble::rowid_to_column("id") %>%
        dplyr::select("id",dplyr::starts_with("zero_prevalence_sample_")) %>%
        dplyr::select(1:(1+zi_n_sample)) %>%
        tidyr::pivot_longer(cols = -1)
      p_binomial <- prediction %>%
        tibble::rowid_to_column("id") %>%
        dplyr::select("id",dplyr::starts_with("binomial_prevalence_sample_")) %>%
        dplyr::select(1:(1+zi_n_sample)) %>%
        tidyr::pivot_longer(cols = -1)
      p_zib <- p_zero %>%
        dplyr::mutate(binomial_value = p_binomial$value,
                      zero = purrr::map_dbl(value, ~ rbinom(n = 1, size = 1, .x)),
                      p_zib = (1 - zero) * binomial_value)
      summary_p_zib <- p_zib %>%
        dplyr::group_by(id) %>%
        dplyr::summarize(zib_estimate = mean(p_zib),
                         zib_se = sd(p_zib),
                         zib_q0_025 = quantile(p_zib,probs = 0.025),
                         zib_q0_5 = quantile(p_zib,probs = 0.5),
                         zib_q0_975 = quantile(p_zib,probs = 0.975),
                         zib_coef_var = zib_se/zib_estimate,
                         zib_half_iqr = .5 * (zib_q0_975 - zib_q0_025)) %>%
        dplyr::select(-id)
      if(n_sample > 0){
        sample_p_zib <- p_zib %>%
          dplyr::group_by(id) %>%
          dplyr::slice(1:n_sample) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(name = name %>% stringr::str_extract("prevalence_sample.*$")) %>%
          dplyr::select(id, name, p_zib) %>%
          tidyr::pivot_wider(names_from = "name", values_from = "p_zib") %>%
          dplyr::select(-id)
        summary_p_zib <- dplyr::bind_cols(summary_p_zib, sample_p_zib)
      }


      prediction <- prediction %>%
        dplyr::select(-dplyr::starts_with("zero_prevalence_sample_"),
                      -dplyr::starts_with("binomial_prevalence_sample_")) %>%
        dplyr::bind_cols(summary_p_zib)
    }

    prediction <- if(inherits(data_prediction, "sf")){
      prediction %>%
        dplyr::bind_cols(data_prediction %>%
                           get_inla_coords() %>%
                           tibble::as_tibble()) %>%
        sf::st_as_sf(coords = c("x", "y"),
                     crs = sf::st_crs(data_estimation)) %>%
        sf::st_join(data_prediction)
    }else{
      prediction %>% dplyr::as_tibble()
    }
  }

  summary_model <- tibble::tibble(
    "dic" = fitted_model$dic$dic,
    "waic" = fitted_model$waic$waic,
    "cpo_failures" = length(fitted_model$cpo$failure[fitted_model$cpo$failure>0]),
    "log_score" = -mean(log(fitted_model$cpo$cpo), na.rm = TRUE)
  )

  list(
    "model" = fitted_model,
    "summary_fixed" = summary_fixed,
    "summary_random" = NULL,
    "summary_hyperpar" = summary_hyperpar,
    "summary_model" = summary_model,
    "prediction" = prediction
  )

}

construct_inla_A <-
  function(data,
           family,
           zi_covariates,
           flag_zi_intercept,
           flag_spatial_re,
           flag_non_spatial_re,
           response,
           mesh) {
    if (flag_spatial_re) {
    switch(
      ifelse(family %>% stringr::str_detect("zero_inflated_") &&
               is.null(zi_covariates) &&
               !flag_zi_intercept &&
               !flag_non_spatial_re,
             family %>%  stringr::str_replace("zero_inflated_",""),
             family),
      "binomial" = list(
        INLA::inla.spde.make.A(mesh,
                               data %>%
                                 get_inla_coords()),
        1
      ),
      "negative_binomial" = list(
        INLA::inla.spde.make.A(mesh,
                               data %>%
                                 get_inla_coords()),
        1
      ),
      "zero_inflated_binomial" = list(INLA::inla.spde.make.A(
        mesh,
        rbind(
          data %>%
            get_inla_coords(),
          data %>%
            get_inla_coords()
        )
      ), 1),
      "zero_inflated_negative_binomial" = list(INLA::inla.spde.make.A(
        mesh,
        rbind(
          data %>%
            get_inla_coords(),
          data %>%
            get_inla_coords()
        )
      ), 1)
    )
  } else{
    list(1)
  }
}

