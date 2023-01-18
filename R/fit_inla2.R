#' Fit inla
#' `r lifecycle::badge("experimental")`
#' @param data_estimation The data for estimation, either a tibble or a sf
#' @param response Character vector of the name of the response in `data_estimation`. In the binomial case a vector of 2 containing 1) the response and 2) the number of trials (both integers)
#' @param covariates Name of the covariates or `NULL`
#' @param covariates01 Name of the covariates for the zero altered part of a zab, zap, zanb model,  `NULL` else.
#' @param family One of "binomial", "zib", "zab", "poisson", "zip", "zap", "negative_binomial", "zinb", "zanb" each for the normal and zero-inflated or zero-altered version
#' @param flag_intercept Set true if a intercept should be included
#' @param flag_intercept01
#' @param mesh Specify a `inla.mesh` if `flag_spatial_re` is true.
#' @param alpha Specify if `flag_spatial_re` is true, alpha = 1.5 is exponential spatial correlation.
#' @param flag_spatial_re
#' @param flag_non_spatial_re Set true if a non spatial random effect should be included
#' @param prior_pc_range The values c(r0, p) of P(r < r0 ) = p
#' @param prior_pc_sigma The values c(sigma0, p) of P(sigma > sigma0) = p
#' @param prior_iid Set the prior as values for $1/sigma^2 ~ Gamma(`prior_iid[1]`, `prior_iid[2]`)$ if `flag_non_spatial_re` is true.
#' @param data_prediction The data for prediction, either a tibble or a sf
#' @param n_sample A integer defining a desired number of draws from the posterior distribution
#' @param flag_spatial_re01
#' @param flag_keep_predictor_marginals
#' @param control.inla Use list(int.strategy = x), where x is one of "auto" (default) "ccd", "grid", "eb"
#'
#' @return
#' @export
fit_inla2 <- function(data_estimation,
                      response,
                      covariates,
                      covariates01 = NULL,
                      family = "binomial",
                      flag_intercept,
                      flag_intercept01 = FALSE,
                      mesh = NULL,
                      alpha = 1.5,
                      flag_spatial_re = FALSE,
                      flag_spatial_re01 = FALSE,
                      flag_non_spatial_re = FALSE,
                      prior_pc_range = c(0.3, 0.5),
                      prior_pc_sigma = c(10, 0.01),
                      prior_iid = c(0.01, 0.01),
                      data_prediction = NULL,
                      n_sample = 0L,
                      flag_keep_predictor_marginals = FALSE,
                      control.inla = list(int.strategy = "auto"),
                      flag_spatial_constr = FALSE,
                      flag_spatial_extraconstr = FALSE){

# check input -------------------------------------------------------------
  is_sf_data(data_estimation)
  is_covariates(data_estimation, covariates)
  if (!is.null(data_prediction)) {
    is_sf_data(data_prediction)
    is_covariates(data_prediction, covariates)
    if (!identical(sf::st_crs(data_estimation), sf::st_crs(data_estimation))) {
      stop("CRS for estimation and projection must be the same.")
    }
  }
  stopifnot(family %in% c("binomial", "zib", "zab",
                          "poisson", "zip", "zap",
                          "negative_binomial", "zinb", "zanb"))
  if(family %in% c("binomial", "zib", "zab")){
    is_binomial_response(data_estimation, response)
  }else{
    is_negative_binomial_response(data_estimation, response)
  }
  if((!family %in% c("zab", "zap", "zanb")) &
     !is.null(covariates01)){
    stop("It is not a zero-altered model. Do not specify covariates `covariates01`.")
  }else{
    if(!is.null(covariates01)){
      is_covariates(data_estimation, covariates01)
    }
  }
  stopifnot(purrr::is_scalar_integer(n_sample),
            n_sample > -1)
  purrr::walk(c(flag_intercept, flag_intercept01,
                flag_spatial_re, flag_non_spatial_re,
                flag_spatial_re01, flag_keep_predictor_marginals,
                flag_spatial_constr,
                flag_spatial_extraconstr), is_flag)
  if(!family %in% c("zib", "zip", "zinb") &&
     flag_intercept01){
    stop("`flag_intercept01` == TRUE only makes sense for zero inflated models.")
  }
  if(flag_spatial_re){
    if(is.null(mesh)){
      stop("Provide a mesh for spatial random effect or turn flag_spatial_re off.")
    }
    if(class(mesh) != "inla.mesh"){
      stop("Provide a mesh of class inla.mesh.")
    }
    stopifnot(!is.na(prior_pc_range),
              !is.na(prior_pc_sigma))
  }else{
    if(!is.null(mesh)){
      stop("Mesh must be NULL, because no spatial re is estimated.")
    }
  }
  if(!flag_spatial_re && (flag_spatial_constr | flag_spatial_extraconstr)){
    stop("`flag_spatial_constr` and `flag_spatial_extraconstr` only applicable if `flag_spatial_re` is TRUE.")
  }
  if (data_estimation %>%
      sf::st_drop_geometry() %>%
      dplyr::select(dplyr::all_of(covariates)) %>%
      dplyr::select(where(is.factor)) %>%
      ncol() > 0) {
    factor_covariates <- data_estimation %>%
      sf::st_drop_geometry() %>%
      dplyr::select(dplyr::all_of(covariates)) %>%
      dplyr::select(where(is.factor)) %>%
      colnames()
    purrr::walk(factor_covariates,
               ~ if(data_estimation %>%
                 dplyr::pull(dplyr::all_of(.x)) %>%
                 levels() %>%
                 stringr::str_detect("( |-)") %>%
                 any()){
                 stop("Factor covariate `" , .x, "` must not contain levels with empty characters or dashes.",
                      call. = FALSE)
               })
  }
  # points (estimation and prediction) must be within the border of the mesh
  # mesh, estimation and prediction must have same crs
  # emit warning if prediction has duplicate locations
  # check covariate names and see if names are occupied (spatial_field, ...01, etc.)


# build up for model ------------------------------------------------------
  input <- environment() %>% as.list()
  design_matrix <- get_covariates_design_matrix(data_estimation,
                                                covariates,
                                                data_prediction,
                                                flag_intercept,
                                                flag_append01 = FALSE)
  formula_fixed <- paste0("y ~ -1 +",
                          paste0(names(design_matrix$covariates_estimation),
                                 collapse = "+"))

  flag_za_cov <- if(any(flag_intercept01, covariates01)){
    design_matrix01 <- get_covariates_design_matrix(data_estimation,
                                                  covariates01,
                                                  data_prediction,
                                                  flag_intercept01,
                                                  flag_append01 = TRUE)
    formula_fixed <- paste0(c(formula_fixed,
                            paste0(names(design_matrix01$covariates_estimation),
                                   collapse = "+")),
                            collapse = "+")
    TRUE
  }else{
    design_matrix01 <- NULL
    FALSE
  }

  projector_A_estimation <- get_inla_projector(data_estimation,
                                    flag_spatial_re,
                                    flag_non_spatial_re,
                                    mesh)
  if(!is.null(data_prediction)){
    projector_A_prediction <- get_inla_projector(data_prediction,
                                                flag_spatial_re,
                                                flag_non_spatial_re,
                                                mesh)
  }
  if(flag_non_spatial_re){
    formula_non_spatial_re <-
      get_inla_non_spatial_re_formula("non_spatial_index",
                                      "loggamma",
                                      prior_iid)
    joint_geom <- dplyr::bind_rows(data_estimation,
                                   data_prediction) %>%
      sf::st_geometry() %>%
      sf::st_as_sf() %>%
      dplyr::distinct()
    non_spatial_index <- data_estimation %>%
      sf::st_intersects(joint_geom, sparse = FALSE) %>%
      apply(1, which)
    if(!is.null(data_prediction)){
      non_spatial_index <- non_spatial_index %>%
        c(data_prediction %>%
            sf::st_intersects(joint_geom, sparse = FALSE) %>%
            apply(1, which))
    }
    if(flag_za_cov){
      stop("not tested yet.")
      formula_non_spatial_re <- paste0(
        formula_non_spatial_re,
        get_inla_non_spatial_re_formula("non_spatial_index01",
                                        "loggamma",
                                        prior_iid))
      non_spatial_index01 <- non_spatial_index
    }else{
      non_spatial_index01 <- NULL
    }
  }else{
    formula_non_spatial_re <- ""
    non_spatial_index <- NULL
  }

  if (flag_spatial_re) {
    spatial_extraconstr <- if(flag_spatial_extraconstr){
      A_constr <- INLA::inla.spde.make.A(mesh,
                             loc = data_estimation %>%
                               sf::st_coordinates())
      Q <- qr.Q(qr(design_matrix %>%
                     purrr::pluck("covariates_estimation")))
      list(A = as.matrix(t(Q) %*% A_constr), e = rep(0, ncol(Q)))
    }else{
      NULL
    }
    spde <- INLA::inla.spde2.pcmatern(
      mesh = mesh,
      alpha = alpha,
      constr = flag_spatial_constr,
      extraconstr = spatial_extraconstr,
      prior.range = prior_pc_range,
      prior.sigma = prior_pc_sigma
    )
    formula_spatial_re <- "+ f(spatial_field, model=spde)"
    spatial_index <- INLA::inla.spde.make.index(name = "spatial_field",
                                                n.spde = spde$n.spde)
    spatial_index01 <- NULL
    if(flag_spatial_re01){
      formula_spatial_re <- paste0(formula_spatial_re,
                                   "+ f(spatial_field01, model=spde)")
      spatial_index01 <- INLA::inla.spde.make.index(name = "spatial_field01",
                                                  n.spde = spde$n.spde)
    }else{
      spatial_index01 <- NULL
    }
  }else{
    formula_spatial_re <- ""
    spatial_index <- NULL
    spatial_index01 <- NULL
    spde = NULL
  }

  stack_list <- list()
  if(!flag_za_cov & !flag_spatial_re01){
    stack_list <- stack_list %>%
      append(list(
        INLA::inla.stack(
          data = get_inla_data(data = data_estimation,
                               response = response,
                               mode = "normal",
                               flag_estimation = TRUE),
          A = projector_A_estimation,
          effects = get_inla_effects(
            design_matrix$covariates_estimation,
            flag_non_spatial_re,
            non_spatial_index[1:nrow(design_matrix$covariates_estimation)],
            flag_spatial_re,
            spatial_index,
            flag_spatial_re01,
            spatial_index01
          ),
          tag = "estimation"
        )
      ))
    if(!is.null(data_prediction)){
      stack_list <- stack_list %>%
        append(list(
          INLA::inla.stack(
            data = get_inla_data(data = data_prediction,
                                 response = response,
                                 mode = "normal",
                                 flag_estimation = FALSE),
            A = projector_A_prediction,
            effects = get_inla_effects(
              design_matrix$covariates_prediction,
              flag_non_spatial_re,
              non_spatial_index[nrow(design_matrix$covariates_estimation)+1:nrow(design_matrix$covariates_prediction)],
              flag_spatial_re,
              spatial_index,
              flag_spatial_re01,
              spatial_index01
            ),
            tag = "prediction"
          )
        ))
    }
  }else{
    stack_list <- stack_list %>%
      append(list(
        INLA::inla.stack(
          data = get_inla_data(data = data_estimation,
                               response = response,
                               mode = "za_truncated",
                               flag_estimation = TRUE),
          A = projector_A_estimation,
          effects = get_inla_effects(
            design_matrix$covariates_estimation,
            flag_non_spatial_re,
            non_spatial_index,
            flag_spatial_re,
            spatial_index,
            FALSE,
            spatial_index01
          ),
          tag = "estimation"
        )
      ))
    stack_list <- stack_list %>%
      append(list(
        INLA::inla.stack(
          data = get_inla_data(data = data_estimation,
                               response = response,
                               mode = "za01",
                               flag_estimation = TRUE),
          A = projector_A_estimation,
          effects = get_inla_effects(
            design_matrix01$covariates_estimation,
            flag_non_spatial_re,
            non_spatial_index01,
            FALSE,
            spatial_index01,
            flag_spatial_re01,
            spatial_index01
          ),
          tag = "estimation01"
        )
      ))
    if(!is.null(data_prediction)){
      stack_list <- stack_list %>%
        append(list(
          INLA::inla.stack(
            data = get_inla_data(data = data_prediction,
                                 response = response,
                                 mode = "normal",
                                 flag_estimation = FALSE),
            A = projector_A_prediction,
            effects = get_inla_effects(
              design_matrix$covariates_prediction,
              flag_non_spatial_re,
              non_spatial_index,
              flag_spatial_re,
              spatial_index,
              FALSE,
              spatial_index01
            ),
            tag = "prediction"
          )
        ))
      stack_list <- stack_list %>%
        append(list(
          INLA::inla.stack(
            data = get_inla_data(data = data_prediction,
                                 response = response,
                                 mode = "normal",
                                 flag_estimation = FALSE),
            A = projector_A_prediction,
            effects = get_inla_effects(
              design_matrix$covariates_prediction,
              flag_non_spatial_re,
              non_spatial_index01,
              FALSE,
              spatial_index01,
              flag_spatial_re01,
              spatial_index01
            ),
            tag = "prediction01"
          )
        ))
    }
  }
  stack <- purrr::reduce(stack_list, INLA::inla.stack)
  formula_str <- paste0(formula_fixed,
                        formula_spatial_re,
                        formula_non_spatial_re)
  formula = as.formula(formula_str)
  inla_ntrials <- get_inla_ntrials(data_estimation, response, family, flag_za_cov, data_prediction)
  inla_family <- get_inla_family(family, flag_za_cov)
  fit <- try(INLA::inla(
    formula,
    data = INLA::inla.stack.data(stack),
    family = inla_family$family,
    Ntrials = inla_ntrials,
    control.family = inla_family$control.family,
    # inla.mode = "experimental",
    control.compute = list(
      dic = TRUE,
      cpo = TRUE,
      waic = TRUE,
      config = TRUE,
      return.marginals.predictor = flag_keep_predictor_marginals
    ),
    control.predictor = list(
      A = INLA::inla.stack.A(stack),
      compute = TRUE,
      link = 1
    ),
    control.inla = control.inla,
    verbose = FALSE
  ))
  if(family != "binomial"){
    new_helper_inla_output(
      fit,
      formula,
      family,
      stack,
      stack$data$index %>% names(),
      design_matrix,
      if(any(flag_intercept01, covariates01)){
        design_matrix01
      }else{
        NULL
      },
      spde,
      input,
      get_summary_fixed(list(
        fit = fit,
        spde = spde,
        family = family,
        input =  input)),
      get_summary_hyperpar(list(
        fit = fit,
        spde = spde,
        family = family,
        input =  input))
    )
  }else{
    new_helper_inla_output_binomial(
      fit,
      formula,
      family,
      stack,
      stack$data$index %>% names(),
      design_matrix,
      if(any(flag_intercept01, covariates01)){
        design_matrix01
      }else{
        NULL
      },
      spde,
      input,
      get_summary_fixed(list(
        fit = fit,
        spde = spde,
        family = family,
        input =  input)),
      get_summary_hyperpar(list(
        fit = fit,
        spde = spde,
        family = family,
        input =  input))
    )
  }
}


# inla posterior functions ------------------------------------------------
#' Plot fitted values
#'
#' @param inla_output
#'
#' @return plot
#' @export
plot_fitted_values <- function(inla_output){
  if(inla_output$input$family %in% c("binomial", "zib", "zab")){
    observed <- inla_output$input$data_estimation[[inla_output$input$response[1]]] /
      inla_output$input$data_estimation[[inla_output$input$response[2]]]
  }else{
    observed <- inla_output$input$data_estimation[[inla_output$input$response[1]]]
  }
  fitted <- get_inla_prediction_summary(inla_output,
                                        stack_tag = "estimation")$estimate
  plot(fitted,
       observed,
       xlab = "Fitted values",
       ylab = paste0("Observed #", inla_output$input$response[1]),
       asp = 1)
  abline(a = 0, b = 1)
}

#' Get INLA prediction summary
#' `r lifecycle::badge("experimental")`
#'
#' @param inla_output Inla output
#' @param stack_tag Either "estimation" or "prediction"
#' @param quantile_interval_probabilities Probabilities between [0,1], may be a vector
#' @param flag_add_predictors Should predictor variables be added to output?
#'
#' @return A `tibble` or `sf` depending on the input
#' @export
#'
#' @examples
get_inla_prediction_summary <- function(inla_output,
                                        stack_tag = "prediction",
                                        quantile_interval_probabilities = NULL,
                                        flag_add_predictors = FALSE){
  is_flag(flag_add_predictors)
  if(!purrr::is_scalar_character(stack_tag)){
    stop("`stack_tag` needs to be a character of length 1
         in `get_inla_prediction_summary`.",
         call. = FALSE)
  }
  if(!is.null(quantile_interval_probabilities)){
    is_probabilities(quantile_interval_probabilities)
    if(is.null(inla_output$fit$marginals.fitted.values)){
      stop("In `get_inla_prediction_summary`: there are no marginals stored for the fitted values.
           Try to set `flag_keep_predictor_marginals` = TRUE when calling `fit_inla2`.",
           call. = FALSE)
    }
  }
  if(inla_output$family %in% c("binomial", "poisson")){
    index_pred <- get_inla_prediction_index(inla_output, stack_tag)
    prediction_summary <- inla_output$fit$summary.fitted.values[index_pred, ] %>%
      tibble::as_tibble() %>%
      rename_inla_coef() %>%
      dplyr::mutate(coef_var = se / estimate,
                    half_iqr = (q0_975 - q0_025) / 2)
    if(!is.null(quantile_interval_probabilities)){
      warning("This bit of `get_inla_prediction_summary is not tested yet.",
              call. = FALSE)
      hdpis <- purrr::map_dfc(quantile_interval_probabilities,
                              function(y) {
                                inla_output$fit$marginals.fitted.values[index_pred] %>%
                                  purrr::map_dfr(
                                    ~ tibble::tibble(
                                      !!paste0("quantile_interval", y, "_low") := INLA::inla.qmarginal(.5 - y / 2, .x),
                                      !!paste0("quantile_interval", y, "_high") := INLA::inla.qmarginal(.5 + y / 2, .x),
                                    )
                                  )
                              })
      prediction_summary <- prediction_summary %>%
        dplyr::bind_cols(hdpis)
    }
  }else if(inla_output$family == "zib"){
    posterior_sample_t <- get_inla_prediction_sample(inla_output,
                               n_sample = 200L,
                               stack_tag = stack_tag)[, 1:200] %>%
      drop_geometry_if_exists() %>%
      as.matrix() %>%
      t()
    prediction_summary <- tibble::tibble(
      estimate = posterior_sample_t %>% apply(2, mean),
      se = posterior_sample_t %>% apply(2, sd),
      q0_025 = posterior_sample_t %>% apply(2, function(x) quantile(x, probs = 0.025)),
      q0_5 = posterior_sample_t %>% apply(2, function(x) quantile(x, probs = 0.5)),
      q0_975 = posterior_sample_t %>% apply(2, function(x) quantile(x, probs = 0.975)),
      coef_var = se / estimate,
      half_iqr = (q0_975 - q0_025) / 2) %>%
      dplyr::mutate(dplyr::across(1:7, ~.x %>% unname()))
    if(!is.null(quantile_interval_probabilities)){
      warning("This bit of `get_inla_prediction_summary is not tested yet.",
              call. = FALSE)
      hdpis <- purrr::map_dfc(
        quantile_interval_probabilities,
        function(y) {
          tibble::tibble(
            !!paste0("quantile_interval", y, "_low") := posterior_sample_t %>%
              apply(2, function(x) quantile(x, probs = c(.5 - y / 2))),
            !!paste0("quantile_interval", y, "_high") := posterior_sample_t %>%
              apply(2, function(x) quantile(x, probs = c(.5 + y / 2)))
          )
        })
      prediction_summary <- prediction_summary %>%
        dplyr::bind_cols(hdpis)
    }
  }else{
    stop("Function `get_inla_prediction_summary` not implemented for this family.")
  }
  if(flag_add_predictors){
    prediction_summary <- if(stack_tag == "prediction"){
      prediction_summary %>%
        dplyr::bind_cols(inla_output$input$data_prediction)
    }else if(stack_tag == "estimation"){
      prediction_summary %>%
        dplyr::bind_cols(inla_output$input$data_estimation)
    }
  }
  prediction_summary %>%
    add_sf_to_inla_prediction(inla_output)
}

#' Get a prediction sample
#' `r lifecycle::badge("experimental")`
#' @inheritParams get_inla_prediction_summary
#' @param n_sample Number of samples
#'
#' @return A `tibble` or `sf` depending on the input
#' @export
get_inla_prediction_sample <- function(inla_output,
                                       n_sample,
                                       stack_tag = "prediction",
                                       flag_add_predictors = FALSE){
  is_flag(flag_add_predictors)
  if(!purrr::is_scalar_integer(n_sample))
    stop(call. = FALSE, "`n_sample` has to be a scalar integer")
  if(!purrr::is_scalar_character(stack_tag)){
    stop("`stack_tag` needs to be a character of length 1
         in `get_inla_prediction_summary`.",
         call. = FALSE)
  }

  index_pred <- inla_output %>%
    get_inla_prediction_index(stack_tag)
  posterior_sample <- INLA::inla.posterior.sample(n_sample, inla_output$fit)
  posterior_sample <- if(inla_output$family == "binomial"){
    posterior_sample %>%
      purrr::imap_dfc(~ tibble::tibble(
        !!paste0("prevalence_sample_", .y) := .x$latent[index_pred] %>%
          inverse_logit()
      ))
  }else if(inla_output$family == "zib") {
    warning("I am not sure whether this is correct.")
    posterior_sample %>%
      purrr::imap_dfc(~ {
        y01 <- rbinom(n = length(index_pred),
                      size = 1,
                      prob = .x$hyperpar["zero-probability parameter for zero-inflated binomial_1"])
        tibble::tibble(
        !!paste0("prevalence_sample_", .y) :=
          (1 - y01) * inverse_logit(.x$latent[index_pred]))
      })
  }else if(inla_output$family %in% c("poisson", "zip", "negative_binomial", "zinb")){
    posterior_sample %>%
      purrr::imap_dfc(~ tibble::tibble(
        !!paste0("prevalence_sample_", .y) := .x$latent[index_pred] %>% exp()
      ))
  }else{
    stop("`get_inla_prediction_sample` has not been implemented for family ",
         inla_output$family, ".")
  }
  if(flag_add_predictors){
    posterior_sample <- if(stack_tag == "prediction"){
      posterior_sample %>%
        dplyr::bind_cols(inla_output$input$data_prediction)
    }else if(stack_tag == "estimation"){
      posterior_sample %>%
        dplyr::bind_cols(inla_output$input$data_estimation)
    }
  }

  posterior_sample %>%
    add_sf_to_inla_prediction(inla_output)
}

get_inla_prediction_index <- function(inla_output,
                                      stack_tag = "prediction"){
  if(!stack_tag %in% inla_output$stack_names){
    stop("INLA has no stack tagged `stack_tag` = '",
         stack_tag,
         "'.")
  }
  INLA::inla.stack.index(inla_output$stack, stack_tag)$data
}

add_sf_to_inla_prediction <- function(prediction, inla_output){
  if(inherits(inla_output$input$data_prediction, "sf")){
    prediction %>%
      dplyr::bind_cols(inla_output$input$data_prediction %>%
                         get_inla_coords() %>%
                         tibble::as_tibble()) %>%
      sf::st_as_sf(coords = c("x", "y"),
                   crs = sf::st_crs(inla_output$input$data_estimation))
  }else{
    prediction
  }
}

#' Plot expected number of zeros from simulations
#'
#' @param inla_output Inla output
#' @param n_sample Number of simulations
#'
#' @return Plot of the simulated number of zeros
#' @export
plot_inla_zero_simulations <- function(inla_output, n_sample = 1000L){
  stopifnot(purrr::is_scalar_integer(n_sample))
  if(!is.null(inla_output$input$data_prediction)){
    stop("This function was not tested for prediction. Data may be wrongly extracted.")
  }
  if(inla_output$family %in% c("poisson","binomial", "zip", "zib", "zap")){
    simulated_data <- INLA::inla.posterior.sample(n = n_sample,
                                                  result = inla_output$fit)
    get_row_num <- function(x){ which(rownames(simulated_data[[1]]$latent) == x) }
    n_obs <- nrow(inla_output$input$data_estimation)
    row_num_predictor <- paste0("APredictor:", 1:n_obs) %>%
      purrr::map_int(get_row_num)

    n_zeros_simulations <-
      purrr::map_dbl(simulated_data, function(x) {
        predictor <- x$latent[row_num_predictor]
        ysim <- if(inla_output$family == "poisson"){
          rpois(n_obs,
                lambda = exp(predictor))
        }else if(inla_output$family == "binomial"){
          inla_output$input$data_estimation %>%
            dplyr::pull(inla_output$input$response[2]) %>%
            purrr::imap_dbl(~ rbinom(1,
                           size = .x,
                           prob = inverse_logit(as.vector(predictor[.y]))))

        }else{
          if (!requireNamespace("VGAM", quietly = TRUE)) {
            stop("Package \"pkg\" needed for `plot_inla_zero_simulations` to work. Please install it.",
                 call. = FALSE)
          }
          if(inla_output$family == "zip"){
            VGAM::rzipois(n_obs,
                          lambda = exp(predictor),
                          pstr0 = x$hyperpar[["zero-probability parameter for zero-inflated poisson_1"]])
          }else if(inla_output$family == "zap" &
                   is.null(inla_output$input$covariates01) &
                   !inla_output$input$flag_intercept01){
            VGAM::rzapois(n_obs,
                          lambda = exp(predictor),
                          pobs0 = x$hyperpar)
          }else if(inla_output$family == "zap" &
                   (!is.null(inla_output$input$covariates01) |
                    inla_output$input$flag_intercept01)){
            stop("Not tested yet.")
            row_num_fixed01 <- inla_output$covariate_matrix01$covariates_estimation %>%
              names() %>%
              paste0(":1") %>%
              purrr::map_int(parameter_names01, get_row_num)
            fixed_01 <- as.matrix(inla_output$covariate_matrix01$covariates_estimation) %*%
              x$latent[row_num_fixed01]
            VGAM::rzapois(nrow(inla_output$covariate_matrix$covariates_estimation),
                          lambda = exp(predictor),
                         pobs0 = 1 - inverse_logit(fixed_01))
          }else if(inla_output$family == "zib"){
            inla_output$input$data_estimation %>%
              dplyr::pull(inla_output$input$response[2]) %>%
              purrr::imap_dbl(
                ~ VGAM::rzibinom(1,
                                 size = .x,
                                 prob = inverse_logit(predictor[.y]),
                                 pstr0 = x$hyperpar["zero-probability parameter for zero-inflated binomial_1"]))
         }
       }
       sum(ysim == 0) })

    n_zeros_data <- sum(inla_output$input$data_estimation %>%
                          dplyr::pull(inla_output$input$response[1]) == 0)

    par(mar = c(5,5,2,2), cex.lab = 1.5, pty = "s")
    plot(
      table(n_zeros_simulations),
      xlab = "How often do we have 0, 1, 2, ... #zeros",
      ylab = paste("#zeros in",
                   n_sample,
                   "simulated data sets"),
      xlim = c(max(min(c(
        n_zeros_data, n_zeros_simulations
      )) - 10,0), max(c(
        n_zeros_data, n_zeros_simulations
      )) + 10),
      main = "Simulation results"
    )
    points(x = n_zeros_data,
           y = 0,
           pch = 16,
           cex = 4,
           col = grDevices::rgb(red = 1, green = 0, blue = 0, alpha = 0.7))
  }else{
    stop("This family is not implemented for `plot_inla_zero_simulations`.")
  }
}

#' Calculating frequentist dispersion
#' @details Zuur et al. p. 167 describe that in a frequentist setting the over/underdispersion is determined with a pearson test (value = 1 means no over/underdispersion, >1 means overdispersion, < 1 underdispersion) and they do it for the Bayesian analysis in the same way, arguing that if diffuse priors are taken the results should be similar.
#' @param inla_output Output from `fit_inla2`
#'
#' @return
#' @export
#'
#' @examples
get_inla_dispersion <- function(inla_output){
  ssr <- get_inla_pearson_residuals(inla_output)
  if(inla_output$family == "binomial"){
    p <- nrow(inla_output$fit$summary.fixed)
  }else if(inla_output$family == "poisson"){
    p <- nrow(inla_output$fit$summary.fixed)
  }else if(inla_output$family == "zip"){
    p <- nrow(inla_output$fit$summary.fixed) + 1
  }else if(inla_output$family == "zap"){
    if("zero-probability parameter for zero-inflated poisson_0" %in% row.names(inla_output$fit$summary.hyperpar)){
    p <- nrow(inla_output$fit$summary.fixed) + 1
    }else{
      p <- nrow(inla_output$fit$summary.fixed)
    }
  }else if(inla_output$family == "negative_binomial"){
    p <- nrow(inla_output$fit$summary.fixed) + 1
  }else if(inla_output$family == "zinb"){
    stop("Not implemented yet. See Zuur et al. Vol 2 p. 401 for formula.")
  }else if(inla_output$family == "zanb"){
    stop("Not implemented yet. See Zuur et al. Vol 2 p. 401 for formula.")
  }else{
    stop("Not implemented yet.")
  }
  if(inla_output$input$flag_spatial_re){
    warning("The assumption of p = p + 2 for the spatial random effect is a quick and dirty assumption (zuur et al. p. 316 )")
    p <- p + 2
  }
  if(inla_output$input$flag_non_spatial_re){
    p <- p + 1
  }
  n <- length(ssr)
  sum(ssr^2) / (n - p)
}

get_inla_pearson_residuals <- function(inla_output){
  ind <- INLA::inla.stack.index(inla_output$stack, "estimation")$data
  observed <- if(ncol(inla_output$stack$data$data) == 1){
    inla_output$stack$data$data$y
  }else{
    temp <- inla_output$stack$data$data$y.1[ind]
    temp[is.na(temp)] <- 0
    temp
  }
  posterior_mean <- inla_output$fit$summary.fitted.values$mean[ind]
  if(length(observed) != length(posterior_mean)){
    stop("Length of observed and expected values do not match.")
  }
  if(inla_output$family == "binomial"){
    #binomial case y ~Binomial(p, n_tries)
    n_tries <- inla_output$input$data_estimation %>%
      dplyr::pull(dplyr::all_of(inla_output$input$response[2]))
    expected <- posterior_mean * n_tries
    variance <- n_tries * posterior_mean * (1 - posterior_mean)
  }else if(inla_output$family == "poisson"){
    #poisson p.166, p.168
    expected <- variance <- posterior_mean
  }else if(inla_output$family == "zip"){
    pi01 <- inla_output$fit$summary.hyperpar["zero-probability parameter for zero-inflated poisson_1","mean"]
    expected <- (1 - pi01) * posterior_mean
    variance <- (1 - pi01) * (posterior_mean + pi01 * posterior_mean^2)
  }else if(inla_output$family == "zap"){
    if("zero-probability parameter for zero-inflated poisson_0" %in% row.names(inla_output$fit$summary.hyperpar)){
      pi01 <- inla_output$fit$summary.hyperpar["zero-probability parameter for zero-inflated poisson_0","mean"]
      expected <- pi01 * posterior_mean / (1 - exp(- posterior_mean))
      variance <-  (pi01 / (1 - exp(- posterior_mean))) * (posterior_mean + posterior_mean^2) -(pi01 * posterior_mean / (1 - exp(- posterior_mean)))^2
      warning("Not tested yet.")
    }else{
      ind01 <- INLA::inla.stack.index(inla_output$stack, "estimation01")$data
      pi01 <- inla_output$fit$summary.fitted.values$mean[ind01]
      expected <- pi01 * posterior_mean / (1 - exp(- posterior_mean))
      variance <-  (pi01 / (1 - exp(- posterior_mean))) * (posterior_mean + posterior_mean^2) -(pi01 * posterior_mean / (1 - exp(- posterior_mean)))^2
    }
  }else if(inla_output$family == "negative_binomial"){
    #negative binomial p173 zuur et al.
    posterior_mean_k <- inla_output$fit$summary.hyperpar["size for the nbinomial observations (1/overdispersion)", "mean"]
    expected <- posterior_mean
    variance <- posterior_mean + posterior_mean^2 / posterior_mean_k
    warning("Not tested yet.")
  }else if(inla_output$family == "zinb"){
    stop("Not implemented yet. See Zuur et al. Vol 2 p. 401 for formula.")
  }else if(inla_output$family == "zanb"){
    stop("Not implemented yet. See Zuur et al. Vol 2 p. 401 for formula.")
  }else{
    stop("Not implemented yet.")
  }
  (observed - expected) / sqrt(variance)
}

# inla prior functions ----------------------------------------------------
get_inla_non_spatial_re_formula <- function(name, distribution, param){
  paste0(
    "+ f(",
    name,
    ", model = 'iid', hyper=list(prec=list(param=c(",
    param[1],
    "," ,
    param[2],
    "),prior='",
    distribution,
    "')))"
  )
}

get_inla_ntrials <- function(data_estimation, response, family, flag_za_cov, data_prediction){
  if(family %in% c("binomial", "zib", "zab")){
    inla_ntrials <- data_estimation[[response[2]]]
    if(!is.null(data_prediction)){
      inla_ntrials <- inla_ntrials %>%
        c(rep(1, nrow(data_prediction)))
    }
    if(flag_za_cov){
      inla_ntrials <- inla_ntrials %>%
        cbind(1)
    }
  }else{
    inla_ntrials <- NULL
  }
  inla_ntrials
}

get_inla_family <- function(family, flag_za_cov){
  inla_family <- switch(
    family,
    "binomial" = "binomial",
    "zib" = "zeroinflatedbinomial1",
    "zab" = "zeroinflatedbinomial0",
    "negative_binomial" = "nbinomial",
    "zinb" = "zeroinflatednbinomial1",
    "zanb" = "zeroinflatednbinomial0",
    "poisson" = "poisson",
    "zip" = "zeroinflatedpoisson1",
    "zap" = "zeroinflatedpoisson0"
  )
  inla_control.family = list()
  if(flag_za_cov){
    stopifnot(family %in% c("zab", "zanb", "zap"))
    inla_family <- c(inla_family, "binomial")
    hyperzap <- list(theta = list(initial = -10,
                                  fixed = TRUE))
    inla_control.family <- list(list(hyper = hyperzap),
                                list())
  }
  list("family" = inla_family,
       "control.family" = inla_control.family)
}

get_inla_projector <- function(data,
                               flag_spatial_re,
                               flag_non_spatial_re,
                               mesh) {
  A <- list(1)
  if(flag_non_spatial_re){
    A <- A %>%
      append(1)
  }
  if (flag_spatial_re) {
    A <- A %>%
      append(INLA::inla.spde.make.A(mesh = mesh,
                                    loc = data %>%
                                      get_inla_coords()))
  }
  A
}

get_inla_effects <- function(covariates_matrix,
                             flag_non_spatial_re,
                             non_spatial_index,
                             flag_spatial_re,
                             spatial_index,
                             flag_spatial_re01,
                             spatial_index01){
  effects <- list(covariates_matrix)
  if(flag_non_spatial_re){
    effects <- append(effects,
                      list("non_spatial_index" = non_spatial_index))

  }
  if(flag_spatial_re){
    effects <- append(effects,
                      list("spatial_index" = spatial_index))
  }
  if(flag_spatial_re01){
    effects <- append(effects,
                      list("spatial_index01" = spatial_index01))
  }
  effects
}

get_inla_data <- function(data, response, mode, flag_estimation) {
  stopifnot(mode %in% c("normal", "za_truncated", "za01"))
  if (flag_estimation) {
    if (mode == "normal") {
      list(y = data[[response[1]]])
    } else if (mode == "za_truncated") {
      list(y = cbind(ifelse(data[[response[1]]] > 0, data[[response[1]]], NA_real_),
                     NA_real_))
    } else if (mode == "za01") {
      list(y = cbind(NA_real_,
                     data[[response[1]]] > 0))
    }
  } else{ # if prediction stack
    if (mode == "normal") {
      list(y = rep(NA_real_, nrow(data)))
    } else{
      list(y = cbind(rep(NA_real_, nrow(data)),
                     rep(NA_real_, nrow(data))))
    }
  }
}

get_covariates_design_matrix <- function(data_estimation,
                                         covariates,
                                         data_prediction,
                                         flag_intercept,
                                         flag_append01){

  scaled_data <- data_estimation  %>%
    scale_vars(covariates = covariates,
               data_prediction = data_prediction)
  data_estimation <- scaled_data$data_estimation
  data_prediction <- scaled_data$data_prediction

  resolved_factors <- data_estimation %>%
    resolve_factors(covariates, data_prediction)
  covariates <- c(resolved_factors$covariates,
                  if(flag_intercept) "intercept" else NULL)
  covariates_estimation <- resolved_factors$data_estimation %>%
    drop_geometry_if_exists()  %>%
    conditional_mutate(intercept = 1, condition = flag_intercept) %>%
    dplyr::select(all_of(covariates)) %>%
    conditional_rename_with(~ paste0(.x, "01"), condition = flag_append01)
  if(!is.null(data_prediction)){
    covariates_prediction <- resolved_factors$data_prediction %>%
      drop_geometry_if_exists() %>%
      conditional_mutate(intercept = 1, condition = flag_intercept) %>%
      dplyr::select(all_of(covariates)) %>%
      conditional_rename_with(~ paste0(.x, "01"), condition = flag_append01)
    if(!is.null(data_prediction) && any(is.na(covariates_prediction))){
      warning("Some prediction covariates are NA.")
    }
    list(
      "covariates_estimation" = covariates_estimation,
      "covariates_prediction" = covariates_prediction
    )
  }else{
    if(any(is.na(covariates_estimation))){
      warning("Some estimation covariates are NA.")
    }
    list(
      "covariates_estimation" = covariates_estimation,
      "covariates_prediction" = NULL
    )
  }
}

