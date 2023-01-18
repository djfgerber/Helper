#' Non-spatial binomial bivariate summary
#' @inheritParams map_binomial_bivariate_summary
#' @param predictor_varname Character string containing the predictor variable name.
#'
#' @return A tibble with two lines (one for the intercept and one for the coefficient).
#' @export
#'
#' @examples
#'  mtcars %>%
#'    dplyr::mutate(x = 1:nrow(mtcars), y = 1:nrow(mtcars)) %>%
#'    sf::st_as_sf(coords = c("x", "y")) %>%
#'    dplyr::mutate(total = 8) %>%
#'    get_binomial_bivariate_summary("cyl", "hp", "total" )
#' mtcars_mod <- mtcars
#' mtcars_mod[10, "hp"] <- NA
#' mtcars_mod %>%
#'    dplyr::mutate(x = 1:nrow(mtcars), y = 1:nrow(mtcars)) %>%
#'    sf::st_as_sf(coords = c("x", "y")) %>%
#'   dplyr::mutate(total = 8) %>%
#'   get_binomial_bivariate_summary("cyl", "hp", "total", drop_na = TRUE )
get_binomial_bivariate_summary <-
  function(data,
           count_varname = "asc_pos",
           predictor_varname = "BIO10",
           total_varname = "total",
           family = "binomial",
           scale_var = TRUE,
           drop_na = FALSE) {
    stopifnot(
      inherits(data, c("sf", "tbl_df")),
      purrr::is_scalar_character(count_varname),
      purrr::is_scalar_character(predictor_varname),
      purrr::is_scalar_character(total_varname),
      family %in% c("binomial", "negative_binomial", "zero_inflated_negative_binomial"),
      purrr::is_scalar_logical(scale_var),
      purrr::is_scalar_logical(drop_na),
      count_varname %in% colnames(data),
      predictor_varname %in% colnames(data),
      family == "binomial" & !is.na(total_varname) & total_varname %in% colnames(data) |
        family %in% c("negative_binomial", "zero_inflated_negative_binomial") & is.na(total_varname)
    )

    data <- data %>%
      sf::st_drop_geometry()
    data <- if (family == "binomial") {
      data %>%
        dplyr::select(.data[[count_varname]],
                      .data[[predictor_varname]],
                      .data[[total_varname]])
    } else{
      data %>%
        dplyr::select(.data[[count_varname]],
                      .data[[predictor_varname]])
    }

    if(!drop_na & any(is.na(data)))
      stop("data should not contain NA. Remove NA manually or set drop_na = TRUE.")
    data <- data %>%
      tidyr::drop_na()
    if(nrow(data) < 2)
      stop("Not enough observations remain after NA removal.")

    if(scale_var & is.numeric(data[[predictor_varname]])){
      data[[predictor_varname]] <- scale(data[[predictor_varname]])
    }


    #TODO: remove waiting for profiling...
    #TODO: does this call MASS:::confint.glm?
    if(family == "binomial"){
      response <- data %>%
        dplyr::mutate(success = .data[[count_varname]],
                      failure = .data[[total_varname]] - success) %>%
        dplyr::select(success, failure) %>%
        as.matrix
      f <- stats::as.formula(paste0("response ~ ", predictor_varname))
      fitted_model <- stats::glm(formula = f,
                                family = stats::binomial,
                                data = data)

      ci <- fitted_model %>%
        stats::confint()
      m_tidy <- fitted_model %>%
        broom::tidy() %>%
        dplyr::select(term, estimate, std.error) %>%
        dplyr::rename(se = std.error) %>%
        tibble::tibble(q0_025 = ci[, 1],
                       q0_975 = ci[, 2])
      m_tidy %>%
        dplyr::select(-se) %>%
        dplyr::mutate(dplyr::across(-term, ~ exp(.))) %>%
        dplyr::full_join(m_tidy, by = "term", suffix = c("_or", "")) %>%
        dplyr::mutate(model = predictor_varname) %>%
        dplyr::select(model, term,!dplyr::ends_with("_or"), dplyr::everything()) %>%
        dplyr::mutate(fitted_model %>% broom::glance()) %>%
        dplyr::mutate(term = term %>% stringr::str_replace("\\(Intercept\\)", "intercept"))
    }else if(family == "negative_binomial"){
      f <- stats::as.formula(paste0(count_varname, " ~ ", predictor_varname))
      fitted_model <- MASS::glm.nb(formula = f, data = data)

      ci <- fitted_model %>%
        stats::confint()

      m_tidy <- fitted_model %>%
        broom::tidy() %>%
        dplyr::select(term, estimate, std.error) %>%
        dplyr::rename(se = std.error) %>%
        tibble::tibble(q0_025 = ci[, 1],
                       q0_975 = ci[, 2])
      m_tidy %>%
        dplyr::select(-se) %>%
        dplyr::mutate(dplyr::across(-term, ~ NA_real_)) %>%
        dplyr::full_join(m_tidy, by = "term", suffix = c("_or", "")) %>%
        dplyr::mutate(model = predictor_varname) %>%
        dplyr::select(model, term,!dplyr::ends_with("_or"), dplyr::everything()) %>%
        dplyr::mutate(fitted_model %>% broom::glance()) %>%
        dplyr::mutate(term = term %>% stringr::str_replace("\\(Intercept\\)", "intercept"))
    }else if(family == "zero_inflated_negative_binomial"){
      f_str_rhs <- paste0( " ~ ", predictor_varname)
      f_str <- count_varname %>% paste0(f_str_rhs)
      f <- stats::as.formula(f_str)
      f_zi <- stats::as.formula(f_str_rhs)

      glmmTMB::glmmTMB(f,
              ziformula = f_zi,
              family = glmmTMB::nbinom2,
              data = data)

    }
  }

#' Non-spatial binomial bivariate summary
#' @description `r lifecycle::badge("experimental")` The functions summarizes a biLvariate binomial\eqn{(n_i, p_i)} model with logit link and one predictor variable, i.e. \eqn{\eta_i = \beta_0 + \beta_1 \cdot x}, for a range of predictors.
#' @param data data which must contain all the variable names of the following.
#' @param positives_varname Character string of the variable containing the positives
#' @param predictor_varnames Character vector (length > 0) containing the predictor varnames
#' @param total_varname Character string of the variable containing the total.
#'
#' @return A per model coefficient summary in form of a tibble containing the terms model (which covariate was used), term (either Intercept or covariate name), estimates of the coefficients (estimate, se, q0_025, q0_975), estimates for the odds ratio (estimate_or, q0_025_or, q0_975_or) and model summaries (null.deviance, df.null, logLik, AIC, BIC, deviance, df.residual, and nobs)
#' @export
#'
#' @examples
map_binomial_bivariate_summary <- function(data, positives_varname,
                                           predictor_varnames, total_varname){
  stopifnot(
    purrr::is_scalar_character(positives_varname),
    purrr::is_scalar_character(total_varname),
    purrr::is_character(predictor_varnames),
    length(predictor_varnames) > 0,
    tibble::is_tibble(data),
    positives_varname %in% colnames(data),
    total_varname %in% colnames(data),
    all(predictor_varnames %in% colnames(data))
  )
  purrr::map(predictor_varnames,
             ~ get_binomial_bivariate_summary(data, positives_varname, .x, total_varname)) %>%
    dplyr::bind_rows()
}

conditional_mutate <- function(data, ..., condition){
  if(condition){
    data %>%
      dplyr::mutate(...)
  }else{
    data
  }
}
conditional_rename_with <- function(data, ..., condition){
  if(condition){
    data %>%
      dplyr::rename_with(...)
  }else{
    data
  }
}
conditional_filter <- function(data, ..., condition){
  if(condition){
    data %>%
      dplyr::filter(...)
  }else{
    data
  }
}


#' Plot bivariate regression coefficients
#'
#' @param data A tibble resulting from `map_binomial_bivariate_summary`
#' @param drop_intercept Flag drop the intercept.
#' @param family If `family == "binomial"` the odds ratio is used,
#' if `family == "negative_binomial"` the incidence rate ratio is used
#'
#' @return A ggplot of the coefficients and errorbars
#' @export
plot_bivariate_coef <- function(data,
                                flag_drop_intercept = FALSE,
                                family = "binomial",
                                translate_covariate_names_type = NA_character_,
                                flag_order_alphabetically = TRUE,
                                term_order = NULL) {
  stopifnot(family %in% c("binomial", "negative_binomial"),
            purrr::is_scalar_character(translate_covariate_names_type))
  is_data(data)
  is_flag(flag_drop_intercept)

  plot_function <- function(mean, upper, lower, x_axis_label){
    data %>%
      dplyr::mutate(influence = dplyr::if_else(
        .data[[lower]] > 1,
        "increasing",
        dplyr::if_else(.data[[upper]] < 1, "decreasing", "neutral")
      )) %>%
      ggplot2::ggplot(ggplot2::aes_string(y = "term", col = "influence")) +
      ggplot2::geom_point(ggplot2::aes_string(x = mean)) +
      ggplot2::geom_errorbar(ggplot2::aes_string(xmin = lower, xmax = upper)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 1)) +
      ggplot2::labs(x = x_axis_label, y = "")+
      ggplot2::theme_light() +
      ggplot2::theme(legend.position = "bottom")
  }

  data <- data %>%
    dplyr::mutate(term_old = term) %>%
    conditional_filter(!term %in% c("intercept", "zi_intercept"),
                       condition = flag_drop_intercept) %>%
    conditional_mutate(term = purrr::map_chr(term,
                                      ~ .x %>%
                                        translate_covariate_names(translate_covariate_names_type)),
                       condition = !is.na(translate_covariate_names_type)) %>%
    conditional_mutate(term = term %>% factor(levels = sort(unique(term), decreasing = TRUE)),
                       condition = flag_order_alphabetically)
  # if(){
  #   data %>%
  #     conditional_mutate(term = term %>% factor(levels = term_order),
  #                      condition = !is.null(term_order) & !flag_order_alphabetically) %>%
  # }


  switch(
    family,
    "binomial" = do.call(
      plot_function,
      list(
        mean = "estimate_or",
        upper = "q0_975_or",
        lower = "q0_025_or",
        x_axis_label = "odds ratio"
      )
    ),
    "negative_binomial" = do.call(
      plot_function,
      list(
        mean = "estimate_exp",
        upper = "q0_975_exp",
        lower = "q0_025_exp",
        x_axis_label = "incidence rate ratio"
      )
    )
  )
}

#' spatial binomial bivariate summary
#'
#' @inheritParams map_binomial_bivariate_summary
#' @param mesh A `inla.mesh`
#' @param prior.range prior range for pcmatern inla call
#' @param prior.sigma prior sigma for pcmatern inla call
#' @param alpha alpha for pcmatern inla call
#'
#' @return A tibble containing model estimation summaries of all bivariate models
#' @export
#'
#' @examples
map_binomial_bivariate_inla_summary <-
  function(data,
           predictor_varnames,
           positives_varname = "positives",
           total_varname = "total",
           mesh,
           prior.range = c(0.3, 0.5),
           prior.sigma = c(10, 0.01),
           alpha = 2) {
    stopifnot(
      purrr::is_scalar_character(positives_varname),
      !is.na(positives_varname),
      purrr::is_scalar_character(total_varname),
      !is.na(total_varname),
      purrr::is_character(predictor_varnames),
      length(predictor_varnames) > 0,
      !any(is.na(predictor_varnames)),
      inherits(data, "sf"),
      positives_varname %in% colnames(data),
      total_varname %in% colnames(data),
      all(predictor_varnames %in% colnames(data)),
      inherits(mesh, "inla.mesh")
    )

    spde <- INLA::inla.spde2.pcmatern(
      mesh,
      alpha = alpha,
      # P(practic.range < 0.3) = 0.5
      prior.range = prior.range,
      # P(sigma > 1) = 0.01
      prior.sigma = prior.sigma
    )
    coords <- data %>%
      sf::st_coordinates() %>%
      tibble::as_tibble() %>%
      dplyr::rename(x = X, y = Y) %>%
      as.matrix()
    A <- INLA::inla.spde.make.A(mesh, coords)
    s.index <- INLA::inla.spde.make.index("spatial.field", spde$n.spde)
    names(predictor_varnames) <- predictor_varnames
    purrr::map_dfr(
      predictor_varnames,
      ~ get_binomial_bivariate_inla_summary(
        data = data,
        predictor_varname = .x,
        positives_varname = positives_varname,
        total_varname = total_varname,
        A = A,
        s.index = s.index,
        spde = spde
      ),
      .id = "model"
    )
  }

#' Spatial binomial bivariate summary
#' @inheritParams get_binomial_bivariate_summary
#' @param data
#' @param A projection weights for inla mesh
#' @param s.index spatial index for inla
#' @param spde spde model formulation for inla
#'
#' @return A tibble containing model estimation summaries
#' @export
#'
#' @examples
get_binomial_bivariate_inla_summary <-
  function(data,
           predictor_varname,
           positives_varname = "positives",
           total_varname = "total",
           A,
           s.index,
           spde) {
    print(predictor_varname)
    covariates_estimation <- data %>%
      build_design_matrix(predictor_varname, predictor_varname, intercept = FALSE) %>%
      tibble::as_tibble()
    formula_fixed <- paste("positives ~ 0 + intercept + ",
                           paste(names(covariates_estimation), collapse = " + "),
                           sep = "")
    formula_random <- "+ f(spatial.field, model=spde) +
f(location_id, model = 'iid', hyper=list(prec=list(param=c(0.01,0.01),prior='loggamma')))"
    formula = stats::as.formula(paste0(formula_fixed, formula_random))
    stack <- INLA::inla.stack(
      data = list(
        positives = data %>% sf::st_drop_geometry() %>% dplyr::pull(positives_varname),
        Ntrials = data %>% sf::st_drop_geometry() %>% dplyr::pull(total_varname)
      ),
      A = c(list(A),
            rep(1, ncol(
              covariates_estimation
            ) + 1) %>%
              as.list),
      effects = c(
        list(c(s.index, list(intercept = 1))),
        covariates_estimation %>%
          dplyr::mutate(location_id = 1:nrow(data))
      )
    )
    output <- INLA::inla(
      formula,
      data = INLA::inla.stack.data(stack, spde = spde),
      family = "binomial",
      Ntrials = INLA::inla.stack.data(stack)$Ntrials,
      control.family = list(link = "logit"),
      control.predictor = list(
        A = INLA::inla.stack.A(stack),
        compute = TRUE,
        link = 1
      ),
      control.results = list(
        return.marginals.random = FALSE,
        return.marginals.predictor = FALSE
      ),
      verbose = FALSE
    )

    output$summary.fixed %>%
      tibble::as_tibble(rownames = "term") %>%
      dplyr::mutate(
        estimate_or = purrr::map_dbl(output$marginals.fixed,
                                     ~ INLA::inla.emarginal(exp, marginal = .x)),
        q0_025_or = purrr::map_dbl(output$marginals.fixed,
                                   ~ INLA::inla.qmarginal(0.025, INLA::inla.tmarginal(fun = exp, marginal = .x))),
        q0_975_or = purrr::map_dbl(output$marginals.fixed,
                                   ~ INLA::inla.qmarginal(0.975, INLA::inla.tmarginal(fun = exp, marginal = .x)))
      ) %>%
      dplyr::rename(estimate = mean,
             se = sd,
             q0_025 = `0.025quant`,
             q0_975 = `0.975quant`,
             q0_5 = `0.5quant`)
  }

