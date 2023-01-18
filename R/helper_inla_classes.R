check_class <- function(x, name, class){
  if(!inherits(x, class)){
    stop(
      "`",name,"` must be a object of class `", class, "`.",
      call. = FALSE
    )
  }
}

# helper_spde_init --------------------------------------------------------
new_helper_spde_init <- function(mesh, prior_spde, alpha){
  environment() %>%
    as.list() %>%
    structure(class = "helper_spde_init")
}

validate_helper_spde_init <- function(x){
  if(length(x) != 3L){
    stop("`helper_spde_init` must be a list of length 3.")
  }

  check_class(x$mesh, "mesh", "inla.mesh")
  check_class(x$prior_spde, "prior_spde", "helper_prior_spde")
  if(!purrr::is_scalar_double(x$alpha)){
    stop("`alpha` must be a scalar numeric.",
         call. = FALSE)
  }
  x
}

helper_spde_init <- function(mesh, prior_spde, alpha = 1.5){
  new_helper_spde_init(mesh, prior_spde, alpha) %>%
    validate_helper_spde_init()
}

# helper_prior ------------------------------------------------------------
new_helper_prior <- function(name, distribution, parameter){
  environment() %>%
    as.list() %>%
    structure(class = "helper_prior")
}

validate_helper_prior <- function(x){
  if(length(x) != 3L){
    stop("`helper_prior` must be a list of length 3.",
         call. = FALSE)
  }

  if(!purrr::is_scalar_character(x$name)){
    stop("`helper_prior$name` must be a scalar character.",
         call. = FALSE)
  }

  if(!purrr::is_scalar_character(x$distribution)){
    stop("`helper_prior$distribution` must be a scalar character.",
         call. = FALSE)
  }

  if(!all(purrr::map_lgl(x$parameter,
                         purrr::is_scalar_double))){
    stop("Not all parameters are doubles of length 1.")
  }

  if(x$distribution == "gamma"){
    if(!all(names(x$parameter) == c("shape", "rate"))){
      stop("The ",
           x$distribution,
           " distribution requires a parameter shape and rate.",
           call. = FALSE)
    }
  }

  if(x$distribution == "pc_prior"){
    if((!all(names(x$parameter) == c("value", "probability"))) ||
       !(x$parameter$probability > 0 && x$parameter$probability < 1)){
      stop("The ",
           x$distribution,
           " distribution requires a parameter value and probability (0 < x < 1).",
           call. = FALSE)
    }
  }

  x
}

helper_prior <- function(name, distribution, parameter){
  new_helper_prior(name, distribution, parameter) %>%
    validate_helper_prior()
}



# helper_inla_output ------------------------------------------------------

new_helper_inla_output <- function(fit,
                                   formula,
                                   family,
                                   stack,
                                   stack_names,
                                   covariate_matrix,
                                   covariate_matrix01 = NULL,
                                   spde,
                                   input,
                                   summary_fixed,
                                   summary_hyperpar,
                                   ...,
                                   class = character()) {
    structure(
      list(
      fit = fit,
      formula = formula,
      family = family,
      stack = stack,
      stack_names = stack_names,
      covariate_matrix = covariate_matrix,
      covariate_matrix01 = covariate_matrix01,
      spde = spde,
      input = input,
      summary_fixed = summary_fixed,
      summary_hyperpar = summary_hyperpar,
      ...),
      class = c(class, "helper_inla_output"))
}

new_helper_inla_output_binomial <- function(fit,
                                            formula,
                                            family,
                                            stack,
                                            stack_names,
                                            covariate_matrix,
                                            covariate_matrix01,
                                            spde,
                                            input,
                                            summary_fixed = summary_fixed,
                                            summary_hyperpar = summary_hyperpar) {
  new_helper_inla_output(fit,
                         formula,
                         family,
                         stack,
                         stack_names,
                         covariate_matrix,
                         covariate_matrix01 = covariate_matrix01,
                         spde,
                         input,
                         summary_fixed = summary_fixed,
                         summary_hyperpar = summary_hyperpar,
                         class = "helper_inla_output_binomial")
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.helper_inla_output <- function(x, ...){
  cat("Fixed effects: --------------\n")
  print(x$summary_fixed)
  cat("Hyperparameters: --------------\n")
  print(x$summary_hyperpar)
  invisible(x)
}

#' Title
#'
#' @param inla_output
#'
#' @return
#' @export
#'
#' @examples
get_summary_fixed <- function(inla_output){
  colname_suffix <- if(inla_output$family %in% c("binomial", "zib", "zab")){
    "or"
  }else if(inla_output$family %in% c("negative_binomial",
                           "zinb",
                           "zanb",
                           "poisson",
                           "zip",
                           "zap")){
    "exp"
  }else{
    stop("Method get_summary_fixed not implemented for this family")
  }

  summary_fixed <- inla_output$fit$summary.fixed %>%
    tibble::as_tibble(rownames = "term") %>%
    dplyr::mutate(
      !!paste0("estimate_", colname_suffix) :=
        purrr::map_dbl(
          inla_output$fit$marginals.fixed,
          ~ INLA::inla.emarginal(exp, marginal = .x)
        ),!!paste0("q0_025_", colname_suffix) := purrr::map_dbl(
          inla_output$fit$marginals.fixed,
          ~ INLA::inla.hpdmarginal(.95, INLA::inla.tmarginal(fun = exp, marginal = .x))[, 1]
        ),!!paste0("q0_5_", colname_suffix) := purrr::map_dbl(
          inla_output$fit$marginals.fixed,
          ~ INLA::inla.qmarginal(.5, INLA::inla.tmarginal(fun = exp, marginal = .x))
        ),!!paste0("q0_975_", colname_suffix) := purrr::map_dbl(
          inla_output$fit$marginals.fixed,
          ~ INLA::inla.hpdmarginal(.95, INLA::inla.tmarginal(fun = exp, marginal = .x))[, 2]
        )
    ) %>%
    rename_inla_coef() %>%
    dplyr::select(term, estimate, se, dplyr::everything())

  if(inla_output$input$family %in% c("zab", "zap", "zanb") &
     (length(inla_output$input$covariates01) > 0 |
      inla_output$input$flag_intercept01)){
    summary_fixed01 <- summary_fixed %>%
      dplyr::filter(term %>% stringr::str_detect("01$")) %>%
      conditional_rename_with(~ .x %>% stringr::str_replace("_exp$", "_or"),
                              condition = inla_output$input$family %in% c("zap", "zanb"))
    summary_fixed <- summary_fixed %>%
      dplyr::filter(term %>% stringr::str_detect("01$", negate = TRUE))
    summary_fixed <- summary_fixed %>%
      dplyr::full_join(summary_fixed01,
                       by = c("term", "estimate", "se", "q0_025",
                              "q0_5", "q0_975", "mode", "kld"))
  }
  summary_fixed
}

# get_summary_fixed <- function(fitted_model, model_name, family){
#   colname_suffix <- if(family %in% c("binomial", "zero_inflated_binomial")){
#     "or"
#   }else if(family %in% c("negative_binomial", "zero_inflated_negative_binomial")){
#     "exp"
#   }
#   summary_fixed <- fitted_model$summary.fixed %>%
#     tibble::as_tibble(rownames = "term") %>%
#     dplyr::mutate(
#       model = model_name,
#       !!paste0("estimate_", colname_suffix) :=
#         purrr::map_dbl(fitted_model$marginals.fixed,
#                        ~ INLA::inla.emarginal(exp, marginal = .x)),
#       !!paste0("q0_025_", colname_suffix) := purrr::map_dbl(
#         fitted_model$marginals.fixed,
#         ~ INLA::inla.hpdmarginal(.95, INLA::inla.tmarginal(fun = exp, marginal = .x))[,1]
#       ),
#       !!paste0("q0_975_", colname_suffix) := purrr::map_dbl(
#         fitted_model$marginals.fixed,
#         ~ INLA::inla.hpdmarginal(.95, INLA::inla.tmarginal(fun = exp, marginal = .x))[,2]
#       )
#     ) %>%
#     rename_inla_coef() %>%
#     dplyr::select(model, term, estimate, se, dplyr::everything())
#   if(family == "zero_inflated_negative_binomial"){
#     summary_fixed <- summary_fixed %>%
#       dplyr::filter(term %>% stringr::str_detect("^zi_")) %>%
#       dplyr::rename_with( ~ .x %>% stringr::str_replace("_exp$", "_or")) %>%
#       dplyr::full_join(summary_fixed %>%
#                    dplyr::filter(term %>% stringr::str_detect("^zi_", negate = TRUE)),
#                 by = c("model", "term", "estimate", "se", "q0_025", "q0_5", "q0_975", "mode", "kld"))
#   }
#   summary_fixed
# }

#' Title
#'
#' @param inla_output
#'
#' @return
#' @export
#'
#' @examples
get_summary_hyperpar <- function(inla_output) {
    summary_hyperpar <- tibble::tibble()
    if (inla_output$input$flag_spatial_re) {
      mod.field <- INLA::inla.spde2.result(inla_output$fit,
                                           name = "spatial_field",
                                           inla_output$spde)
      summary_hyperpar <- dplyr::bind_rows(
        mod.field$marginals.variance.nominal[[1]] %>%
          summarize_random_effect("spatial_variance"),
        mod.field$marginals.range.nominal[[1]] %>%
          summarize_random_effect("spatial_range")
      )
    }

    if (inla_output$input$flag_non_spatial_re) {
      summary_hyperpar <- summary_hyperpar %>%
        dplyr::bind_rows(
          INLA::inla.tmarginal(function(x)
            1 / x,
            inla_output$fit$marginals.hyperpar[["Precision for non_spatial_index"]]) %>%
            summarize_random_effect("non_spatial_variance")
        )
    }

    if(inla_output$input$family == "zib"){
      summary_hyperpar <- summary_hyperpar %>%
        dplyr::bind_rows(
          INLA::inla.tmarginal(function(x)
            x,
            inla_output$fit$marginals.hyperpar[["zero-probability parameter for zero-inflated binomial_1"]]) %>%
            summarize_random_effect("zero_probability")
        )
    }

    if(inla_output$input$family == "zip" |
       (inla_output$input$family == "zap" &
        length(inla_output$input$covariates01) == 0 &
         !inla_output$input$flag_intercept01)){
      summary_hyperpar <- summary_hyperpar %>%
        dplyr::bind_rows(
          INLA::inla.tmarginal(function(x)
            x,
            inla_output$fit$marginals.hyperpar[[paste0(
              "zero-probability parameter for zero-inflated poisson_",
              switch(inla_output$input$family,
                     "zip" = 1,
                     "zap" = 0)
            )]]) %>%
            summarize_random_effect("zero_probability")
        )
    }


    if(!inla_output$family %in% c("binomial", "poisson", "zib", "zip", "zap")){
      stop("Not implemented yet.")
    }
    summary_hyperpar
  }

# get_summary_hyperpar <- function(fitted_model,
#                                  spde,
#                                  flag_spatial_re,
#                                  flag_non_spatial_re,
#                                  family,
#                                  model_name,
#                                  flag_zi_intercept,
#                                  zi_covariates) {
#   if (family == "zero_inflated_binomial" && (is.null(zi_covariates) && !flag_zi_intercept && !flag_non_spatial_re)) {
#     summary_hyperpar <- summary_hyperpar %>%
#       dplyr::bind_rows(
#         INLA::inla.tmarginal(function(x)
#           x,
#           fitted_model$marginals.hyperpar[["zero-probability parameter for zero-inflated binomial_1"]]) %>%
#           summarize_random_effect("zero_probability")
#       )
#   } else if (family == "zero_inflated_binomial" && (!is.null(zi_covariates) | flag_zi_intercept | flag_non_spatial_re)) {
#     summary_hyperpar <- summary_hyperpar %>%
#       dplyr::bind_rows(
#         INLA::inla.tmarginal(function(x)
#           x,
#           fitted_model$marginals.hyperpar[["zero-probability parameter for zero-inflated binomial_1[2]"]]) %>%
#           summarize_random_effect("zero_probability")
#       )
#   } else if (family == "negative_binomial") {
#     summary_hyperpar <- summary_hyperpar %>%
#       dplyr::bind_rows(
#         INLA::inla.tmarginal(function(x)
#           x,
#           fitted_model$marginals.hyperpar[["size for the nbinomial observations (1/overdispersion)"]]) %>%
#           summarize_random_effect("size"),
#         INLA::inla.tmarginal(function(x)
#           1 / x,
#           fitted_model$marginals.hyperpar[["size for the nbinomial observations (1/overdispersion)"]]) %>%
#           summarize_random_effect("overdisperson")
#       )
#   } else if (family == "zero_inflated_negative_binomial" && is.null(zi_covariates)) {
#     summary_hyperpar <- summary_hyperpar %>%
#       dplyr::bind_rows(
#         INLA::inla.tmarginal(function(x)
#           1 / x,
#           fitted_model$marginals.hyperpar[["size for nbinomial zero-inflated observations"]]) %>%
#           summarize_random_effect("size"),
#         INLA::inla.tmarginal(function(x)
#           1 / x,
#           fitted_model$marginals.hyperpar[["size for nbinomial zero-inflated observations"]]) %>%
#           summarize_random_effect("overdisperson")
#       ) %>%
#       dplyr::bind_rows(
#         INLA::inla.tmarginal(function(x)
#           x,
#           fitted_model$marginals.hyperpar[["zero-probability parameter for zero-inflated nbinomial_1"]]) %>%
#           summarize_random_effect("zero_probability")
#       )
#   } else if (family == "zero_inflated_negative_binomial" && !is.null(zi_covariates)) {
#     summary_hyperpar <- summary_hyperpar %>%
#       dplyr::bind_rows(
#         INLA::inla.tmarginal(function(x)
#           1 / x,
#           fitted_model$marginals.hyperpar[["size for nbinomial zero-inflated observations[2]"]]) %>%
#           summarize_random_effect("size"),
#         INLA::inla.tmarginal(function(x)
#           1 / x,
#           fitted_model$marginals.hyperpar[["size for nbinomial zero-inflated observations[2]"]]) %>%
#           summarize_random_effect("overdisperson")
#       ) %>%
#       dplyr::bind_rows(
#         INLA::inla.tmarginal(function(x)
#           x,
#           fitted_model$marginals.hyperpar[["zero-probability parameter for zero-inflated nbinomial_1[2]"]]) %>%
#           summarize_random_effect("zero_probability")
#       )
#   }
#   summary_hyperpar %>%
#     dplyr::mutate(model = model_name) %>%
#     dplyr::select(model, everything())
# }

#' Spatial correlation
#'
#' @param inla_output
#' @param xlab
#' @param ylab
#' @param xlim
#'
#' @return
#' @export
#'
#' @examples
plot_spatial_correlation <- function(inla_output,
                                     xlab = "distance",
                                     ylab = "correlation",
                                     xlim = c(0,200)){
  spatial_field <- INLA::inla.spde2.result(inla = inla_output$fit,
                    name = "spatial_field",
                    spde = inla_output$spde,
                    do.transfer = TRUE)
  kappa <- INLA::inla.emarginal(function(x) x,
                          spatial_field$marginals.kappa[[1]])
  sigma <- INLA::inla.emarginal(function(x) sqrt(x),
                          spatial_field$marginals.variance.nominal[[1]])
  r <- INLA::inla.emarginal(function(x) x,
                      spatial_field$marginals.range.nominal[[1]])
  nu <- inla_output$input$alpha-1
  distances <- as.matrix(dist(inla_output$input$mesh$loc[,1:2]))
  d.vec <- seq(0, max(distances), length = 100)
  correlation_matern <- purrr::map_dbl(d.vec,
                                       ~ correlation_matern(., nu, kappa))
  correlation_matern[1] <- 1
  plot(x = d.vec,
       y = correlation_matern,
       type = "l",
       xlab = xlab,
       ylab = ylab,
       xlim = xlim,
       main = paste("E[r] =", round(r,1), "E[sigma_sp] = ", round(sigma,1)))
}

correlation_matern <- function(h, nu, kappa){
  ifelse(h > 0,
         besselK(h * kappa, nu) * (h * kappa)^nu /
           (gamma(nu) *2^(nu -1)),
         1)
}

#' Plot the posterior mean of the spatial random field
#' vgl. zuur et al. p. 259-260
#'
#' @param inla_output
#' @param measure
#' @param map_base
#' @param map_top
#'
#' @return
#' @export
plot_spatial_field <- function(inla_output, measure = "mean", map_base = NULL, map_top = NULL){
  if(!measure %in% c("mean", "sd", "0.025quant", "0.5quant", "0.975quant", "mode")){
    stop("only measure = mean is implemented yet.")
  }
  w_pm <- inla_output$fit$summary.random$spatial_field[, measure] #posterior mean of spatial_field
  w_proj <- INLA::inla.mesh.projector(inla_output$input$mesh,
                                xlim = range(inla_output$input$mesh$loc[,1]),
                                ylim = range(inla_output$input$mesh$loc[,2]))
  w_pm100_100 <- INLA::inla.mesh.project(w_proj, w_pm)
  grid <- expand.grid(x = w_proj$x,
                      y = w_proj$y)
  grid$z <- as.vector(w_pm100_100)
  map_base+
    grid %>%
    raster::rasterFromXYZ(crs = sf::st_crs(inla_output$input$data_estimation)$input) %>%
    tmap::tm_shape() +
    tmap::tm_raster("z",
                    n = 10,
                    midpoint = 0,
                    title = "Posterior mean spatial random field") +
    tmap::tm_layout(legend.outside = TRUE)+
    map_top
}






