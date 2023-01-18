#' Fit a binomial frequentist model
#' @description `r lifecycle::badge('experimental')`
#' @return A list with the $model, $summary_fixed effects, $summary_random effects, and $summary_hyperpar ameters.
#' @export
#'
#' @examples
#' \dontrun{
#' fit_binomial_freq(sf_gambia, c("positives", "total"), "netuse", TRUE, TRUE, FALSE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), NULL, TRUE, TRUE, FALSE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), "netuse", FALSE, TRUE, FALSE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), "netuse", TRUE, FALSE, FALSE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), "netuse", TRUE, TRUE, FALSE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), c("age","netuse"), TRUE, TRUE, FALSE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), "netuse", TRUE, TRUE, TRUE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), NULL, TRUE, TRUE, TRUE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), "netuse", FALSE, TRUE, TRUE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), "netuse", TRUE, FALSE, TRUE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), "netuse", TRUE, TRUE, TRUE)
#' fit_binomial_freq(sf_gambia, c("positives", "total"), c("age","netuse"), TRUE, TRUE, TRUE)
#' }
fit_binomial_freq <- function(data,
                              response,
                              covariates,
                              flag_intercept,
                              flag_scale_vars,
                              flag_non_spatial_re){
  is_sf_data(data)
  is_binomial_response(data, response)
  is_covariates(data, covariates)
  purrr::walk(c(flag_intercept, flag_scale_vars, flag_non_spatial_re), is_flag)
  if(is.null(covariates) & !flag_intercept)
    stop("Model must have at least one fixed effect (or intercept).")

  if(flag_scale_vars){
    data <- data %>%
      scale_vars(covariates = covariates, data_prediction = NULL) %>%
      purrr::pluck("data_estimation")
  }

  resolved_factors <- data %>%
    resolve_factors(covariates)
  data <- resolved_factors$data
  covariates <- resolved_factors$covariates

  data <- data %>%
    drop_geometry_if_exists()

  data_response <- data %>%
    dplyr::mutate(success = .data[[response[1]]],
                  failure = .data[[response[2]]] - success) %>%
    dplyr::select(success, failure) %>%
    as.matrix()

  if(flag_intercept){
    data <- data %>%
      dplyr::mutate(intercept = 1)
    covariates <- c("intercept", covariates)
  }

  formula_str <- paste0("data_response ~ 0 + ",
              paste0(covariates, collapse = " + "))

  if(flag_non_spatial_re){
    formula_str <- paste0( formula_str, " + (1|location_id)" )
    data <- data %>%
      dplyr::mutate(location_id = 1:nrow(data))
    fitted_model <- formula_str %>%
      stats::as.formula() %>%
      lme4::glmer(family = stats::binomial,
                  data = data)
    m_tidy <- tibble::tibble(
      term = fitted_model %>% lme4::fixef() %>% names(),
      estimate = fitted_model %>% lme4::fixef()
    )

    ci <- fitted_model %>%
      lme4::confint.merMod(parm = m_tidy$term, method = "Wald")
    m_tidy <- m_tidy %>%
      dplyr::mutate(q0_025 = ci[, 1],
                    q0_975 = ci[, 2])
    m_tidy <- m_tidy %>%
      dplyr::mutate(dplyr::across(-term, ~ exp(.))) %>%
      dplyr::full_join(m_tidy, by = "term", suffix = c("_or", "")) %>%
      dplyr::mutate(model = "binomial freq non-spatial random effect") %>%
      dplyr::select(model, term,!dplyr::ends_with("_or"), dplyr::everything())
    warning("This is experimental. In particular, the random effect does not look good.")

  }else{
    f <- formula_str %>%
      stats::as.formula()
    fitted_model <- formula_str %>%
      stats::as.formula() %>%
      stats::glm(family = stats::binomial,
                 data = data)
    ci <- fitted_model %>%
      stats::confint() %>%
      matrix(ncol = 2)
    m_tidy <- fitted_model %>%
      broom::tidy() %>%
      dplyr::select(term, estimate, std.error) %>%
      dplyr::rename(se = std.error) %>%
      tibble::tibble(q0_025 = ci[, 1],
                     q0_975 = ci[, 2])
    m_tidy <- m_tidy %>%
      dplyr::select(-se) %>%
      dplyr::mutate(dplyr::across(-term, ~ exp(.))) %>%
      dplyr::full_join(m_tidy, by = "term", suffix = c("_or", "")) %>%
      dplyr::mutate(model = "binomial freq non-spatial") %>%
      dplyr::select(model, term,!dplyr::ends_with("_or"), dplyr::everything()) %>%
      dplyr::mutate(fitted_model %>% broom::glance())
  }


  list("model" = fitted_model,
       "summary_fixed" = m_tidy,
       "summary_random" = NULL,
       "summary_hyperpar" = NULL)
}

#' Fit a non spatial negative binomial model frequentistically
#' @description `r lifecycle::badge('experimental')`
#' @param response is a scalar character vector describing the response count variable.
#' @return A list with the $model, $summary_fixed effects, $summary_random effects, and $summary_hyperpar ameters.
#' @export
#'
#' @examples
#' #fit_negative_binomial_freq(sf_gambia, c("positives"), c("age","netuse"), TRUE, TRUE, FALSE)
#' #fit_negative_binomial_freq(sf_gambia, c("positives"), c("age","netuse"), TRUE, FALSE, FALSE)
fit_negative_binomial_freq <- function(data,
                              response,
                              covariates,
                              flag_intercept,
                              flag_scale_vars,
                              flag_non_spatial_re){
  is_sf_data(data)
  is_negative_binomial_response(data, response)
  is_covariates(data, covariates)
  purrr::walk(c(flag_intercept, flag_scale_vars, flag_non_spatial_re), is_flag)
  if(flag_non_spatial_re)
    stop("Non spatial random effect not implemented yet.")

  if(flag_scale_vars){
    data <- data %>%
      scale_vars(covariates = covariates, data_prediction = NULL) %>%
      purrr::pluck("data_estimation")
  }

  resolved_factors <- data %>%
    resolve_factors(covariates)
  data <- resolved_factors$data
  covariates <- resolved_factors$covariates

  data <- data %>%
    drop_geometry_if_exists()

  if(flag_intercept){
    data <- data %>%
      dplyr::mutate(intercept = 1)
    covariates <- c("intercept", covariates)
  }

  formula_str <- paste0(response,
                        " ~ 0 +",
                        paste0(covariates, collapse = "+"))
  f <- formula_str %>%
    stats::as.formula()
  fitted_model <-
    MASS::glm.nb(
      formula = f,
      data = data
    )

  try_ci <- try({
    ci <- fitted_model %>%
    stats::confint()
  })
  if(class(try_ci) == "try-error"){
    ci <- fitted_model %>%
        stats::confint.default()
    warning("confint.default had to be used because confint threw error.")
  }
  m_tidy <- fitted_model %>%
    broom::tidy() %>%
    dplyr::select(term, estimate, std.error) %>%
    dplyr::rename(se = std.error) %>%
    tibble::tibble(q0_025 = ci[, 1],
                   q0_975 = ci[, 2])
  m_tidy <- m_tidy %>%
    dplyr::select(-se) %>%
    dplyr::mutate(dplyr::across(-term, ~ exp(.))) %>%
    dplyr::full_join(m_tidy, by = "term", suffix = c("_exp", "")) %>%
    dplyr::mutate(model = formula_str) %>%
    dplyr::select(model, term,!dplyr::ends_with("_exp"), dplyr::everything()) %>%
    dplyr::mutate(fitted_model %>% broom::glance())
  list("model" = fitted_model,
       "summary_fixed" = m_tidy,
       "summary_random" = NULL,
       "summary_hyperpar" = NULL)
}


#' Help to build a mesh
#' @description `r lifecycle::badge('experimental')`
#'
#' @param data_estimation  A sf and tibble data object of the estimation data
#' @param country_id A country identifier as recognized by gadm.
#' @param maps_dir A maps directory where to retrieve country id gadm map.
#' @param boundary A countries boundaries as sf object
#' @param kappa A numerical scalar determining the density of the mesh.
#' @param crs A coordinate reference system
#'
#' @return
#' @export
#'
#' @examples
build_mesh <-
  function(data_estimation,
           country_id,
           kappa = 8,
           crs = "+proj=longlat +datum=WGS84 +no_defs",
           maps_dir = "../africa/Data/",
           boundary = NULL) {
    is_sf_data(data_estimation)
    stopifnot(purrr::is_scalar_double(kappa))
    stopifnot(dir.exists(maps_dir))

    if (is.null(boundary)){
      boundary <-
        get_map_boundary(country_id = country_id,
                         crs = crs,
                         maps_dir = maps_dir)
    }else{
      is_sf_data(boundary)
    }
    coords_boundary <- boundary %>%
      get_inla_coords()
    coords_estimation <- data_estimation %>%
      get_inla_coords()

    INLA::inla.mesh.2d(
      loc  = coords_estimation,
      loc.domain = coords_boundary,
      max.edge = c(1 / kappa, 2 / kappa),
      cutoff = 0.4 / kappa,
      plot.delay = NULL
    )
  }

#' Build mesh2
#'
#' @param range_guess The range guess
#' @param convex If country_id is NULL, a boundary is made with the convex argument
#' @param country_id The country id, e.g. "BGD" for Bangladesh (GADM-codes)
#' @param maps_dir The directory where the maps are saved
#' @param data_estimation The data for estimation
#' @param flag_reverse_interior Used to reverse interior/exterior (boundary as island, or boundary as hole)
#'
#' @return
#' @export
build_mesh2 <-
  function(data_estimation,
           range_guess = 100,
           convex = 50,
           country_id = NULL,
           flag_reverse_interior = FALSE,
           maps_dir = "../africa/Data/") {
    is_sf_data(data_estimation)
    stopifnot(purrr::is_scalar_double(convex))
    stopifnot(purrr::is_scalar_double(range_guess))
    stopifnot(dir.exists(maps_dir))
    if(is.null(country_id) & flag_reverse_interior){
      stop("`country_id` must be provided if `flag_reverse_interior = TRUE`.")
    }

    max_edge <- range_guess / 5
    loc_estimation <- data_estimation %>%
      get_inla_coords()
    if(is.null(country_id)){
      boundary <- loc_estimation %>%
        INLA::inla.nonconvex.hull(convex = convex)
    }else{
      boundary_sf <- get_map_boundary(country_id,
                       crs = sf::st_crs(data_estimation),
                       maps_dir = maps_dir)
      boundary <- boundary_sf %>%
        sf::as_Spatial() %>%
        INLA::inla.sp2segment()
      boundary$loc <- INLA::inla.mesh.map(boundary$loc)
      if(flag_reverse_interior){
        boundary$loc <- boundary$loc[nrow(boundary$loc):1,]
      }
    }

    INLA::inla.mesh.2d(
      loc = loc_estimation,
      boundary = boundary,
      max.edge = c(1, 5) * max_edge,
      cutoff  = max_edge / 5
    )
  }

get_map_boundary <- function(country_id,
                             crs = "+proj=longlat +datum=WGS84 +no_defs",
                             maps_dir =  "../africa/Data/"){
  stopifnot(nchar(country_id) == 3,
            country_id == country_id %>% toupper(),
            dir.exists(maps_dir))
  download_gadm_sf(country_id, maps_dir)
  paste0(maps_dir,
         "level_0/",
         country_id %>%
           tolower(),
         ".rds") %>%
    readr::read_rds() %>%
    sf::st_transform(crs = crs)
}

get_inla_coords <- function(data){
  is_sf_data(data)
  data %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::select(X,Y) %>%
    dplyr::rename(x = X, y = Y) %>%
    as.matrix()
}

drop_geometry_if_exists <- function(data){
  is_data(data)
  if(inherits(data, "sf")){
    sf::st_drop_geometry(data)
  }else{
    data
  }
}


get_covariates_design_matrices <- function(data_estimation,
                                      data_prediction,
                                      covariates,
                                      zi_covariates,
                                      flag_scale_vars,
                                      flag_intercept,
                                      flag_zi_intercept,
                                      flag_non_spatial_re,
                                      flag_zero_inflated = FALSE
                                      ){
  is_flag(flag_zero_inflated)
  covariates_all <- union(covariates, zi_covariates)

  if(flag_scale_vars){
    scaled_data <- data_estimation  %>%
      scale_vars(covariates = covariates_all,
                 data_prediction = data_prediction)
    data_estimation <- scaled_data %>%
      purrr::pluck("data_estimation")
    data_prediction <- scaled_data %>%
      purrr::pluck("data_prediction")
  }else{
    warning("I believe data should be scaled or at least centered before fitted.")
  }

  resolved_factors_estimation <- data_estimation  %>%
    resolve_factors(covariates)
  data_estimation  <- resolved_factors_estimation$data_estimation
  covariates_old <- covariates
  covariates <- resolved_factors_estimation$covariates
  if(!is.null(zi_covariates)){
    resolved_factors_estimation <- data_estimation  %>%
      resolve_factors(zi_covariates)
    data_estimation  <- resolved_factors_estimation$data_estimation
    zi_covariates_old <- zi_covariates
    zi_covariates <- resolved_factors_estimation$covariates
  }

  if(!is.null(data_prediction)){
    resolved_factors_prediction <- data_prediction  %>%
      resolve_factors(covariates_old)
    data_prediction  <- resolved_factors_prediction$data_estimation
    covariates_prediction <- resolved_factors_prediction$covariates
    if(!is.null(zi_covariates)){
      resolved_factors_estimation <- data_estimation  %>%
        resolve_factors(zi_covariates_old)
      data_estimation  <- resolved_factors_estimation$data_estimation
      zi_covariates_prediction <- resolved_factors_estimation$covariates
      if(!all(zi_covariates == zi_covariates_prediction))
        stop("zi_covariates for estimation and prediction are not the same after taking care of factors.")
    }
    if(!all(covariates == covariates_prediction))
      stop("covariates for estimation and prediction are not the same after taking care of factors.")
    data_prediction <- data_prediction %>%
      drop_geometry_if_exists()
  }

  data_estimation  <- data_estimation  %>%
    drop_geometry_if_exists()

  if(flag_intercept){
    data_estimation  <- data_estimation  %>%
      dplyr::mutate(intercept = 1)
    covariates <- c("intercept", covariates)
    if(!is.null(data_prediction)){
      data_prediction <- data_prediction %>%
        dplyr::mutate(intercept = 1)
    }
  }

  if(flag_zi_intercept){
    data_estimation  <- data_estimation  %>%
      dplyr::mutate(zi_intercept = 1)
    zi_covariates <- c("zi_intercept", zi_covariates)
    if(!is.null(data_prediction)){
      data_prediction <- data_prediction %>%
        dplyr::mutate(zi_intercept = 1)
    }
  }

  if(flag_non_spatial_re){
    covariates <- c(covariates, "location_id")
    data_estimation <- data_estimation %>%
      dplyr::mutate(location_id = 1:nrow(data_estimation))
    if(!is.null(data_prediction)){
      data_prediction <- data_prediction %>%
        dplyr::mutate(location_id = nrow(data_estimation) + 1:nrow(data_prediction))
    }
  }

  covariates_all <- union(covariates, zi_covariates)
  covariates_estimation <- data_estimation  %>%
    dplyr::select(all_of(covariates_all))
  if(!is.null(data_prediction)){
    covariates_prediction <- data_prediction %>%
      dplyr::select(all_of(covariates_all))
  }


  if(any(is.na(covariates_estimation))){
    warning("Some estimation covariates are NA.")
  }
  if(!is.null(data_prediction) && any(is.na(covariates_prediction))){
    warning("Some estimation covariates are NA.")
  }

  if(flag_zero_inflated && (!is.null(zi_covariates) | flag_non_spatial_re)){
    covariates_estimation <- covariates_estimation %>%
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  ~ .x %>% as.numeric()))
    covariates_estimation <- covariates_estimation %>%
      dplyr::select(dplyr::all_of(zi_covariates)) %>%
      dplyr::rename_with( ~ paste0("zi_", .x),
                          .cols = dplyr::all_of(
                            setdiff(zi_covariates, "zi_intercept"))) %>%
      dplyr::bind_cols(covariates_estimation %>%
                         dplyr::select(dplyr::all_of(covariates)) %>%
                         dplyr::mutate(
                           dplyr::across(
                             dplyr::all_of(setdiff(covariates, "location_id")),
                             ~ NA_real_))) %>%
      dplyr::bind_rows(
        covariates_estimation  %>%
          dplyr::select(dplyr::all_of(zi_covariates)) %>%
          dplyr::rename_with( ~ paste0("zi_", .x),
                              .cols = dplyr::all_of(
                                setdiff(zi_covariates, "zi_intercept"))) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(),
                                      ~ NA_real_)) %>%
          dplyr::bind_cols(covariates_estimation %>%
                             dplyr::select(dplyr::all_of(covariates)))
      )

    if(!is.null(data_prediction)){
      covariates_prediction <- covariates_prediction %>%
        dplyr::select(dplyr::all_of(zi_covariates)) %>%
        dplyr::rename_with( ~ paste0("zi_", .x),
                            .cols = dplyr::all_of(
                              setdiff(zi_covariates, "zi_intercept"))) %>%
        dplyr::bind_cols(covariates_prediction %>%
                           dplyr::select(dplyr::all_of(covariates)) %>%
                           dplyr::mutate(
                             dplyr::across(
                               dplyr::all_of(setdiff(covariates, "location_id")),
                               ~ NA_real_))) %>%
        dplyr::bind_rows(
          covariates_prediction  %>%
            dplyr::select(dplyr::all_of(zi_covariates)) %>%
            dplyr::rename_with( ~ paste0("zi_", .x),
                                .cols = dplyr::all_of(
                                  setdiff(zi_covariates, "zi_intercept"))) %>%
            dplyr::mutate(dplyr::across(dplyr::everything(),
                                                                                                                    ~ NA_real_)) %>%
            dplyr::bind_cols(covariates_prediction %>%
                               dplyr::select(dplyr::all_of(covariates)))
        )
    }
  }
  covariates_prediction <- if(is.null(data_prediction)){
    NULL
  }else{
    covariates_prediction
  }
  covariates_all <- if(flag_zi_intercept){
    covariates <- c(covariates, "zi_intercept")
    if(length(setdiff(zi_covariates, "zi_intercept")) > 0){
      covariates <- c(covariates,
                      paste0("zi_", setdiff(zi_covariates, "zi_intercept")))
    }
    covariates
  }else{
    if(length(zi_covariates) > 0){
      covariates <- c(covariates,
                      paste0("zi_", zi_covariates))
    }
    covariates
  }

  list(
    "covariates" = setdiff(covariates_all, "location_id"),
    "covariates_estimation" = covariates_estimation,
    "covariates_prediction" = covariates_prediction
  )
}




inverse_logit <- function (x){
  p <- 1/(1 + exp(-x))
  ifelse(x == Inf, 1, p)
}

rename_inla_coef <- function(x) {
  x %>%
    dplyr::rename(
      estimate = mean,
      se = sd,
      q0_025 = `0.025quant`,
      q0_5 = `0.5quant`,
      q0_975 = `0.975quant`
    )
}

#' Plot summary fixed effect of one or more models
#'
#' @param fit Fit from fit_binomial or similar models.
#' @param ... More fits
#' @param flag_binomial_or If the fixed effect coefficients should be displayed as odds ratio
#' @param flag_drop_intercept Should the intercept be dropped?
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' plot_summary_fixed(fit_binomial_freq(sf_gambia, c("positives", "total"),
#'   "netuse", TRUE, TRUE, FALSE),
#'   fit_binomial_freq(sf_gambia, c("positives", "total"),
#'   NULL, TRUE, TRUE, FALSE))
#' plot_summary_fixed(fit_binomial_freq(sf_gambia, c("positives", "total"),
#'   "netuse", TRUE, TRUE, FALSE),
#'   fit_binomial_freq(sf_gambia, c("positives", "total"),
#'   NULL, TRUE, TRUE, FALSE), flag_binomial_or = TRUE, flag_drop_intercept = TRUE)
#'  }
plot_summary_fixed <- function(fit, ..., flag_binomial_or = FALSE, flag_drop_intercept = FALSE){
  is_flag(flag_binomial_or)
  is_flag(flag_drop_intercept)

  dots <- list(...)
  summary_fixed <- fit$summary_fixed
  purrr::walk(dots, function(x) summary_fixed <<- summary_fixed %>%
         dplyr::full_join(x$summary_fixed))
  # summary_fixed <- summary_fixed %>%
  #   dplyr::mutate(model = model %>% forcats::as_factor() %>% forcats::fct_rev())
  if(flag_drop_intercept){
    summary_fixed <- summary_fixed %>%
      dplyr::filter(term != "intercept")
  }
  if (flag_binomial_or) {
    summary_fixed %>%
      ggplot2::ggplot(ggplot2::aes(y = term
                                   # , col = model
                                   )) +
      ggplot2::geom_point(ggplot2::aes(x = estimate_or),
                          position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = q0_025_or, xmax = q0_975_or),
                             position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_vline(xintercept = 1) +
      ggplot2::scale_color_discrete(guide = ggplot2::guide_legend(reverse =
                                                                    TRUE)) +
      ggplot2::labs(x = "odds ratio", y = "fixed effect") +
      ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
  } else{
    summary_fixed %>%
      ggplot2::ggplot(ggplot2::aes(y = term
                                   # , col = model
                                   )) +
      ggplot2::geom_point(ggplot2::aes(x = estimate),
                          position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = q0_025, xmax = q0_975),
                             position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_vline(xintercept = 0) +
      ggplot2::scale_color_discrete(guide = ggplot2::guide_legend(reverse =
                                                                    TRUE)) +
      ggplot2::labs(x = "odds ratio", y = "estimate") +
      ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
  }
}
