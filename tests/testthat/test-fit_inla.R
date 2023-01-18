test_that("binomial works", {
  skip("Takes a long time to run.")
  data_estimation <-readr::read_rds("../oversampling/data/mali/data_estimation.rds")
  range_guess <- 100
  non_convex_hull <- INLA::inla.nonconvex.hull(data_estimation %>% sf::st_coordinates(),
                                               convex = 80)
  max_edge <- range_guess / 5
  mesh <- inla.mesh.2d(loc = data_estimation %>% sf::st_coordinates(),
                       boundary = non_convex_hull,
                       max.edge = c(1,5) * max_edge,
                       cutoff = max_edge / 5)
  fit_inla(
    data_estimation = data_estimation,
    response = c("positives", "total"),
    covariates = "bio4",
    zi_covariates = NULL,
    family = "binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(100, .7),
    prior_pc_sigma = c(4,.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(2.01, 1.01),
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  )

  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = c("positives", "total"),
    covariates = "age",
    zi_covariates = NULL,
    family = "binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ),
  NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = c("positives", "total"),
    covariates = "age",
    family = "binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = NA,
    prior_pc_sigma = NA,
    prior_tau_gamma = c(2.01,1.01),
    prior_decay_gamma = c(2.01,1.01),
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ))
  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = c("positives", "total"),
    covariates = "age",
    family = "binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = NA,
    prior_pc_sigma = NA,
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ),
  NA)

  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = c("positives", "total"),
    covariates = "age",
    family = "binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = FALSE,
    flag_non_spatial_re = TRUE,
    alpha = 1.5,
    prior_pc_range = NA,
    prior_pc_sigma = NA,
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ),
  NA)

  expect_error(fit_inla(
    data_estimation = sf_gambia[1:60,],
    response = c("positives", "total"),
    covariates = "age",
    family = "binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = NA,
    prior_pc_sigma = NA,
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = sf_gambia[61:65,],
    n_sample = 3L,
    hdpi_probabilities = c(.5,.7)
  ),
  NA)

  fit <- fit_inla(
    data_estimation = sf_gambia[1:60,],
    response = c("positives", "total"),
    covariates = "age",
    family = "binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    data_prediction = sf_gambia[61:65,],
    n_sample = 3L,
    hdpi_probabilities = c(.5,.7)
  )

  expect_error(fit_inla(
    data_estimation = sf_gambia[1:60,],
    response = c("positives", "total"),
    covariates = "age",
    family = "binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = FALSE,
    flag_non_spatial_re = TRUE,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = sf_gambia[61:65,],
    n_sample = 3L,
    hdpi_probabilities = c(.5,.7)
  ),
  NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = c("zero_inflated_positives", "total"),
    covariates = "age",
    zi_covariates = NULL,
    family = "zero_inflated_binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = FALSE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ), NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = c("zero_inflated_positives", "total"),
    covariates = "age",
    zi_covariates = NULL,
    family = "zero_inflated_binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ), NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = c("zero_inflated_positives", "total"),
    covariates = "age",
    zi_covariates = NULL,
    family = "zero_inflated_binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ), NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = c("zero_inflated_positives", "total"),
    covariates = "age",
    zi_covariates = "netuse",
    family = "zero_inflated_binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ), NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia[1:60,],
    response = c("zero_inflated_positives", "total"),
    covariates = "age",
    zi_covariates = NULL,
    family = "zero_inflated_binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = sf_gambia[61:65,],
    n_sample = 0L,
    hdpi_probabilities = NULL
  ), NA)

  expect_error(fit_inla(
    data_estimation = sf_gambia[1:60,],
    response = c("zero_inflated_positives", "total"),
    covariates = "age",
    zi_covariates = "age",
    family = "zero_inflated_binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = c(0.3, 0.5),
    prior_pc_sigma = c(10, 0.01),
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = sf_gambia[61:65,],
    n_sample = 250L,
    hdpi_probabilities = NULL
  ), NA)
})

test_that("negative binomial works", {
  skip("Takes a long time to run.")
  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = "count_low",
    covariates = "age",
    family = "negative_binomial",
    flag_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior.range = c(0.3, 0.5),
    prior.sigma = c(10, 0.01),
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ), NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia,
    response = "count_low",
    covariates = "age",
    family = "negative_binomial",
    flag_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = FALSE,
    flag_non_spatial_re = TRUE,
    alpha = 1.5,
    prior.range = c(0.3, 0.5),
    prior.sigma = c(10, 0.01),
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = NULL,
    n_sample = 0L,
    hdpi_probabilities = NULL
  ), NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia[1:60,],
    response = "count_low",
    covariates = "age",
    family = "negative_binomial",
    flag_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior.range = c(0.3, 0.5),
    prior.sigma = c(10, 0.01),
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = sf_gambia[61:65,],
    n_sample = 3L,
    hdpi_probabilities = c(.5,.7)
  ), NA)
  expect_error(fit_inla(
    data_estimation = sf_gambia[1:60,],
    response = "count_low",
    covariates = "age",
    family = "negative_binomial",
    flag_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = FALSE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior.range = c(0.3, 0.5),
    prior.sigma = c(10, 0.01),
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = sf_gambia[61:65,],
    n_sample = 3L,
    hdpi_probabilities = c(.5,.7)
  ), NA)
})

test_that("zero inflated binomial works", {
  skip("Takes a long time to run.")
  expect_error(
    fit_inla(
      data_estimation = sf_gambia,
      response = c("zero_inflated_positives", "total"),
      covariates = "age",
      family = "zero_inflated_binomial",
      flag_intercept = TRUE,
      flag_scale_vars = TRUE,
      flag_spatial_re = TRUE,
      flag_non_spatial_re = TRUE,
      mesh = gambia_mesh,
      alpha = 1.5,
      prior.range = c(0.3, 0.5),
      prior.sigma = c(10, 0.01),
      prior.iid = c(0.01, 0.01),
      range_multiplier = 111,
      data_prediction = NULL,
      n_sample = 0L,
      hdpi_probabilities = NULL
    ),
    NA
  )
  expect_error(
    fit_inla(
      data_estimation = sf_gambia,
      response = c("zero_inflated_positives", "total"),
      covariates = "age",
      family = "zero_inflated_binomial",
      flag_intercept = TRUE,
      flag_scale_vars = TRUE,
      flag_spatial_re = FALSE,
      flag_non_spatial_re = TRUE,
      mesh = gambia_mesh,
      alpha = 1.5,
      prior.range = c(0.3, 0.5),
      prior.sigma = c(10, 0.01),
      prior.iid = c(0.01, 0.01),
      range_multiplier = 111,
      data_prediction = NULL,
      n_sample = 0L,
      hdpi_probabilities = NULL
    ),
    NA
  )
  expect_error(
    fit_inla(
      data_estimation = sf_gambia[1:60, ],
      response = c("zero_inflated_positives", "total"),
      covariates = "age",
      zi_covariates = "age",
      family = "zero_inflated_binomial",
      flag_intercept = TRUE,
      flag_scale_vars = TRUE,
      flag_spatial_re = TRUE,
      flag_non_spatial_re = TRUE,
      mesh = gambia_mesh,
      alpha = 1.5,
      prior_pc_range = NA_real_,
      prior_pc_sigma = NA_real_,
      prior_tau_gamma = NA_real_,
      prior_decay_gamma = NA_real_,
      prior.iid = c(0.01, 0.01),
      range_multiplier = 111,
      data_prediction = sf_gambia[61:65, ],
      n_sample = 3L,
      hdpi_probabilities = c(.5, .7)
    ),
    NA
  )
  fit <- fit_inla(
    data_estimation = sf_gambia[1:60, ],
    response = c("zero_inflated_positives", "total"),
    covariates = "age",
    zi_covariates = "age",
    family = "zero_inflated_binomial",
    flag_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    mesh = gambia_mesh,
    alpha = 1.5,
    prior_pc_range = NA_real_,
    prior_pc_sigma = NA_real_,
    prior_tau_gamma = NA_real_,
    prior_decay_gamma = NA_real_,
    prior.iid = c(0.01, 0.01),
    range_multiplier = 111,
    data_prediction = sf_gambia[61:65, ],
    n_sample = 0L,
    hdpi_probabilities = c(.5, .7)
  )
  expect_error(
    fit_inla(
      data_estimation = sf_gambia[1:60, ],
      response = c("zero_inflated_positives", "total"),
      covariates = "age",
      family = "zero_inflated_binomial",
      flag_intercept = TRUE,
      flag_scale_vars = TRUE,
      flag_spatial_re = FALSE,
      flag_non_spatial_re = TRUE,
      mesh = gambia_mesh,
      alpha = 1.5,
      prior.range = c(0.3, 0.5),
      prior.sigma = c(10, 0.01),
      prior.iid = c(0.01, 0.01),
      range_multiplier = 111,
      data_prediction = sf_gambia[61:65, ],
      n_sample = 3L,
      hdpi_probabilities = c(.5, .7)
    ),
    NA
  )
})

test_that("negative binomial works", {
  skip("Takes a long time to run.")
  expect_error(
    fit_inla(
      data_estimation = sf_gambia,
      response = "zero_inflated_count_low",
      covariates = "age",
      family = "zero_inflated_negative_binomial",
      flag_intercept = TRUE,
      flag_scale_vars = TRUE,
      flag_spatial_re = TRUE,
      flag_non_spatial_re = TRUE,
      mesh = gambia_mesh,
      alpha = 1.5,
      prior.range = c(0.3, 0.5),
      prior.sigma = c(10, 0.01),
      prior.iid = c(0.01, 0.01),
      range_multiplier = 111,
      data_prediction = NULL,
      n_sample = 0L,
      hdpi_probabilities = NULL
    ),
    NA
  )
  expect_error(
    fit_inla(
      data_estimation = sf_gambia[1:60, ],
      response = "zero_inflated_count_low",
      covariates = "age",
      family = "zero_inflated_negative_binomial",
      flag_intercept = TRUE,
      flag_scale_vars = TRUE,
      flag_spatial_re = TRUE,
      flag_non_spatial_re = TRUE,
      mesh = gambia_mesh,
      alpha = 1.5,
      prior.range = c(0.3, 0.5),
      prior.sigma = c(10, 0.01),
      prior.iid = c(0.01, 0.01),
      range_multiplier = 111,
      data_prediction = sf_gambia[61:65, ],
      n_sample = 3L,
      hdpi_probabilities = c(.5, .7)
    ),
    NA
  )
  expect_error(
    fit_inla(
      data_estimation = sf_gambia[1:60, ],
      response = "zero_inflated_count_low",
      covariates = "age",
      family = "zero_inflated_negative_binomial",
      flag_intercept = TRUE,
      flag_scale_vars = TRUE,
      flag_spatial_re = FALSE,
      flag_non_spatial_re = TRUE,
      mesh = gambia_mesh,
      alpha = 1.5,
      prior.range = c(0.3, 0.5),
      prior.sigma = c(10, 0.01),
      prior.iid = c(0.01, 0.01),
      range_multiplier = 111,
      data_prediction = sf_gambia[61:65, ],
      n_sample = 3L,
      hdpi_probabilities = c(.5, .7)
    ),
    NA
  )



# debug bangladesh --------------------------------------------------------
  library(tidyverse)
  library(sf)
  prior_pc_range  <-  c(100, .7) # P(r < r0 ) = p
  prior_pc_sigma  <-  c(4,.01) # P(sigma > sigma0) = p
  # prior_pc_range  <-  c(1.37, .5) # P(r < r0 ) = p
  # prior_pc_sigma  <-  c(2.6,.01) # P(sigma > sigma0) = p

  data_estimation <- read_rds("../bangladesh/Data/estimation_aggregated_age_groups_for_mda_prediction.rds") %>%
    mutate(across(c(asc_pos, hk_pos, trich_pos, total), ~ .x %>%  as.integer()),
           age = case_when(agegr_2 == 0 & agegr_3 == 0 ~ "pre_sac",
                           agegr_2 == 1 & agegr_3 == 0 ~ "sac",
                           agegr_2 == 0 & agegr_3 == 1 ~ "adults",
                           TRUE ~ NA_character_) %>%
             factor(labels = c("pre_sac", "sac", "adults")))

  data_prediction <- read_rds("../bangladesh/Data/prediction_aggregated_age_groups_for_mda_prediction.rds") %>%
    mutate(age = case_when(agegr_2 == 0 & agegr_3 == 0 ~ "pre_sac",
                           agegr_2 == 1 & agegr_3 == 0 ~ "sac",
                           agegr_2 == 0 & agegr_3 == 1 ~ "adults",
                           TRUE ~ NA_character_) %>%
             factor(labels = c("pre_sac", "sac", "adults")))
  data_prediction <- data_prediction %>%
    mutate(altitude = altitude %>% as_quantile_factor_prediction(data_estimation$altitude, probs = c(.33,.66)),
           slope = slope %>% as_quantile_factor_prediction(data_estimation$slope, probs = c(.33,.66)))
  data_estimation <- data_estimation %>%
    mutate(altitude = altitude %>% as_quantile_factor(probs = c(.33,.66)),
           slope = slope %>% as_quantile_factor(probs = c(.33,.66)))

  mesh <- data_estimation %>%
    build_mesh(
      country_id = "BGD",
      kappa = 8 / 100,
      crs = read_rds("../bangladesh/Data/projection.rds")
    )

  # hookworm ----------------------------------------------------------------
  species_var <- "hookworm"
  cat("----", species_var, "-----\n")
  res <- Helper::read_simulation(paste0("3-BVS_", species_var, "_vif_10_binomial_zi"),
                                 rmd_name = "bangladesh")
  hpd_model <- get_bvs_hpd_model(res, 30000L)
  trans_model <- data_estimation %>%
    translate_bvs_hpd_model(hpd_model = hpd_model,
                            data_prediction = data_prediction)


  fit <- fit_inla(
    data_estimation = trans_model$data_estimation,
    response = c("hk_pos", "total"),
    covariates = c("age", hpd_model$parameters %>% unlist() %>% unname()),
    zi_covariates = NULL,
    family = "zero_inflated_binomial",
    flag_intercept = TRUE,
    flag_zi_intercept = TRUE,
    flag_scale_vars = TRUE,
    flag_spatial_re = TRUE,
    flag_non_spatial_re = TRUE,
    prior_pc_range = prior_pc_range,
    prior_pc_sigma = prior_pc_sigma,
    prior_tau_gamma = NA,
    prior_decay_gamma = NA,
    mesh = mesh,
    alpha = 1.5,
    data_prediction = trans_model$data_prediction %>%
      filter(age == "sac"),
    prior.iid = c(2.01,1.01),
    n_sample = 250L
  )

 })

test_that("get_covariates_design_matrices works", {
  a <- get_covariates_design_matrices(
    data_estimation = sf_gambia[1:60,],
    data_prediction = sf_gambia[61:65,],
    covariates = "age",
    zi_covariates = "netuse",
    flag_scale_vars = TRUE,
    flag_intercept = TRUE,
    flag_zi_intercept = TRUE,
    flag_non_spatial_re = TRUE,
    flag_zero_inflated = TRUE
  )
  expect_identical(dim(a$covariates_estimation), c(120L,5L))
  expect_identical(a$covariates, c("intercept", "age", "zi_intercept", "zi_netuse"))

  a <- get_covariates_design_matrices(
    data_estimation = sf_gambia[1:60,],
    data_prediction = sf_gambia[61:65,],
    covariates = "age",
    zi_covariates = "netuse",
    flag_scale_vars = TRUE,
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_non_spatial_re = TRUE,
    flag_zero_inflated = TRUE
  )
  expect_identical(dim(a$covariates_estimation), c(120L,4L))
  expect_identical(a$covariates, c("intercept", "age", "zi_netuse"))

  a <- get_covariates_design_matrices(
    data_estimation = sf_gambia[1:60,],
    data_prediction = sf_gambia[61:65,],
    covariates = "age",
    zi_covariates = NULL,
    flag_scale_vars = TRUE,
    flag_intercept = TRUE,
    flag_zi_intercept = TRUE,
    flag_non_spatial_re = TRUE,
    flag_zero_inflated = TRUE
  )
  expect_identical(dim(a$covariates_estimation), c(120L,4L))
  expect_identical(a$covariates, c("intercept", "age", "zi_intercept"))

  a <- get_covariates_design_matrices(
    data_estimation = sf_gambia[1:60,],
    data_prediction = sf_gambia[61:65,],
    covariates = "age",
    zi_covariates = NULL,
    flag_scale_vars = TRUE,
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_non_spatial_re = TRUE,
    flag_zero_inflated = TRUE
  )
  expect_identical(dim(a$covariates_estimation), c(120L,3L))
  expect_identical(a$covariates, c("intercept", "age"))

  a <- get_covariates_design_matrices(
    data_estimation = sf_gambia[1:60,],
    data_prediction = sf_gambia[61:65,],
    covariates = "age",
    zi_covariates = NULL,
    flag_scale_vars = TRUE,
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_non_spatial_re = FALSE,
    flag_zero_inflated = TRUE
  )
  expect_identical(dim(a$covariates_estimation), c(60L,2L))
  expect_identical(a$covariates, c("intercept", "age"))

  a <- get_covariates_design_matrices(
    data_estimation = sf_gambia[1:60,],
    data_prediction = sf_gambia[61:65,],
    covariates = "age",
    zi_covariates = NULL,
    flag_scale_vars = TRUE,
    flag_intercept = TRUE,
    flag_zi_intercept = FALSE,
    flag_non_spatial_re = FALSE,
    flag_zero_inflated = FALSE
  )
  expect_identical(dim(a$covariates_estimation), c(60L,2L))
  expect_identical(a$covariates, c("intercept", "age"))
})




