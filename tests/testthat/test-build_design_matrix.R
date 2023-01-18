test_that("binomial", {
  skip("Takes a long time")
  fit_bvs_jags_model(data = sf_gambia,
                     response = c("positives", "total"),
                     covariates = c("age", "netuse"),
                     continuous_categorical = "age",
                     family = "binomial",
                     flag_intercept = TRUE,
                     flag_scale_vars = TRUE,
                     flag_spike_slab = TRUE,
                     flag_covariates_zi = TRUE,
                     n_chains = 2,
                     prior_alpha = c(1, 1, 1),
                     prior_pind_a = 1,
                     prior_pind_b = 1,
                     prior_tau_fixed = NA_real_,
                     prior_tau_gamma_shape = 5,
                     prior_tau_gamma_rate = 25,
                     prior_decay_gamma_shape = 0.01,
                     prior_decay_gamma_rate = 0.01,
                     prior_r_gamma_shape = 0.001,
                     prior_r_gamma_rate = 0.001,
                     prior_tau2.sp_shape = 2.01,
                     prior_tau2.sp_rate = 1.01,
                     prior_tau2.e_shape = 2.01,
                     prior_tau2.e_rate = 1.01,
                     range_threshold = 0.1353353,
                     u0 = 4000,
                     inits = NULL,
                     n_runs = 1,
                     n_iter = 1000,
                     burnin = 500,
                     save_dir = NULL,
                     save_file = NULL)
  fit_bvs_jags_model(data = sf_gambia %>%
                       dplyr::mutate(phc = phc %>% factor(labels = c("t", "f"))),
                     response = c("positives", "total"),
                     covariates = c("age", "phc"),
                     continuous_categorical = "age",
                     family = "binomial",
                     flag_intercept = TRUE,
                     flag_scale_vars = TRUE,
                     flag_spike_slab = TRUE,
                     flag_covariates_zi = TRUE,
                     n_chains = 2,
                     prior_alpha = c(1, 1, 1),
                     prior_pind_a = 1,
                     prior_pind_b = 1,
                     prior_tau_fixed = NA_real_,
                     prior_tau_gamma_shape = 5,
                     prior_tau_gamma_rate = 25,
                     prior_decay_gamma_shape = 0.01,
                     prior_decay_gamma_rate = 0.01,
                     prior_r_gamma_shape = 0.001,
                     prior_r_gamma_rate = 0.001,
                     prior_tau2.sp_shape = 2.01,
                     prior_tau2.sp_rate = 1.01,
                     prior_tau2.e_shape = 2.01,
                     prior_tau2.e_rate = 1.01,
                     range_threshold = 0.1353353,
                     u0 = 4000,
                     inits = NULL,
                     n_runs = 1,
                     n_iter = 1000,
                     burnin = 500,
                     save_dir = NULL,
                     save_file = NULL)
  covariate_names <- paste0(
    "../bangladesh/Data/model_variables_all_vif10.rds"
  ) %>%
    readr::read_rds()
  fit_bvs_jags_model(data = paste0("../bangladesh/Data/ascaris_estimation.rds") %>%
                       readr::read_rds(),
                     response = c("positives", "total"),
                     covariates = covariate_names,
                     continuous_categorical =
                       covariate_names[-which(covariate_names %in% c("rural", "poverty", "altitude", "slope"))],
                     family = "binomial",
                     flag_intercept = TRUE,
                     flag_scale_vars = TRUE,
                     flag_spike_slab = TRUE,
                     flag_covariates_zi = TRUE,
                     n_chains = 2,
                     prior_alpha = c(1, 1, 1),
                     prior_pind_a = 1,
                     prior_pind_b = 1,
                     prior_tau_fixed = NA_real_,
                     prior_tau_gamma_shape = 5,
                     prior_tau_gamma_rate = 25,
                     prior_decay_gamma_shape = 0.01,
                     prior_decay_gamma_rate = 0.01,
                     prior_r_gamma_shape = 0.001,
                     prior_r_gamma_rate = 0.001,
                     prior_tau2.sp_shape = 2.01,
                     prior_tau2.sp_rate = 1.01,
                     prior_tau2.e_shape = 2.01,
                     prior_tau2.e_rate = 1.01,
                     range_threshold = 0.1353353,
                     u0 = 4000,
                     inits = NULL,
                     n_runs = 1,
                     n_iter = 1000,
                     burnin = 500,
                     save_dir = NULL,
                     save_file = NULL)
})

test_that("negative_binomial", {
  skip("Takes a long time")

fit_bvs_jags_model(data = sf_gambia,
                   response = c("count_high"),
                   covariates = c("age", "netuse"),
                   continuous_categorical = "age",
                   family = "negative_binomial",
                   flag_intercept = TRUE,
                   flag_scale_vars = TRUE,
                   flag_spike_slab = TRUE,
                   flag_covariates_zi = TRUE,
                   n_chains = 2,
                   prior_alpha = c(1, 1, 1),
                   prior_pind_a = 1,
                   prior_pind_b = 1,
                   prior_tau_fixed = NA_real_,
                   prior_tau_gamma_shape = 5,
                   prior_tau_gamma_rate = 25,
                   prior_decay_gamma_shape = 0.01,
                   prior_decay_gamma_rate = 0.01,
                   prior_r_gamma_shape = 0.001,
                   prior_r_gamma_rate = 0.001,
                   prior_tau2.sp_shape = 2.01,
                   prior_tau2.sp_rate = 1.01,
                   prior_tau2.e_shape = 2.01,
                   prior_tau2.e_rate = 1.01,
                   range_threshold = 0.1353353,
                   u0 = 4000,
                   inits = NULL,
                   n_runs = 1,
                   n_iter = 1000,
                   burnin = 500,
                   save_dir = NULL,
                   save_file = NULL)
})


test_that("zero_inflated_binomial", {
  skip("Takes a long time")

  fit_bvs_jags_model(data = sf_gambia,
                     response = c("positives", "total"),
                     covariates = c("age", "netuse"),
                     continuous_categorical = "age",
                     family = "zero_inflated_binomial",
                     flag_intercept = TRUE,
                     flag_scale_vars = TRUE,
                     flag_spike_slab = TRUE,
                     flag_covariates_zi = TRUE,
                     n_chains = 2,
                     prior_alpha = c(1, 1, 1),
                     prior_pind_a = 1,
                     prior_pind_b = 1,
                     prior_tau_fixed = NA_real_,
                     prior_tau_gamma_shape = 5,
                     prior_tau_gamma_rate = 25,
                     prior_decay_gamma_shape = 0.01,
                     prior_decay_gamma_rate = 0.01,
                     prior_r_gamma_shape = 0.001,
                     prior_r_gamma_rate = 0.001,
                     prior_tau2.sp_shape = 2.01,
                     prior_tau2.sp_rate = 1.01,
                     prior_tau2.e_shape = 2.01,
                     prior_tau2.e_rate = 1.01,
                     range_threshold = 0.1353353,
                     u0 = 4000,
                     inits = NULL,
                     n_runs = 1,
                     n_iter = 1000,
                     burnin = 500,
                     save_dir = NULL,
                     save_file = NULL)
  fit_bvs_jags_model(data = sf_gambia,
                     response = c("zero_inflated_positives", "total"),
                     covariates = c("age", "netuse"),
                     continuous_categorical = "age",
                     family = "zero_inflated_binomial",
                     flag_intercept = TRUE,
                     flag_scale_vars = TRUE,
                     flag_spike_slab = TRUE,
                     flag_covariates_zi = FALSE,
                     n_chains = 2,
                     prior_alpha = c(1, 1, 1),
                     prior_pind_a = 1,
                     prior_pind_b = 1,
                     prior_tau_fixed = NA_real_,
                     prior_tau_gamma_shape = 5,
                     prior_tau_gamma_rate = 25,
                     prior_decay_gamma_shape = 0.01,
                     prior_decay_gamma_rate = 0.01,
                     prior_r_gamma_shape = 0.001,
                     prior_r_gamma_rate = 0.001,
                     prior_tau2.sp_shape = 2.01,
                     prior_tau2.sp_rate = 1.01,
                     prior_tau2.e_shape = 2.01,
                     prior_tau2.e_rate = 1.01,
                     range_threshold = 0.1353353,
                     u0 = 4000,
                     inits = NULL,
                     n_runs = 1,
                     n_iter = 1000,
                     burnin = 500,
                     save_dir = NULL,
                     save_file = NULL)
})

test_that("zero_inflated_negative_binomial", {
  skip("Takes a long time")

  fit_bvs_jags_model(data = sf_gambia,
                     response = c("zero_inflated_count_high"),
                     covariates = c("age", "netuse"),
                     continuous_categorical = "age",
                     family = "zero_inflated_negative_binomial",
                     flag_intercept = TRUE,
                     flag_scale_vars = TRUE,
                     flag_spike_slab = TRUE,
                     flag_covariates_zi = TRUE,
                     n_chains = 2,
                     prior_alpha = c(1, 1, 1),
                     prior_pind_a = 1,
                     prior_pind_b = 1,
                     prior_tau_fixed = NA_real_,
                     prior_tau_gamma_shape = 5,
                     prior_tau_gamma_rate = 25,
                     prior_decay_gamma_shape = 0.01,
                     prior_decay_gamma_rate = 0.01,
                     prior_r_gamma_shape = 0.001,
                     prior_r_gamma_rate = 0.001,
                     prior_tau2.sp_shape = 2.01,
                     prior_tau2.sp_rate = 1.01,
                     prior_tau2.e_shape = 2.01,
                     prior_tau2.e_rate = 1.01,
                     range_threshold = 0.1353353,
                     u0 = 4000,
                     inits = NULL,
                     n_runs = 1,
                     n_iter = 1000,
                     burnin = 500,
                     save_dir = NULL,
                     save_file = NULL)
  fit_bvs_jags_model(data = sf_gambia,
                     response = c("zero_inflated_count_high"),
                     covariates = c("age", "netuse"),
                     continuous_categorical = "age",
                     family = "zero_inflated_negative_binomial",
                     flag_intercept = TRUE,
                     flag_scale_vars = TRUE,
                     flag_spike_slab = TRUE,
                     flag_covariates_zi = FALSE,
                     n_chains = 2,
                     prior_alpha = c(1, 1, 1),
                     prior_pind_a = 1,
                     prior_pind_b = 1,
                     prior_tau_fixed = NA_real_,
                     prior_tau_gamma_shape = 5,
                     prior_tau_gamma_rate = 25,
                     prior_decay_gamma_shape = 0.01,
                     prior_decay_gamma_rate = 0.01,
                     prior_r_gamma_shape = 0.001,
                     prior_r_gamma_rate = 0.001,
                     prior_tau2.sp_shape = 2.01,
                     prior_tau2.sp_rate = 1.01,
                     prior_tau2.e_shape = 2.01,
                     prior_tau2.e_rate = 1.01,
                     range_threshold = 0.1353353,
                     u0 = 4000,
                     inits = NULL,
                     n_runs = 1,
                     n_iter = 1000,
                     burnin = 500,
                     save_dir = NULL,
                     save_file = NULL)
})


test_that("as_dummy works", {
  skip("No test implemented yet")
  test_factor <- factor(c("a","a", "b"))
  as_dummy(test_factor,
           var_name = NA_character_,
           drop_base = FALSE,
           drop_orig = TRUE)
  as_dummy(test_factor,
           var_name = "dum",
           drop_base = FALSE,
           drop_orig = TRUE)

})


test_that("as_quantile_factor works", {
  skip("No test implemented yet")
  data.frame(v = 0:10,
             v_f = as_quantile_factor(0:10, c(0.25,0.5,0.75)),
             v_q = as_quantile_factor(0:10, probs = NULL, quantiles = c(2,4,6,8)))
  as_quantile_factor(0:10, c(0.25,0.5,0.75)) %>% summary()
  as_quantile_factor(0:3, c(0.25,0.5,0.75)) %>% summary()
  as_quantile_factor_prediction(v_prediction = 0:10,
                                v_estimation = c(0:3),
                                probs = c(0.25,0.5,0.75)) %>% summary()

})


test_that("flextable works", {
  skip("No test implemented yet")
  data.frame(v = 0:10,
             v_f = as_quantile_factor(0:10, c(0.25,0.5,0.75)),
             v_q = as_quantile_factor(0:10, probs = NULL, quantiles = c(2,4,6,8)))
  as_quantile_factor(0:10, c(0.25,0.5,0.75)) %>% summary()
  fit2 <- fit_inla(
    data_estimation = sf_gambia,
    response = c("positives", "total"),
    covariates = c("age", "netuse"),
    family = "binomial",
    flag_intercept = TRUE,
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
  )
  sf_gambia$age <- sf_gambia$age %>% as_quantile_factor()
  sf_gambia$netuse <- sf_gambia$netuse %>% as_quantile_factor()

  fit <- fit_inla(
    data_estimation = sf_gambia,
    response = c("positives", "total"),
    covariates = c("age", "netuse"),
    family = "binomial",
    flag_intercept = TRUE,
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
  )

  fit3 <- fit_inla(
    data_estimation = sf_gambia,
    response = c("positives", "total"),
    covariates = c("altitude", "age"),
    family = "binomial",
    flag_intercept = TRUE,
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
  )

  ft <- flextable_inla_binomial(list(fit, fit2, fit3), c("altitude", "age", "intercept", "netuse"))
  autofit(ft)

})


test_that("decrypting factor works", {
  expect_error(decrypt_factorlevel(12))
  test_strings <- c("factorlevel_geq1.21_baselevel_leq1.21",
    "factorlevel_g1.21_baselevel_leq1.21" ,
    "factorlevel_l1.21_baselevel_leq1.21",
    "factorlevel_leq1.21_baselevel_leq1.21",
    "factorlevel_2010to2015_baselevel_leq2009",
    "factorlevel_poorest_baselevel_poorest",
    "factorlevel_second_poorest_baselevel_poorest")
  test_strings %>%
    purrr::map_chr(decrypt_factorlevel) %>%
    expect_identical(c(paste(c("\U2265",">", "<", "\U2264"), "1.21"),
                       "2010 - 2015", "poorest", "second poorest"))

  test_strings %>%
    purrr::map_chr(decrypt_baselevel) %>%
    expect_identical(c(rep("\U2264 1.21", 4), "\U2264 2009", "poorest", "poorest"))
})



