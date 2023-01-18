# rm(list = ls())
# mesh <- build_mesh(sf_gambia, "GMB")

# fitb <- sf_gambia %>%
#   fit_binomial_bayes_inla_spatial(
#     response = c("positives", "total"),
#     covariates = "age",
#     flag_intercept = TRUE,
#     flag_scale_vars = TRUE,
#     flag_non_spatial_re = TRUE,
#     mesh = mesh
#   )
# fitb <- sf_gambia[1:60,] %>%
#   fit_binomial_bayes_inla_spatial(
#     response = c("positives", "total"),
#     covariates = "age",
#     flag_intercept = TRUE,
#     flag_scale_vars = TRUE,
#     flag_non_spatial_re = TRUE,
#     mesh = mesh,
#     data_prediction = sf_gambia[61:65,],
#     n_sample = 2L
#   )
#
# fitb <- sf_gambia %>%
#   fit_negative_binomial_bayes_inla_spatial(
#     response = c("count_high"),
#     covariates = "age",
#     flag_intercept = TRUE,
#     flag_scale_vars = TRUE,
#     flag_non_spatial_re = TRUE,
#     mesh = mesh
#   )
# fitb <- sf_gambia[1:60,] %>%
#   fit_negative_binomial_bayes_inla_spatial(
#     response = c("count_high"),
#     covariates = "age",
#     flag_intercept = TRUE,
#     flag_scale_vars = TRUE,
#     flag_non_spatial_re = TRUE,
#     mesh = mesh,
#     data_prediction = sf_gambia[61:65,],
#     n_sample = 2L,
#     hdpi_probabilities = c(.6,.7)
#   )
#
#
# fitb <- sf_gambia[1:60,] %>%
#   fit_zibinomial_bayes_inla_spatial(
#     response = c("zero_inflated_positives", "total"),
#     covariates = "age",
#     flag_intercept = TRUE,
#     flag_scale_vars = TRUE,
#     flag_non_spatial_re = TRUE,
#     mesh = mesh,
#     data_prediction = sf_gambia[61:65,]
#   )
#
# fitb <- sf_gambia[1:60,] %>%
#   fit_zi_negative_binomial_bayes_inla_spatial(
#     response = c("zero_inflated_count_low"),
#     covariates = "age",
#     flag_intercept = TRUE,
#     flag_scale_vars = TRUE,
#     flag_non_spatial_re = TRUE,
#     mesh = mesh,
#     data_prediction = sf_gambia[61:65,],
#     n_sample = 10L,
#     hdpi_probabilities = c(0.5,0.7)
#   )
# fitb <- sf_gambia[1:60,] %>%
#   fit_zi_negative_binomial_bayes_inla_spatial(
#     response = c("zero_inflated_count_high"),
#     covariates = c("age", "netuse"),
#     flag_intercept = TRUE,
#     flag_scale_vars = TRUE,
#     flag_non_spatial_re = TRUE,
#     mesh = mesh,
#     data_prediction = sf_gambia[61:65,],
#     n_sample = 1L,
#     hdpi_probabilities = c(0.5,0.7)
#   )
#
#
# fitb$prediction %>%
#   select(zi_prevalence_sample_1, prevalence_sample_1)
#
# rbinom(100, 1, 1-0.805) * rbinom(100, 30, 0.411)
#
# species <- "trichiuris"
# dd <- paste0("../bangladesh/Data/", species, "_estimation.rds") %>%
#   read_rds()
# mesh <- build_mesh(dd, "BGD")
# res <- dd %>%
#   fit_negative_binomial_bayes_inla_spatial(
#     response = c("eggs"),
#     "bio2",
#     flag_intercept = TRUE,
#     flag_scale_vars = TRUE,
#     flag_non_spatial_re = TRUE,
#     mesh = mesh,
#     prior.iid = c(0.01, 0.01)
#   )
# library(tidyverse)
# a <- readLines("fit_model_part.txt")
# a <- a %>%
#   str_replace_all("(^ *| *$)", "")
# a <- a[which(a != "")]
# b <- paste0(a, collapse = "\n") %>%
#   str_split("###") %>%
#   map(~ str_split(.x, "\\n")) %>%
#   pluck(1)
# b <- b[2:5]
# b %>% lengths()
# b[[1]] == b[[2]]
# b[[1]] == b[[3]]
# b[[1]][1:72] == b[[4]]
#
# d <- b[[1]]
# d <- d[d != ""]
#
# d[d %>% str_detect("<-")]
#
#
# fit <- sf_gambia %>%
#   fit_bvs_jags_model(response = c("positives", "total"),
#                      covariates = "age",
#                      continuous_only = "age",
#                      flag_spike_slab = FALSE,
#                      prior_decay_gamma_shape = NA_real_,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)

# fit2 <- sf_gambia %>%
#   fit_bvs_jags_model(response = c("positives", "total"),
#                      covariates = "age",
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)
# fit2 <- sf_gambia %>%
#   fit_bvs_jags_model(response = c("positives", "total"),
#                      covariates = "age",
#                      continuous_only = "age",
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)
# sf_gambia %>%
#   dplyr::mutate(phc = phc %>% as.factor()) %>%
#   fit_bvs_jags_model(response = c("positives", "total"),
#                      covariates = c("age", "phc"),
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)
# sf_gambia %>%
#   dplyr::mutate(phc = phc %>% as.factor()) %>%
#   fit_bvs_jags_model(response = c("positives", "total"),
#                      covariates = c("age", "phc"),
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      flag_intercept = FALSE,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)
# sf_gambia %>%
#   dplyr::mutate(phc = phc %>% as.factor()) %>%
#   fit_bvs_jags_model(family = "negative_binomial",
#                        response = c("count_high"),
#                      covariates = c("age", "phc"),
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      flag_intercept = FALSE,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)
#
# sf_gambia %>%
#   dplyr::mutate(phc = phc %>% as.factor()) %>%
#   fit_bvs_jags_model(family = "zero_inflated_binomial",
#                      response = c("zero_inflated_positives", "total"),
#                      covariates = c("age", "phc"),
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      flag_intercept = FALSE,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)
# sf_gambia %>%
#   dplyr::mutate(phc = phc %>% as.factor()) %>%
#   fit_bvs_jags_model(family = "zero_inflated_binomial",
#                      response = c("zero_inflated_positives", "total"),
#                      covariates = c("age", "phc"),
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      flag_intercept = TRUE,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)

# sf_gambia %>%
#   dplyr::mutate(phc = phc %>% as.factor()) %>%
#   fit_bvs_jags_model(family = "zero_inflated_negative_binomial",
#                        response = c("zero_inflated_count_high"),
#                      covariates = c("age", "phc"),
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      flag_intercept = FALSE,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)
#
# sf_gambia %>%
#   dplyr::mutate(phc = phc %>% as.factor()) %>%
#   fit_bvs_jags_model(family = "zero_inflated_negative_binomial",
#                      response = c("zero_inflated_count_high"),
#                      covariates = c("age", "phc"),
#                      prior_decay_gamma_shape = 0.01,
#                      prior_decay_gamma_rate = 0.01,
#                      flag_intercept = TRUE,
#                      n_chains = 4L,
#                      n_iter = 1000,
#                      burnin = 100,
#                      n_runs = 1L)
#
#
#
# rm(list = ls())
# library(sf)
# library(INLA)
# library(Helper)
# library(tidyverse)
#
# species <- c("s-haematobium")
# country_name <- "tgo"
# n_riskmap <- 20L
# dd <- "../oversampling/Data/" %>%
#   paste0(species,"_estimation.rds") %>%
#   read_rds()
# dd_prediction <- "../oversampling/Data/" %>%
#   paste0(species,"_prediction.rds") %>%
#   read_rds()
# coords=as.matrix(data.frame(dd %>% st_coordinates()))
# source("../oversampling/R/pv_mesh.R")
# mesh <- meshCreator(
#   country = "Togo",
#   coords = coords,
#   niter = 5500,
#   range = -1
# )
# proj4string <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=km
# +nadgrids=@null +wktext +no_defs"
# dd <- dd %>%
#   st_transform(crs = st_crs(proj4string))
# dd_prediction <- dd_prediction  %>%
#   st_transform(crs = st_crs(proj4string))
# mesh$n
# plot(mesh)
# points(dd %>%
#          st_transform(crs = st_crs(proj4string)) %>%
#          st_coordinates(), col = "red")
# points(dd_prediction %>% st_coordinates(), col = "green")
#
# covariate_names <- c("bio2","bio3","bio6",
#                      # "bio13",
#                      # "bio17",
#                      "ndvi",
#                      # "bio12",
#                      "slope","imprsan","rural")
# gold_standard0 <- fit_binomial_bayes_inla_spatial(data_estimation = dd,
#                                                  response = c("positives", "total"),
#                                                  covariates = covariate_names,
#                                                  prior.iid = c(0.01, 0.01),
#                                                  flag_intercept = TRUE,
#                                                  flag_scale_vars = TRUE,
#                                                  flag_non_spatial_re = TRUE,
#                                                  mesh = mesh,
#                                                  alpha = 1.5,
#                                                  data_prediction = dd_prediction,
#                                                  n_sample = n_riskmap)
#
# covariate_names <- c("bio2","bio3","bio6",
#                      "bio13",
#                      # "bio17",
#                      "ndvi",
#                      # "bio12",
#                      "slope","imprsan","rural")
# gold_standard1 <- fit_binomial_bayes_inla_spatial(data_estimation = dd,
#                                                  response = c("positives", "total"),
#                                                  covariates = covariate_names,
#                                                  prior.iid = c(0.01, 0.01),
#                                                  flag_intercept = TRUE,
#                                                  flag_scale_vars = TRUE,
#                                                  flag_non_spatial_re = TRUE,
#                                                  mesh = mesh,
#                                                  alpha = 1.5,
#                                                  data_prediction = dd_prediction,
#                                                  n_sample = n_riskmap)
#
# covariate_names <- c("bio2","bio3","bio6",
#                      # "bio13",
#                      "bio17",
#                      "ndvi",
#                      # "bio12",
#                      "slope","imprsan","rural")
# gold_standard2 <- fit_binomial_bayes_inla_spatial(data_estimation = dd,
#                                                  response = c("positives", "total"),
#                                                  covariates = covariate_names,
#                                                  prior.iid = c(0.01, 0.01),
#                                                  flag_intercept = TRUE,
#                                                  flag_scale_vars = TRUE,
#                                                  flag_non_spatial_re = TRUE,
#                                                  mesh = mesh,
#                                                  alpha = 1.5,
#                                                  data_prediction = dd_prediction,
#                                                  n_sample = n_riskmap)
#
# covariate_names <- c("bio2","bio3","bio6",
#                      # "bio13",
#                      # "bio17",
#                      "ndvi",
#                      "bio12",
#                      "slope","imprsan","rural")
# gold_standard3 <- fit_binomial_bayes_inla_spatial(data_estimation = dd,
#                                                  response = c("positives", "total"),
#                                                  covariates = covariate_names,
#                                                  prior.iid = c(0.01, 0.01),
#                                                  flag_intercept = TRUE,
#                                                  flag_scale_vars = TRUE,
#                                                  flag_non_spatial_re = TRUE,
#                                                  mesh = mesh,
#                                                  alpha = 1.5,
#                                                  data_prediction = dd_prediction,
#                                                  n_sample = n_riskmap)
#
# gold_standard0$prediction$half_iqr %>% summary()
# gold_standard1$prediction$half_iqr %>% summary()
# gold_standard2$prediction$half_iqr %>% summary()
# gold_standard3$prediction$half_iqr %>% summary()
#
#
# x <- "binomial"
# x <- "zero_inflated_binomial"
#
# x <- "negative_binomial"
#
# switch(x,
#        "binomial" = "binomial",
#        "negative_binomial" = "nbinomial",
#        "zero_inflated_binomial" = c("binomial", "zeroinflatedbinomial1"),
#        "zero_inflated_negative_binomial" = c("binomial", "zeroinflatednbinomial1"))






