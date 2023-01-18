test_that("flextable_inla_binomial2 works", {
  skip("Table output needs to be inspected manually")
  c("ascaris", "hookworm", "trichiuris")%>%
    purrr::map(.f = ~ paste0("../bangladesh/Data/", .x, "_estimation_bivariate.rds") %>%
          readr::read_rds()) %>%
    flextable_inla_binomial2(
      measures = NULL,
      translate_covariate_names_type = "names_pv",
      term_order = readr::read_rds("../africa/Data/covariates_ordered.rds"),
      flag_drop_intercept = TRUE,
      central_value = "median"
    ) %>%
    flextable::italic(i = 1, italic = TRUE, part = "header") %>%
    flextable::align(i = 1, align = "center", part = "header") %>%
    flextable::set_header_labels(
      term = "",
      m1 = "A. lumbricoides",
      m2 = "Hookworm",
      m3 = "T. trichiura") %>%
    flextable::hline(i = flextable::nrow_part(., part = "body")-5, part = "body") %>%
    flextable::hline(i = flextable::nrow_part(., part = "body")-1, part = "body") %>%
    flextable::set_caption(caption = "Median (95%-HPDI) estimates of the parameters posterior distributions of the geostatistical model for the prevalence of each STH species. The parameters for the regression coefficients are given as odds ratios.") %>%
    fit_flextable_to_page()
})

test_that("flextable_inla_binomial2 works", {
  skip("Table output needs to be inspected manually")
  model_list <- c("ascaris", "hookworm")%>%
    purrr::map(.f = ~ paste0("../bangladesh/Data/", .x, "_estimation_bivariate.rds") %>%
                 readr::read_rds())
  model_list %>%
    flextable_inla_binomial2(
      measures = NULL,
      translate_covariate_names_type = "names_pv",
      term_order = readr::read_rds("../africa/Data/covariates_ordered.rds"),
      flag_drop_intercept = TRUE,
      central_value = "mean"
    ) %>% flextable::autofit()
  model_list %>%
    flextable_inla_binomial2(
      measures = NULL,
      translate_covariate_names_type = "names_pv",
      term_order = readr::read_rds("../africa/Data/covariates_ordered.rds"),
      flag_drop_intercept = TRUE,
      central_value = "median"
    ) %>% flextable::autofit()
  model_list %>%
    flextable_inla_binomial2(
      measures = NULL,
      translate_covariate_names_type = "names_pv",
      term_order = readr::read_rds("../africa/Data/covariates_ordered.rds"),
      flag_drop_intercept = TRUE,
      flag_summary_fixed_or = FALSE,
      central_value = "median"
    ) %>% flextable::autofit()
})

test_that("flextable_inla_binomial2 works", {
  skip("Table output needs to be inspected manually")
  model_list <- c("ascaris", "hookworm")%>%
    purrr::map(.f = ~ paste0("../bangladesh/Data/", .x, "_estimation_bivariate.rds") %>%
                 readr::read_rds())
  model_list %>%
    flextable_inla_binomial2(
      measures = NULL,
      translate_covariate_names_type = "names_pv",
      term_order = readr::read_rds("../africa/Data/covariates_ordered.rds"),
      flag_drop_intercept = TRUE,
      central_value = "mean"
    ) %>% flextable::autofit()
  model_list %>%
    flextable_inla_binomial2(
      measures = NULL,
      translate_covariate_names_type = "names_pv",
      term_order = readr::read_rds("../africa/Data/covariates_ordered.rds"),
      flag_drop_intercept = TRUE,
      central_value = "median"
    ) %>% flextable::autofit()
  model_list %>%
    flextable_inla_binomial2(
      measures = NULL,
      translate_covariate_names_type = "names_pv",
      term_order = readr::read_rds("../africa/Data/covariates_ordered.rds"),
      flag_drop_intercept = TRUE,
      flag_summary_fixed_or = FALSE,
      central_value = "median"
    ) %>% flextable::autofit()

  sf_gambia_with_factors <- sf_gambia %>%
    dplyr::mutate(poverty = rep(1:5, 13) %>%
                    factor(labels = c("poorest", "poor", "middle_soso", "wealthy", "wealthiest")),
                  netusefac = netuse %>% as_quantile_factor())
  mlist <- fit_inla2(sf_gambia_with_factors,
            response = c("positives", "total"),
            covariates = c("netusefac", "phc", "poverty"),
            flag_intercept = TRUE,
            flag_spatial_re = TRUE,
            mesh = gambia_mesh) %>%
    list()
  mlist %>%
    flextable_inla_binomial2(
      measures = NULL,
      term_order = c("netusefac", "phc", "poverty"),
      flag_drop_intercept = TRUE,
      flag_summary_fixed_or = FALSE,
      central_value = "median"
    ) %>% flextable::autofit()
})

