test_that("binomial_bivariate_summary works", {
  testthat::skip("no test implemented")
  dat <- "../bangladesh/Data/binomial_bivariate_spatial_" %>%
    paste0("ascaris", ".rds") %>%
    readr::read_rds() %>%
    dplyr::mutate(species = "A. lumbricoides")
  dat %>%
    conditional_mutate(dplyr::across(c(estimate, se), ~ .x + 1), condition = T)
  dat %>%
    conditional_mutate(dplyr::across(c(estimate, se), ~ .x + 1), condition = F)
  dat %>%
    conditional_filter(!term %in% c("intercept", "zi_intercept"), condition = T)
  dat %>%
    conditional_filter(!term %in% c("intercept", "zi_intercept"), condition = F)
  dat %>% plot_bivariate_coef(translate_covariate_names_type = "names_pv",
                              flag_drop_intercept = TRUE)
  dat %>% plot_bivariate_coef(translate_covariate_names_type = "names_pv",
                              flag_drop_intercept = TRUE,
                              flag_order_alphabetically = TRUE)
  dat %>% plot_bivariate_coef(translate_covariate_names_type = "names_pv",
                              flag_drop_intercept = TRUE,
                              flag_order_alphabetically = FALSE)
  dat %>% plot_bivariate_coef(translate_covariate_names_type = "names_pv",
                              flag_drop_intercept = TRUE,
                              flag_order_alphabetically = FALSE,
                              term_order = readr::read_rds("../africa/Data/covariates_ordered.rds"))
})
