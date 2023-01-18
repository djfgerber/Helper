test_that("flextable_bvs_frequencies works", {
  skip("No test implemented yet")
  res1 <- Helper::read_simulation("3-BVS_ascaris_vif_10_binomial",
                          rmd_name = "bangladesh")
  res2 <- Helper::read_simulation("3-BVS_hookworm_vif_10_binomial",
                                 rmd_name = "bangladesh")
  res3 <- Helper::read_simulation("3-BVS_trichiuris_vif_10_binomial",
                                 rmd_name = "bangladesh")
  bvs_results_list <- list(res1,res2,res3)
  flextable_bvs_frequencies(bvs_results_list = bvs_results_list,
                            discard_first = rep(NA_integer_, 3),
                            model_names = c("A. lumbricoides",  "Hookworm", "T. Trichiura"))
  flextable_bvs_frequencies(bvs_results_list = bvs_results_list,
                            discard_first = c(20000, 10000, 30000) %>% as.integer(),
                            model_names = c("A. lumbricoides",  "Hookworm", "T. Trichiura"))
  flextable_bvs_frequencies(bvs_results_list = bvs_results_list,
                            discard_first = c(20000, 10000, 30000) %>% as.integer(),
                            model_names = c("A. lumbricoides",  "Hookworm", "T. Trichiura"),
                            read_rds("../africa/Data/covariates_ordered.rds"))

  res1 %>% get_bvs_hpd_model(20000L, "bla")
})
