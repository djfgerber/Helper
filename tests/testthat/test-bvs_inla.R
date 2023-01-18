test_that("bvs_inla works", {
  expect_identical(bvs_inla(covariates = c("green", "phc"),
                      probs = c(0.22,.5,.75),
                      data_estimation = sf_gambia %>% dplyr::mutate(phc = phc %>% as.factor()),
                      response = c("positives", "total"),
                      mesh = gambia_mesh,
                      flag_intercept = TRUE,
                      flag_spatial_re = TRUE,
                      flag_non_spatial_re = TRUE,
                      workers = 8L) %>% dim(),
                  c(6L,4L))
})

