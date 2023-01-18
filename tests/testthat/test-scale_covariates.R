test_that("scale_vars works", {
  # scale_vars(sf_gambia,
  #            c("green", "netuse"),
  #            NULL)
  # scale_vars(sf_gambia,
  #            NULL,
  #            NULL)
  # scale_vars(sf_gambia,
  #            NULL,
  #            sf_gambia)
  expect_identical(scale_vars(sf_gambia[1:50, 8:1],
                              c("green", "netuse"),
                              sf_gambia[51:65, 8:1]) %>%
                     purrr::map(~.x[,8:1]),
                   scale_vars(sf_gambia[1:50, 1:8],
                              c("green", "netuse"),
                              sf_gambia[51:65, 1:8]))
})

test_that("resolve_factors works", {
  sf_gambia_with_factors <- sf_gambia %>%
    dplyr::mutate(poverty = rep(1:5, 13) %>%
                    factor(labels = c("poorest", "poor", "middle", "wealthy", "wealthiest")))
  expect_identical(resolve_factors(sf_gambia_with_factors, "treated")$data_estimation,
                   sf_gambia_with_factors)
  expect_true(!"poverty" %in% resolve_factors(sf_gambia_with_factors, "poverty")$covariates &&
                !"poverty" %in% names(resolve_factors(sf_gambia_with_factors, "poverty")$data_estimation)  )
  expect_equal(ncol(resolve_factors(sf_gambia_with_factors, c("poverty", "treated"))$data_estimation),
               ncol(sf_gambia_with_factors)+3L)

  expect_identical(resolve_factors(sf_gambia_with_factors[1:60, ],
                                   "phc",
                                   sf_gambia_with_factors[61:65, ])$data_estimation,
                   sf_gambia_with_factors[1:60, ])
  expect_identical(resolve_factors(sf_gambia_with_factors[1:60, ],
                                   "phc",
                                   sf_gambia_with_factors[61:65, ])$data_prediction,
                   sf_gambia_with_factors[61:65, ])
  expect_true(!"poverty" %in%   resolve_factors(sf_gambia_with_factors[1:60, ],
                                                "poverty",
                                                sf_gambia_with_factors[61:65, ])$covariates &&
                !"poverty" %in% names(resolve_factors(sf_gambia_with_factors[1:60, ],
                                                        "poverty",
                                                        sf_gambia_with_factors[61:65, ])$data_estimation)  &&
                !"poverty" %in% names(resolve_factors(sf_gambia_with_factors[1:60, ],
                                                      "poverty",
                                                      sf_gambia_with_factors[61:65, ])$data_prediction))
  expect_equal(ncol(resolve_factors(sf_gambia_with_factors[1:60,],
                                    c("poverty", "treated"),
                                    sf_gambia_with_factors[61:65,])$data_estimation),
               ncol(sf_gambia_with_factors)+3L)
  expect_equal(ncol(resolve_factors(sf_gambia_with_factors[1:60,],
                                    c("poverty", "treated"),
                                    sf_gambia_with_factors[61:65,])$data_prediction),
               ncol(sf_gambia_with_factors)+3L)
})
