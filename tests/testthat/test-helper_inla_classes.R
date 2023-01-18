test_that("construction and validation helper_spde_init works", {
  x <- new_helper_spde_init(1,2,3)
  expect_identical(length(x),3L)
  expect_error(validate_helper_spde_init(new_helper_spde_init(1,2,3)))
  expect_error(validate_helper_spde_init(
    new_helper_spde_init(gambia_mesh,
                         2,
                         3)))
  expect_error(validate_helper_spde_init(
    new_helper_spde_init(gambia_mesh,
                         structure(2, class = "helper_prior_spde"),
                         3L)))
  expect_error(validate_helper_spde_init(
    new_helper_spde_init(gambia_mesh,
                         structure(2, class = "helper_prior_spde"),
                         3)), NA)
})

test_that("construction and validation helper_prior works", {
  helper_prior("spatial_variance", "pc_prior", list(value = 2, probability = .01))
  helper_prior("non_spatial_variance", "gamma", list(shape = .5, rate = .01))
  helper_prior("range", "pc_prior", c(100,.5))
})
