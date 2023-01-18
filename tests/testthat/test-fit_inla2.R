test_that("get_covariates_design_matrix works", {
  skip("No test implemented yet")
  sf_gambia_with_factors <- sf_gambia %>%
    dplyr::mutate(poverty = rep(1:5, 13) %>%
                    factor(labels = c("poorest", "poor", "middle", "wealthy", "wealthiest")))
  get_covariates_design_matrix(sf_gambia_with_factors,
                               covariates = "treated",
                               data_prediction = NULL,
                               flag_intercept = TRUE,
                               flag_append01 = FALSE)
  get_covariates_design_matrix(sf_gambia_with_factors,
                               covariates = "treated",
                               data_prediction = NULL,
                               flag_intercept = TRUE,
                               flag_append01 = TRUE)
  get_covariates_design_matrix(sf_gambia_with_factors,
                               covariates = c("treated", "poverty"),
                               data_prediction = NULL,
                               flag_intercept = TRUE,
                               flag_append01 = TRUE)
  get_covariates_design_matrix(sf_gambia_with_factors[1:60,],
                               covariates = "treated",
                               data_prediction = sf_gambia_with_factors[61:65,],
                               flag_intercept = TRUE,
                               flag_append01 = TRUE)
  get_covariates_design_matrix(sf_gambia_with_factors[1:60,],
                               covariates = c("treated", "poverty"),
                               data_prediction = sf_gambia_with_factors[61:65,],
                               flag_intercept = TRUE,
                               flag_append01 = TRUE)
})

test_that("get_inla_data works", {
  a1 <- get_inla_data(
    sf_gambia,
    response = "positives",
    mode = "normal",
    flag_estimation = TRUE
  )
  expect_type(a1, "list")
  expect_identical(length(a1), 1L)
  expect_identical(names(a1), "y")
  expect_identical(dim(a1[[1]]), NULL)
  expect_identical(length(a1[[1]]), 65L)

  a2 <- get_inla_data(sf_gambia,
                response = "zero_inflated_positives",
                mode = "normal",
                flag_estimation = TRUE)
  expect_type(a2, "list")
  expect_identical(length(a2), 1L)
  expect_identical(names(a2), "y")
  expect_identical(dim(a2[[1]]), NULL)
  expect_identical(length(a2[[1]]), 65L)

  a3 <- get_inla_data(sf_gambia,
                response = "zero_inflated_positives",
                mode = "za_truncated",
                flag_estimation = TRUE)
  expect_type(a3, "list")
  expect_identical(length(a3), 1L)
  expect_identical(names(a3), "y")
  expect_identical(dim(a3[[1]]), c(65L, 2L))
  expect_true(!any(a3[[1]][,1] == 0, na.rm = TRUE))
  expect_true(all(is.na(a3[[1]][,2])))


  a4 <- get_inla_data(sf_gambia,
                response = "zero_inflated_positives",
                mode = "za01",
                flag_estimation = TRUE)
  expect_type(a4, "list")
  expect_identical(length(a4), 1L)
  expect_identical(names(a4), "y")
  expect_identical(dim(a4[[1]]), c(65L, 2L))
  expect_true(all(is.na(a4[[1]][,1])))
  expect_true(all(!is.na(a4[[1]][,2])))
  expect_true(all((a4[[1]][,2] %in% c(0,1))))
})

test_that("fit_inla2 binomial estimation works", {
  expect_error(fit_inla2(sf_gambia,
            family = "binomial",
            response = c("positives", "total"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = FALSE),
            NA)

  expect_error(fit_inla2(sf_gambia,
            response = c("positives", "total"),
            covariates = c("netuse"),
            family = "binomial",
            flag_intercept = TRUE,
            flag_spatial_re = TRUE,
            flag_non_spatial_re = FALSE,
            mesh = gambia_mesh),
            NA)

  expect_error(fit_inla2(sf_gambia,
            family = "binomial",
            response = c("positives", "total"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = TRUE),
            NA)
  expect_error(fit_inla2(sf_gambia,
                         family = "binomial",
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         flag_intercept = TRUE,
                         flag_spatial_re = TRUE,
                         mesh = gambia_mesh,
                         flag_non_spatial_re = TRUE),
               NA)
  expect_error(fit_inla2(sf_gambia,
                         family = "binomial",
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         flag_intercept = TRUE,
                         flag_spatial_re = TRUE,
                         flag_non_spatial_re = TRUE,
                         mesh = gambia_mesh,
                         flag_spatial_constr = TRUE),
               NA)
  expect_error(fit_inla2(sf_gambia,
                         family = "binomial",
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         flag_intercept = TRUE,
                         flag_spatial_re = TRUE,
                         flag_non_spatial_re = TRUE,
                         mesh = gambia_mesh,
                         flag_spatial_constr = TRUE,
                         flag_spatial_extraconstr = TRUE),
               NA)
})


test_that("fit_inla2 binomial prediction works", {
  expect_error(m1 <- fit_inla2(sf_gambia[1:60,],
                         family = "binomial",
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         flag_intercept = TRUE,
                         flag_spatial_re = FALSE,
                         flag_non_spatial_re = FALSE,
                         data_prediction = sf_gambia[61:65,],
                         flag_keep_predictor_marginals = TRUE),
               NA)
  expect_error(m1_prediction <- m1 %>%
                 get_inla_prediction_summary(),
               NA)
  expect_identical(class(m1_prediction), c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(dim(m1_prediction), c(5L, 9L))
  expect_error(m1_prediction <- m1 %>%
                 get_inla_prediction_summary(flag_add_predictors = TRUE),
               NA)
  expect_identical(class(m1_prediction), c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(dim(m1_prediction), c(5L, 23L))
  expect_error(m1_prediction <- m1 %>%
                 get_inla_prediction_summary(hdpi_probabilities = c(.5,.7)),
               NA)
  expect_identical(class(m1_prediction), c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(dim(m1_prediction), c(5L, 9L+4L))

  expect_error(m1_sample <- m1 %>%
                 get_inla_prediction_sample(7L),
               NA)
  expect_identical(dim(m1_sample), c(5L, 8L))
  expect_error(m1_sample <- m1 %>%
                 get_inla_prediction_sample(7L, flag_add_predictors = TRUE),
               NA)
  expect_identical(dim(m1_sample), c(5L, 22L))

  expect_error(m2 <- fit_inla2(sf_gambia[1:60,],
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         family = "binomial",
                         flag_intercept = TRUE,
                         flag_spatial_re = TRUE,
                         flag_non_spatial_re = FALSE,
                         mesh = gambia_mesh,
                         data_prediction = sf_gambia[61:65,]),
               NA)
  expect_error(m2_prediction <- m2 %>%
                 get_inla_prediction_summary(),
               NA)
  expect_identical(class(m2_prediction), c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(dim(m2_prediction), c(5L, 9L))
  expect_error(m2_sample <- m2 %>%
                 get_inla_prediction_sample(7L),
               NA)
  expect_identical(dim(m2_sample), c(5L, 8L))

  expect_error(m3 <- fit_inla2(sf_gambia[1:60,],
                         family = "binomial",
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         flag_intercept = TRUE,
                         flag_spatial_re = FALSE,
                         flag_non_spatial_re = TRUE,
                         data_prediction = sf_gambia[61:65,]),
               NA)
  expect_error(m3_prediction <- m3 %>%
                 get_inla_prediction_summary(),
               NA)
  expect_identical(class(m3_prediction), c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(dim(m3_prediction), c(5L, 9L))
  expect_error(m3_sample <- m3 %>%
                 get_inla_prediction_sample(7L),
               NA)
  expect_identical(dim(m3_sample), c(5L, 8L))
})

test_that("fit_inla2 zib works", {
  skip("This is an example, no test.")
  expect_error(fit_inla2(sf_gambia,
            family = "zib",
            response = c("positives", "total"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = FALSE),
            NA)

  expect_error(fit_inla2(sf_gambia,
            response = c("positives", "total"),
            covariates = c("netuse"),
            family = "zib",
            flag_intercept = TRUE,
            flag_spatial_re = TRUE,
            flag_non_spatial_re = FALSE,
            mesh = gambia_mesh),
            NA)

  expect_error(fit_inla2(sf_gambia,
            family = "zib",
            response = c("positives", "total"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = TRUE),
            NA)
})

test_that("fit_inla2 zib prediction works", {
  expect_error(m1 <- fit_inla2(sf_gambia[1:60,],
                               family = "zib",
                               response = c("positives", "total"),
                               covariates = c("netuse"),
                               flag_intercept = TRUE,
                               flag_spatial_re = FALSE,
                               flag_non_spatial_re = FALSE,
                               data_prediction = sf_gambia[61:65,]),
               NA)
  expect_error(m1_prediction <- m1 %>%
                 get_inla_prediction_summary(),
               NA)
  # pred_ind <- INLA::inla.stack.index(m1$stack, "prediction")$data
  # p_z <- m1$fit$summary.hyperpar["zero-probability parameter for zero-inflated binomial_1","mean"]
  # p_y <- m1$fit$summary.fitted.values$mean[pred_ind]
  # n <- m1$input$data_prediction$total
  # e_x <- (1-p_z)*p_y*n
  # var_x <- (1-p_z)*(n*p_y*(1-p_y)+n^2*p_y^2) - e_x
  # expect_equal(e_x/n, m1_prediction$estimate, tolerance = 0.05)
  # expect_equal(sqrt(var_x), m1_prediction$se, tolerance = 0.05)
  expect_identical(class(m1_prediction),
                   c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(dim(m1_prediction), c(5L, 8L))
  expect_error(m1_sample <- m1 %>%
                 get_inla_prediction_sample(5L),
               NA)
  expect_identical(dim(m1_sample), c(5L, 6L))

  expect_error(m2 <- fit_inla2(sf_gambia[1:60,],
                               response = c("positives", "total"),
                               covariates = c("netuse"),
                               family = "zib",
                               flag_intercept = TRUE,
                               flag_spatial_re = TRUE,
                               flag_non_spatial_re = FALSE,
                               mesh = gambia_mesh,
                               data_prediction = sf_gambia[61:65,]),
               NA)
  expect_error(m2_prediction <- m2 %>%
                 get_inla_prediction_summary(),
               NA)
  expect_identical(class(m2_prediction),
                   c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(dim(m2_prediction), c(5L, 8L))
  expect_error(m2_sample <- m2 %>%
                 get_inla_prediction_sample(5L),
               NA)
  expect_identical(dim(m2_sample), c(5L, 6L))

  expect_error(m3 <- fit_inla2(sf_gambia[1:60,],
                               family = "zib",
                               response = c("positives", "total"),
                               covariates = c("netuse"),
                               flag_intercept = TRUE,
                               flag_spatial_re = FALSE,
                               flag_non_spatial_re = TRUE,
                               data_prediction = sf_gambia[61:65,]),
               NA)
  expect_error(m3_prediction <- m3 %>%
                 get_inla_prediction_summary(),
               NA)
  expect_identical(class(m3_prediction),
                   c("sf", "tbl_df", "tbl", "data.frame"))
  expect_identical(dim(m3_prediction), c(5L, 8L))
  expect_error(m3_sample <- m3 %>%
                 get_inla_prediction_sample(5L),
               NA)
  expect_identical(dim(m3_sample), c(5L, 6L))
})

test_that("zab works", {
  skip("Does not work yet. Fix get_summary_fixed and _hyperpar")
  expect_error(fit_inla2(sf_gambia,
            family = "zab",
            response = c("positives", "total"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = FALSE),
            NA)
  expect_error(fit_inla2(sf_gambia,
            response = c("positives", "total"),
            covariates = c("netuse"),
            family = "zab",
            flag_intercept = TRUE,
            flag_spatial_re = TRUE,
            flag_non_spatial_re = FALSE,
            mesh = gambia_mesh),
            NA)
  expect_error(fit_inla2(sf_gambia,
            family = "zab",
            response = c("positives", "total"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = TRUE),
            NA)
  expect_error(fit_inla2(sf_gambia,
                         family = "zab",
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         covariates01 = c("netuse"),
                         flag_intercept = TRUE,
                         flag_intercept01 = TRUE,
                         flag_spatial_re = FALSE,
                         flag_non_spatial_re = FALSE),
               NA)
  expect_error(fit_inla2(sf_gambia,
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         covariates01 = c("netuse"),
                         family = "zab",
                         flag_intercept = TRUE,
                         flag_intercept01 = TRUE,
                         flag_spatial_re = TRUE,
                         flag_spatial_re01 = FALSE,
                         flag_non_spatial_re = FALSE,
                         mesh = gambia_mesh),
               NA)
  expect_error(fit_inla2(sf_gambia,
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         covariates01 = c("netuse"),
                         family = "zab",
                         flag_intercept = TRUE,
                         flag_intercept01 = TRUE,
                         flag_spatial_re = TRUE,
                         flag_spatial_re01 = TRUE,
                         flag_non_spatial_re = FALSE,
                         mesh = gambia_mesh),
               NA)
  expect_error(fit_inla2(sf_gambia,
                         family = "zab",
                         response = c("positives", "total"),
                         covariates = c("netuse"),
                         covariates01 = c("netuse"),
                         flag_intercept = TRUE,
                         flag_intercept01 = TRUE,
                         flag_spatial_re = FALSE,
                         flag_non_spatial_re = TRUE),
               NA)
})

test_that("negative_binomial works", {
  skip("This is an example, no test.")
  fit_inla2(sf_gambia,
            family = "negative_binomial",
            response = c("positives"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = FALSE)

  fit_inla2(sf_gambia,
            response = c("positives"),
            covariates = c("netuse"),
            family = "negative_binomial",
            flag_intercept = TRUE,
            flag_spatial_re = TRUE,
            flag_non_spatial_re = FALSE,
            mesh = gambia_mesh)

  fit_inla2(sf_gambia,
            family = "negative_binomial",
            response = c("positives"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = TRUE)
})

test_that("poisson works", {
  skip("This is an example, no test.")
  fit_inla2(sf_gambia,
            family = "poisson",
            response = c("positives"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = FALSE)

  fit_inla2(sf_gambia,
            response = c("positives"),
            covariates = c("netuse"),
            family = "poisson",
            flag_intercept = TRUE,
            flag_spatial_re = TRUE,
            flag_non_spatial_re = FALSE,
            mesh = gambia_mesh)

  fit_inla2(sf_gambia,
            family = "poisson",
            response = c("positives"),
            covariates = c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = TRUE)
})


test_that("zuur_spatial_chp13 works",{
  # skip("This is an example, no test.")
  LP <- read.table(file = "J:/BS/SSM/daniel.gerber/Switchdrive/Literature/Books/Zuur, Ieno, Saveliev - Spatial, Termporal and Spatial-Temporal Ecological Data Analysis with R-INLA/Volume 1/LaPalma.txt",
                   header = TRUE,
                   dec = ".")
  names(LP)
  str(LP)
  library(sp)
  utmcoor <- SpatialPoints(cbind(LP$Longitude, LP$Latitude),
                           proj4string = CRS("+proj=utm +zone=28N"))

  longlatcoor <- spTransform(utmcoor, CRS("+proj=longlat"))
  LP$Longitude2 <- coordinates(longlatcoor)[,1]
  LP$Latitude2  <- coordinates(longlatcoor)[,2]
  range(LP$Longitude2)
  range(LP$Latitude2)
  LP <- LP %>%
    tibble::as_tibble()
  dd <- LP %>%
    sf::st_as_sf(coords = c("Longitude2", "Latitude2"),
                 crs = CRS("+proj=longlat"))
  covariates <- c("CR_CAN", "CR_LP", "INTER_VAR", "MAP",
                  "Easterness", "Age", "Northerness", "Slope",
                  "TCI")
  myI1 <- fit_inla2(dd,
            family = "poisson",
            response = c("nSIE"),
            covariates = covariates,
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = FALSE,
            mesh = NULL)
  myExpY <- myI1$fit$summary.fitted.values %>%
    tibble::as_tibble(rownames = "rownames") %>%
    dplyr::filter(rownames %>% stringr::str_detect("fitted.APredictor")) %>%
    dplyr::pull("mean")
  I1 <- INLA::inla(as.formula(paste0("nSIE ~ ", paste0(covariates, collapse = "+"))),
             family = "poisson",
             control.predictor = list(compute = TRUE),
             data = LP %>%
               dplyr::mutate(
                 dplyr::across(dplyr::all_of(covariates[-which(covariates %in% c("Easterness", "Northerness"))]),
                               ~ (.x- mean(.x))/sd(.x))))
  ExpY <- I1$summary.fitted.values[,"mean"]
  expect_equal(myExpY, ExpY, tolerance =  0.000001)

  LP <- LP %>%
    dplyr::mutate(Xkm = Longitude /1000,
                  Ykm = Latitude / 1000)
  Loc <- cbind(LP$Xkm, LP$Ykm)
  ShapeF.utm <- rgdal::readOGR(dsn = "J:/BS/SSM/daniel.gerber/Switchdrive/Literature/Books/Zuur, Ieno, Saveliev - Spatial, Termporal and Spatial-Temporal Ecological Data Analysis with R-INLA/Volume 1/lapalmashapefile/lapalma.shp",
                        layer = "lapalma")
  LaPalma_df <- ggplot2::fortify(ShapeF.utm)
  head(LaPalma_df)
  LaPalma_df$Xkm <- LaPalma_df$long / 1000
  LaPalma_df$Ykm <- LaPalma_df$lat / 1000
  CoastLine <- LaPalma_df[,c("Xkm", "Ykm")]
  N <- nrow(CoastLine)
  ReOrder <- N:1
  Loc.Reverse <- CoastLine[ReOrder, c("Xkm", "Ykm")]
  mesh2 <- INLA::inla.mesh.2d(
    loc.domain = CoastLine,
    max.edge = 1.5,
    boundary = INLA::inla.mesh.segment(Loc.Reverse))
  dd <- LP %>%
    sf::st_as_sf(coords = c("Xkm", "Ykm"),
                 crs = CRS("+proj=utm +zone=28N +units=km"))
  covariates <- c("CR_CAN", "CR_LP", "INTER_VAR", "MAP",
                   "Age", "Slope",
                  "TCI")
  myI2 <- fit_inla2(dd,
            family = "poisson",
            response = c("nSIE"),
            covariates = covariates,
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = FALSE) %>%
    print()
  I3 <- fit_inla2(dd,
                  family = "poisson",
                  response = c("nSIE"),
                  covariates = covariates,
                  flag_intercept = TRUE,
                  flag_spatial_re = TRUE,
                  flag_non_spatial_re = FALSE,
                  mesh = mesh2) %>%
    print()
  mesh2$n
  w.pm <- I3$fit$summary.random$spatial_field$mean
  length(w.pm)
  wproj <- INLA::inla.mesh.projector(mesh2)
  w.pm100_100 <- INLA::inla.mesh.project(wproj, w.pm)
  grid <- expand.grid(Xkm = wproj$x,
                      Ykm = wproj$y)
  grid$w.pm <- as.vector(w.pm100_100)
  grid$w.pm_exp <- exp(as.vector(w.pm100_100))
  lattice::levelplot(w.pm ~Xkm + Ykm,
                     data = grid,
                     aspect = "iso",
                     col.regions = rev(rainbow(30, alpha = .35)),
                     scales = list(draw=TRUE))
  lattice::levelplot(w.pm_exp ~ Xkm + Ykm,
                     data = grid,
                     aspect = "iso",
                     col.regions = rev(rainbow(30, alpha = .35)),
                     scales = list(draw=TRUE))

  uproj <- INLA::inla.mesh.projector(mesh2, loc = Loc) #as in u_i = sum(a_ik * w_k)
  u.mean <- INLA::inla.mesh.project(uproj, w.pm)
  length(u.mean)
  sum(u.mean  > 0, na.rm = TRUE)
  sum(u.mean  < 0, na.rm = TRUE)
})

test_that("fit_inla2 zuur_spatial_chp18 works",{
  Skate <- read.table(file = "J:/BS/SSM/daniel.gerber/Switchdrive/Literature/Books/Zuur, Ieno, Saveliev - Spatial, Termporal and Spatial-Temporal Ecological Data Analysis with R-INLA/Volume 2/Skate2.txt",
                   header = TRUE,
                   dec = ".")
  Skate$fYear       <- factor(Skate$Year)
  Skate$fMonth      <- factor(Skate$Month)
  Skate$fBottomType <- factor(Skate$BottomType,
                              levels = c(1, 2, 3, 4),
                              labels = c("Mud",
                                         "SandMud",
                                         "Sand",
                                         "SandShellRest"))
  Skate <- Skate %>%
    dplyr::mutate(Temp.std = (Temperature - mean(Temperature))/sd(Temperature),
                  SweptArea.std = (SweptArea - mean(SweptArea))/sd(SweptArea))

  # model without zero treatment
  I1 <- INLA::inla(SB ~ Temp.std + SweptArea.std,
             family = "poisson",
             control.compute = list(dic = TRUE, waic = TRUE),
             data = Skate)
  helperI1 <- Skate %>%
    fit_inla2(family = "poisson",
              response = "SB",
              covariates = c("Temperature", "SweptArea"),
              flag_intercept = TRUE,
              flag_spatial_re = FALSE,
              flag_non_spatial_re = FALSE)
  expect_equal(I1$summary.fixed$mean,
               helperI1$fit$summary.fixed$mean[c(3,1,2)],
               tolerance = 0.000001)
  expect_equal(I1$summary.fixed$sd,
               helperI1$fit$summary.fixed$sd[c(3,1,2)],
               tolerance = 0.00001)

  # zero inflated model no covariates (not possible)
  I2 <- INLA::inla(SB ~ Temp.std + SweptArea.std,
             family = "zeroinflatedpoisson1",
             data = Skate,
             control.compute=list(config = TRUE, dic = TRUE, waic = TRUE))
  helperI2 <- fit_inla2(Skate,
                  family = "zip",
                  response = c("SB"),
                  covariates = c("Temperature", "SweptArea"),
                  flag_intercept = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = FALSE)
  expect_equal(I2$summary.fixed$mean,
               helperI2$fit$summary.fixed$mean[c(3,1,2)],
               tolerance = 0.000001)
  expect_equal(I2$summary.fixed$sd,
               helperI2$fit$summary.fixed$sd[c(3,1,2)],
               tolerance = 0.0001)
  expect_equal(I2$summary.hyperpar$mean,
               helperI2$fit$summary.hyperpar$mean,
               tolerance = 0.00001)
  expect_equal(I2$summary.hyperpar$sd,
               helperI2$fit$summary.hyperpar$sd,
               tolerance = 0.1)

  # zero altered model no covariates
  I3 <- INLA::inla(SB ~ Temp.std + SweptArea.std,
             family = "zeroinflatedpoisson0",
             data = Skate,
             control.compute = list(config = TRUE))
  helperI3 <- fit_inla2(Skate,
                  family = "zap",
                  response = c("SB"),
                  covariates =  c("Temperature", "SweptArea"),
                  flag_intercept = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = FALSE)
  expect_equal(I3$summary.fixed$mean,
               helperI3$fit$summary.fixed$mean[c(3,1,2)],
               tolerance = 0.000001)
  expect_equal(I3$summary.fixed$sd,
               helperI3$fit$summary.fixed$sd[c(3,1,2)],
               tolerance = 0.00001)
  expect_equal(I3$summary.hyperpar$mean,
               helperI3$fit$summary.hyperpar$mean,
               tolerance = 0.000001)
  expect_equal(I3$summary.hyperpar$sd,
               helperI3$fit$summary.hyperpar$sd,
               tolerance = 0.00001)

  # zero altered model with covariates
  Skate$SB01  <- ifelse(Skate$SB==0, 0, 1)
  N <- nrow(Skate)
  Skate$SBpos <- ifelse(Skate$SB > 0, Skate$SB, NA)
  Xpos <- data.frame(IntercPos = rep(1, N),
             TempPos   = Skate$Temp.std,
             SAPos     = Skate$SweptArea.std)
  X01 <- data.frame(Interc01 = rep(1, N),
                    Temp01   = Skate$Temp.std,
                    SA01     = Skate$SweptArea.std)
  StackPos <- INLA::inla.stack(
    tag  = "FitPos",
    data = list(AllY = cbind(Skate$SBpos, NA)),
    A    = list(1),
    effects = list(
      list(Xpos = Xpos)))
  Stack01 <- INLA::inla.stack(
    tag  = "Fit01",
    data = list(AllY = cbind(NA, Skate$SB01)),
    A    = list(1),
    effects = list(
      list(X01 = X01)))
  Stack4 <- INLA::inla.stack(StackPos, Stack01)
  f4 <- AllY ~ -1 + IntercPos + TempPos + SAPos +
    Interc01 + Temp01 + SA01
  HyperZap <- list(theta = list(intial = -10, fixed = TRUE))
  I4 <- INLA::inla(f4,
             family = c("zeroinflatedpoisson0", "binomial"),
             control.family = list(list(hyper = HyperZap),
                                   list()),
             data = INLA::inla.stack.data(Stack4),
             control.compute = list(dic = TRUE, waic = TRUE),
             control.predictor = list(
               link = 1,
               A = INLA::inla.stack.A(Stack4)))
  summary(I4)
  helperI4 <- fit_inla2(Skate,
                  family = "zap",
                  response = c("SB"),
                  covariates = c("Temperature", "SweptArea"),
                  covariates01 = c("Temperature", "SweptArea"),
                  flag_intercept = TRUE,
                  flag_intercept01 = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = FALSE)
  helperI4 # this is exactly the same as form book.
  expect_equal(I4$summary.fixed$mean,
               helperI4$fit$summary.fixed$mean[c(3,1,2, 6,4,5)],
               tolerance = 0.00001)
  expect_equal(I4$summary.fixed$sd,
               helperI4$fit$summary.fixed$sd[c(3,1,2, 6,4,5)],
               tolerance = 0.000001)

  # chpt 19 p. 417
  Loc <- as.matrix(Skate[,c("X", "Y")])
  RangeGuess <- 75 * 1000
  MaxEdge    <- RangeGuess / 5
  ConvHull   <- INLA::inla.nonconvex.hull(Loc, convex = 50 * 1000)
  mesh1      <- INLA::inla.mesh.2d(loc = Loc,
                             boundary = ConvHull,
                             max.edge = c(1, 5) * MaxEdge,
                             cutoff  = MaxEdge / 5)
  A1 <- INLA::inla.spde.make.A(mesh1, loc = Loc)
  spde1 <- INLA::inla.spde2.pcmatern(mesh1,
                               prior.range = c(50 * 1000 , 0.01),
                               prior.sigma = c(1.5, 0.01))
  w1.index <- INLA::inla.spde.make.index(name = 'w', n.spde  = spde1$n.spde)
  Xm <- model.matrix(~fYear + Temp.std + SweptArea.std + fBottomType,
                     data = Skate)
  N <- nrow(Skate)
  X <- data.frame(fYear1999         = Xm[, 2],
                  fYear2003         = Xm[, 3],
                  fYear2005         = Xm[, 4],
                  Temp.std          = Xm[, 5],
                  SweptArea.std     = Xm[, 6],
                  fBottomTypeSanMud = Xm[, 7],
                  fBottomTypeSand   = Xm[, 8],
                  fBottomTypeRest   = Xm[, 9])
  Stack.mesh1 <- INLA::inla.stack(
    tag  = "Fit",
    data = list(y = Skate$SB),
    A    = list(1, 1, A1, 1),
    effects = list(
      Intercept  = rep(1, N),
      X  = as.data.frame(X),
      w = w1.index,
      iidx = 1:nrow(X)))
  fPois.mesh1 <- y ~ -1 + Intercept + fYear1999 + fYear2003 + fYear2005 +
    Temp.std + SweptArea.std +
    fBottomTypeSanMud + fBottomTypeSand + fBottomTypeRest +
    f(w, model = spde1)
  Pois.mesh1 <- INLA::inla(fPois.mesh1,
                     family = "poisson",
                     data = INLA::inla.stack.data(Stack.mesh1),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(A = INLA::inla.stack.A(Stack.mesh1)))
  Pois.mesh1 %>% summary()
  helperPois.mesh1 <- Skate %>%
    sf::st_as_sf(coords = c("X", "Y")) %>%
    fit_inla2(
      response = c("SB"),
      covariates = c("fYear", "fBottomType", "Temperature", "SweptArea"),
      flag_intercept = TRUE,
      family = "poisson",
      mesh = mesh1,
      flag_spatial_re = TRUE,
      flag_non_spatial_re = FALSE,
      prior_pc_range = c(50 * 1000 , 0.01),
      prior_pc_sigma = c(1.5, 0.01)
    )
  expect_equal(
    Pois.mesh1$summary.hyperpar$mean[1],
    helperPois.mesh1$fit$summary.hyperpar$mean[1],
    tolerance = 0.3
  )
  expect_equal(
    Pois.mesh1$summary.hyperpar$mean[2],
    helperPois.mesh1$fit$summary.hyperpar$mean[2],
    tolerance = 0.03
  )

  plot_fitted_values(helperPois.mesh1)  # see Figure 19.11
  helperPois.mesh1 %>%
    plot_inla_zero_simulations()
})

test_that("get_inla_dispersion works",{
  # zuur et al. chpt 10 p 168
  # model summary are not identical because interaction term is scaled in helperM1
  TP <- read.table(file = "J:/BS/SSM/daniel.gerber/Switchdrive/Literature/Books/Zuur, Ieno, Saveliev - Spatial, Termporal and Spatial-Temporal Ecological Data Analysis with R-INLA/Volume 1/DataChapter10/Turcoparasitos.txt",
                   header = TRUE,
                   dec = ".")
  TP$fSex <- factor(TP$SEX)
  TP$fLoc <- factor(TP$Location)
  M1 <- INLA::inla(Totalparasites ~ fSex + LT * fLoc,
             control.compute = list(dic = TRUE),
             family = "poisson",
             data = TP)
  mu1 <- M1$summary.fitted.values[,"mean"]
  E1  <- (TP$Totalparasites - mu1) / sqrt(mu1) #Mistake in book
  N   <- nrow(TP)
  p   <- nrow(M1$summary.fixed)
  Dispersion <- sum(E1^2) / (N - p)
  model.matrix(Totalparasites~ LT * fLoc, TP)
  helperM1 <- TP %>%
    dplyr::mutate(LT_fLoc2 = ifelse(Location == 2, LT, 0),
                  LT_fLoc3 = ifelse(Location == 3, LT, 0),
                  SEX = SEX %>% as.factor(),
                  Location = Location %>% as.factor) %>%
    fit_inla2(response = "Totalparasites",
              covariates = c("Location", "LT", "SEX", "LT_fLoc2", "LT_fLoc3"),
              family = "poisson",
              flag_intercept = TRUE,
              flag_spatial_re = FALSE,
              flag_non_spatial_re = FALSE)
  expect_equal(Dispersion,
               get_inla_dispersion(helperM1),
               tolerance = 0.0001)

  # zuur et al. chpt 10 p 185
  # model summary are not identical because interaction term is scaled in helperM1
  Mites <- read.table(file = "J:/BS/SSM/daniel.gerber/Switchdrive/Literature/Books/Zuur, Ieno, Saveliev - Spatial, Termporal and Spatial-Temporal Ecological Data Analysis with R-INLA/Volume 1/DataChapter10/DrugsMites.txt",
                   header = TRUE,
                   dec = ".")
  Mites$fToxic <- factor(Mites$Toxic)
  M1 <- INLA::inla(Dead_mites ~ Concentration  * fToxic,
             family = "binomial",
             control.compute = list(dic = TRUE, waic = TRUE),
             Ntrials = Total,
             data = Mites)
  Pi  <- M1$summary.fitted.values[,"mean"]
  ExpY <- Pi * Mites$Total
  VarY <- Mites$Total * Pi * (1 - Pi)
  E1 <- (Mites$Dead_mites - ExpY) / sqrt(VarY)
  N <- nrow(Mites)
  p <- nrow(M1$summary.fixed)
  Dispersion <- sum(E1^2) / (N - p)
  helperM1 <- Mites %>%
    dplyr::mutate(Concentration_fToxic2 = ifelse(Toxic == 2, Concentration, 0),
                  Concentration_fToxic3 = ifelse(Toxic == 3, Concentration, 0),
                  Concentration_fToxic4 = ifelse(Toxic == 4, Concentration, 0)) %>%
    fit_inla2(response = c("Dead_mites", "Total"),
              covariates = c("Concentration", "fToxic", "Concentration_fToxic2",
                             "Concentration_fToxic3", "Concentration_fToxic4"),
              family = "binomial",
              flag_intercept = TRUE,
              flag_spatial_re = FALSE,
              flag_non_spatial_re = FALSE)
  expect_equal(Dispersion,
               get_inla_dispersion(helperM1),
               tolerance = 0.001)

  # zuur et al. chpt 18/19
  Skate <- read.table(file = "J:/BS/SSM/daniel.gerber/Switchdrive/Literature/Books/Zuur, Ieno, Saveliev - Spatial, Termporal and Spatial-Temporal Ecological Data Analysis with R-INLA/Volume 2/Skate2.txt",
                      header = TRUE,
                      dec = ".")
  Skate$fYear       <- factor(Skate$Year)
  Skate$fMonth      <- factor(Skate$Month)
  Skate$fBottomType <- factor(Skate$BottomType,
                              levels = c(1, 2, 3, 4),
                              labels = c("Mud",
                                         "SandMud",
                                         "Sand",
                                         "SandShellRest"))
  Skate <- Skate %>%
    dplyr::mutate(Temp.std = (Temperature - mean(Temperature))/sd(Temperature),
                  SweptArea.std = (SweptArea - mean(SweptArea))/sd(SweptArea))

  # zero inflated model no covariates p. 380
  I2 <- INLA::inla(SB ~ Temp.std + SweptArea.std,
                   family = "zeroinflatedpoisson1",
                   data = Skate,
                   control.compute=list(config = TRUE, dic = TRUE, waic = TRUE))
  helperI2 <- fit_inla2(Skate,
                        family = "zip",
                        response = c("SB"),
                        covariates = c("Temperature", "SweptArea"),
                        flag_intercept = TRUE,
                        flag_spatial_re = FALSE,
                        flag_non_spatial_re = FALSE)
  Pi.inla <- I2$summary.hyperpar[1,"mean"]
  mu2 <- I2$summary.fitted.values[,"mean"]
  ExpY <- (1 - Pi.inla) * mu2
  VarY <- (1 - Pi.inla) * (mu2 + Pi.inla * mu2^2)
  E2   <- (Skate$SB - ExpY) / sqrt(VarY)
  N <- nrow(Skate)
  p <- length(I2$names.fixed) + 1
  expect_equal(get_inla_dispersion(helperI2),
               sum(E2 ^ 2) / (N - p),
               tolerance = 0.0001)

  # zero altered model with covariates p. 393
  Skate$SB01  <- ifelse(Skate$SB==0, 0, 1)
  N <- nrow(Skate)
  Skate$SBpos <- ifelse(Skate$SB > 0, Skate$SB, NA)
  Xpos <- data.frame(IntercPos = rep(1, N),
                     TempPos   = Skate$Temp.std,
                     SAPos     = Skate$SweptArea.std)
  X01 <- data.frame(Interc01 = rep(1, N),
                    Temp01   = Skate$Temp.std,
                    SA01     = Skate$SweptArea.std)
  StackPos <- INLA::inla.stack(
    tag  = "FitPos",
    data = list(AllY = cbind(Skate$SBpos, NA)),
    A    = list(1),
    effects = list(
      list(Xpos = Xpos)))
  Stack01 <- INLA::inla.stack(
    tag  = "Fit01",
    data = list(AllY = cbind(NA, Skate$SB01)),
    A    = list(1),
    effects = list(
      list(X01 = X01)))
  Stack4 <- INLA::inla.stack(StackPos, Stack01)
  f4 <- AllY ~ -1 + IntercPos + TempPos + SAPos +
    Interc01 + Temp01 + SA01
  HyperZap <- list(theta = list(intial = -10, fixed = TRUE))
  I4 <- INLA::inla(f4,
                   family = c("zeroinflatedpoisson0", "binomial"),
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = INLA::inla.stack.data(Stack4),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = INLA::inla.stack.A(Stack4)))
  helperI4 <- fit_inla2(Skate,
                        family = "zap",
                        response = c("SB"),
                        covariates = c("Temperature", "SweptArea"),
                        covariates01 = c("Temperature", "SweptArea"),
                        flag_intercept = TRUE,
                        flag_intercept01 = TRUE,
                        flag_spatial_re = FALSE,
                        flag_non_spatial_re = FALSE)
  RowsPos <- INLA::inla.stack.index(Stack4, tag='FitPos')$data
  Rows01  <- INLA::inla.stack.index(Stack4, tag='Fit01')$data
  mu.ZTruncPois <- I4$summary.fitted.values[RowsPos, "mean"]
  Pi            <- I4$summary.fitted.values[Rows01, "mean"]
  muZAP <- Pi * mu.ZTruncPois
  varZAP  <- (Pi / (1 - exp(-mu.ZTruncPois))) * (mu.ZTruncPois + mu.ZTruncPois^2) - (  Pi * mu.ZTruncPois / (1 - exp(-mu.ZTruncPois))  )^2
  EZAP <- (Skate$SB - muZAP) / sqrt(varZAP)
  p <- 2 * 3
  expect_equal(get_inla_dispersion(helperI4),
               sum(EZAP^2) / (nrow(Skate) - p),
               tolerance = 0.001)

  # zuur et al. p 407
  Poi <- INLA::inla(SB ~ fYear + Temp.std + SweptArea.std + fBottomType ,
                    family = "poisson",
                    control.compute = list(dic = TRUE, waic = TRUE),
                    data = Skate)
  muPoi <- Poi$summary.fitted.values[,"mean"]
  EPoi <- (Skate$SB - muPoi) / sqrt(muPoi)
  N <- nrow(Skate)
  Poi$names.fixed
  p <- length(Poi$names.fixed)
  Dispersion <- sum(EPoi^2) / (N - p)
  helperPoi <- fit_inla2(Skate,
                         family = "poisson",
                         response = "SB",
                         covariates = c("Temperature", "SweptArea", "fBottomType", "fYear"),
                         flag_intercept = TRUE,
                         flag_spatial_re = FALSE,
                         flag_non_spatial_re = FALSE)
  expect_equal(get_inla_dispersion(helperPoi),
               Dispersion,
               tolerance = 0.0001)
})

test_that("plot_inla_zero_simulations works",{
  Skate <- read.table(file = "J:/BS/SSM/daniel.gerber/Switchdrive/Literature/Books/Zuur, Ieno, Saveliev - Spatial, Termporal and Spatial-Temporal Ecological Data Analysis with R-INLA/Volume 2/Skate2.txt",
                      header = TRUE,
                      dec = ".")
  helperI1 <- Skate %>%
    fit_inla2(family = "poisson",
              response = "SB",
              covariates = c("Temperature", "SweptArea"),
              flag_intercept = TRUE,
              flag_spatial_re = FALSE,
              flag_non_spatial_re = FALSE)
  expect_error(helperI1 %>%
    plot_inla_zero_simulations(),
    NA) # compare to Zuur et al. code chpt 18.

  helperI2 <- Skate %>%
    fit_inla2(family = "zip",
              response = "SB",
              covariates = c("Temperature", "SweptArea"),
              flag_intercept = TRUE,
              flag_spatial_re = FALSE,
              flag_non_spatial_re = FALSE)
  expect_error(helperI2 %>%
                 plot_inla_zero_simulations(),
               NA) # compare to Zuur et al. code chpt 18.4 p. 383

  helperI3 <- fit_inla2(Skate,
                        family = "zap",
                        response = c("SB"),
                        covariates =  c("Temperature", "SweptArea"),
                        flag_intercept = TRUE,
                        flag_spatial_re = FALSE,
                        flag_non_spatial_re = FALSE)
  expect_error(helperI3 %>%
                 plot_inla_zero_simulations(),
               NA) # no comparison

  helperI4 <- fit_inla2(Skate,
                        family = "zap",
                        response = c("SB"),
                        covariates =  c("Temperature", "SweptArea"),
                        covariates01 =  c("Temperature", "SweptArea"),
                        flag_intercept = TRUE,
                        flag_intercept01 = TRUE,
                        flag_spatial_re = FALSE,
                        flag_non_spatial_re = FALSE)
  expect_error(helperI4 %>%
    plot_inla_zero_simulations(),
    NA)# compare to Zuur et al. code chpt 18.5.6 p. 398

  m1 <- fit_inla2(sf_gambia,
                        family = "binomial",
                        response = c("positives", "total"),
                        covariates =  c("netuse"),
                        flag_intercept = TRUE,
                        flag_spatial_re = FALSE,
                        flag_non_spatial_re = FALSE)
  m1 %>% plot_inla_zero_simulations()
  m1_spatial_re <- fit_inla2(sf_gambia,
                             family = "binomial",
                             response = c("positives", "total"),
                             covariates =  c("netuse"),
                             flag_intercept = TRUE,
                             flag_spatial_re = TRUE,
                             mesh = gambia_mesh,
                             prior_pc_range = c(1, .8),
                             prior_pc_sigma = c(2, .01),
                             flag_non_spatial_re = FALSE)
  m1_spatial_re %>% plot_inla_zero_simulations()
  m1_spatial_re %>% plot_spatial_field(map_base = map_base, map_top = map_top)
  m1_non_spatial_re <- fit_inla2(sf_gambia,
                  family = "binomial",
                  response = c("positives", "total"),
                  covariates =  c("netuse"),
                  flag_intercept = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = TRUE)
  m1_non_spatial_re %>% plot_inla_zero_simulations()
  m1_both_re <- fit_inla2(sf_gambia,
                             family = "binomial",
                             response = c("positives", "total"),
                             covariates =  c("netuse"),
                             flag_intercept = TRUE,
                             flag_spatial_re = TRUE,
                             mesh = gambia_mesh,
                             prior_pc_range = c(1, .8),
                             prior_pc_sigma = c(2, .01),
                             flag_non_spatial_re = TRUE)
  m1_both_re %>% get_summary_hyperpar()
  m1_both_re %>% plot_inla_zero_simulations()
  m1_both_re %>% plot_spatial_field(map_base = map_base, map_top = map_top)

  m2 <- fit_inla2(sf_gambia,
                  family = "binomial",
                  response = c("zero_inflated_positives", "total"),
                  covariates =  c("netuse"),
                  flag_intercept = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = FALSE)
  m2 %>%
    plot_inla_zero_simulations()
  m2_spatial_re <- fit_inla2(sf_gambia,
                             family = "binomial",
                             response = c("zero_inflated_positives", "total"),
                             covariates =  c("netuse"),
                             flag_intercept = TRUE,
                             flag_spatial_re = TRUE,
                             mesh = gambia_mesh,
                             prior_pc_range = c(1, .8),
                             prior_pc_sigma = c(2, .01),
                             flag_non_spatial_re = FALSE)
  m2_spatial_re %>% plot_inla_zero_simulations()
  m2_spatial_re %>% plot_spatial_field(map_base = map_base, map_top = map_top)
  m2_non_spatial_re <- fit_inla2(sf_gambia,
                                 family = "binomial",
                                 response = c("zero_inflated_positives", "total"),
                                 covariates =  c("netuse"),
                                 flag_intercept = TRUE,
                                 flag_spatial_re = FALSE,
                                 flag_non_spatial_re = TRUE)
  m2_non_spatial_re %>% plot_inla_zero_simulations()
  m2_both_re <- fit_inla2(sf_gambia,
                          family = "binomial",
                          response = c("zero_inflated_positives", "total"),
                          covariates =  c("netuse"),
                          flag_intercept = TRUE,
                          flag_spatial_re = TRUE,
                          mesh = gambia_mesh,
                          prior_pc_range = c(1, .8),
                          prior_pc_sigma = c(2, .01),
                          flag_non_spatial_re = TRUE)
  m2_both_re %>% get_summary_hyperpar()
  m2_both_re %>% plot_inla_zero_simulations()
  m2_both_re %>% plot_spatial_field(map_base = map_base, map_top = map_top)

  m3 <- fit_inla2(sf_gambia,
                  family = "zib",
                  response = c("positives", "total"),
                  covariates =  c("netuse"),
                  flag_intercept = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = FALSE)
  m3 %>% plot_inla_zero_simulations()
  m3_spatial_re <- fit_inla2(sf_gambia,
                  family = "zib",
                  response = c("positives", "total"),
                  covariates =  c("netuse"),
                  flag_intercept = TRUE,
                  flag_spatial_re = TRUE,
                  mesh = gambia_mesh,
                  prior_pc_range = c(1, .8),
                  prior_pc_sigma = c(2, .01),
                  flag_non_spatial_re = FALSE)
  m3_spatial_re %>% plot_inla_zero_simulations()
  m3_spatial_re %>% plot_spatial_field(map_base = map_base, map_top = map_top)
  m3_non_spatial_re <- fit_inla2(sf_gambia,
                  family = "zib",
                  response = c("positives", "total"),
                  covariates =  c("netuse"),
                  flag_intercept = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = TRUE)
  m3_non_spatial_re %>% get_summary_hyperpar()
  m3_non_spatial_re %>% plot_inla_zero_simulations()
  m3_both_re <- fit_inla2(sf_gambia,
                          family = "zib",
                          response = c("positives", "total"),
                          covariates =  c("netuse"),
                          flag_intercept = TRUE,
                          mesh = gambia_mesh,
                          prior_pc_range = c(1, .8),
                          prior_pc_sigma = c(2, .01),
                          flag_spatial_re = TRUE,
                          flag_non_spatial_re = TRUE)
  m3_both_re %>% get_summary_hyperpar()
  m3_both_re %>% plot_inla_zero_simulations()
  m3_both_re %>% plot_spatial_field(map_base = map_base, map_top = map_top)

  m4 <- fit_inla2(sf_gambia,
                  family = "zib",
                  response = c("zero_inflated_positives", "total"),
                  covariates =  c("netuse"),
                  flag_intercept = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = FALSE)
  m4 %>% plot_inla_zero_simulations()
  m4_spatial_re <- fit_inla2(sf_gambia,
                  family = "zib",
                  response = c("zero_inflated_positives", "total"),
                  covariates =  c("netuse"),
                  flag_intercept = TRUE,
                  flag_spatial_re = TRUE,
                  mesh = gambia_mesh,
                  prior_pc_range = c(1, .8),
                  prior_pc_sigma = c(2, .01),
                  flag_non_spatial_re = FALSE)
  m4_spatial_re %>% plot_inla_zero_simulations()
  m4_spatial_re %>% plot_spatial_field(map_base = map_base, map_top = map_top)
  m4_non_spatial_re <- fit_inla2(sf_gambia,
                  family = "zib",
                  response = c("zero_inflated_positives", "total"),
                  covariates =  c("netuse"),
                  flag_intercept = TRUE,
                  flag_spatial_re = FALSE,
                  flag_non_spatial_re = TRUE)
  m4_non_spatial_re %>% plot_inla_zero_simulations()
  m4_both_re <- fit_inla2(sf_gambia,
                             family = "zib",
                             response = c("zero_inflated_positives", "total"),
                             covariates =  c("netuse"),
                             flag_intercept = TRUE,
                             flag_spatial_re = TRUE,
                             mesh = gambia_mesh,
                             prior_pc_range = c(1, .8),
                             prior_pc_sigma = c(2, .01),
                             flag_non_spatial_re = TRUE)
  m4_both_re %>% get_summary_hyperpar()
  m4_both_re %>% plot_inla_zero_simulations()
  m4_both_re %>% plot_spatial_field(map_base = map_base, map_top = map_top)
})

test_that("get_inla_ntrials works",{
  expect_identical( get_inla_ntrials(sf_gambia[1:60,],
                   response = c("positives", "total"),
                   family = "binomial",
                   flag_za_cov = FALSE,
                   data_prediction = NULL) %>%
    length(),
    60L)
  expect_identical(get_inla_ntrials(sf_gambia[1:60,],
                   response = c("positives", "total"),
                   family = "binomial",
                   flag_za_cov = FALSE,
                   data_prediction = sf_gambia[61:65,]) %>%
    length(),
    65L
  )
  expect_identical(
    get_inla_ntrials(
      sf_gambia[1:60, ],
      response = c("positives", "total"),
      family = "zab",
      flag_za_cov = TRUE,
      data_prediction = NULL
    ) %>%
      dim(),
    c(60L,2L)
  )
  expect_identical(
    get_inla_ntrials(
      sf_gambia[1:60, ],
      response = c("positives", "total"),
      family = "zab",
      flag_za_cov = TRUE,
      data_prediction = sf_gambia[61:65,]
    ) %>%
      dim(),
    c(65L,2L)
  )
})

test_that("get_inla_prediction_summary works",{
  skip("No test implemented")
  m1 <- fit_inla2(sf_gambia[1:60,],
            family = "binomial",
            response = c("positives", "total"),
            covariates =  c("netuse"),
            flag_intercept = TRUE,
            flag_spatial_re = FALSE,
            flag_non_spatial_re = FALSE,
            data_prediction = sf_gambia[61:65,],
            flag_keep_predictor_marginals = TRUE)
  get_inla_prediction_summary(m1)
  get_inla_prediction_summary(m1, hdpi_probabilities = c(.55,.95))
  m1$fit$summary.fitted.values$mean[61:65]
  inverse_logit(m1$fit$summary.linear.predictor$mean[61:65])
 get_inla_prediction_sample(m1, 5L)

 m2 <- fit_inla2(sf_gambia[1:60,],
                 family = "zib",
                 response = c("positives", "total"),
                 covariates =  c("netuse"),
                 flag_intercept = TRUE,
                 flag_spatial_re = FALSE,
                 flag_non_spatial_re = FALSE,
                 data_prediction = sf_gambia[61:65,],
                 flag_keep_predictor_marginals = TRUE)
 get_inla_prediction_summary(m2)
 get_inla_prediction_summary(m2, hdpi_probabilities = c(.55,.95))

 m1$fit$summary.fitted.values$mean[61:65]
 inverse_logit(m1$fit$summary.linear.predictor$mean[61:65])
 get_inla_prediction_sample(m2, 5L)
})

test_that("fit_inla2 (edge cases) works",{
  skip("No test implemented")
  i_survey <- 1L
  covariates <- readr::read_rds("../oversampling/data/ghana/bvs_inla_result.rds") %>%
    dplyr::arrange(dic) %>%
    dplyr::slice(1) %>%
    dplyr::pull(covariates) %>%
    stringr::str_split(", ") %>%
    unlist()
  covariates_quantile <- covariates[covariates %>%
                                      stringr::str_detect("_quantile$")] %>%
    stringr::str_replace("_quantile$", "")
  covariates_continuous <- covariates[covariates %>%
                                        stringr::str_detect("_quantile$",
                                                   negate = TRUE)]

  mesh <- readr::read_rds("../oversampling/data/ghana/mesh_district.rds")
  prior_pc_range  <-  c(50, .7) # P(r < r0 ) = p
  prior_pc_sigma  <-  c(4,.01)

  data <- readr::read_rds("../oversampling/data/auxillary-factors-problem-ss.rds")
  data_prediction <- data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(paste0(covariates_quantile,"_quantile")),
                  ~ .x %>%
                    as_quantile_factor_prediction(v_estimation = data %>%
                                                    dplyr::filter(.data[[paste0("valid_", i_survey)]] == "train") %>%
                                                    dplyr::pull(.x),probs = c(.33,.66)
                    )))
  data_estimation <- data %>%
    dplyr::filter(.data[[paste0("valid_", i_survey)]] == "train") %>%
    dplyr::mutate(gold_positives = round(gold_prevalence * gold_n_sac) %>% as.integer,
                  dplyr::across( dplyr::all_of(paste0(covariates_quantile,"_quantile")),
                  as_quantile_factor))
  data_estimation %>%
    fit_inla2(response = c("gold_positives", "gold_n_sac"),
              family = "binomial",
              covariates = c(paste0(covariates_quantile,"_quantile"), covariates_continuous),
              flag_intercept = TRUE,
              mesh = mesh,
              flag_spatial_re = TRUE,
              flag_non_spatial_re = TRUE,
              prior_pc_range = prior_pc_range,
              prior_pc_sigma = prior_pc_sigma,
              data_prediction = data_prediction[1:100,])
  # inla_output$fit$marginals.hyperpar[["Precision for non_spatial_index"]] gives infinite values
  data_estimation %>%
    fit_inla2(response = c("gold_positives", "gold_n_sac"),
              family = "binomial",
              covariates = c(paste0(covariates_quantile,"_quantile"), covariates_continuous),
              flag_intercept = TRUE,
              mesh = mesh,
              flag_spatial_re = TRUE,
              flag_non_spatial_re = TRUE,
              prior_pc_range = prior_pc_range,
              prior_pc_sigma = prior_pc_sigma
    )
  # same problem does not occur
  data_estimation %>%
    fit_inla2(response = c("gold_positives", "gold_n_sac"),
              family = "binomial",
              covariates = c(paste0(covariates_quantile,"_quantile"), covariates_continuous),
              flag_intercept = TRUE,
              mesh = mesh,
              flag_spatial_re = TRUE,
              flag_non_spatial_re = TRUE,
              prior_pc_range = prior_pc_range,
              prior_pc_sigma = prior_pc_sigma,
              prior_iid = c(2.01, 1.01),
              data_prediction = data_prediction[1:100,]
    )
})



#
# test_that("zuur_spatial_chp19 works",{
#   dd <- read.table(file = "J:/BS/SSM/daniel.gerber/Switchdrive/Literature/Books/Zuur, Ieno, Saveliev - Spatial, Termporal and Spatial-Temporal Ecological Data Analysis with R-INLA/Volume 2/Skate2.txt",
#                    header = TRUE,
#                    dec = ".")
#   dd <- dd %>%
#     dplyr::mutate(fYear = factor(Year),
#                   fMonth = factor(Month),
#                   fBottomType = factor(BottomType,
#                                        levels = 1:4,
#                                        labels = c("Mud", "SanMud", "Sand", "Rest")),
#                   Xkm = X/1000,
#                   Ykm = Y/1000,
#                   dplyr::across(c(Temperature, SweptArea),
#                                 ~ (.x - mean(.x))/sd(.x),
#                                 .names = "{.col}.std"))
#   Poi <- INLA::inla(SB ~ fYear + Temperature.std + SweptArea.std + fBottomType,
#               family = "poisson",
#               control.compute = list(dic = TRUE,
#                                      waic = TRUE),
#               data = dd)
#   covariates <- c("fYear","Temperature", "SweptArea", "fBottomType")
#   myPoi <- fit_inla2(dd,
#                      family = "poisson",
#                      response = c("SB"),
#                      covariates = covariates,
#                      covariates01 = NULL,
#                      flag_intercept = TRUE,
#                      flag_intercept01 = FALSE,
#                      flag_spatial_re = FALSE,
#                      flag_non_spatial_re = FALSE,
#                      mesh = NULL)
#   expect_true(all(abs(Poi$summary.fixed$mean -
#               myPoi$fit$summary.fixed$mean[c(9,3:5,1:2,6:8)]) < 0.00001))
#   expect_true(all(abs(Poi$summary.fixed$sd -
#                         myPoi$fit$summary.fixed$sd[c(9,3:5,1:2,6:8)]) < 0.00001))
#
#   Loc <- as.matrix(dd[,c("X", "Y")])
#   RangeGuess <- 75 * 1000
#   MaxEdge <- RangeGuess/5
#   ConvHull <- INLA::inla.nonconvex.hull(Loc, convex = 50*1000)
#   mesh1 <- INLA::inla.mesh.2d(loc = Loc,
#                               boundary = ConvHull,
#                               max.edge = c(1,5) * MaxEdge,
#                               cutoff = MaxEdge/5)
#   spde1 <- INLA::inla.spde2.pcmatern(mesh1,
#                                prior.range = c(50*1000, 0.01),
#                                prior.sigma = c(1.5, 0.01))
#   w1.index <- INLA::inla.spde.make.index(name = 'w',
#                                    n.spde = spde1$n.spde)
#   A1 <- INLA::inla.spde.make.A(mesh1, loc = Loc)
#   Xm <- model.matrix(~fYear + Temp.std + SweptArea.std + fBottomType,
#                      data = Skate)
#   N <- nrow(Skate)
#   X <- data.frame(fYear1999         = Xm[, 2],
#                   fYear2003         = Xm[, 3],
#                   fYear2005         = Xm[, 4],
#                   Temp.std          = Xm[, 5],
#                   SweptArea.std     = Xm[, 6],
#                   fBottomTypeSanMud = Xm[, 7],
#                   fBottomTypeSand   = Xm[, 8],
#                   fBottomTypeRest   = Xm[, 9])
#   Stack.mesh1 <- INLA::inla.stack(
#     tag  = "Fit",
#     data = list(y = Skate$SB),
#     A    = list(1, 1, A1),
#     effects = list(
#       Intercept  = rep(1, N),
#       X  = as.data.frame(X),
#       w = w1.index))
#   fPois.mesh1 <- y ~ -1 + Intercept + fYear1999 + fYear2003 + fYear2005 +
#     Temp.std + SweptArea.std +
#     fBottomTypeSanMud + fBottomTypeSand + fBottomTypeRest + f(w, model = spde1)
#   Pois.mesh1 <- INLA::inla(fPois.mesh1,
#                      family = "poisson",
#                      data = INLA::inla.stack.data(Stack.mesh1),
#                      control.compute = list(dic = TRUE, waic = TRUE),
#                      control.predictor = list(A = INLA::inla.stack.A(Stack.mesh1)))
#   Pois.mesh1 %>% summary()
#   myPois.mesh1 <- fit_inla2(dd %>%
#                               sf::st_as_sf(coords = c("X", "Y")),
#                      family = "poisson",
#                      response = c("SB"),
#                      covariates = covariates,
#                      covariates01 = NULL,
#                      flag_intercept = TRUE,
#                      flag_intercept01 = FALSE,
#                      flag_spatial_re = TRUE,
#                      flag_non_spatial_re = FALSE,
#                      prior_pc_range = c(50*1000, 0.01),
#                      prior_pc_sigma = c(1.5, 0.01),
#                      mesh = mesh1)
#   myPois.mesh1
#   myPois.mesh1 %>% get_summary_fixed()
#   myPois.mesh1 %>% get_summary_hyperpar()
#   expect_true(all(abs(Pois.mesh1$summary.fixed$mean -
#                         myPois.mesh1$fit$summary.fixed$mean[c(9,3:5,1:2,6:8)]) < 0.4))
#   expect_true(all(abs(Pois.mesh1$summary.fixed$sd -
#                         myPois.mesh1$fit$summary.fixed$sd[c(9,3:5,1:2,6:8)]) < 0.05))
#
# })

test_that("reprex works", {
  skip("https://haakonbakkagit.github.io/btopic113.html")
  set.seed(2017)
  df = data.frame(y = INLA::Seeds$r, Ntrials = INLA::Seeds$n, INLA::Seeds[, 3:5])
  holdout = c(7, 12)
  # - alternatively: sort(sample(1:nrow(df), 2))
  df.holdout = df
  df.holdout$y[holdout] = NA
  hyper1 = list(theta = list(prior="pc.prec", param=c(1,0.01)))
  formula1 = y ~ x1 + x2 + f(plate, model="iid", hyper=hyper1)
  family1 = "binomial"
  control.family1 = list(control.link=list(model="logit"))
  res1 = INLA::inla(formula=formula1, data=df.holdout,
              family=family1, Ntrials=Ntrials,
              control.family=control.family1,
              control.predictor=list(compute=T),
              control.compute=list(config=T))
  summary(res1)
  fit_inla2(df.holdout %>%
              tibble::as_tibble() %>%
              dplyr::mutate(across(c(y,Ntrials), as.integer)),
            response = c("y", "Ntrials"),
            covariates = c("x1", "x2"),
            flag_intercept = TRUE,
            flag_non_spatial_re = TRUE)


})
