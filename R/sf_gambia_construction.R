# rm(list = ls())
# library(tidyverse)
# library(geoR)
# library(sf)
# set.seed(1234)
# data(gambia)
# gambia <- gambia %>%
#   group_by(x, y) %>%
#   summarize(total = n(),
#             positives = sum(pos) %>% as.integer(),
#             prevalence = positives/total,
#             age = mean(age),
#             netuse= mean(netuse),
#             treated= mean(treated),
#             green = mean(green),
#             phc = mean(phc)) %>%
#   ungroup()
# gambia <- gambia %>%
#   st_as_sf(coords = c("x", "y"), crs = st_crs("+proj=utm +zone=28")) %>%
#   st_transform(st_crs("+proj=longlat +datum=WGS84"))
# altitude <- raster::getData(name = 'alt', country = 'GMB', mask = TRUE)
# sf_gambia <- gambia %>%
#   mutate(altitude = raster::extract(altitude, gambia %>% st_coordinates())) %>%
#   select(positives, total, prevalence, altitude, everything())
# sf_gambia <- sf_gambia %>%
#   dplyr::mutate(count_low = round(prevalence * 30, 0) %>% as.integer(),
#                 count_high = round(prevalence * 200, 0) %>% as.integer(),
#                 zero_inflated_positives = positives,
#                 zero_inflated_count_low = count_low,
#                 zero_inflated_count_high = count_high)
# sf_gambia$zero_inflated_positives[which(rbinom(nrow(sf_gambia), 1, prob = 0.5) == 1)] <- 0L
# sf_gambia$zero_inflated_count_low[which(rbinom(nrow(sf_gambia), 1, prob = 0.5) == 1)] <- 0L
# sf_gambia$zero_inflated_count_high[which(rbinom(nrow(sf_gambia), 1, prob = 0.5) == 1)] <- 0L
# usethis::use_data(sf_gambia, overwrite = TRUE)
#
# gambia_mesh <- Helper::build_mesh2(sf_gambia, 30/111, country_id = "GMB")
# plot(gambia_mesh)
# gambia_mesh$n
# points(sf_gambia %>% st_coordinates(), col = "red")
# usethis::use_data(gambia_mesh, overwrite = TRUE)
# fit <- sf_gambia[1:60,] %>%
#   Helper::fit_inla2(response = c("positives", "total"),
#             covariates = "altitude",
#             flag_intercept = TRUE,
#             flag_non_spatial_re = TRUE,
#             flag_spatial_re = TRUE,
#             mesh = gambia_mesh,
#             data_prediction = sf_gambia[61:65,],
#             flag_keep_predictor_marginals = TRUE)
# fit %>% plot_spatial_field()
# fit
# gambia_prediction_points <- fit %>%
#   get_inla_prediction_summary(hdpi_probabilities = c(.5,.7),
#                               flag_add_predictors = TRUE)
# usethis::use_data(gambia_prediction_points, overwrite = TRUE)
# raster::writeRaster(altitude, "data/altitude.tiff")
#
# altitude_sf <- stars::read_stars("data/altitude.tiff") %>%
#   sf::st_as_sf(as_points = TRUE, merge = FALSE) %>%
#   dplyr::rename(altitude = altitude.tiff)
# usethis::use_data(altitude_sf)
# altitude_SpatRast <- terra::rast("data/altitude.tiff") %>%
#   terra::wrap() %>%
#   terra::rast()
# usethis::use_data(altitude_SpatRast) #THIS CAUSES TROUBLE WHEN BUILDING THE PACKAGE
# file.remove("data/altitude.tiff")
# file.remove("GMB_msk_alt.grd")
# file.remove("GMB_msk_alt.gri")
# file.remove("GMB_msk_alt.vrt")
#

#
# fit2 <- sf_gambia %>%
#   fit_inla2(response = c("positives", "total"),
#             covariates = "altitude",
#             flag_intercept = TRUE,
#             flag_non_spatial_re = TRUE,
#             flag_spatial_re = TRUE,
#             mesh = mesh,
#             data_prediction = altitude_sf)
# gambia_prediction_grid <- fit2 %>%
#   get_inla_prediction_summary(flag_add_predictors = TRUE)
# gambia_prediction_grid[,1] %>% plot()
# usethis::use_data(gambia_prediction_grid, overwrite = TRUE)

# create_map_base("GMB", "SEN")
# wb <- terra::rast("../africa/waterbodies/occurrence_20W_20Nv1_3_2020.tif")
# wb %>% terra::plot()
# wb_gmb <- wb %>%
#   terra::crop(read_rds("../africa/data/level_0/gmb.rds") %>%
#                 terra::ext())
# wb_gmb %>% terra::plot()
# wb_gmb[wb_gmb <= 50] <- NA_real_
# wb_gmb[wb_gmb > 50] <- 1
# wb_gmb %>%
#   terra::writeRaster("data/waterbodies_gambia.tif", overwrite = TRUE)
