test_that("download_gadm_sf works", {
  # expect_error(
  #   download_gadm_sf("BGD"),
  #   NA
  # )
})

test_that("create_map_base works", {
  # expect_error(
  #   create_map_base("CHE", c("DEU", "ITA", "AUT", "FRA", "LIE")),
  #   NA
  # )
  # expect_error(
  #   create_map_base("CHE", c("DEU", "ITA", "AUT", "FRA", "LIE"),
  #                   crs = "+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=283.7,735.9,261.1,0,0,0,0 +units=km +no_defs"),
  #   NA
  # )
})


test_that("create_map_top works", {
  # expect_error(
  #   create_map_top("CHE"),
  #   NA
  # )
  # expect_error(
  #   create_map_top("CHE",
  #                   crs = "+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=283.7,735.9,261.1,0,0,0,0 +units=km +no_defs"),
  #   NA
  # )
})

test_that("create_map_raster works", {
  # map_base <- create_map_base2("GMB", "SEN")
  # map_base <- create_map_base("GMB", "SEN")

  # create_map_raster(sf_gambia, gambia_prediction_grid, "estimate", map_base, map_top,theme_who, title = "prevalence" )
  # create_map_raster2(data_prediction = gambia_prediction_grid,
  #                   var_name = "estimate",
  #                   map_base = map_base,
  #                   map_top = map_top,
  #                   data_estimation = sf_gambia,
  #                   theme = theme_who)
  # create_map_raster2(data_prediction = gambia_prediction_grid,
  #                    var_name = "estimate",
  #                    data_estimation = sf_gambia,
  #                    map_base = create_map_base2("GMB", "SEN"),
  #                    map_top = map_top,
  #                    theme = "Blues",
  #                    legend.title = "prevalence")
  # create_map_raster2(data_prediction = gambia_prediction_grid,
  #                    var_name = "estimate",
  #                    data_estimation = sf_gambia,
  #                    map_base = create_map_base2("GMB", "SEN"),
  #                    map_top = map_top,
  #                    waterbodies_filename = "data/waterbodies_gambia.tif")
  # create_map_raster2(data_prediction = altitude_sf,
  #                    var_name = "altitude",
  #                    data_estimation = sf_gambia,
  #                    map_base = create_map_base2("GMB", "SEN"),
  #                    map_top = map_top,
  #                    waterbodies_filename = "data/waterbodies_gambia.tif")
  # create_map_raster2(data_prediction = altitude_SpatRast,
  #                    var_name = "altitude",
  #                    data_estimation = sf_gambia,
  #                    map_base = create_map_base2("GMB", "SEN"),
  #                    map_top = map_top,
  #                    waterbodies_filename = "data/waterbodies_gambia.tif")
  # create_map_raster3(data_raster = altitude_SpatRast,
  #                    var_name = "altitude",
  #                    data_points = sf_gambia,
  #                    map_base = create_map_base2("GMB", "SEN"),
  #                    map_top = map_top,
  #                    tm_dots = list(size = 1),
  #                    waterbodies_filename = "data/waterbodies_gambia.tif")

})
