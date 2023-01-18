#' Create map raster (version 3)
#' @description `r lifecycle::badge("experimental")` Creates a map with a raster layer for coloring the surface. May include points on top of it.
#'
#' @param data_raster The raster either as `sf` POINTS or as `SpatRaster` from `terra`
#' @param var_name The `col` argument of `tmap::tm_raster`
#' @param map_base A base map constructed with a `create_map_base*`-function
#' @param map_top A top map constructed with a `create_map_top*`-function
#' @param waterbodies_filename Waterbodies filename
#' @param tm_raster Arguments to `tmap::tm_raster` without the `col` argument which is given by `var_name`
#' @param tm_dots Arguments to `tmap::tm_dots` to control appearance of dots
#' @param tm_layout Arguments to `tmap::tm_layout` to control tmap layout.
#' @param data_points The data points as `sf` POINTS or NULL
#'
#' @return A `tmap`object
#' @export
#' @example \dontrun{
#'  create_map_raster2(data_prediction = gambia_prediction_grid,
#'  var_name = "estimate",
#'  data_estimation = sf_gambia,
#'  map_base = create_map_base2("GMB", "SEN"),
#'  map_top = map_top,
#'  theme = "Blues",
#'  legend.title = "prevalence")
#'  create_map_raster2(data_prediction = gambia_prediction_grid,
#'                     var_name = "estimate",
#'                     data_estimation = sf_gambia,
#'                     map_base = create_map_base2("GMB", "SEN"),
#'                     map_top = map_top,
#'                     waterbodies_filename = "data/waterbodies_gambia.tif")
#'  create_map_raster2(data_prediction = altitude_sf,
#'                     var_name = "altitude",
#'                     data_estimation = sf_gambia,
#'                     map_base = create_map_base2("GMB", "SEN"),
#'                     map_top = map_top,
#'                     waterbodies_filename = "data/waterbodies_gambia.tif")
#'  create_map_raster2(data_prediction = altitude_SpatRast,
#'                     var_name = "altitude",
#'                     data_estimation = sf_gambia,
#'                     map_base = create_map_base2("GMB", "SEN"),
#'                     map_top = map_top,
#'                     waterbodies_filename = "data/waterbodies_gambia.tif")
#'  create_map_raster3(data_raster = altitude_SpatRast,
#'                     var_name = "altitude",
#'                     data_points = sf_gambia,
#'                     map_base = create_map_base2("GMB", "SEN"),
#'                     map_top = map_top,
#'                     tm_dots = list(size = 1),
#'                     waterbodies_filename = "data/waterbodies_gambia.tif")
#' }
create_map_raster3 <- function(data_raster,
                               var_name,
                               map_base,
                               map_top,
                               data_points = NULL,
                               waterbodies_filename = NA_character_,
                               tm_raster = list(title = "Legend title",
                                                palette = "Oranges"),
                               tm_dots = list(),
                               tm_layout = list(
                                 legend.position =  c("right", "bottom"),
                                 legend.bg.color = "white",
                                 legend.frame = TRUE,
                                 title.bg.color = "white")) {
  if(!is.null(data_points)){
    is_sf_data(data_points)
  }
  stopifnot(inherits(map_base, "tmap"),
            inherits(map_top, "tmap"),
            purrr::is_scalar_character(var_name),
            purrr::is_scalar_character(waterbodies_filename))
  if(!var_name %in% names(data_raster)){
    stop("`var_name` must be in the `data_prediction`.",
         call. = FALSE)
  }

  if(inherits(data_raster, "sf")){
    bbox <- data_raster %>% sf::st_bbox()
    n <- nrow(data_raster)
    ratio <- (bbox["xmax"] - bbox["xmin"])/(bbox["ymax"] - bbox["ymin"])
    b <- sqrt(n/ratio)
    a <- n/b
    data_raster <- stars::st_rasterize(
      data_raster,
      bbox %>%
        stars::st_as_stars(nx = round(a),
                           ny = round(b),
                           values = NA_real_))
  }else if(inherits(data_raster, "SpatRaster")){
    data_raster <- data_raster %>%
      methods::as("Raster")
  }else{
    stop("In `create_map_raster3`, `data_prediction`
         must be of class `sf` or `SpatRaster`.",
         call. = FALSE)
  }

  tm_raster_layer <- tmap::tm_shape(data_raster) +
    do.call(tmap::tm_raster, c(list(col = var_name), tm_raster))
  map <- map_base +
    tm_raster_layer +
    map_base
  if(!is.na(waterbodies_filename)){
    if(!file.exists(waterbodies_filename)){
      stop("File `waterbodies_filename` in call of `create_map_raster2`
         does not exist.",
           call. = FALSE)
    }else{
      map <- map +
        tmap::tm_shape(terra::rast(waterbodies_filename))+
        tmap::tm_raster(palette = "#bee8ff",
                        legend.show = FALSE)
    }
  }
  map <- map +
    map_top
  if(!is.null(data_points)){
    map <- map +
      tmap::tm_shape(data_points) +
      do.call(tmap::tm_dots, tm_dots)
  }
  map +
    do.call(tmap::tm_layout, tm_layout)
}




