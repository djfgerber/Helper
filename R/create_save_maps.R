#' Download GADM map
#' @description `r lifecycle::badge('stable')`
#' @param country_ids Country ids to download from GADM, as 3 letter cap.
#' @param maps_dir Destination base directory, must have subdirectories level0, level1, level2.
download_gadm_sf <- function(country_ids, maps_dir = "../africa/Data/"){
  stopifnot(is.character(country_ids),
            length(country_ids) > 0,
            dir.exists(maps_dir))
  for(country_id in country_ids){
    country_id_lower <- country_id %>% tolower()
    for(level in 0:4){
      dest_file <- paste0(maps_dir,"level_",level,"/", country_id_lower, ".rds")
      if(!file.exists(dest_file)){
        download_try <- try({
          GADMTools::gadm_sf_loadCountries(fileNames = country_id,
                                           level = level,
                                           basefile = dest_file)
        })
        if(class(download_try) == "try-error"){
          cat("country_id:", country_id, " level:", level, "failed.\n")
        }else{
          file.rename(from = paste0(dest_file, country_id, "_adm", level, ".sf.rds"),
                      to = dest_file)
        }
      }
    }
  }
}

#' Create map base
#' @description `r lifecycle::badge('stable')`
#'
#' @param country_id Country id (3 cap letters) of country to plot
#' @param neighbours_id Country id (3 cap letters) of country_id's neighbours
#' @param maps_dir Directory name of the GADM maps
#' @param crs A projection to which the default longlat should be transformed. Default is no transformation.
#'
#' @return tmap
#' @export
#'
#' @examples
#' \dontrun{
#' create_map_base("CHE", c("DEU", "ITA", "AUT", "FRA", "LIE"))
#' }
create_map_base <-
  function(country_id,
           neighbours_id,
           crs = "+proj=longlat +datum=WGS84 +no_defs",
           maps_dir =  "../africa/Data/",
           alpha = 1) {
    stopifnot(is.character(country_id),
              length(country_id) > 0,
              is.character(neighbours_id),
              dir.exists(maps_dir))
    download_gadm_sf(union(country_id, neighbours_id), maps_dir)
    country_level0 <- paste0(maps_dir, "level_0/", tolower(country_id), ".rds") %>%
      readr::read_rds() %>%
      sf::st_transform(crs = crs)
    map_base <- tmap::tm_shape(country_level0) +
      tmap::tm_layout(bg.color = "lightblue")+
      tmap::tm_fill(col = "white")
    for (i in seq_along(neighbours_id)) {
      neighbour_level0 <- paste0(maps_dir,
                                 "level_0/",
                                 tolower(neighbours_id[i]),
                                 ".rds") %>%
        readr::read_rds() %>%
        sf::st_transform(crs = crs)
      map_base <- map_base +
        tmap::tm_shape(neighbour_level0) +
        tmap::tm_fill()
    }
    map_base
  }

#' Create map base layer
#'
#' @description `r lifecycle::badge('stable')` This function creates the bounding box as well as the background fill
#'  of a map, in particular, neighboring countries are grayed, the country
#'  is left blank and the rest is water colored.
#'
#' @describeIn create_map_base
#' @return tmap object
#' @export
#'
#' @examples
#' \dontrun{
#' create_map_base2("CHE", c("DEU", "ITA", "AUT", "FRA", "LIE"))
#' }
create_map_base2 <-
  function(country_id,
           neighbours_id,
           crs = "+proj=longlat +datum=WGS84 +no_defs",
           maps_dir =  "../africa/Data/") {
    stopifnot(is.character(country_id),
              length(country_id) > 0,
              is.character(neighbours_id),
              dir.exists(maps_dir))
    download_gadm_sf(union(country_id, neighbours_id), maps_dir)
    country_level0 <- paste0(maps_dir, "level_0/", tolower(country_id), ".rds") %>%
      readr::read_rds() %>%
      sf::st_transform(crs = crs)
    box <- country_level0 %>%
      sf::st_bbox()
    max_dist <- max(box["xmax"]-box["xmin"], box["ymax"]-box["ymin"])
    centroid <- c(mean(box[c("xmax","xmin")]),
                  mean(box[c("ymax","ymin")]))
    box["xmin"] <- centroid[1] - max_dist
    box["ymin"] <- centroid[2] - max_dist
    box["xmax"] <- centroid[1] + max_dist
    box["ymax"] <- centroid[2] + max_dist
    bounding_polygon <- box %>%
      sf::st_as_sfc()
    all_neighbours <- neighbours_id %>%
      purrr::map(~ paste0(maps_dir,
                          "level_0/",
                          tolower(.x),
                          ".rds") %>%
                   readr::read_rds() %>%
                   sf::st_transform(crs = crs)) %>%
      purrr::reduce(sf::st_union)
    outside_water <- sf::st_difference(bounding_polygon,
                                   all_neighbours %>%
                                     sf::st_union(country_level0) %>%
                                     sf::st_make_valid())
    map_base <- tmap::tm_shape(country_level0)+
      tmap::tm_fill(alpha = 0) +
      tmap::tm_shape(all_neighbours) +
      tmap::tm_fill(col = "lightgrey")+
      tmap::tm_shape(outside_water)+
      tmap::tm_fill(col = "#bee8ff")

    map_base
  }

#' Create and save map base
#' @description `r lifecycle::badge('stable')`
#' @inheritParams create_map_base
#' @param dest_dir Directory where to save the map.
#' @param dest_file Filename where to save the map, without extension.
#' @export
#'
#' @examples
save_map_base <-
  function(country_id,
           neighbours_id,
           crs = "+proj=longlat +datum=WGS84 +no_defs",
           maps_dir = "../africa/Data/",
           dest_dir = "Data/",
           dest_file = "map_base") {
    stopifnot(is.character(country_id),
            length(country_id) > 0,
            is.character(neighbours_id),
            dir.exists(maps_dir),
            dir.exists(dest_dir))
  map_base <-
    create_map_base(
      country_id = country_id,
      neighbours_id = neighbours_id,
      crs = crs,
      maps_dir = maps_dir
    )
  map_base %>%
    readr::write_rds(paste0(dest_dir, dest_file, ".rds"))
}

#' Create the map top of tmap package
#' @description `r lifecycle::badge('stable')`
#' @inheritParams create_map_base
#' @param position_compass Where to draw the compass
#' @param position_scale_bar Where to draw the scale_bar
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' create_map_top("CHE")
#' }
create_map_top <-
  function(country_id,
           position_compass =  c("right", "top"),
           position_scale_bar = c("left", "bottom"),
           crs = "+proj=longlat +datum=WGS84 +no_defs",
           maps_dir =  "../africa/Data/") {
    download_gadm_sf(country_id, maps_dir)
    country_level0 <-
      readr::read_rds(paste0(maps_dir, "level_0/", tolower(country_id), ".rds")) %>%
      sf::st_transform(crs = crs)
    country_level1 <-
      readr::read_rds(paste0(maps_dir, "level_1/", tolower(country_id), ".rds")) %>%
      sf::st_transform(crs = crs)
    country_level2 <-
      readr::read_rds(paste0(maps_dir, "level_2/", tolower(country_id), ".rds")) %>%
      sf::st_transform(crs = crs)

    tmap::tm_shape(country_level1) +
      tmap::tm_borders(alpha = .4) +
      tmap::tm_shape(country_level2) +
      tmap::tm_borders(alpha= .2) +
      tmap::tm_shape(country_level0) +
      tmap::tm_borders()+
      tmap::tm_compass(type = "8star", position = position_compass)+
      tmap::tm_scale_bar(text.size = 0.5, position = position_scale_bar)
  }

#' Create and save map top
#'
#' @inheritParams create_map_top
#' @param dest_dir Where to save the map.
#' @param dest_file Filename where to save the map, without extension.
#' @export
#'
#' @examples
save_map_top <-
  function(country_id,
           position_compass =  c("right", "top"),
           position_scale_bar = c("left", "bottom"),
           crs = "+proj=longlat +datum=WGS84 +no_defs",
           maps_dir =  "../africa/Data/",
           dest_dir = "Data/",
           dest_file = "map_top") {
    map_top <- create_map_top(country_id = country_id,
                              position_compass = position_compass,
                              position_scale_bar = position_scale_bar,
                              crs = crs,
                              maps_dir = maps_dir)
    map_top %>%
      readr::write_rds(paste0(dest_dir, dest_file, ".rds"))
  }


#' Save a map of the covariate `cov_name`
#' @description Saves map to Doc/figures/covariate_maps/ and creates this map if it does not exist
#'
#' @param cov_name Name of the covariate which should be mapped
#' @param breaks The number of breaks (optional), to be customized for factors
#' @param labels The labels (length(labels) = length(breaks)+1) (optional), to be customized for factors
#' @param palette The palette as in `tm_raster`
#' @param map_base The base map is the filling of the map if nothing else (setup of the bounding box)
#' @param map_top The top map is used to add additional information, e.g. admin boundaries, windrose
#' @param dest_dir The directory where it should be saved to.
#' @param data_estimation Data for estimation
#' @param data_prediction Data for prediction
#'
#' @export
map_covariate <- function(cov_name,
                          data_estimation,
                          data_prediction,
                          breaks = NULL,
                          labels = NULL,
                          palette = NULL,
                          map_base,
                          map_top,
                          dest_dir = "Doc/figures/covariate_maps/") {
  data_prediction_raster <- data_prediction %>%
    dplyr::select(dplyr::all_of(cov_name)) %>%
    stars::st_rasterize()
  cov_map <-
    map_base +
    tmap::tm_shape(data_prediction_raster) +
    tmap::tm_raster(cov_name,
                    breaks = breaks,
                    labels = labels,
                    palette = palette) +
    tmap::tm_shape(data_estimation) +
    tmap::tm_dots() +
    map_top
  if(!dir.exists(dest_dir))
    dir.create(dest_dir, recursive = TRUE)
  tmap::tmap_save(cov_map,
                  filename = paste0(dest_dir, cov_name, ".png"))
}


#' Vectorized map_covariate
#' @description Is a vectorized version of map_covariate, which should only
#' attempt to save a map if the cov_name is in the dd_pred data set
#' @inheritParams map_covariate
#' @param var_names the variable names to try.
#' @export
#'
#' @examples
#' \dontrun{
#' c(paste0("bio", c(1:11)), "lstd", "lstn") %>%
#'  map_covariate_if_any(dd_est, dd_pred, palette = "YlOrRd")
#' "aez" %>%
#'  map_covariate_if_any(dd_est, dd_pred, 0.5 + c(0:4), letters[1:4], palette = "div")
#' }
map_covariate_if_any <-
  function(var_names,
           data_estimation,
           data_prediction,
           breaks = NULL,
           labels = NULL,
           palette = NULL,
           map_base,
           map_top,
           dest_dir = "Doc/figures/covariate_maps/") {
    if(any(var_names %in% colnames(data_prediction))){
      intersect(var_names, colnames(data_prediction)) %>%
        purrr::map(~ map_covariate(.x,
                                   data_estimation, data_prediction,
                                   breaks, labels, palette,
                                   map_base, map_top, dest_dir))
    }
  }



#' Plot the observed prevalence on a map
#'
#' @param var_name Variable name of the prevalence to map
#' @param map_base Mantel base `tmap`
#' @param map_top Mantel top `tmap`
#' @param theme A list with slots $palette, $breaks, $labels
#' @param title A title for the legend
#' @param data A sf data object
#' @param layout Layout options, see `?tmap::tm_layout`, usefull: `title`, `legend.position.`
#' @param waterbodies_filename
#'
#' @return tmap object
#' @export
#'
#' @examples
#' \dontrun{
#' sf_gambia %>%
#'   create_map_points("prevalence", map_base, map_top, theme_who, "prevalence")
#' }
create_map_points <- function(data,
                               var_name,
                               map_base,
                               map_top,
                               theme,
                               title,
                               layout = list(legend.position = c("right", "bottom"),
                                             legend.bg.color = "white",
                                             legend.frame = TRUE,
                                             title.bg.color = "white"),
                              waterbodies_filename = NA_character_) {
  stopifnot(purrr::is_scalar_character(var_name))
  stopifnot(purrr::is_scalar_character(title))
  stopifnot(inherits(theme, "list") & length(theme) == 3 |
              purrr::is_scalar_character(theme))
  is_sf_data(data)
  stopifnot(
    var_name %in% colnames(data),
    inherits(map_base, "tmap"),
    inherits(map_top, "tmap")
  )
  if(!is.na(waterbodies_filename) & !file.exists(waterbodies_filename)){
    stop("File `waterbodies_filename` in call of `create_map_raster2` does not exist.",
         call. = FALSE)
  }

  tm_point_layer <- if(length(theme) == 3){
    tmap::tm_shape(data) +
      tmap::tm_dots(
        col = var_name,
        size = 0.3,
        alpha = 0.8,
        palette = theme$palette,
        breaks = theme$breaks,
        labels = theme$labels,
        title = title
      )
  }else if(length(theme) == 1 & is.character(theme)){
    tmap::tm_shape(data) +
      tmap::tm_dots(
        col = var_name,
        size = 0.3,
        alpha = 0.8,
        palette = theme,
        title = title
      )
  }else{
    stop("Provide valid theme in `create_map_points.", call. = FALSE)
  }

  map <- map_base

  if(!is.na(waterbodies_filename)){
    map <- map +
      tmap::tm_shape(terra::rast(waterbodies_filename))+
      tmap::tm_raster(palette = "#bee8ff",
                      legend.show = FALSE)
  }
  map +
    tm_point_layer +
    do.call(tmap::tm_layout, layout) +
    map_top
}

#' Create and save map points
#'
#' @inheritParams create_map_points
#' @param dest_dir Where to save the map incl trailing backslash.
#' @param file_name Filename to save without extension.
#' @export
save_map_points <- function(data,
                            var_name,
                            map_base,
                            map_top,
                            theme,
                            title,
                            layout = list(legend.position = c("right", "bottom"),
                                          legend.bg.color = "white",
                                          legend.frame = TRUE,
                                          title.bg.color = "white"),
                            dest_dir = "Doc/figures/",
                            file_name = "map_points") {
  map_points <- create_map_points(
    data = data,
    var_name = var_name,
    map_base = map_base,
    map_top = map_top,
    theme = theme,
    title = title,
    layout = layout
  )
  if(!dir.exists(dest_dir))
    dir.create(dest_dir, recursive = TRUE)
  tmap::tmap_save(map_points,
                  filename = paste0(dest_dir, file_name, ".png"))
}


#' Create map raster
#' `r lifecycle::badge("superseded")` by create_map_raster(higher number)
#' @describeIn save_map_raster
#' @return
#' @export
#'
#' @examples
create_map_raster <- function(data_estimation,
                              data_prediction,
                              var_name,
                              map_base,
                              map_top,
                              theme,
                              title,
                              legend.position = c("right", "bottom")) {
  is_sf_data(data_estimation)
  is_sf_data(data_prediction)
  is_covariates(data_prediction, var_name)
  stopifnot(inherits(map_base, "tmap"),
            inherits(map_top, "tmap"),
            purrr::is_scalar_character(title))
  map_base +
    tmap::tm_shape(stars::st_rasterize(data_prediction)) +
    tmap::tm_raster(
      var_name,
      breaks = theme$breaks,
      labels = theme$labels,
      palette = theme$palette,
      title = title
    ) +
    tmap::tm_shape(data_estimation) +
    tmap::tm_dots() +
    tmap::tm_layout(
      legend.position = legend.position,
      legend.bg.color = "white",
      legend.frame = TRUE,
      title.bg.color = "white"
    ) +
    map_top
}

#' Title
#' `r lifecycle::badge("superseded")`
#'
#' @param data_prediction
#' @param var_name
#' @param map_base
#' @param map_top
#' @param data_estimation
#' @param waterbodies_filename
#' @param theme
#' @param legend.title
#' @param legend.position
#'
#' @return
#' @export
create_map_raster2 <- function(data_prediction,
                              var_name,
                              map_base,
                              map_top,
                              data_estimation = NULL,
                              waterbodies_filename = NA_character_,
                              theme = "Oranges",
                              legend.title = "Legend title",
                              legend.position = c("right", "bottom")) {
  if(!is.null(data_estimation)){
    is_sf_data(data_estimation)
  }
  stopifnot(inherits(map_base, "tmap"),
            inherits(map_top, "tmap"),
            purrr::is_scalar_character(legend.title),
            purrr::is_scalar_character(var_name),
            purrr::is_scalar_character(waterbodies_filename),
            is.character(legend.position) & length(legend.position) == 2
  )
  if(!var_name %in% names(data_prediction)){
    stop("`var_name` must be in the `data_prediction`.",
         call. = FALSE)
  }

  if(!is.na(waterbodies_filename) & !file.exists(waterbodies_filename)){
    stop("File `waterbodies_filename` in call of `create_map_raster2`
         does not exist.",
         call. = FALSE)
  }

  if(inherits(data_prediction, "sf")){
    bbox <- data_prediction %>% sf::st_bbox()
    n <- nrow(data_prediction)
    ratio <- (bbox["xmax"] - bbox["xmin"])/(bbox["ymax"] - bbox["ymin"])
    b <- sqrt(n/ratio)
    a <- n/b
    data_prediction <- stars::st_rasterize(
      data_prediction,
      bbox %>%
        stars::st_as_stars(nx = round(a),
                           ny = round(b),
                           values = NA_real_))
  }else if(inherits(data_prediction, "SpatRaster")){
    data_prediction <- data_prediction %>%
      methods::as("Raster")
  }else{
    stop("In `create_map_raster2`, `data_prediction`
         must be of class `sf` or `SpatRaster`.",
         call. = FALSE)
  }

  tm_raster_layer <- if(length(theme) == 3){
    tmap::tm_shape(data_prediction) +
      tmap::tm_raster(
        var_name,
        breaks = theme$breaks,
        labels = theme$labels,
        palette = theme$palette,
        title = legend.title
      )
  }else if(length(theme) == 1 & is.character(theme)){
    tmap::tm_shape(data_prediction) +
      tmap::tm_raster(
        var_name,
        palette = theme,
        title = legend.title
      )
  }else{
    stop("Provide valid theme in `create_map_raster2.", call. = FALSE)
  }

  map <- map_base +
    tm_raster_layer +
    map_base

  if(!is.na(waterbodies_filename)){
    map <- map +
      tmap::tm_shape(terra::rast(waterbodies_filename))+
      tmap::tm_raster(palette = "#bee8ff",
                legend.show = FALSE)
  }
  map <- map +
    map_top
  if(!is.null(data_estimation)){
    map <- map +
      tmap::tm_shape(data_estimation) +
      tmap::tm_dots()
  }
  map +
    tmap::tm_layout(
      legend.position = legend.position,
      legend.bg.color = "white",
      legend.frame = TRUE,
      title.bg.color = "white"
    )
}


#' Save map raster
#'
#' @param data_estimation Estimation data for drawing estimation points
#' @param data_prediction Prediction data, which gives the raster (the map)
#' @param var_name The variable to plot
#' @inheritParams save_map_points
#'
#' @return
#' @export
#'
#' @examples
save_map_raster <- function(data_estimation,
                              data_prediction,
                              var_name,
                              map_base,
                              map_top,
                              theme,
                              title,
                              legend.position = c("right", "bottom"),
                            dest_dir = "Doc/figures/",
                            file_name = "map_raster") {
  map <- create_map_raster(
    data_estimation = data_estimation,
    data_prediction = data_prediction,
    var_name =var_name ,
    map_base = map_base,
    map_top = map_top,
    theme = theme,
    title = title,
    legend.position = legend.position
  )
  if(!dir.exists(dest_dir))
    dir.create(dest_dir, recursive = TRUE)
  tmap::tmap_save(map,
                  filename = paste0(dest_dir, file_name, ".png"))
}
