
#' Get caching directory
#' @description `r lifecycle::badge("experimental")`. To be used in combination with `xfun::cache_rds`.
#' @param rmd_name Name of the current markdown.
#'
#' @return The directory in which the cache should be saved
#' @export
#'
#' @examples
get_cache_dir <- function(rmd_name = knitr::current_input()) {
  sim_dir_name <- paste0(
    get_switchdrive_path(),
    "/Simulations/",
    rmd_name %>%
      stringr::str_replace("\\.Rmd", "/")
  )
  if (!dir.exists(sim_dir_name)) {
    dir.create(sim_dir_name)
  }
  sim_dir_name
}


#' Save simulations
#'
#' @description `r lifecycle::badge("experimental")`, use `xfun::cache_rds` instead. Save simulations from Rmd-chunk to Data/rmd_name/chunkname.rds with gz compression
#'
#' @param x Object to save
#' @param rmd_name name of the markdown from which the function is executed
#' @param chunk_name Character of the current chunk_name
#'
#' @export
write_simulation <- function(x, chunk_name, rmd_name = knitr::current_input()) {
  stopifnot(purrr::is_scalar_character(chunk_name),
            nchar(chunk_name) > 0)
  if(is.null(rmd_name)){
    rmd_name <- getwd() %>%
      stringr::str_replace("^.*/")
  }
  sim_dir_name <- paste0(get_switchdrive_path(),
                         "/Simulations/",
                         rmd_name %>%
                           stringr::str_replace("\\.Rmd", ""))
  print(sim_dir_name)
  if (!dir.exists(sim_dir_name)) {
    dir.create(sim_dir_name)
  }
  sim_file_name <-
    paste0(sim_dir_name,
           "/",
           chunk_name,
           Sys.time() %>% stringr::str_replace_all(":", "_"),
           ".rds")
  print(sim_file_name)
  print(file.exists(sim_file_name))
  x %>%
    readr::write_rds(file = sim_file_name, compress = "gz")
}

cache_computation <- function(expr = {}, hash = list(), rmd_name = knitr::current_input()){
  sim_dir_name <- paste0(get_switchdrive_path(),
                         "/Simulations/",
                         rmd_name %>%
                           stringr::str_replace("\\.Rmd", ""))
  if (!dir.exists(sim_dir_name)) {
    dir.create(sim_dir_name)
  }
  xfun::cache_rds(expr = expr, dir = sim_dir_name, hash = hash)
}

#' Read simulations
#'
#' @description `r lifecycle::badge("experimental")`. Read simulations saved with write_simulations. Use `xfun::cache_rds` instead
#'
#' @param chunk_name Character of the current chunk_name
#' @param rmd_name Character readmename resp location of the save.
#' @param date Date in the form yyyy_mm_dd hh:mm:ss
#'
#' @return Object saved with write_simulations
#' @export
read_simulation <- function(chunk_name,  rmd_name = knitr::current_input(), date = NA_character_) {
  stopifnot(purrr::is_scalar_character(chunk_name),
            !is.na(chunk_name),
            nchar(chunk_name) > 0,
            purrr::is_scalar_character(date))
  sim_dir_name <- paste0(get_switchdrive_path(),
                         "/Simulations/",
                         rmd_name %>%
                           stringr::str_replace("\\.Rmd", ""))
  if(!dir.exists(sim_dir_name)){
    stop("Directory not found. You probably messed up rmd_name.")
  }
  if (!is.na(date)) {
    file_name <- paste0(sim_dir_name,
           "/",
           chunk_name,
           date %>% stringr::str_replace_all(":", "_"),
           ".rds")
    if(!file.exists(file_name)){
      stop("File does not exist")
    }
  } else{
    fl <- list.files(sim_dir_name, pattern = chunk_name)
    if(length(fl) == 0){
      stop("File does not exist")
    }
    timestamps <- fl %>%
      stringr::str_replace(chunk_name, "") %>%
      stringr::str_replace("\\.rds", "") %>%
      stringr::str_replace_all("_", ":") %>%
      lubridate::as_datetime()
    if(any(is.na(timestamps))){
      stop("Timestamps were NA. You probably messed up the chunk_name.")
    }

    file_name <- paste0(sim_dir_name,
           "/",
           fl[which(timestamps == max(timestamps))])
  }
  file_name %>% readr::read_rds()
}

#' Get the switchdrive path
#'
#' @return The Switchdrive path on each computer.
#' @export
#'
#' @examples
get_switchdrive_path <- function(){
  if(dir.exists("J:/BS/SSM/daniel.gerber/Switchdrive")){
    "J:/BS/SSM/daniel.gerber/Switchdrive"
  }else if(dir.exists("C:/Users/danth/switchdrive")){
    "C:/Users/danth/switchdrive"
  }else{
    stop("Can not find Switchdrive.")
  }
}
