
#' Translate covariate names
#'
#' @param name The covariate name to translate
#' @param type The translation list "long_names" or "names_pv".
#'
#' @return
#' @export
#'
#' @examples
translate_covariate_names <- function(name, type){
  if(!purrr::is_scalar_character(type)){
    stop("type must be a scalar character.")
  }
  if(!type %in% c("names_pv", "long_names", "names_ju")){
    stop("type", type, "not recognized.")
  }
  if(!purrr::is_scalar_character(name)){
    stop("name must be a scalar character.")
  }
  dict <- openxlsx::read.xlsx("../africa/Data/covariates_ordered_translate.xlsx") %>%
    tibble::as_tibble()

  ind <- match(name, dict$short_names)
  i_top <- name %>% stringr::str_count("_") + 1
  i <- 1
  while(name %>% stringr::str_detect("_") & is.na(ind) & i <= i_top){
    name_ <- name %>%
      stringr::str_split("_") %>%
      purrr::pluck(1)
    ind <- match(paste0(name_[1:i], collapse = "_"), dict$short_names)

    if(length(ind) == 1 & !is.na(ind)){
      new_name <- dict %>%
        dplyr::slice(ind) %>%
        dplyr::pull(dplyr::all_of(type))
      if(length(name_) > i){
        new_name <- c(new_name, name_[(i+1):length(name_)] %>%
                        paste0(collapse = "_") %>%
                        translate_factorlevel_to_string()) %>%
          paste0(collapse = " ")

      }
      return(new_name)
    }
    i <- i+1
  }

  if(length(ind) == 1 & is.na(ind)){
    stop("No translation of type ", type, " found for ", name)
  }else if(length(ind) > 1){
    stop("> 1 translations of type ", type, "found for ", name)
  }else{
    dict %>%
      dplyr::slice(ind) %>%
      dplyr::pull(dplyr::all_of(type))
  }
}

translate_factorlevel_to_string <- function(x){
  if(x %>% stringr::str_detect("geq.+l.+$")){
    x %>%
      stringr::str_replace("geq", "") %>%
      stringr::str_replace("l", " - ")
  }else if(x %>% stringr::str_detect("geq")){
    x %>%
      stringr::str_replace("geq", "\\u2265 ")
  }else{
    x
  }
}
