#' Search code
#'
#' @param directory Directory ending with a /, provide . if base directory should be searched
#' @param file_pattern File names are checked for file_pattern for example ".R"
#' @param keyword Keyword to look for as regular expression
#' @param recursive Should direcories be searched recusively? boolean (default = TRUE)
#'
#' @return Tibble with file name and text of all found instances
#' @export
search_code <- function(directory,
                        keyword,
                        file_pattern = NULL,
                        recursive = TRUE) {
  stopifnot(purrr::is_scalar_character(directory),
            is.null(file_pattern) || purrr::is_scalar_character(file_pattern),
            purrr::is_scalar_character(keyword),
            purrr::is_scalar_logical(recursive),
            !stringr::str_detect(directory, "/$")
  )
  list.files(directory,
             pattern = file_pattern,
             full.names = TRUE,
             recursive = recursive) %>%
    purrr::map_dfr( ~ dplyr::tibble(file = .x,
                      text = paste(readr::read_lines(.x),
                                   collapse = "\n"))) %>%
    dplyr::filter(text %>% stringr::str_detect(keyword))
}
