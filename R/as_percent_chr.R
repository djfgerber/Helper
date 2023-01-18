
#' Convert to percent string
#'
#' @param share A share of something e.g. share 0.5 will be converted to 50%
#' @param digits Number of digits after the comma.
#' @param separator A separator for numbers
#'
#' @return A character string
#' @export
#'
#' @examples
#' 0.5 %>% as_percent_chr()
as_percent_chr <- function(share, digits = 1L, separator = "."){
  stopifnot(purrr::is_scalar_integer(digits),
            is.numeric(share),
            purrr::is_scalar_character(separator))
  format_num_as_chr(100*share, digits, separator) %>%
    paste0("%")
}

#' Format numeric vector as character with certain number of digits
#'
#' @param x numeric vector
#' @inheritParams as_percent_chr
#' @return A character string
#' @export
format_num_as_chr <- function(x, digits = 1L, separator = "."){
  stopifnot(purrr::is_scalar_integer(digits),
            is.numeric(x),
            purrr::is_scalar_character(separator))
  purrr::map_chr(x,
                 function(x){
                   res <- round(x, digits) %>%
                     as.character()
                   if(digits > 0){
                     if(res %>% stringr::str_detect("\\.")){
                       res_split <- res %>%
                         stringr::str_split("\\.") %>%
                         unlist()
                       res <- paste0(res_split[1],
                                     separator,
                                     res_split[2],
                                     paste0(rep("0", digits - nchar(res_split[2])),
                                            collapse = ""))
                     }else{
                       res <- res %>%
                         paste0(separator,
                                paste0(rep("0", digits), collapse = ""))
                     }
                   }
                   return(res)
                   })
}
