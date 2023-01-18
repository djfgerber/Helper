

#' Check if all the variables are in an gross acceptable range
#' @description Known variables in data are checked for being in a acceptable
#' range, e.g. mean temperature between -40°C and 50°C, proportion between 0
#' and 1, etc.
#' @param data A data frame as tibble
#'
#' @return A tibble containing the report of the checks.
#' @export
#'
#' @examples
#' tibble::tibble(evi = runif(10,-1,1), ndvi = runif(10, -2,-1)) %>%
#'   check_variables_ranges()
#' tibble::tibble(ndvi = runif(10,-1,1), evi = runif(10, -2,-1)) %>%
#'   check_variables_ranges()
#' tibble::tibble(ndvi = c(NA,runif(10,-1,1)), evi = c(NA,runif(10, -2,-1))) %>%
#'   check_variables_ranges()
check_variables_ranges <- function(data) {
  ref <- tibble::tibble(
    variable = c('bio1', 'bio2', 'bio3', 'bio4', 'bio5',
                 'bio6', 'bio7', 'bio8', 'bio9', 'bio10',
                 'bio11', 'bio12', 'bio13', 'bio14', 'bio15',
                 'bio16', 'bio17', 'bio18', 'bio19',
                 'lstd', 'lstn', 'rain',
                 'ndvi', 'evi',
                 'altitude', 'slope', 'dist_wb',
                 'bdod', 'cec', 'cfvo', 'clay', 'ocd',
                 'ocs', 'phh2o', 'silt', 'sand', 'nitrogen',
                 'soc',
                 'imprsan', 'imprdr', 'opendef', 'handwashf', 'watersoap',
                 'population',
                 'shrubs', 'forest', 'crops', 'water'
                 ),
    upper = c(60,100,Inf,5000,80,
              60,100,rep(60,4),
              rep(Inf, 8),
              60,60,10000,
              1,1,
              10000, Inf, 100000,
              1000,1000,1000,1000,2000,
              5000,100,1000,1000,2000,
              5000,
              rep(1,5),
              7000000000,
              rep(1,4)),
    lower = c(-40,0,0,0,-40,
              -40,0,rep(-40,4),
              rep(0,8),
              -40,-40,0,
              -1,-1,
              0,0,0,
              rep(0,11),
              rep(0,5),
              0,
              rep(0,4)))

  check_result <- ref %>%
    purrr::pmap_chr(
      ~ ifelse(
        ..1 %in% colnames(data),
        data %>%
          dplyr::pull(..1) %>%
          check_range(..2, ..3),
        "Variable not found"
      )
    )
  not_checked <- colnames(data)[!colnames(data) %in% ref$variable]
  res <- ref %>%
    dplyr::bind_cols(tibble::tibble("check" = check_result)) %>%
    dplyr::bind_rows(
      tibble::tibble(
        variable = not_checked,
        upper = NA_real_,
        lower = NA_real_,
        check = "No check available"
      )
    ) %>%
    dplyr::filter(check != "Variable not found")
  res[match(colnames(data), res$variable),]
}

check_range <- function(x, upper, lower) {
  stopifnot(
    is.numeric(x),
    purrr::is_scalar_double(upper),
    purrr::is_scalar_double(lower),
    lower < upper
  )
  if(all(is.na(x)))
    return("All NA")
  addstr <- ""
  if(any(is.na(x))){
    x <- x[!is.na(x)]
    addstr <- ", contains NAs"
  }
  dplyr::if_else(all(x <= upper & x >= lower),
                 "OK",
                 "Not in limits") %>%
    paste0(addstr)
}







