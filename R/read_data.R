
#' Read data
#' This function intends to facilitate frequent reading of data, e.g. read_data('data/ghana/')
#' can be used to construct a function which then always reads from 'data/ghana/' using a specified reading function.
#'
#' @param dir The directory to read from frequently with "/" at the end.
#'
#' @return A function which reads data, e.g.
#' @export
#'
#' @examples
#' read_data('my_data_dir/')
read_data <- function(dir) {
  if (!stringr::str_detect(dir, "/$"))
    stop("`read_data`: argument dir needs a / in the end.")
  function(fn, read_function = readr::read_rds)
    do.call(read_function,
            list(dir %>% paste0(fn)))
}
