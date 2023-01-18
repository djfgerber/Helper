
#' Adapt flextable to page width
#' @description credits to [Sarah on stackexchange](https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar).
#' Add this to the end of a flextable pipe to render nicely.
#'
#' @param ft Flextable object
#' @param pgwidth The pagewidth in inches, I guess
#'
#' @return A flexable output
#' @export
#'
#' @examples
fit_flextable_to_page <- function(ft, pgwidth = 6){
  stopifnot(inherits(ft, "flextable"))
  ft_out <- ft %>% flextable::autofit()
  ft_out <- flextable::width(ft_out,
                  width = dim(ft_out)$widths * pgwidth /
                    (flextable::flextable_dim(ft_out)$widths))
  return(ft_out)
}
