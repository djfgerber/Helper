
#' Plot inla fixed effects
#'
#' @param summary.fixed output$summary.fixed of a inla object
#'
#' @export
#'
#' @examples
plot_inla_summary.fixed <- function(summary.fixed){
  summary.fixed %>%
    dplyr::as_tibble(rownames = "coef") %>%
    dplyr::mutate(
      coef = coef %>%
        forcats::as_factor() %>%
        forcats::fct_rev(),
      influence = dplyr::if_else(
        `0.025quant` > 0,
        "increasing",
        dplyr::if_else(`0.975quant` < 0, "decreasing", "neutral")
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(y = coef, col = influence)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = `0.025quant`, xmax = `0.975quant`)) +
    ggplot2::geom_point(ggplot2::aes(x = `0.5quant`)) +
    ggplot2::geom_vline(xintercept = 0)+
    ggplot2::labs(x="", y="")+
    ggplot2::theme(legend.position = "none")
}
