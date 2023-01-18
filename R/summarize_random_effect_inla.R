#' Summarize random effect inla
#'
#' Summarizes the random effect of an inla model (mean, sd, quantiles 0.025, 0.5, 0.095) and renders them in a tidy format.
#'
#' @param inla_output The `...$marignals.variance.nominal[[1]]`, `...$marginals.range.nominal[[1]]` or `inla.tmarginal(function(x) 1/x, ...$marginals.hyperpar[[...]])`
#' @param parameter_name Name of the parameter as character scalar.
#'
#' @return Tibble with marginal mean, standard deviation and 0.025, 0.5, and 0.975 quantiles
#' @export
#'
#' @examples
summarize_random_effect <- function(inla_output, parameter_name) {
  stopifnot(purrr::is_scalar_character(parameter_name))
  tibble::tibble(
    term = parameter_name,
    estimate = INLA::inla.emarginal(function(x)
      x, inla_output),
    se = sqrt(INLA::inla.emarginal(function(x)
      x ^ 2, inla_output) - estimate ^ 2),
    q0_025 = INLA::inla.qmarginal(c(0.025), inla_output),
    q0_5 = INLA::inla.qmarginal(c(0.5), inla_output),
    q0_975 = INLA::inla.qmarginal(c(0.975), inla_output)
  )
}
