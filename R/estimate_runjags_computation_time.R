#' Estimate the number of remaining time of a runjags model
#'
#' @param elapsed_time How many seconds have elapsed yet
#' @param iter_stars If the most recent, how many iterations stars are visible
#' @param burnin_stars If the most recent, how many burnin stars are visible
#' @param adapt_cross If the most recent, how many adaptation crosses are visible
#' @param nsample Number of iterations
#' @param burnin Number of burnin steps
#' @param adaption Number of Adaption steps
#'
#' @return Projections about time and stuff
#' @export
#'
#' @examples
#' estimate_runjags_computation_time(36*60, iter_stars = 23L)
#' estimate_runjags_computation_time(36*60, burnin_stars = 23L)
#' estimate_runjags_computation_time(36*60, adapt_cross = 23L)
estimate_runjags_computation_time <-
  function(elapsed_time,
           iter_stars = NA_integer_,
           burnin_stars = NA_integer_,
           adapt_cross = NA_integer_,
           nsample = 50000,
           burnin = 0.1 * nsample,
           adaption = 1000) {
    stopifnot(
      purrr::is_scalar_integer(iter_stars),
      purrr::is_scalar_integer(burnin_stars),
      purrr::is_scalar_integer(adapt_cross),
      !is.na(iter_stars) & is.na(burnin_stars) & is.na(adapt_cross) |
        is.na(iter_stars) & !is.na(burnin_stars) & is.na(adapt_cross) |
        is.na(iter_stars) & is.na(burnin_stars) & !is.na(adapt_cross),
      purrr::is_scalar_double(nsample),
      purrr::is_scalar_double(burnin),
      purrr::is_scalar_double(adaption)
    )
    if(!is.na(iter_stars)){
      completed_iterations <- nsample * iter_stars / 50 + burnin + adaption
    }else if(!is.na(burnin_stars)){
      completed_iterations <- burnin * burnin_stars / 50 + adaption
    }else if(!is.na(adapt_cross)){
      completed_iterations <- adaption * adapt_cross / 50
    }
    time_per_iteration <- elapsed_time / completed_iterations
    tibble::tibble(
      completed_iterations,
      time_per_iteration_sec = time_per_iteration,
      remaining_iterations = nsample + burnin + adaption - completed_iterations,
      remaining_time_hours = remaining_iterations * time_per_iteration /
        60 / 60
    ) %>%
      tidyr::pivot_longer(1:4)
  }
