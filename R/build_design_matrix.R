#' Assemble the JAGS model string
#'
#' @param family Family of the distribution
#' @param prior_tau_fixed Prior on the coefficient if NA then a gamma is used
#' @param flag_spike_slab Spike and slab use?
#' @param flag_intercept intercept add?
#' @param prior_decay_gamma_shape prior on decay gamma?
#' @param b_len_contin same as other
#' @param b_len_both same as other
#'
#' @describeIn get_model_string_spike_slab
#'
#' @return A Jags model string
#'
#' @examples
#' \dontrun{
#' Helper:::get_bvs_model_string("binomial", 0.01)
#' Helper:::get_bvs_model_string("zero_inflated_binomial", NA_real_)
#' }
get_bvs_model_string <-   function(family,
                                   prior_tau_fixed,
                                   flag_spike_slab,
                                   flag_covariates_zi,
                                   flag_intercept,
                                   prior_decay_gamma_shape,
                                   b_len_contin,
                                   b_len_both) {
  stopifnot(purrr::is_scalar_double(prior_tau_fixed))
  model_strings <- list(
    get_model_string_likelihood(family = family,
                                flag_intercept = flag_intercept,
                                flag_covariates_zi = flag_covariates_zi),
    get_model_string_spike_slab(
      family = family,
      flag_spike_slab = flag_spike_slab,
      flag_covariates_zi = flag_covariates_zi,
      flag_intercept = flag_intercept,
      prior_tau_fixed = prior_tau_fixed,
      b_len_contin = b_len_contin,
      b_len_both = b_len_both
    ),
    get_model_string_nugget(),
    get_model_string_gaussian_process(prior_decay_gamma_shape)
  )
  paste(
    "
model{
    ",
    paste(c(
      model_strings %>%
        purrr::map_chr( ~ .x %>% purrr::pluck("model_string")) %>%
        paste(collapse = "\n"),
      paste("#data#", model_strings %>%
              purrr::map( ~ .x %>%
                            purrr::pluck("data")) %>%
              unlist() %>%
              unique() %>%
              paste0(collapse =", ")),
      paste("#inits#", model_strings %>%
              purrr::map( ~ .x %>%
                            purrr::pluck("inits")) %>%
              unlist() %>%
              unique() %>%
              c(".RNG.name", ".RNG.seed") %>%
              paste0(collapse =", ")),
      paste("#monitor#", model_strings %>%
              purrr::map( ~ .x %>%
                            purrr::pluck("monitor")) %>%
              unlist() %>%
              unique() %>%
              paste0(collapse =", "))),
      collapse = "\n"
    ),
    "\n}\n"
  )
}

#' Get the likelihood part of a JAGS model string
#'
#' @param family Must be binomial, negative_binomial, zero_inflated_binomial or zero_inflated_negative_binomial
#' @param prior_tau_fixed Must be NA_real_ or a double.
#' @param flag_spike_slab Whether to use a spike and slab prior or not
#' @param flag_intercept Add intercept ?
#' @param b_len_contin Length of the continuous variables
#' @param b_len_both Length of variables which are tested as continuous or categorical
#'
#' @return list with $model_string, $data (vector of used data parameters), $inits (vector of used inits), $monitor (vector of used monitors)
#' @export
#'
#' @examples
#' \dontrun{
#' Helper:::get_model_string_spike_slab("binomial", 0.01)
#' Helper:::get_model_string_spike_slab("zero_inflated_binomial", NA_real_)
#' }
get_model_string_spike_slab <- function(family,
                                        flag_spike_slab,
                                        flag_covariates_zi,
                                        flag_intercept,
                                        prior_tau_fixed,
                                        b_len_contin,
                                        b_len_both){
  if(!is.na(prior_tau_fixed)){
    prior_tau <- paste0("  tau <- ", prior_tau_fixed)
    data_add <- NULL
    inits_add <- NULL
    monitor_add <- NULL
  }else{
    prior_tau <- "  tau ~ dgamma(prior_tau_gamma_shape, prior_tau_gamma_rate)"
    data_add <- c("prior_tau_gamma_shape", "prior_tau_gamma_rate")
    inits_add <- "tau"
    monitor_add <- "tau"
  }

  if(flag_covariates_zi && family %in% c("zero_inflated_binomial", "zero_inflated_negative_binomial")){
    zero_infated_priors <- c("b_zi[c_level[k] + level_i] ~ dnorm(0.0, taub[k])",
      "b_zi[contin_end+(k-1)*3+j] ~ dnorm(0.0, taub[contin_end+(k-1)*3+j])",
      "b_zi[c_level[k] + level_i] ~ dnorm(0.0, tau)")
    inits_add <- c(inits_add, "b_zi")
    monitor_add <- c(monitor_add, "b_zi")
  }else{
    zero_infated_priors <- c("", "", "")
  }
  if(flag_intercept){
    intercept_prior <- "  intercept ~ dnorm(0.0, 0.01)"
    if(family %in% c("zero_inflated_binomial", "zero_inflated_negative_binomial")){
      intercept_prior <- "  intercept ~ dnorm(0.0, 0.01)
  intercept_zi ~ dnorm(0.0, 0.01)"
    }

    inits_add <- c(inits_add, "intercept")
    monitor_add <- c(monitor_add, "intercept")
  }else{
    intercept_prior <- ""
    inits_add <- c(inits_add, NULL)
    monitor_add <- c(monitor_add, NULL)
  }
  if(flag_spike_slab){
    if(b_len_contin > 0){
      continuous_prior <-  paste0("
  # spike and slab prior for const and continuous covariates
  pind ~ dbeta(prior_pind_a, prior_pind_b)
  for (k in 1:b_len_contin) {
    ind[k] ~ dbern(pind)
    taub[k] <- ind[k]*tau + (1-ind[k]) * u0 * tau
    for(level_i in 1:n_level[k]){
      b[c_level[k] + level_i] ~ dnorm(0.0, taub[k])
      ",
                                  zero_infated_priors[1],
                                  "
    }
  }")
      data_add <- c(data_add, "prior_pind_a", "prior_pind_b", "b_len_contin", "n_level", "c_level", "u0")
      inits_add <- c(inits_add, "b", "ind", "pind")
      monitor_add <- c(monitor_add, "b", "ind", "pind")
    }else{
      continuous_prior <- ""
      data_add <- c(data_add, NULL)
      inits_add <- c(inits_add, NULL)
      monitor_add <- c(monitor_add, NULL)
    }
    if(b_len_both > 0){
      both_prior <-  paste0("
  # spike and slab prior for covariates that should be selected either the continuous or categorical form.
  # indb values: 1=excluded, 2=included as continuous, 3=included as categorical with 3 levels
  a[1:3] ~ ddirch(prior_alpha[])
  for (k in 1:b_len_both) {
    indb[k] ~ dcat(a[])
    for(j in 1:3) {
      b[contin_end+(k-1)*3+j] ~ dnorm(0.0,taub[contin_end+(k-1)*3+j])
      ",
                            zero_infated_priors[2],
                            "
    }
    taub[contin_end+(k-1)*3+1] <- (equals(indb[k],1) + equals(indb[k],3))*u0*tau + # Continuous
                                   equals(indb[k],2) * tau
    taub[contin_end+(k-1)*3+2] <- (equals(indb[k],1) + equals(indb[k],2))*u0*tau + # Categorical
                                   equals(indb[k],3) * tau
    taub[contin_end+(k-1)*3+3] <- (equals(indb[k],1)+equals(indb[k],2))*u0*tau+
                                   equals(indb[k],3) * tau
  }")
      data_add <- c(data_add, "prior_alpha", "b_len_both", "contin_end", "u0")
      inits_add <- c(inits_add, "a", "b", "indb")
      monitor_add <- c(monitor_add, "a", "b", "indb")
    }else{
      both_prior <- ""
      data_add <- c(data_add, NULL)
      inits_add <- c(inits_add, NULL)
      monitor_add <- c(monitor_add, NULL)
    }
    coefficient_prior <- paste(c(continuous_prior,
                               both_prior),
                               collapse = "\n")
  }else{
    # not spike and slab
    coefficient_prior <- paste(
      "
  for (k in 1:b_len_contin) {
    for(level_i in 1:n_level[k]){
      b[c_level[k] + level_i] ~ dnorm(0.0, tau)
      ",
      zero_infated_priors[3],
      "
    }
  }
")
    data_add <- c(data_add, "b_len_contin", "n_level", "c_level")
    inits_add <- c(inits_add, "b")
    monitor_add <- c(monitor_add, "b")
  }
  list(
    "model_string" =
      paste(c(
        intercept_prior,
        coefficient_prior,
        prior_tau),
        collapse = "\n"
      ),

  "data" = data_add %>% unique(),
  "inits" = inits_add %>% unique(),
  "monitor" = monitor_add %>% unique())
}

#' Get the likelihood part of a JAGS model string
#'
#' @param family Must be binomial, negative_binomial, zero_inflated_binomial or zero_inflated_negative_binomial
#' @param flag_intercept If an incept should be added
#'
#' @return list with $model_string, $data (vector of used data parameters), $inits (vector of used inits), $monitor (vector of used monitors)
#'
#' @examples
#' \dontrun{
#' Helper:::get_model_string_likelihood("binomial")
#' Helper:::get_model_string_likelihood("zero_inflated_negative_binomial")
#' }
get_model_string_likelihood <- function(family, flag_intercept, flag_covariates_zi){
  if(flag_intercept){
    intercept <- "intercept + "
    add_inits <- c("intercept")
    add_monitor <- c("intercept")
    if(family %in% c("zero_inflated_binomial", "zero_inflated_negative_binomial")){
      intercept <- c(intercept, "intercept_zi + ")
      add_inits <- c("intercept", "intercept_zi")
      add_monitor <- c("intercept", "intercept_zi")
    }
  }else{
    intercept <- ""
    if(family %in% c("zero_inflated_binomial", "zero_inflated_negative_binomial")){
      intercept <- c(intercept, "")
    }
    add_inits <- NULL
    add_monitor <- NULL
  }
  if(flag_covariates_zi){
    covariates_zi <- "inprod(XMAT[i,], b_zi) +"
    add_data <- "XMAT"
    add_inits <- c(add_inits, "b_zi")
    add_monitor <- c(add_monitor, "b_zi")
  }else{
    covariates_zi <- ""
    add_data <- NULL
    add_inits <- c(add_inits, NULL)
    add_monitor <- c(add_monitor, NULL)
  }

  if(family == "binomial"){
    list("model_string" = paste0(
 "
  for (i in 1:M) {
    positives[i] ~ dbin(p[i], total[i])
    logit(p[i]) <- ", intercept, "inprod(XMAT[i,], b) + u[idloc[i]] + e[idloc[i]]
  }
"),
 "data" = c(add_data, "M", "positives", "total", "XMAT", "idloc"),
 "inits" = c(add_inits, "b", "u", "e"),
 "monitor" = c(add_monitor, "b"))
  }else if(family == "negative_binomial"){
    list("model_string" = paste0(
"
  for (i in 1:M) {
    counts[i] ~ dnegbin(p[i], r)
    p[i] <- r/(r+lambda[i])
    log(lambda[i]) <- ", intercept, "inprod(XMAT[i,], b) + u[idloc[i]] + e[idloc[i]]
  }
  r ~ dgamma(prior_r_gamma_shape, prior_r_gamma_rate)
"),
  "data" = c(add_data, "M", "counts", "XMAT", "idloc", "prior_r_gamma_shape", "prior_r_gamma_rate"),
  "inits" = c(add_inits, "b", "u", "e", "r"),
"monitor" = c(add_monitor, "b", "r"))
  }else if(family == "zero_inflated_binomial"){
    list("model_string" = paste0(
           "
  for (i in 1:M) {
    positives[i] ~ dbin(p.hacked[i], total[i])
    p.hacked[i] <- p[i]*(1-zero[i]) + 1e-10*zero[i]
    p[i] <-  ilogit(", intercept[1], "inprod(XMAT[i,], b) + u[idloc[i]] + e[idloc[i]])
    ## Zero-Inflation
    zero[i] ~ dbern(pi[i])
    pi[i] <- ilogit(", intercept[2], covariates_zi, "u[idloc[i]] + e[idloc[i]])
  }
"),
         "data" = c(add_data, "M", "positives", "total", "XMAT", "idloc"),
         "inits" = c(add_inits, "b", "u", "e", "zero"),
         "monitor" = c(add_monitor, "b"))
  }else if(family == "zero_inflated_negative_binomial"){
    list("model_string" = paste0(
          "
  for (i in 1:M) {
    counts[i] ~ dnegbin(p[i],r)
    p[i] <- r/(r+(1-zero[i])*lambda.count[i]) - 1e-10*zero[i]
    lambda.count[i] <- exp(", intercept[1], "inprod(XMAT[i,], b)+u[idloc[i]]+e[idloc[i]])
    ## Zero-Inflation
    zero[i] ~ dbern(pi[i])
    pi[i] <- ilogit(", intercept[2], covariates_zi, "u[idloc[i]] + e[idloc[i]])
  }
  r ~ dgamma( prior_r_gamma_shape, prior_r_gamma_rate )
"),
         "data" = c(add_data, "M",  "counts", "XMAT", "idloc",  "prior_r_gamma_shape", "prior_r_gamma_rate"),
         "inits" = c(add_inits, "b", "u", "e", "zero"),
         "monitor" = c(add_monitor, "b"))
  }
}

#' Get the iid random effect (nugget) part of a JAGS model string
#'
#' @return list with $model_string, $data (vector of used data parameters), $inits (vector of used inits), $monitor (vector of used monitors)
#'
#' @examples
#' \dontrun{
#' Helper:::get_model_string_nugget()
#' }
get_model_string_nugget <- function(){
  list("model_string" =
         "
  # prior for exchangeable re
  for (i in 1:N) {
    e[i] ~ dnorm(0,tau2.e)
  }
  tau2.e ~ dgamma(prior_tau2.e_shape, prior_tau2.e_rate)
  sigma2.e = 1 / tau2.e
",
       "data" = c("N", "prior_tau2.e_shape", "prior_tau2.e_rate"),
       "inits" = c("e", "tau2.e")
       ,
       "monitor" = c("tau2.e", "sigma2.e"))
}

#' Get the Gaussian process part of a JAGS model string
#'
#' @param prior_decay_gamma_shape If a gamma prior should be used
#'
#' @return list with $model_string, $data (vector of used data parameters), $inits (vector of used inits), $monitor (vector of used monitors)
#'
#' @examples
#' \dontrun{
#' Helper:::get_model_string_gaussian_process()
#' }
get_model_string_gaussian_process <- function(prior_decay_gamma_shape = NA_real_){
  if(is.na(prior_decay_gamma_shape)){
    prior_decay_str <- "decay ~ dunif(prior_decay_min, prior_decay_max)"
    add_data <- NULL
  }else{
    prior_decay_str <- "decay ~ dgamma(prior_decay_gamma_shape, prior_decay_gamma_rate) T(prior_decay_min, prior_decay_max)"
    add_data <- c("prior_decay_gamma_shape", "prior_decay_gamma_rate")
  }
  list("model_string" =
         paste0("
  # prior for Gaussian process
  u[1:N] ~ dmnorm(mu[], sigma_inv[,])
  for (i in 1:N) {
     mu[i] <- 0
  }
  for(i in 1:N)	{
    for(j in 1:N)	{
       sigma[i,j] <- sigma2.sp * exp(-decay * dist[i,j])
    }
  }
  sigma_inv <- inverse(sigma)
  tau2.sp ~ dgamma(prior_tau2.sp_shape, prior_tau2.sp_rate)
  sigma2.sp <- 1 / tau2.sp
  ",
  prior_decay_str,
  "
  range <- - log(range_threshold) / decay
", collapse = "\n"),
       "data" = c("N", "dist", "prior_tau2.sp_shape", "prior_tau2.sp_rate", "prior_decay_min", "prior_decay_max", "range_threshold", add_data),
       "inits" = c("u", "tau2.sp", "decay"),
       "monitor" = c("tau2.sp", "sigma2.sp", "decay", "range"))

}



#' Model construction for bayesian variable selection with binomial count data
#' @description `r lifecycle::badge('experimental')`
#'
#' @param data A tibble or data.frame containing all variables
#' @param u0 Factor for multiplying the inverse variance leading to the spike part of the b prior
#' @param prior_alpha prior specification
#' @param prior_pind_a prior specification
#' @param prior_pind_b prior specification
#' @param prior_tau2.sp_shape prior specification
#' @param prior_tau2.sp_rate prior specification
#' @param prior_tau2.e_shape prior specification
#' @param prior_tau2.e_rate prior specification
#' @param family A family name, "binomial", "negative_binomial", "zero_inflated_binomial", "zero_inflated_negative_binomial" are possible
#' @param prior_tau_fixed prior specification
#' @param prior_tau_gamma_shape prior specification (do not use, probably bad)
#' @param prior_tau_gamma_rate prior specification (do not use, probably bad)
#' @param prior_r_gamma_shape prior specification
#' @param prior_r_gamma_rate prior specification
#' @param response Character indicating the count or count and total if binomial
#' @param covariates Variables to consider, e.g. from collinearity preprocessing
#' @param flag_intercept Model has an intercept (default = TRUE)
#' @param flag_scale_vars Scaling the covariates?
#' @param n_chains Number of chains
#' @param inits Initial values
#' @param n_runs Number of runs of n_iter iterations
#' @param n_iter Number of iterations per run
#' @param burnin Burnin
#' @param save_dir Directory for saving
#' @param save_file File for saving (without extension)
#' @param range_threshold As in -log(threshold) /dist_min_max = decay limits, usually 0.1353353 (compatibility with INLA) or 0.04978707 (class biostat)
#' @param flag_spike_slab If not spike and slab (no variable selection) all variables must be continuous_only.
#' @param prior_decay_gamma_shape If is.na then a uniform prior is used
#' @param prior_decay_gamma_rate Prior on decay if gamma prior.
#' @param continuous_categorical
#' @param flag_covariates_zi
#'
#' @return A list with useful stuff including the model
#' @export
#'
#' @examples
#' \dontrun{
#'  sf_gambia %>%
#'    fit_bvs_jags_model(
#'      response = c("positives", "total"),
#'      covariates = c("age", "netuse"),
#'      continuous_only = "age",
#'      n_chains = 4,
#'      prior_tau_fixed = 0.01,
#'      prior_tau_gamma_rate = NA_real_,
#'      prior_tau_gamma_shape = NA_real_,
#'      save_dir = "helper",
#'      save_file = "try",
#'      n_runs = 3,
#'      n_iter = 500,
#'      burnin = 50
#'  )
#'}
fit_bvs_jags_model <- function(data,
                               response,
                               covariates,
                               continuous_categorical = character(),
                               family = "binomial",
                               flag_intercept = TRUE,
                               flag_scale_vars = TRUE,
                               flag_spike_slab = TRUE,
                               flag_covariates_zi = NA,
                               n_chains = 2,
                               prior_alpha = c(1, 1, 1),
                               prior_pind_a = 1,
                               prior_pind_b = 1,
                               prior_tau_fixed = NA_real_,
                               prior_tau_gamma_shape = 5,
                               prior_tau_gamma_rate = 25,
                               prior_decay_gamma_shape = 0.01,
                               prior_decay_gamma_rate = 0.01,
                               prior_r_gamma_shape = 0.001,
                               prior_r_gamma_rate = 0.001,
                               prior_tau2.sp_shape = 2.01,
                               prior_tau2.sp_rate = 1.01,
                               prior_tau2.e_shape = 2.01,
                               prior_tau2.e_rate = 1.01,
                               range_threshold = 0.1353353,
                               u0 = 4000,
                               inits = NULL,
                               n_runs = 5,
                               n_iter = 50000,
                               burnin = 5000,
                               save_dir = NULL,
                               save_file = NULL) {

# test arguments ----------------------------------------------------------
  is_sf_data(data)
  is_covariates(data, covariates)
  stopifnot(family %in% c(
    "binomial",
    "negative_binomial",
    "zero_inflated_binomial",
    "zero_inflated_negative_binomial"
  ))
  switch(family,
         "binomial" = is_binomial_response(data, response),
         "zero_inflated_binomial" = is_binomial_response(data, response),
         "negative_binomial" = is_negative_binomial_response(data, response),
         "zero_inflated_negative_binomial" = is_negative_binomial_response(data, response))
  if(family %in% c("zero_inflated_binomial", "zero_inflated_negative_binomial")){
    stopifnot(!is.na(flag_covariates_zi))
  }else{
    flag_covariates_zi <- FALSE
  }
  stopifnot(
    length(covariates) > 0 | flag_intercept,
    all(continuous_categorical %in% covariates),
    purrr::map_lgl(
      c(prior_pind_a,
        prior_pind_b,
        prior_tau_fixed,
        prior_tau_gamma_shape,
        prior_tau_gamma_rate,
        prior_decay_gamma_shape,
        prior_decay_gamma_rate,
        prior_r_gamma_shape,
        prior_r_gamma_rate,
        prior_tau2.sp_shape,
        prior_tau2.sp_rate,
        prior_tau2.e_shape,
        prior_tau2.e_rate,
        range_threshold,
        u0
      ),
      purrr::is_scalar_double
    ) %>%
      all(),
    is.na(prior_tau_fixed) & !is.na(prior_tau_gamma_shape) & !is.na(prior_tau_gamma_rate) |
      !is.na(prior_tau_fixed) & is.na(prior_tau_gamma_shape) & is.na(prior_tau_gamma_rate),
    is.na(prior_decay_gamma_shape) & is.na(prior_decay_gamma_rate) |
      !is.na(prior_decay_gamma_shape) & !is.na(prior_decay_gamma_rate)
  )
  fct_names_in_data <- data %>%
    purrr::map(is.factor) %>%
    purrr::keep(.p = identity) %>%
    names()
  fct_names <- intersect(covariates, fct_names_in_data)
  continuous_only <- setdiff(covariates, union(continuous_categorical, fct_names))

  purrr::walk(c(flag_intercept, flag_scale_vars, flag_spike_slab, flag_covariates_zi), is_flag)
  stopifnot(flag_spike_slab | # TODO: what if length(covariates) == 0?
              !flag_spike_slab & covariates  == continuous_only)
  if(is.null(save_dir) | is.null(save_file)){
    cat("!!! WARNING !!! Save_dir or save_file not given.!!! WARNING !!! \n")
  }

  coords <- data %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::distinct()
  idloc <- cumsum(!duplicated(coords))
  N <- coords %>% nrow()
  dist <- spBayes::iDist(coords)
  decay_limits <- dist %>% Helper::get_decay_limits(threshold = range_threshold)
  prior_decay_min <- decay_limits[1]
  prior_decay_max <- decay_limits[2]

  data_sf <- data
  data <- data %>% sf::st_drop_geometry()

  fct_n_levels <- fct_names %>%
    purrr::map_int(~ data %>%
                     dplyr::pull(.x) %>%
                     levels() %>%
                     length())
  fct_names <- fct_names[order(fct_n_levels)]
  fct_n_levels <- fct_n_levels[order(fct_n_levels)]

  both <-
    covariates[!covariates %in% union(fct_names, continuous_only)]
  continuous <- c(intersect(covariates, continuous_only), fct_names)
  if(length(continuous) > 0){
    n_level <- rep(1, length(continuous_only))
    n_level <- c(n_level, fct_n_levels - 1L)
    c_level <- c(0, n_level %>% purrr::accumulate(sum))
    contin_end <- c_level[length(c_level)]
    c_level <- c_level[-length(c_level)]
  }else{
    contin_end <- 0
    n_level <- 0
    c_level <- 0
  }

  b_len_contin <- length(continuous)
  b_len_both <- length(both)
  jags_model <- get_bvs_model_string(family = family,
                                prior_tau_fixed = prior_tau_fixed,
                                flag_spike_slab = flag_spike_slab,
                                flag_covariates_zi = flag_covariates_zi,
                                flag_intercept = flag_intercept,
                                prior_decay_gamma_shape = prior_decay_gamma_shape,
                                b_len_contin = b_len_contin,
                                b_len_both = b_len_both)
  jags_data <- as.list(environment())[c(
    "b_len_contin",
    "n_level",
    "c_level",
    "contin_end",
    "b_len_both",
    "dist",
    "N",
    "idloc",
    "u0",
    "prior_decay_min",
    "prior_decay_max",
    "prior_alpha",
    "prior_pind_a",
    "prior_pind_b",
    "prior_tau_fixed",
    "prior_tau_gamma_shape",
    "prior_tau_gamma_rate",
    "prior_decay_gamma_shape",
    "prior_decay_gamma_rate",
    "prior_r_gamma_shape",
    "prior_r_gamma_rate",
    "prior_tau2.sp_shape",
    "prior_tau2.sp_rate",
    "prior_tau2.e_shape",
    "prior_tau2.e_rate",
    "range_threshold"
  )] %>%
    append(list(
      "XMAT" = data_sf %>%
        build_design_matrix(
          covariates,
          continuous_only = continuous_only,
          intercept = FALSE,
          scale_vars = flag_scale_vars
        ),
      "M" = data %>% nrow()
    ))
  jags_data <- if (family %in% c("binomial",
                                 "zero_inflated_binomial")) {
    jags_data %>%
      append(list(
        "positives" = data %>%
          dplyr::pull(dplyr::all_of(response[1])),
        "total" = data %>%
          dplyr::pull(dplyr::all_of(response[2]))
      ))
  } else if (family %in% c("negative_binomial",
                           "zero_inflated_negative_binomial")) {
    jags_data %>%
      append(list("counts" = data %>%
                    dplyr::pull(dplyr::all_of(response[1]))))
  }

  jags_indicator_varnames <- list("ind_varnames" = continuous,
                                  "indb_varnames" = both)
  jags_inits <- if(is.null(inits)){
    get_bvs_initials(
      family = family,
      n_chains = n_chains,
      contin_end = contin_end,
      b_len_both = b_len_both,
      b_len_contin = b_len_contin,
      N = N,
      prior_tau_fixed = prior_tau_fixed,
      flag_intercept = flag_intercept,
      decay_limits = decay_limits
    )
  }else{
    inits
  }

  fitted_model <- runjags::run.jags(
      model = jags_model,
      data = jags_data,
      inits = jags_inits,
      n.chains = n_chains,
      method = "parallel",
      burnin = burnin,
      sample = n_iter,
      modules = "glm"
    )

  if((!is.null(save_dir))&&(!is.null(save_file))){
    list("model" = fitted_model,
         "indicator_varnames" = jags_indicator_varnames) %>%
      write_simulation(chunk_name = save_file,
                       rmd_name = save_dir)
  }

  for(i in seq_len(n_runs - 1)){
    fitted_model <- runjags::extend.jags(fitted_model, sample = n_iter)
    if((!is.null(save_dir))&&(!is.null(save_file))){
      list("model" = fitted_model,
           "indicator_varnames" = jags_indicator_varnames) %>%
        write_simulation(chunk_name = save_file,
                         rmd_name = save_dir)
    }
  }
  fitted_model
}

get_bvs_initials <-
  function(family,
           n_chains,
           contin_end,
           b_len_both,
           b_len_contin,
           N,
           prior_tau_fixed,
           flag_intercept,
           decay_limits) {
    decay <- c(0.05,0.25,0.75,0.95)*(decay_limits[2]-decay_limits[1]) + decay_limits[1]
    if (family == "binomial") {
      inits <- list(
        list(
          "b" = rnorm(contin_end + 3 * b_len_both),
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(1, b_len_both),
          "a" = c(0.2, 0.4, 0.4),
          "tau2.sp" = 1,
          "decay" = decay[1],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 1,
          ".RNG.name" = "base::Super-Duper"
        ),
        list(
          "b" = rep(0.2, contin_end + 3 * b_len_both),
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(1, b_len_both),
          "a" = c(0.4, 0.3, 0.3),
          "tau2.sp" = 2,
          "decay" = decay[4],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 2,
          ".RNG.name" = "base::Wichmann-Hill"
        ),
        list(
          "b" = rnorm(contin_end + 3 * b_len_both),
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(2, b_len_both),
          "a" = c(0.4, 0.3, 0.3),
          "tau2.sp" = 2,
          "decay" = decay[2],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 3,
          ".RNG.name" = "base::Wichmann-Hill"
        ),
        list(
          "b" = rep(0.2, contin_end + 3 * b_len_both),
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(3, b_len_both),
          "a" = c(0.2, 0.4, 0.4),
          "tau2.sp" = 1,
          "decay" = decay[3],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 4,
          ".RNG.name" = "base::Super-Duper"
        )
      )[1:n_chains]
      if (is.na(prior_tau_fixed)) {
        inits <- inits %>%
          purrr::map_depth(1,
                           ~ .x %>%
                             append(list("tau" = 0.01)))
      }
      if(flag_intercept){
        inits <- inits %>%
          purrr::map_depth(1,
                           ~ .x %>%
                             append(list("intercept" = rnorm(1))))
      }
    } else if (family == "zero_inflated_binomial") {
      inits <- list(
        list(
          "b" = rnorm(contin_end + 3 * b_len_both),
          "b_zi" = rnorm(contin_end + 3 * b_len_both),
          "zero" = rbinom(N, 1, 0.5),
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(1, b_len_both),
          "a" = c(0.2, 0.4, 0.4),
          "tau2.sp" = 1,
          "decay" = decay[1],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 1,
          ".RNG.name" = "base::Super-Duper"
        ),
        list(
          "b" = rep(0.2, contin_end + 3 * b_len_both),
          "b_zi" = rep(0.2, contin_end + 3 * b_len_both),
          "zero" = rbinom(N, 1, 0.2),
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(1, b_len_both),
          "a" = c(0.4, 0.3, 0.3),
          "tau2.sp" = 2,
          "decay" = decay[4],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 2,
          ".RNG.name" = "base::Wichmann-Hill"
        ),
        list(
          "b" = rnorm(contin_end + 3 * b_len_both),
          "b_zi" = rep(0.2, contin_end + 3 * b_len_both),
          "zero" = rbinom(N, 1, 0.8),
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(2, b_len_both),
          "a" = c(0.4, 0.3, 0.3),
          "tau2.sp" = 2,
          "decay" = decay[2],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 3,
          ".RNG.name" = "base::Wichmann-Hill"
        ),
        list(
          "b" = rep(0.2, contin_end + 3 * b_len_both),
          "b_zi" = rnorm(contin_end + 3 * b_len_both),
          "zero" = rbinom(N, 1, 0.5),
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(3, b_len_both),
          "a" = c(0.2, 0.4, 0.4),
          "tau2.sp" = 1,
          "decay" = decay[3],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 4,
          ".RNG.name" = "base::Super-Duper"
        )
      )[1:n_chains]
      if (is.na(prior_tau_fixed)) {
        inits <- inits %>%
          purrr::map_depth(1,
                           ~ .x %>%
                             append(list("tau" = 0.01)))
      }
      if(flag_intercept){
        inits <- inits %>%
          purrr::map_depth(1,
                           ~ .x %>%
                             append(list("intercept" = rnorm(1),
                                         "intercept_zi" = rnorm(1))))
      }
    } else if (family == "negative_binomial") {
      inits <- list(
        list(
          "b" = rnorm(contin_end + 3 * b_len_both),
          "r" = 20,
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(1, b_len_both),
          "a" = c(0.2, 0.4, 0.4),
          "tau2.sp" = 1,
          "decay" = decay[1],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 1,
          ".RNG.name" = "base::Super-Duper"
        ),
        list(
          "b" = rep(0.2, contin_end + 3 * b_len_both),
          "r" = 70,
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(1, b_len_both),
          "a" = c(0.4, 0.3, 0.3),
          "tau2.sp" = 2,
          "decay" = decay[4],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 2,
          ".RNG.name" = "base::Wichmann-Hill"
        ),
        list(
          "b" = rnorm(contin_end + 3 * b_len_both),
          "r" = 100,
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(2, b_len_both),
          "a" = c(0.4, 0.3, 0.3),
          "tau2.sp" = 2,
          "decay" = decay[2],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 3,
          ".RNG.name" = "base::Wichmann-Hill"
        ),
        list(
          "b" = rep(0.2, contin_end + 3 * b_len_both),
          "r" = 200,
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(3, b_len_both),
          "a" = c(0.2, 0.4, 0.4),
          "tau2.sp" = 1,
          "decay" = decay[3],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 4,
          ".RNG.name" = "base::Super-Duper"
        )
      )[1:n_chains]
      if (is.na(prior_tau_fixed)) {
        inits <- inits %>%
          purrr::map_depth(1,
                           ~ .x %>%
                             append(list("tau" = 0.01)))
      }
      if(flag_intercept){
        inits <- inits %>%
          purrr::map_depth(1,
                           ~ .x %>%
                             append(list("intercept" = rnorm(1))))
      }
    }else if (family == "zero_inflated_negative_binomial") {
      inits <- list(
        list(
          "b" = rnorm(contin_end + 3 * b_len_both),
          "b_zi" = rnorm(contin_end + 3 * b_len_both),
          "zero" = rbinom(N, 1, 0.5),
          "r" = 20,
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(1, b_len_both),
          "a" = c(0.2, 0.4, 0.4),
          "tau2.sp" = 1,
          "decay" = decay[1],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 1,
          ".RNG.name" = "base::Super-Duper"
        ),
        list(
          "b" = rep(0.2, contin_end + 3 * b_len_both),
          "b_zi" = rep(0.2, contin_end + 3 * b_len_both),
          "zero" = rbinom(N, 1, 0.2),
          "r" = 70,
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(1, b_len_both),
          "a" = c(0.4, 0.3, 0.3),
          "tau2.sp" = 2,
          "decay" = decay[4],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 2,
          ".RNG.name" = "base::Wichmann-Hill"
        ),
        list(
          "b" = rnorm(contin_end + 3 * b_len_both),
          "b_zi" = rep(0.2, contin_end + 3 * b_len_both),
          "zero" = rbinom(N, 1, 0.8),
          "r" = 100,
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(2, b_len_both),
          "a" = c(0.4, 0.3, 0.3),
          "tau2.sp" = 2,
          "decay" = decay[2],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 3,
          ".RNG.name" = "base::Wichmann-Hill"
        ),
        list(
          "b" = rep(0.2, contin_end + 3 * b_len_both),
          "b_zi" = rnorm(contin_end + 3 * b_len_both),
          "zero" = rbinom(N, 1, 0.5),
          "r" = 200,
          "ind" = rep(1, b_len_contin),
          "pind" = 0.5,
          "indb" = rep(3, b_len_both),
          "a" = c(0.2, 0.4, 0.4),
          "tau2.sp" = 1,
          "decay" = decay[3],
          "u" = rep(0, N),
          "tau2.e" = 1,
          "e" = rep(0, N),
          ".RNG.seed" = 4,
          ".RNG.name" = "base::Super-Duper"
        )
      )[1:n_chains]
      if (is.na(prior_tau_fixed)) {
        inits <- inits %>%
          purrr::map_depth(1,
                           ~ .x %>%
                             append(list("tau" = 0.01)))
      }
      if(flag_intercept){
        inits <- inits %>%
          purrr::map_depth(1,
                           ~ .x %>%
                             append(list("intercept" = rnorm(1),
                                         "intercept_zi" = rnorm(1))))
      }
    } else{
      stop("No initials for this family provided.")
    }
    inits
  }


#' Get decay limits
#'
#' @param dist a distance matrix
#' @param threshold threshold of `threshold = exp(-practical_range)`
#'
#' @return c(min_decay, max_decay)
#' @export
#'
#' @examples
#' \dontrun{
#' Helper::get_decay_limits(spBayes::iDist(tibble::tibble(x = c(1:5), y = c(1:5))))
#' }
get_decay_limits <- function(dist, threshold = 0.01) {
  stopifnot(isSymmetric(dist))
  if (min(dist + diag(nrow(dist))) == 0)
    stop("Distances are not all > 0.")
  c(-log(threshold) / max(dist),-log(threshold) / min(dist[dist > 0]))
}


#' Build Design Matrix for Bayesian variable selection
#' @description Build a design matrix for Bayesian variable selection implemented in JAGS.
#'
#' @param data A data.frame object containing all `var_names`
#' @param var_names The variable names to use as covariates
#' @param continuous_only Continuous only variables
#' @param intercept Flag: Add an intercept
#' @param scale_vars Flag: scale continuous variables before building the desing matrix
#'
#' @return A design matrix of type matrix
#' @export
#'
#' @examples
build_design_matrix <-
  function(data,
           var_names,
           continuous_only = character(),
           intercept = TRUE,
           scale_vars = TRUE) {
    stopifnot(
      inherits(data, "sf"),
      purrr::is_character(var_names),
      purrr::is_character(continuous_only),
      purrr::is_scalar_logical(intercept),
      purrr::is_scalar_logical(scale_vars),
      length(var_names) > 0 | intercept,
      all(var_names %in% colnames(data)),
      all(continuous_only %in% var_names)
    )

    scale_true <- function(x, scale_vars){
      if(scale_vars){
        x <- x %>% scale()
        x[,1]
      }else{
        x
      }
    }

    data <- data %>%
      sf::st_drop_geometry()
    if(any(is.na(data %>% dplyr::select(dplyr::all_of(var_names))))){
      stop("Some covariates in data are NA. Drop them to get a result.")
    }

    fct_names_in_data <- data %>%
      purrr::map(is.factor) %>%
      purrr::keep(.p = identity) %>%
      names()
    fct_names <- intersect(var_names, fct_names_in_data)
    continuous_only <- setdiff(continuous_only, fct_names)

    both <-
      var_names[!var_names %in% union(fct_names, continuous_only)]

    design_both <- if (length(both) > 0) {
      both %>%
        purrr::map_dfc(
          ~ data %>%
            dplyr::pull(.x) %>%
            scale_true(scale_vars) %>%
            get_quantile_dummy_matrix(var_name = .x, drop_orig = FALSE)
        )
    } else{
      NULL
    }

    design_factor <- if (length(fct_names) > 0) {
      n_levels <- fct_names %>%
        purrr::map_int(~ data %>%
                         dplyr::pull(.x) %>%
                         levels() %>%
                         length())
      fct_names <- fct_names[order(n_levels)]
      fct_names %>%
        purrr::map_dfc( ~ data %>%
                          dplyr::pull(.x) %>%
                          as_dummy(var_name = .x))
    }else{
      NULL
    }

    design_continuous <- if (length(continuous_only) > 0) {
      data %>%
        dplyr::select(dplyr::all_of(continuous_only)) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(),
                                    ~ .x %>% scale_true(scale_vars)))
    } else{
      NULL
    }


    design_intercept <- if (intercept) {
      tibble::tibble(intercept = rep(1, nrow(data)))
    } else{
      NULL
    }

    dplyr::bind_cols(
      design_intercept,
      design_continuous,
      design_factor,
      design_both
    ) %>%
      as.matrix()
  }

#' Convert factors to dummy
#'
#' @param v A factor.
#' @param var_name A variable name (optional).
#' @param drop_base Logical indicating whether the base level (first level) should be dropped.
#' @param drop_orig Logical indicating whether the original factor should be dropped.
#'
#' @return A tibble containing dummy variables
#' @export
#'
#' @examples
#' \dontrun{
#' my_factor <- factor(c("a 2","b","a 2"))
#' as_dummy(my_factor)
#' my_factor %>% as_dummy()
#' my_factor %>% as_dummy("fancy_name")
#' as_dummy(my_factor, drop_base = FALSE)
#' as_dummy(my_factor, drop_orig = FALSE)
#' }
as_dummy <- function(v,
                     var_name = NA_character_,
                     drop_base = TRUE,
                     drop_orig = TRUE) {
  #hugely borrowed from varhandle package.
  stopifnot(is.factor(v),
            purrr::is_scalar_character(var_name))
  if (is.na(var_name)) {
    var_name <- as.list(match.call())$v %>% as.character
  }
  v_orig <- v
  v_levels <- levels(v)
  v <- as.character(v)
  for (i in 1:length(v_levels)) {
    assign(x = paste("v", i, sep = ""),
           value = as.numeric(v ==
                                v_levels[i]))
  }
  df <- eval(parse(text = paste(
    "cbind(",
    paste("v",
          1:i, sep = "", collapse = ", "),
    ")",
    collapse = "",
    sep = ""
  )))
  factor_levels <- gsub("\\s+", "_", gsub("^\\s+|\\s+$",
                                          "", v_levels))
  factor_levels[which(factor_levels == "")] <- "BLANK"
  colnames(df) <- paste(var_name,
                        "factorlevel",
                        factor_levels,
                        "baselevel",
                        factor_levels[1],
                        sep = "_")
  df <- df %>%
    tibble::as_tibble()
  if (drop_base) {
    df <- df %>%
      dplyr::select(-1)
  }
  if (!drop_orig) {
    df <- dplyr::bind_cols(tibble::tibble(!!var_name := v_orig),
                           df)
  }
  df
}

#' Get quantile factor
#'
#' @param v A integer or double vector to be factorized
#' @param probs Probabilities of the quantiles
#' @param quantiles The quantiles (either this or probs need to be provided)
#' @param digits Integer digits
#'
#' @return A vector as factor
#' @export
#'
#' @examples
#' data.frame(v = 0:10,
#' v_f = as_quantile_factor(0:10, c(0.25,0.5,0.75)),
#' v_q = as_quantile_factor(0:10, probs = NULL, quantiles = c(2,4,6,8)))
#' as_quantile_factor(0:10, c(0.25,0.5,0.75)) %>% summary()
as_quantile_factor <- function(v, probs = c(.33, .66), quantiles = NULL, digits = 2L){
  stopifnot(is.integer(v) | is.double(v),
            length(quantiles) == 0 | length(probs) == 0)
  if(is.null(quantiles)){
    quantiles <- stats::quantile(v, probs = probs) %>% unname()
  }
  v_f <- rep(NA_character_,length(v))
  labels <- rep(NA_character_,length(quantiles))
  labels[1] <- paste0("l", round(quantiles[1], digits))
  v_f <- ifelse(v < quantiles[1], labels[1], v_f)
  for(i in 2:(length(quantiles))){
    labels[i] <- paste0(round(quantiles[i-1], digits), "to", round(quantiles[i], digits))

    v_f <- ifelse(v >= quantiles[i-1] & v < quantiles[i],
                  labels[i], v_f)
  }
  labels[i+1] <- paste0("geq", round(quantiles[i], digits))
  v_f <- ifelse(v >= quantiles[i], labels[i+1], v_f)
  v_f %>% factor(levels = labels)
}

#' Transform vector to quantile factor using a reference (estimation) vector which is used for the quantiles.
#'
#' @param v_prediction A vector in the prediction data
#' @param v_estimation A vector in the estimation data
#' @param probs The probabilities of the quantiles for the estimation data
#' @param digits The digits cutoff for the factor labels
#'
#' @return The prediction vector as factor
#' @export
#'
#' @examples
as_quantile_factor_prediction <- function(v_prediction, v_estimation, probs, digits = 2L){
  quantiles <- stats::quantile(v_estimation, probs = probs) %>% unname()
  as_quantile_factor(v = v_prediction, probs = NULL, quantiles = quantiles, digits = digits)
}

#' Get the quantile dummy matrix of a numeric vector
#'
#' @param v The numeric or integer vector to transform to a quantile dummy matrix
#' @param probs The probabilities of the quantiles (default = c(.33,.66))
#' @param drop_base Logical indicating whether the base level (first quantile) should be dropped.
#' @param drop_orig Logical indicating whether the original vector should be dropped.
#' @param var_name The variable name
#'
#' @return A matrix with the dummies as columns
#' @export
#'
#' @examples
#' \dontrun{
#' a <- 1:10
#' get_quantile_dummy_matrix(a)
#' get_quantile_dummy_matrix(a, "b")
#' get_quantile_dummy_matrix(a, probs = 0.2*c(1:4))
#' get_quantile_dummy_matrix(a, drop_base = FALSE)
#' get_quantile_dummy_matrix(a, drop_orig = FALSE)
#' get_quantile_dummy_matrix(a,"b", drop_orig = FALSE)
#'
#' b <- 1:3
#' get_quantile_dummy_matrix(b)
#' }
get_quantile_dummy_matrix <-
  function(v,
           var_name = NA_character_,
           probs = c(.33, .66),
           drop_base = TRUE,
           drop_orig = TRUE) {
    is_probabilities(probs)
    is_flag(drop_base)
    is_flag(drop_orig)
    stopifnot(purrr::is_scalar_character(var_name))
    if (is.na(var_name)) {
      var_name <- as.list(match.call())$v %>% as.character
    }
    v %>%
      as_quantile_factor(probs = probs) %>%
      as_dummy(var_name = var_name,
               drop_base = drop_base,
               drop_orig = drop_orig) %>%
      conditional_mutate(!!var_name:= v, condition = !drop_orig)
  }


decrypt_factorlevel <- function(string){
  stopifnot(purrr::is_scalar_character(string))
  string %>%
    stringr::str_replace("^.*factorlevel_", "") %>%
    stringr::str_replace("_baselevel_.*$", "") %>%
    decrypt_level()
}


decrypt_baselevel <- function(string) {
  stopifnot(purrr::is_scalar_character(string))
  if(stringr::str_detect(string, "_baselevel_")) {
    string %>%
      stringr::str_replace("^.*_baselevel_", "") %>%
      decrypt_level()
  }else{
    ""
  }
}

decrypt_level <- function(string){
  stopifnot(purrr::is_scalar_character(string))
  string %>%
    stringr::str_replace("geq(?=\\d)", "\U2265 ") %>%
    stringr::str_replace("leq(?=\\d)", "\U2264 ") %>%
    stringr::str_replace("l(?=\\d)", "< ") %>%
    stringr::str_replace("g(?=\\d)", "> ") %>%
    stringr::str_replace("(?<=\\d)to(?=\\d)", " - ") %>%
    stringr::str_replace_all("_", " ")
}
