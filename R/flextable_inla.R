treat_summary_fixed <- function(summary_fixed,
                                flag_drop_intercept = FALSE,
                                model_name){
  purrr::is_scalar_character(model_name)
  is_data(summary_fixed)
  if(flag_drop_intercept){
    summary_fixed <- summary_fixed %>%
      dplyr::filter(!term %in% c("intercept", "zi_intercept"))
  }
  summary_fixed <- summary_fixed %>%
    dplyr::mutate(estimate_or_rounded = sprintf("%.2f", round(estimate_or, 2)),
           ci_or_rounded = paste0("(", sprintf("%.2f",  round(q0_025_or,2)),
                                  "; ",  sprintf("%.2f",  round(q0_975_or,2)),
                                  ")"),
           Estimate = paste(estimate_or_rounded, ci_or_rounded)) %>%
    dplyr::select(term, Estimate)

  fcts <- summary_fixed$term[summary_fixed$term %>%
                               stringr::str_detect("_geq")] %>%
    unique()
  fct_names <- fcts %>% stringr::str_extract("^.*_") %>%
    unique()
  for(fct_name in fct_names){
    ind <- which(summary_fixed$term %>%
                   stringr::str_detect(fct_name))[1]
    levels <- fcts[fcts %>%
                     stringr::str_detect(fct_name)] %>%
      stringr::str_replace_all(fct_name, "") %>%
      paste0(collapse = "") %>%
      stringr::str_extract_all("[\\d\\.]*") %>%
      unlist() %>%
      unique()
    levels <- levels[levels != ""] %>%
      as.numeric() %>%
      sort()
    baselevel <- fcts[fcts %>%
           stringr::str_detect(fct_name)] %>%
      stringr::str_extract("(?<=_baselevel_).*$")
    summary_fixed <- summary_fixed %>%
      tibble::add_case(term = fct_name %>%
                         stringr::str_replace("_$", ""),
                       .before = ind) %>%
      tibble::add_case(term = paste0("\\quad _factorlevel_",
                                     baselevel,
                                     "_baselevel_",
                                     baselevel),
                       Estimate  = as.character(1),
                       .after = ind) %>%
      dplyr:: mutate(term = dplyr::case_when(
        term %>% stringr::str_detect(fct_name) ~  term %>%
          stringr::str_replace_all(fct_name, "\\\\quad "),
        TRUE~ term))
  }
  summary_fixed %>%
    dplyr::mutate(term = dplyr::case_when(term %>%
                                            stringr::str_detect("geq.*l.*$")
                            ~ term %>%
                              stringr::str_replace("geq", "") %>%
                              stringr::str_replace("l", " - "),
                            term %>%
                              stringr::str_detect("geq.*$")
                            ~ term %>%
                              stringr::str_replace("geq", "\\\\geq"),
                            TRUE ~ term))%>%
    dplyr::rename(!!model_name := Estimate)
}

treat_summary_fixed2 <- function(summary_fixed,
                                flag_drop_intercept = FALSE,
                                model_name,
                                flag_summary_fixed_or){
  purrr::is_scalar_character(model_name)
  is_data(summary_fixed)
  if(flag_drop_intercept){
    summary_fixed <- summary_fixed %>%
      dplyr::filter(!term %in% c("intercept", "zi_intercept"))
  }
  summary_fixed <- summary_fixed %>%
    dplyr::select(
      term,
      all_of(paste0(c("estimate", "q0_5", "q0_025", "q0_975"),
                    ifelse(flag_summary_fixed_or, "_or", "")))) %>%
    conditional_rename_with(
      .cols = - term,
      .fn = ~ .x %>% stringr::str_replace("_or$", ""),
      condition = flag_summary_fixed_or)

  fcts <- summary_fixed$term[summary_fixed$term %>%
                               stringr::str_detect("_factorlevel_")] %>%
    unique()
  fct_names <- fcts %>% stringr::str_extract("^.*(?=_factorlevel_)") %>%
    unique()
  for(fct_name in fct_names){
    ind <- which(summary_fixed$term %>%
                   stringr::str_detect(fct_name))[1]
    levels <- fcts[fcts %>%
                     stringr::str_detect(fct_name)] %>%
      stringr::str_replace_all(fct_name, "") %>%
      paste0(collapse = "") %>%
      stringr::str_extract_all("[\\d\\.]*") %>%
      unlist() %>%
      unique()
    levels <- levels[levels != ""] %>%
      as.numeric() %>%
      sort()
    baselevel <- fcts[fcts %>%
                        stringr::str_detect(fct_name)][1] %>%
      stringr::str_extract("(?<=_baselevel_).*$")
    summary_fixed <- summary_fixed %>%
      tibble::add_case(term = fct_name %>%
                         stringr::str_replace("_$", ""),
                       .before = ind) %>%
      tibble::add_case(term = paste0("\t _factorlevel_",
                                     baselevel,
                                     "_baselevel_",
                                     baselevel),
                       estimate = ifelse(flag_summary_fixed_or, 1, 0),
                       q0_5  = ifelse(flag_summary_fixed_or, 1, 0),
                       q0_025 = NA_real_,
                       q0_975 = NA_real_,
                       .after = ind) %>%
      dplyr:: mutate(
        term = dplyr::case_when(term %>%
                                  stringr::str_detect(paste0(fct_name, "(?=_factorlevel_)"))
                                ~  term %>%
          stringr::str_replace_all(fct_name, "\t "),
        TRUE~ term))
  }
  summary_fixed %>%
    dplyr::mutate(
      term = dplyr::case_when(
        term %>% stringr::str_detect("_factorlevel_")
        ~ paste0("\t ",
                 term %>%
          purrr::map_chr(decrypt_factorlevel)),
        TRUE ~ term
      )
    ) %>%
    dplyr::rename_with(.cols = dplyr::all_of(
      c("estimate", "q0_5", "q0_025", "q0_975")),
                       .fn = ~ paste0(model_name, "_", .))
}


treat_summary_hyperpar <- function(summary_hyperpar, model_name){
  if (!is.null(summary_hyperpar)) {
    summary_hyperpar %>%
      dplyr::filter(term != "spatial_range [deg]") %>%
      dplyr::group_by(term) %>%
      dplyr::mutate(
        Estimate = dplyr:: case_when(
          term != "spatial_range" ~  paste0(
            sprintf("%.2f", round(estimate, 2)),
            " (",
            sprintf("%.2f", round(q0_025, 2)),
            "; ",
            sprintf("%.2f", round(q0_975, 2)),
            ")"
          ),
          term == "spatial_range" ~  paste0(
            sprintf("%.0f", round(estimate, 2)),
            " (",
            sprintf("%.0f", round(q0_025, 2)),
            "; ",
            sprintf("%.0f", round(q0_975, 2)),
            ")"
          ),
        ),
        term = dplyr::case_when(
          term == "spatial_variance" ~ "\\sigma_{spatial}^2",
          term == "non_spatial_variance" ~ "\\sigma_{non-spatial}^2",
          term == "spatial_range" ~ "Range (km)",
          term == "zero_probability" ~ "\\text{Mixing probability }\\theta",
          TRUE ~ term
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(term) %>%
      dplyr::select(term, Estimate) %>%
      dplyr::rename(!!model_name := Estimate)
  } else{
    tibble::tibble(term = character(0))
  }
}

treat_summary_hyperpar2 <- function(summary_hyperpar, model_name){
  if (!is.null(summary_hyperpar)) {
    summary_hyperpar %>%
      dplyr::filter(term != "spatial_range [deg]") %>%
      dplyr::mutate(
        term = dplyr::case_when(
          term == "spatial_variance" ~ "\\sigma_\\text{spatial}^2",
          term == "non_spatial_variance" ~ "\\sigma_\\text{non-spatial}^2",
          term == "spatial_range" ~ "Range (km)",
          term == "zero_probability" ~ "\\text{Mixing probability }\\theta",
          TRUE ~ term
        )
      ) %>%
      dplyr::arrange(term) %>%
      dplyr::select(term, estimate, q0_5, q0_025, q0_975) %>%
      dplyr::rename_with(.cols = dplyr::all_of(
        c("estimate", "q0_5", "q0_025", "q0_975")),
        .fn = ~ paste0(model_name, "_", .))
    }else{
    tibble::tibble(term = character(0))
  }
}

treat_summary_model <- function(summary_model, measures = NULL, model_name){
  summary_model %>%
    tidyr::pivot_longer(1:4, names_to = "term", values_to = model_name) %>%
    dplyr::filter(term %in% measures) %>%
    dplyr::mutate(!!model_name := round(.data[[model_name]], 1) %>% as.character(),
                  term = dplyr::case_when(term == "dic" ~ "DIC",
                                          term == "waic" ~ "WAIC",
                                          term == "cpo_failures" ~ "CPO failures",
                                          term == "log_score" ~ "Log score",
                                          TRUE ~ term))
}


get_model_name <- function(list_model_fit, i){
  if(is.null(names(list_model_fit))){
    paste("Model", i)
  }else{
    name <- names(list_model_fit)[i]
    ifelse(nchar(name) > 0, name, paste("model", i))
  }
}

#' Treat model summaries
#' @describeIn flextable_inla_binomial
#'
#' @return joined summaries of all the models in the list
#' @export
treat_model_summaries <- function(list_model_fit,
                                  term_order = NULL,
                                  flag_drop_intercept = FALSE,
                                  translate_covariate_names_type = NULL,
                                  measures = NULL){

  list_summary_fixed <- list_model_fit %>%
    purrr::imap( ~ .x %>%
                   purrr::pluck("summary_fixed") %>%
                   treat_summary_fixed(model_name = get_model_name(list_model_fit, .y),
                                       flag_drop_intercept = flag_drop_intercept))
  custom_join <- function(x,y) dplyr::full_join(x = x, y = y, by = "term")
  joined_summaries_fixed <- purrr::reduce(list_summary_fixed, custom_join)
  if(!is.null(term_order)){
    ind_coef <- joined_summaries_fixed$term %>%
      stringr::str_detect("^\\\\quad", negate = TRUE)
    ind_reorder <- joined_summaries_fixed$term[ind_coef] %>%
      tolower() %>%
      match(term_order)
    if(any(is.na(ind_reorder))){
      stop("The provided `term_order` does not include all fixed effects.")
    }
    ind_order <- ind_coef %>%
      cumsum()
    ind_order_new <- rep(NA_real_,length(ind_order))
    for(i in seq_along(ind_reorder)){
      ind_order_new[ind_order == i] <- ind_reorder[i]
    }
    joined_summaries_fixed <- joined_summaries_fixed[order(ind_order_new),]
  }

  if (!is.null(translate_covariate_names_type)) {
    joined_summaries_fixed <- joined_summaries_fixed %>%
      dplyr::mutate(term = term %>% purrr::map_chr(~ {
        if(.x %>% stringr::str_detect("\\\\quad", negate = TRUE) ){
          .x %>%
            translate_covariate_names(translate_covariate_names_type)
        }else{
          .x
        }}
      ))
  }

  list_summary_hyperpar <- list_model_fit %>%
    purrr::imap( ~ treat_summary_hyperpar(summary_hyperpar = .x %>%
                                            purrr::pluck("summary_hyperpar"),
                                          model_name = get_model_name(list_model_fit, .y)))
  joined_summaries_hyperpar <- purrr::reduce(list_summary_hyperpar, custom_join)
  model_summaries <- dplyr::bind_rows(joined_summaries_fixed,
            joined_summaries_hyperpar)
  if(!is.null(measures)){
    model_summaries <- model_summaries %>%
      dplyr::bind_rows(
        list_model_fit %>%
          purrr::imap( ~ treat_summary_model(summary_model = .x %>%
                                               purrr::pluck("summary_model"),
                                             measures = measures,
                                             model_name = get_model_name(list_model_fit, .y))) %>%
          purrr::reduce(custom_join))
  }
  model_summaries
}

treat_model_summaries2 <- function(list_model_fit,
                                  term_order = NULL,
                                  flag_drop_intercept = FALSE,
                                  translate_covariate_names_type = NULL,
                                  measures = NULL,
                                  flag_summary_fixed_or){
  list_summary_fixed <- tibble::tibble(
    index = 1:length(list_model_fit),
    model = list_model_fit) %>%
    purrr::pmap(function(index, model){
      model %>%
        purrr::pluck("summary_fixed") %>%
        treat_summary_fixed2(model_name = get_model_name(list_model_fit,index),
                             flag_drop_intercept = flag_drop_intercept,
                             flag_summary_fixed_or)})
  custom_join <- function(x,y) dplyr::full_join(x = x, y = y, by = "term")
  joined_summaries_fixed <- purrr::reduce(list_summary_fixed, custom_join)
  if(!is.null(term_order)){
    ind_coef <- joined_summaries_fixed$term %>%
      stringr::str_detect("^\\t", negate = TRUE)
    ind_reorder <- joined_summaries_fixed$term[ind_coef] %>%
      tolower() %>%
      match(term_order)
    if(any(is.na(ind_reorder))){
      stop("The provided `term_order` does not include all fixed effects.")
    }
    ind_order <- ind_coef %>%
      cumsum()
    ind_order_new <- rep(NA_real_,length(ind_order))
    for(i in seq_along(ind_reorder)){
      ind_order_new[ind_order == i] <- ind_reorder[i]
    }
    joined_summaries_fixed <- joined_summaries_fixed[order(ind_order_new),]
  }

  if (!is.null(translate_covariate_names_type)) {
    joined_summaries_fixed <- joined_summaries_fixed %>%
      dplyr::mutate(term = term %>% purrr::map_chr(~ {
        if(.x %>% stringr::str_detect("^\\t", negate = TRUE) ){
          .x %>%
            translate_covariate_names(translate_covariate_names_type)
        }else{
          .x
        }}
      ))
  }

  list_summary_hyperpar <- tibble::tibble(
    index = 1:length(list_model_fit),
    model = list_model_fit) %>%
    purrr::pmap(function(index, model){
      treat_summary_hyperpar2(summary_hyperpar = model %>%
                                  purrr::pluck("summary_hyperpar"),
                                model_name =  get_model_name(list_model_fit, index))})
  joined_summaries_hyperpar <- purrr::reduce(list_summary_hyperpar, custom_join)
  model_summaries <- dplyr::bind_rows(joined_summaries_fixed,
                                      joined_summaries_hyperpar)
  if(!is.null(measures)){
    model_summaries <- model_summaries %>%
      dplyr::bind_rows(
        tibble::tibble(
          index = 1:length(list_model_fit),
          model = list_model_fit) %>%
          purrr::pmap(function(index, model){
            treat_summary_model(summary_model = model%>%
                                  purrr::pluck("summary_model"),
                                measures = measures,
                                model_name = get_model_name(list_model_fit, index))
            }) %>%
          purrr::reduce(custom_join))
          }
          model_summaries
  }


#' A flextable summary of inla output
#' `r lifecycle::badge("superseded")`, use `flextable_inla_binomial` instead.
#' @param list_model_fit list of model fits of fit_inla
#' @param term_order Term ordering of the fixed effects
#' @param flag_drop_intercept Flag if intercept should be dropped
#' @param translate_covariate_names_type Reference to a xlsx list with long variable names
#' @param measures One of "dic", "log_score", "cpo", or "cpo_failures", "waic", or several
#'
#' @return a `flextable` with neat coefficients
#' @export
flextable_inla_binomial <- function(list_model_fit,
                                    term_order = NULL,
                                    flag_drop_intercept = FALSE,
                                    translate_covariate_names_type = NULL,
                                    measures = NULL){
  stopifnot(is.list(list_model_fit),
            length(list_model_fit) > 0,
            is.null(measures) ||
              is.character(measures) &&
              measures %in% c("dic", "waic", "cpo_failures", "log_score"))
  if(!is.null(names(list_model_fit))){
    stop("Input to flextable_inla_binomial must be a list of models. You probably forgot to wrap the model in list(model).", call. = FALSE)
  }
  is_flag(flag_drop_intercept)
  treated_model_summaries <- list_model_fit %>%
    treat_model_summaries(term_order = term_order,
                          measures = measures,
                          flag_drop_intercept = flag_drop_intercept,
                          translate_covariate_names_type = translate_covariate_names_type)
  tb <- treated_model_summaries %>%
    flextable::flextable()

  if(any(treated_model_summaries$term %>% stringr::str_detect("\\\\"))){
    tb <- tb %>%
      flextable::mk_par(j = "term",
                        i = ~ term %>% stringr::str_detect("\\\\"),
                        value = flextable::as_paragraph(flextable::as_equation(term)))
  }
  tb
}


#' A flextable summary of inla output
#' `r lifecycle::badge("experimental")`
#' @param list_model_fit
#' @param term_order link to look up table
#' @param flag_drop_intercept  TRUE or FALSE
#' @param translate_covariate_names_type link to look up table
#' @param measures Goodness of fit measures, one of "dic", "waic", "cpo_failures", "log_score"
#' @param central_value  Either "mean" or "median"
#' @param digits_round Integer
#' @param digits_display Integer
#' @param flag_summary_fixed_or TRUE or FALSE
#'
#' @return A `flextable` with neat coefficients
#' @export
#'
#' @examples
#' sf_gambia_with_factors <- sf_gambia %>%
#' dplyr::mutate(poverty = rep(1:5, 13) %>%
#'                 factor(labels = c("poorest", "poor", "middle", "wealthy", "wealthiest"))) %>%
#'   dplyr::mutate(netuse= as_quantile_factor(netuse))
#' fta <- fit_inla2(sf_gambia,
#'                  family = "binomial",
#'                  response = c("positives", "total"),
#'                  covariates = c("netuse"),
#'                  flag_intercept = TRUE,
#'                  flag_spatial_re = FALSE,
#'                  flag_non_spatial_re = TRUE)
#' ft <- fit_inla2(sf_gambia_with_factors,
#'                 family = "binomial",
#'                 response = c("positives", "total"),
#'                 covariates = c("netuse", "poverty"),
#'                 flag_intercept = TRUE,
#'                 flag_spatial_re = FALSE,
#'                 flag_non_spatial_re = TRUE)
#' list("aha" = ft,
#'      "oho" = fta) %>% flextable_inla_binomial2()
flextable_inla_binomial2 <- function(list_model_fit,
                                    term_order = NULL,
                                    flag_drop_intercept = FALSE,
                                    translate_covariate_names_type = NULL,
                                    measures = NULL,
                                    central_value = "mean",
                                    digits_round = 2L,
                                    digits_display = 2L,
                                    flag_summary_fixed_or = TRUE){
  stopifnot(is.list(list_model_fit),
            length(list_model_fit) > 0,
            is.null(measures) ||
              is.character(measures) &&
              measures %in% c("dic", "waic", "cpo_failures", "log_score"),
            central_value %in% c("mean", "median"),
            purrr::is_scalar_integer(digits_round),
            purrr::is_scalar_integer(digits_display),
            digits_display <= digits_round)
  is_flag(flag_summary_fixed_or)
  if(!all(c("fit", "formula", "family", "stack") %in% names(list_model_fit[[1]]))){
    stop("Input to flextable_inla_binomial must be a list of models. You probably forgot to wrap the model in list(model).", call. = FALSE)
  }
  is_flag(flag_drop_intercept)
  treated_model_summaries <- list_model_fit %>%
    treat_model_summaries2(term_order = term_order,
                          measures = measures,
                          flag_drop_intercept = flag_drop_intercept,
                          translate_covariate_names_type = translate_covariate_names_type,
                          flag_summary_fixed_or)
  tb <- treated_model_summaries %>%
    flextable::flextable(col_keys = c("term",
                                      paste0("m", 1:length(list_model_fit)))) %>%
    flextable::set_header_labels(
      values = purrr::map(1:length(list_model_fit),
                          ~ get_model_name(list_model_fit, .x)) %>%
                      purrr::set_names(paste0("m", 1:length(list_model_fit)))
    )
  for(i in 1:length(list_model_fit)){
    model_name <- get_model_name(list_model_fit, i)
    tb <- tb %>%
      flextable::mk_par(j = paste0("m", i),
             value = flextable::as_paragraph(
               ifelse(!is.na(.data[[
                 paste0(model_name, "_", switch(central_value,
                                                "mean" = "estimate",
                                                "median" = "q0_5"))]]),
                 sprintf(paste0("%.", digits_display, "f"),
                         round(.data[[
                           paste0(model_name, "_", switch(central_value,
                                                          "mean" = "estimate",
                                                          "median" = "q0_5"))]],
                           digits_round)),
                 ""),
               ifelse(!is.na(.data[[paste0(model_name, "_", "q0_025")]]),
                      paste0(" (",
                      sprintf(paste0("%.", digits_display, "f"),
                              round(.data[[
                                paste0(model_name, "_", "q0_025")]],
                                digits_round)),
                      "; ",
                      sprintf(paste0("%.", digits_display, "f"),
                              round(.data[[
                                paste0(model_name, "_", "q0_975")]],
                                digits_round)),
                      ")"),
                      "")
               ))
  }

  if(any(treated_model_summaries$term %>% stringr::str_detect("\\\\"))){
    tb <- tb %>%
      flextable::mk_par(j = "term",
                        i = ~ term %>%
                          stringr::str_detect("\\\\"),
                        value = flextable::as_paragraph(flextable::as_equation(term)))
  }
  tb
}
