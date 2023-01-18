#' Get the model with the highes posterior density
#'
#' @param runjags_result Result from a runjags::run.jags call.
#' @param discard_first A list of continous only and continuous/categorical varnames.
#'
#' @return A list with the best 6 models $best_models and
#' @export
#'
#' @examples
get_bvs_hpd_model <-
  function(runjags_result, discard_first = NA_integer_, model_name = "model1") {
    if (class(runjags_result) != "list" ||
        length(runjags_result) != 2 ||
        class(runjags_result$model) != "runjags") {
      stop(
        "Runjags_result must be a list of length 2 with $model containing the model result and $indicator_varnames containig the variable names for continuous only and continuous or categorical."
      )
    }
    if (!purrr::is_scalar_integer(discard_first)) {
      stop("discard_first must be a integer or NA_integer_.")
    }
    mcmc <- coda::as.mcmc.list(runjags_result$model$mcmc, vars = "ind")
    if (is.na(discard_first)) {
      discard_first <- 1
    }
    nrow_mcmc <- nrow(mcmc[[1]])
    mcmc <- mcmc %>%
      purrr::map_dfr( ~ .x %>%
                        tibble::as_tibble() %>%
                        dplyr::slice(discard_first:nrow_mcmc) %>%
                        dplyr::select(matches("^ind(\\[|$)"), matches("^indb(\\[|$)"))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("indb"), ~ (.x - 1)))
    inclusion_probability_table <- mcmc %>%
      summarize(across(everything(), ~ sum(.x == 0) / n())) %>%
      pivot_longer(cols = everything(),
                   names_to = "term",
                   values_to = paste0(model_name, "_exclusion")) %>%
      full_join(mcmc %>%
               summarize(across(everything(), ~ sum(.x == 1) / n())) %>%
               pivot_longer(cols = everything(),
                            names_to = "term",
                            values_to = paste0(model_name, "_continuous")),
               by = "term") %>%
      full_join(mcmc %>%
                  summarize(across(starts_with("indb"), ~ sum(.x == 2) / n())) %>%
                  pivot_longer(cols = everything(),
                               names_to = "term",
                               values_to = paste0(model_name, "_categorical")),
                by = "term") %>%
      mutate(term = unlist(runjags_result$indicator_varnames))

    mcmc <- mcmc %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tidyr::unite("model", dplyr::everything()) %>%
      dplyr::mutate(model = model %>% as.factor())
    best_models <- mcmc %>%
      janitor::tabyl(model, prop = TRUE) %>%
      dplyr::arrange(-percent) %>%
      head()
    inds <- best_models$model[1] %>%
      stringr::str_split("_") %>%
      unlist() %>%
      as.numeric()



    bvs_hdp_output <- list(
      "best_models" = best_models,
      "best_model_table" = tibble::tibble(variable = unlist(runjags_result$indicator_varnames),
                                          selected = ifelse(inds == 0, "excluded", ifelse(inds == 1, "continuous", "categorical"))),
      "inclusion_probability_table" = inclusion_probability_table,
      "parameters" = list(
        "continous" = unlist(runjags_result$indicator_varnames)[inds == 1] %>% unname(),
        "categorical" = unlist(runjags_result$indicator_varnames)[inds == 2] %>% unname()
      )
    )

    proba <- bvs_hdp_output$best_models$percent[1]
    param <- bvs_hdp_output$parameters
    text <- if(sum(lengths(param))== 0){
      paste0("The model with the highest posterior probability (",
             as_percent_chr(proba),
             ") for ",
             model_name,
             " is a model with no covariates.")
    }else if(length(param$continous) == 0) {
      paste0("The model with the highest posterior probability (",
             as_percent_chr(proba),
             ") for ",
             model_name,
             " is a model with ",
             knitr::combine_words(param$categorical),
             " as categorical variable",
             ifelse(length(param$categorical) > 1,"s", ""),
             ".")
    }else if(length(param$categorical) == 0) {

      paste0("The model with the highest posterior probability (",
             as_percent_chr(proba),
             ") for ",
             model_name,
             " is a model with ",
             knitr::combine_words(param$continous),
             " as continous variable",
             ifelse(length(param$continous) > 1,"s", ""),
             ".")
    }else{
      paste0("The model with the highest posterior probability (",
             as_percent_chr(proba),
             ") for ",
             model_name,
             " is a model with ",
             knitr::combine_words(param$continous),
             " as continous variable",
             ifelse(length(param$continous) > 1,"s", ""),
             " and ",
             knitr::combine_words(param$categorical),
             " as categorical variable",
             ifelse(length(param$categorical) > 1,"s", ""),
             ".")
    }
    bvs_hdp_output %>%
      append(list("text" = text))
  }

#' Flextable fo inclusion frequencies of JAGS Bayesian variable selection
#'
#' @param bvs_results_list A list from a jags bvs algorithm
#' @param discard_first Integer of the first number of iterations to drop per bvs_result
#' @param model_names The name of each model
#' @param term_order The default order of the terms
#'
#' @return A neat flextable with the inclusion frequencies
#' @export
flextable_bvs_frequencies <- function(bvs_results_list, discard_first, model_names, term_order = NULL){
  stopifnot(length(discard_first) == length(bvs_results_list),
            length(discard_first) == length(bvs_results_list),
            purrr::is_character(model_names),
            purrr::is_integer(discard_first))
  term_join <- function(x,y) dplyr::full_join(x, y, by = "term")
  frequency_table <- bvs_results_list %>%
    purrr::imap(
      ~ .x %>%
        get_bvs_hpd_model(discard_first = discard_first[.y],
                          model_name = model_names[.y]) %>%
        purrr::pluck("inclusion_probability_table")
    ) %>%
    purrr::reduce(term_join)
  if(!is.null(term_order)){
    order <- match(frequency_table$term, term_order)
    if(any(is.na(order))){
      stop("The provided `term_order` does not include all variables.")
    }
    frequency_table <- frequency_table[order(order),]
  }
  header_str_list <- colnames(frequency_table) %>%
    stringr::str_split("_")
  header_df <- tibble::tibble(col_keys = colnames(frequency_table),
                      model = header_str_list %>%
                        purrr::map_chr(~ifelse(length(.x) == 2, .x[[1]], NA_character_)),
                      p = header_str_list %>%
                        purrr::map_chr(~ifelse(length(.x) == 2, .x[[2]], NA_character_)))
  frequency_table %>%
    flextable::flextable() %>%
    flextable::set_header_df(mapping = header_df, key = "col_keys") %>%
    flextable::merge_h(part = "header") %>%
    flextable::align(i = NULL, j = NULL, align = "center", part = "header") %>%
    flextable::mk_par(part = "header",
           i = 2,
           j = 2:10,
           value = flextable::as_paragraph(flextable::as_equation(paste0("p_{\\text{",.,"}}"))),
           use_dot = TRUE) %>%
    flextable::colformat_double(j = 1+1:9, digits = 2) %>%
    flextable::vline(j = c(1,1+3*1:3), part = "body") %>%
    flextable::hline(i = 2, part = "header")
}

#' Translate Bayesian variable selection output to model input
#'
#' @param data_estimation Data of estimation
#' @param data_prediction Data of prediction
#' @param hpd_model Output from `get_bvs_hpd_model`
#' @param probs Probabilities of quantiles
#' @param digits Number of digits in factor label names
#'
#' @return list of data_estimation and data_prediction with the corresponding variables as categorical, if any.
#' @export
translate_bvs_hpd_model <- function(data_estimation, data_prediction = NULL, hpd_model, probs = c(.33,.66), digits=2L){
  cat_vars <- hpd_model$parameters$categorical
  if(length(cat_vars) > 0){
    for(i in seq_along(cat_vars)){
      if(!is.null(data_prediction)){
        data_prediction[[cat_vars[i]]] <- data_prediction[[cat_vars[i]]] %>%
          as_quantile_factor_prediction(v_estimation = data_estimation[[cat_vars[i]]],
                                        probs = probs,
                                        digits = digits)
      }
      data_estimation[[cat_vars[i]]] <- data_estimation[[cat_vars[i]]] %>%
        as_quantile_factor(probs = probs, digits = digits)
    }
  }
  return(list(
    "data_estimation" = data_estimation,
    "data_prediction" = data_prediction
  ))
}


#' Translate a bivariate analysis variable selection
#'
#' @param bivariate_summary_fixed `summary_fixed` of a bivariate analysis or several bivariate analysis
#' @param flag_use_or Should function use odds ratio to decide? (default = FALSE)
#'
#' @return Returns a vector with the significant coefficients.
#' @export
#'
#' @examples
translate_bivariate_analysis_inla <-
  function(bivariate_summary_fixed, flag_use_or = FALSE) {
    is_flag(flag_use_or)
    bivariate_summary_fixed <- bivariate_summary_fixed %>%
      dplyr::filter(term != "intercept",
                    term != "zi_intercept")
    if (flag_use_or) {
      stopifnot("q0_975_or" %in% names(bivariate_summary_fixed),
                "q0_025_or" %in% names(bivariate_summary_fixed))
      bivariate_summary_fixed %>%
        filter(q0_025_or > 1 | q0_975_or < 1) %>%
        dplyr::pull(term)
    } else{
      bivariate_summary_fixed %>%
        filter(q0_025 > 0 | q0_975 < 0) %>%
        dplyr::pull(term)
    }
}
