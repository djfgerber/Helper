#' Split data set in training and test data for validation
#'
#'
#' @param k scalar integer: how many test sets do we want. Has to be a square number if mode == "area"
#' @param mode one of "random", "fold", "inhibit_close_pairs",  "k_means", "purposive_proportional", "purposive_inverseproportional", "purposive_even"
#' @param seed_start A seed.
#' @param data data or sf data points
#' @param group_var A variable to group by
#' @param prefix A string to prepend to the variable which contains the "train"/"test" values
#' @param delta Option for the inhibit close pairs
#' @param zeta Option for the inhibit close pairs
#' @param k_close_pairs Option for the inhibit close pairs
#' @param prop_train Proportion to train points (between 0 and 1). If `group_var` is not NULL, can be vector of proportions applied to each group.
#' @param n_train Number of training points (integer). If `group_var` is not NULL, can be vector of number of training points applied to each group.
#' @param purposive_variable If purposive-mode, the variable to do the purpose on
#' @param cp.criterion cp.criterion for geosample: either 'cp.neighb' or 'cp.zeta'
#'
#' @return A tibble with additional columns, each indicating one fold and the values "train" and "test" for each row.
#' @export
#'
#' @examples
#' \dontrun{
#' dd <- tibble::tibble(a = 1:10)
#' dd %>% split_validation("random", k = 5L, prop_train = .8)
#' dd %>% split_validation("fold", k = 5L)
#' dd <- dd %>%
#'   dplyr::mutate(group1 = c(rep(1,5), rep(2,5)),
#'                 group2 = c(rep(1,5), 2,2,2,3,3))
#' dd %>%
#'   split_validation("stratified", k = 5L, prop_train = .75, group_var = "group1")
#' dd %>%
#'   split_validation("stratified", k = 5L, prop_train = .75, group_var = "group2")
#'
#' sf_gambia %>%
#'   split_validation("random", k = 5L, prop_train = 0.8, prefix = "my_valid_")
#' sf_gambia %>%
#'   dplyr::mutate(group = sample(4,nrow(sf_gambia), TRUE)) %>%
#'   split_validation("stratified", k = 5L, 0.8, 23L, "group")
#' sf_gambia %>%
#'   split_validation(
#'     mode = "inhibit_close_pairs",
#'     k = 2L,
#'     prop_train = 0.8,
#'     delta = 0.01,
#'     zeta = 0.001,
#'     k_close_pairs = 2
#'   )
#' }
split_validation <-
  function(data,
           mode,
           k,
           prop_train = NULL,
           n_train = NULL,
           seed_start = 1L,
           group_var = NULL,
           prefix = "valid_",
           delta = NULL,
           cp.criterion = NULL,
           zeta = NULL,
           k_close_pairs = 0L,
           purposive_variable = NULL) {
    is_data(data)
    stopifnot(
      purrr::is_scalar_character(mode),
      mode %in% c(
        "random",
        "fold",
        "inhibit_close_pairs",
        "k_means",
        "purposive_proportional",
        "purposive_inverseproportional",
        "purposive_even"
      ),
      purrr::is_scalar_integer(k),
      k > 0,
      is.null(prop_train) | is.null(n_train),
      is.null(n_train) || purrr::is_integer(n_train),
      is.null(prop_train) || purrr::is_double(prop_train),
      purrr::is_scalar_integer(seed_start),
      purrr::is_scalar_integer(k_close_pairs),
      k_close_pairs == 0 ||
        (purrr::is_scalar_character(cp.criterion) &&
           ((cp.criterion == "cp.neighb" & is.null(zeta)) |
            (cp.criterion == "cp.zeta" &
               !is.null(zeta) &&
               purrr::is_scalar_double(zeta) &&
               zeta > 0 &&
               zeta < delta)
           )),
         purrr::is_scalar_character(prefix),
      !is.na(prefix),
      is.null(purposive_variable) |
        (purrr::is_scalar_character(purposive_variable) &
           purposive_variable %in% colnames(data))
    )
    if (any(colnames(data) %>%
            stringr::str_detect(paste0("^", prefix, "\\d")))){
      stop("Provide a valid prefix.",
           call. = FALSE)
    }
    if (!is.null(group_var)) {
      is_group_var(data, group_var)
      n_groups <- length(unique(data[[group_var]]))
      if(!length(prop_train) %in% c(0, 1, n_groups)){
        stop("Length of `prop_train` must either be 1 or same as unique levels of the `group_var`.",
             call. = FALSE)
      }
      if(!length(n_train) %in% c(0, 1, n_groups)){
        stop("Length of `n_train` must either be 1 or same as unique levels of the `group_var`.",
             call. = FALSE)
      }
      if(!length(k_close_pairs) %in% c(0, 1, n_groups)){
        stop("Length of `k_close_pairs` must either be 1 or same as unique levels of the `group_var`.",
             call. = FALSE)
      }
      if(!length(delta) %in% c(0, 1, n_groups)){
        stop("Length of `delta` must either be 1 or same as unique levels of the `group_var`.",
             call. = FALSE)
      }
      group_order <- data[[group_var]] %>%
        unique() %>%
        order()
      (data %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::group_split())[match(1:n_groups, group_order)] %>%
        purrr::imap_dfr( ~ {
          prop_train  <-  subvect_if_not_NULL(prop_train, index = .y)
          n_train  <-  subvect_if_not_NULL(n_train, index = .y)
          k_close_pairs <- subvect_if_not_NULL(k_close_pairs, index = .y)
          delta  <-  subvect_if_not_NULL(delta, index = .y)

          .x %>%
            split_validation(
              mode = mode,
              k = k,
              prop_train = prop_train,
              n_train = n_train,
              seed_start = seed_start + .y,
              group_var = NULL,
              prefix = prefix,
              delta = delta,
              cp.criterion = cp.criterion,
              zeta = zeta,
              k_close_pairs = k_close_pairs,
              purposive_variable = purposive_variable
            )
        })
    }else{
      if (mode == "random") {
        if(!is.null(prop_train)){
          is_probability(prop_train)
        }
        dd_row_int <- tibble::tibble(rows = 1:nrow(data))
        for (i in seq_len(k)) {
          set.seed(seed_start + i)
          row_ind <- if(!is.null(prop_train)){
            dd_row_int %>%
              dplyr::slice_sample(prop = prop_train) %>%
              dplyr::pull(rows)
          }else{
            dd_row_int %>%
              dplyr::slice_sample(n = n_train) %>%
              dplyr::pull(rows)
          }
          data[, paste0(prefix, i)] <- "test"
          data[row_ind, paste0(prefix, i)] <- "train"
        }
      } else if (mode == "fold") {
        set.seed(seed_start)
        rows_ind <- 1:nrow(data)
        rows_ind <- sample(rows_ind, size = length(rows_ind))
        length_fold <- length(rows_ind) %/% k
        i <- 1
        tests <- list()
        while (i < (length(rows_ind) - length_fold)) {
          tests <- c(tests, list(rows_ind[i:(i + length_fold - 1)]))
          i <- i + length_fold
        }
        tests <- c(tests, list(rows_ind[i:length(rows_ind)]))
        for (i in seq_len(k)) {
          data[, paste0(prefix, i)] <- "train"
          data[tests[[i]], paste0(prefix, i)] <- "test"
        }
      } else if (mode == "inhibit_close_pairs") {
        is_sf_data(data)
        if(!is.null(prop_train)){
          is_probability(prop_train)
        }
        coords <- data %>%
          sf::st_coordinates() %>%
          as.data.frame %>%
          sf::st_as_sf(coords = c(1, 2))
        size <- if(!is.null(prop_train)){
          round(prop_train * nrow(coords), 0)
        }else{
          n_train
        }
        bb <- data %>% sf::st_bbox()
        a <- max(c(bb["ymax"] - bb["ymin"],
                   bb["xmax"] - bb["xmin"]))
        max_delta <- (a*(1+ sqrt(size))) / (size - 1)
        if(delta > max_delta){
          stop("In `split_validation` mode `inhibit_close_pairs`: delta = ",
                  delta,
                  " should probably be smaller than ",
                  round(max_delta,2),
                  ". Check if crs matches your expectations.", call. = FALSE)
        }
        if(k_close_pairs == 0L && !is.null(zeta)){
          stop("In `split_validation` mode `inhibit_close_pairs`: if number of closed pairs is 0, zeta should be NULL.", call. = FALSE)
        }
        if(!is.null(zeta) && zeta > delta){
          stop("In `split_validation` mode `inhibit_close_pairs`: zeta should be smaller than delta.", call. = FALSE)
        }
        for (i in seq_len(k)) {
          set.seed(seed_start + i)
          sample_locs <- if(is.null(cp.criterion) || cp.criterion == "cp.zeta"){
            discrete.inhibit.sample(
              obj = coords,
              size = size,
              delta = delta,
              delta.fix = TRUE,
              k = k_close_pairs,
              cp.criterion = cp.criterion,
              zeta = zeta
            )$sample.locs
          }else if(k_close_pairs > 0  && cp.criterion == "cp.neighb"){
            discrete_inhibit_sample_neighb(
              obj = coords,
              size = size,
              delta = delta,
              k = k_close_pairs
            )$sample.locs
          }else{
            stop("cp.criterion not implemented.")
          }
          if (nrow(sample_locs) != size) {
            stop("Something went wrong in geosample.")
          }
          sf::st_crs(sample_locs) <- sf::st_crs(data)
          selected_ind <- (data %>%
                             sf::st_equals(sample_locs, sparse = FALSE) %>%
                             apply(1, sum) == 1) %>%
            ifelse("train", "test") %>%
            factor(levels = c("train", "test"))
          data <- data %>%
            dplyr::mutate(!!paste0(prefix, i) := selected_ind)
        }
      } else if (mode == "k_means") {
        if(!is.null(prop_train)){
          is_probability(prop_train)
        }
        if(!is.null(k_close_pairs)){
          stop("Closed pairs not implemented for k_means, use k_close_pairs = NULL")
        }
        size <- if(!is.null(prop_train)){
          round(prop_train * nrow(coords), 0)
        }else{
          n_train
        }
        new_data <- data %>%
          dplyr::select(geometry) %>%
          dplyr::mutate(obs_id = 1:nrow(data))
        for (i in seq_len(k)) {
          set.seed(seed_start + i)
          k_means_out <- new_data %>%
            sf::st_coordinates() %>%
            stats::kmeans(centers = size)
          new_data_train <- new_data %>%
            dplyr::mutate(k_means = k_means_out$cluster) %>%
            dplyr::group_by(k_means) %>%
            dplyr::slice_sample(n = 1) %>%
            dplyr::ungroup()
          if (nrow(new_data_train) != size) {
            stop("Something went wrong in k_means.")
          }
          new_valid <- new_data %>%
            dplyr::mutate(valid = ifelse(obs_id %in% new_data_train$obs_id, "train", "test") %>%
                            factor(levels = c("train", "test"))) %>%
            dplyr::pull(valid)
          data <- data %>%
            dplyr::mutate(!!paste0(prefix, i) := new_valid)
        }
      } else if (mode == "purposive_proportional" |
                 mode == "purposive_inverseproportional"|
                 mode == "purposive_even"){
        if(!is.null(prop_train)){
          is_probability(prop_train)
        }
        dd_row_int <- tibble::tibble(rows = 1:nrow(data))
        dd_row_int <- if (mode == "purposive_proportional") {
          dd_row_int %>%
            dplyr::mutate(weights = data[[purposive_variable]])
        } else if (mode == "purposive_inverseproportional") {
          if(any(data[[purposive_variable]] == 0)){
            stop("In split_validation: purposive variable contains 0.")
          }
          dd_row_int %>%
            dplyr::mutate(weights = 1 / data[[purposive_variable]])
        } else if (mode == "purposive_even"){
          density_pvar <- data[[purposive_variable]] %>%
            stats::density()
          dd_row_int %>%
            dplyr::mutate(weights = data[[purposive_variable]] %>%
                     purrr::map_dbl(~ 1 / density_pvar$y[which.min(abs(density_pvar$x - .x))]))
        }

        for (i in seq_len(k)) {
          set.seed(seed_start + i)
          row_ind <- if(!is.null(prop_train)){
            dd_row_int %>%
              dplyr::slice_sample(prop = prop_train,
                                  weight_by = weights) %>%
              dplyr::pull(rows)
          }else{
            dd_row_int %>%
              dplyr::slice_sample(n = n_train,
                                  weight_by = weights) %>%
              dplyr::pull(rows)
          }
          data[, paste0(prefix, i)] <- "test"
          data[row_ind, paste0(prefix, i)] <- "train"
        }
      } else {
        stop("In split_validation: `mode` not recognized.", call. = FALSE)
      }
      data %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with(prefix),
                                    ~ .x %>% factor(levels = c("train", "test"))))
    }
  }

subvect_if_not_NULL <- function(x, index){
  if (!is.null(x)) {
    ifelse(length(x) == 1,
           x,
           x[index])
  } else{
    NULL
  }
}

