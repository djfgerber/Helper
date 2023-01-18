test_that("mode = random works", {
  dd <- tibble::tibble(a = 1:10) %>%
    dplyr::mutate(group1 = c(rep(1, 5), rep(2, 5)),
                  group2 = c(rep(1, 5), 2, 2, 2, 3, 3),
                  group3 = c(rep("c", 5), rep("a", 5)),
                  group4 = letters[c(rep(2, 5), 3, 3, 3, 1, 1)])
  dd %>%
    split_validation("random", k = 5L, prop_train = .8) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(n = 8L))
  dd %>%
    split_validation("random", k = 5L, prop_train = .5) %>%
    dplyr::summarize(n = sum(valid_5 == "train")) %>%
    expect_identical(tibble::tibble(n = 5L))
  dd %>%
    split_validation("random", k = 5L, n_train = 2L) %>%
    dplyr::summarize(n = sum(valid_5 == "train")) %>%
    expect_identical(tibble::tibble(n = 2L))
  dd %>%
    split_validation("random", k = 5L, n_train = 4L) %>%
    dplyr::summarize(n = sum(valid_5 == "train")) %>%
    expect_identical(tibble::tibble(n = 4L))
})

test_that("grouping works", {
  # test with group of 2 levels
  expect_error({
    dd %>%
      split_validation("random",
                       k = 5L,
                       prop_train = c(.5,.8,.2),
                       group_var = "group1")
  })
  expect_error({
    dd %>%
      split_validation("random",
                       k = 5L,
                       n_train = c(1L,3L,5L),
                       group_var = "group1")
  })
  dd %>%
    split_validation("random",
                     k = 5L,
                     prop_train = .8,
                     group_var = "group1") %>%
    dplyr::group_by(group1) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(group1 = as.numeric(c(1:2)),
                                    n = as.integer(c(4,4))))
  dd %>%
    split_validation("random",
                     k = 5L,
                     n_train = 3L,
                     group_var = "group1") %>%
    dplyr::group_by(group1) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(group1 = as.numeric(c(1:2)),
                                    n = as.integer(c(3,3))))
  dd %>%
    split_validation("random",
                     k = 5L,
                     prop_train = c(.5,.8),
                     group_var = "group1") %>%
    dplyr::group_by(group1) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(group1 = as.numeric(c(1:2)),
                                    n = as.integer(c(2,4))))
  dd %>%
    split_validation("random",
                     k = 5L,
                     n_train = c(3L,4L),
                     group_var = "group1") %>%
    dplyr::group_by(group1) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(group1 = as.numeric(c(1:2)),
                                    n = as.integer(c(3,4))))

  # test with group of 3 levels
  expect_error({
    dd %>%
      split_validation("random",
                       k = 5L,
                       prop_train = c(.5,.2),
                       group_var = "group2")
  })
  dd %>%
    split_validation("random",
                     k = 5L,
                     prop_train = .8,
                     group_var = "group2") %>%
    dplyr::group_by(group2) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(group2 = as.numeric(c(1:3)),
                                    n = as.integer(c(4,2,1))))
  dd %>%
    split_validation("random",
                     k = 5L,
                     prop_train = c(.5, .8, .1),
                     group_var = "group2") %>%
    dplyr::group_by(group2) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(group2 = as.numeric(c(1:3)),
                                    n = as.integer(c(2,2,0))))
})

test_that("test that group order is respected", {
  dd %>%
    split_validation("random",
                     k = 5L,
                     n_train = c(2L, 1L),
                     group_var = "group3") %>%
    dplyr::group_by(group3) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(group3 = c("a", "c"),
                                    n = as.integer(c(1,2))))
  dd %>%
    split_validation("random",
                     k = 5L,
                     n_train = c(4L, 2L, 1L),
                     group_var = "group4") %>%
    dplyr::group_by(group4) %>%
    dplyr::summarize(n = sum(valid_1 == "train")) %>%
    expect_identical(tibble::tibble(group4 = c("a", "b", "c"),
                                    n = as.integer(c(1, 4, 2))))
})


test_that("mode = fold works", {
  dd %>%
    split_validation("fold", k = 5L) %>%
    dplyr::summarize(dplyr::across(dplyr::all_of(paste0("valid_", 1:5)),
                                   ~ sum(.x == "test"))) %>%
    tidyr::pivot_longer(1:5) %>%
    dplyr::pull(value) %>%
    expect_identical(rep(2L,5))
})


test_that("mode == inhibit_close_pairs works", {
  skip("no good test implemented")
  dd <- tidyr::expand_grid(x = 1:4, y = 1:4) %>%
    sf::st_as_sf(coords = c("x", "y"))

  dd1a <- dd %>%
    split_validation(mode = "inhibit_close_pairs",
                     k = 5L,
                     n_train = 3L,
                     delta = 0.5)
  dd1a %>% dplyr::select(valid_1) %>% plot(pch =19, cex = 2)
  dd1b <- dd %>%
    split_validation(mode = "inhibit_close_pairs",
                     k = 5L,
                     n_train = 3L,
                     delta = 1)
  dd1b %>% dplyr::select(valid_1) %>% plot(pch =19, cex = 2)
  dd2a <- dd %>%
    split_validation(mode = "inhibit_close_pairs",
                     k = 5L,
                     n_train = 3L,
                     delta = 1.1)
  dd2a %>% dplyr::select(valid_1) %>% plot(pch =19, cex = 2)
  dd2b <- dd %>%
    split_validation(mode = "inhibit_close_pairs",
                     k = 5L,
                     n_train = 3L,
                     delta = 1.5)
  dd2b %>% dplyr::select(valid_2) %>% plot(pch =19, cex = 2)
  dd3 <- dd %>%
    split_validation(mode = "inhibit_close_pairs",
                     k = 5L,
                     n_train = 3L,
                     delta = 2)
  # this gives an error even though it should not


  # look at toy data
  set.seed(1234)
  x <- 0.015+0.03*(1:33)
  xall <- rep(x,33)
  yall <- c(t(matrix(xall,33,33)))
  xy <- cbind(xall,yall)+matrix(-0.0075+0.015*runif(33*33*2),33*33,2)
  xy <- xy %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2))
  plot(sf::st_geometry(xy),pch=19,cex=0.25,xlab="longitude",ylab="latitude",
       cex.lab=1,cex.axis=1,cex.main=1, axes = TRUE)
  split_validation(xy,
                   mode = "inhibit_close_pairs",
                   k = 1L,
                   n_train = 50L,
                   delta = 0.08) %>%
    dplyr::filter(valid_1 == "train") %>%
    sf::st_geometry() %>%
    plot(add = TRUE, col = "blue", pch = 19)

  plot(sf::st_geometry(xy),pch=19,cex=0.25,xlab="longitude",ylab="latitude",
       cex.lab=1,cex.axis=1,cex.main=1, axes = TRUE)
  split_validation(xy,
                   mode = "inhibit_close_pairs",
                   k = 1L,
                   n_train = 50L,
                   delta = 0.08,
                   k_close_pairs = 5L,
                   cp.criterion = "cp.neighb") %>%
    dplyr::filter(valid_1 == "train") %>%
    sf::st_geometry() %>%
    plot(add = TRUE, col = "blue", pch = 19)

  plot(sf::st_geometry(xy),pch=19,cex=0.25,xlab="longitude",ylab="latitude",
       cex.lab=1,cex.axis=1,cex.main=1, axes = TRUE)
  split_validation(xy,
                   mode = "inhibit_close_pairs",
                   k = 1L,
                   n_train = 50L,
                   delta = 0.08,
                   k_close_pairs = 5L,
                   cp.criterion = "cp.zeta",
                   zeta = .03) %>%
    dplyr::filter(valid_1 == "train") %>%
    sf::st_geometry() %>%
    plot(add = TRUE, col = "blue", pch = 19)

  # look at gambia
  expect_error(sf_gambia %>%
    split_validation(
      mode = "inhibit_close_pairs",
      k = 2L,
      n_train = 30L,
      delta = 5,
      zeta = 1/111,
      k_close_pairs = 0L,
      prefix = "icp"
    ))
  expect_error(sf_gambia %>%
                 split_validation(
                   mode = "inhibit_close_pairs",
                   k = 2L,
                   n_train = 30L,
                   delta = 2/111,
                   zeta = 1/111,
                   k_close_pairs = 0L,
                   prefix = "icp"
                 ))
  sf_gambia_icp <- sf_gambia %>%
    split_validation(
      mode = "inhibit_close_pairs",
      k = 2L,
      n_train = 30L,
      delta = 5/111,
      zeta = NULL,
      k_close_pairs = 0L,
      prefix = "icp"
    )
  map_base +
    tmap::tm_shape(sf_gambia_icp)+
    tmap::tm_dots(col = "blue", size = .2, palette = c(rgb(1,1,0), rgb(0,0,0)), alpha = .5)+
    tmap::tm_shape(sf_gambia_icp %>%
                     dplyr::filter(icp1 == "train"))+
    tmap::tm_dots(col = "icp1", size = .2, palette = c(rgb(1,0,0), rgb(0,0,0)), alpha = .5)+
    tmap::tm_shape(sf_gambia_icp %>%
                     dplyr::filter(icp2 == "train"))+
    tmap::tm_dots(col = "icp2", size = .2, palette = c(rgb(0,1,0), rgb(0,0,0)), alpha = .5)+
    map_top

  sf_gambia_icp <- sf_gambia %>%
    split_validation(
      mode = "inhibit_close_pairs",
      k = 2L,
      n_train = 30L,
      delta = 5/111,
      cp.criterion = "cp.zeta",
      zeta = 1/111,
      k_close_pairs = 2L,
      prefix = "icp"
    )

  sf_gambia_icp <- sf_gambia %>%
    split_validation(
      mode = "inhibit_close_pairs",
      k = 2L,
      n_train = 30L,
      delta = 5/111,
      cp.criterion = "cp.neighb",
      k_close_pairs = 2L,
      prefix = "icp"
    )

})

test_that("mode == k_means works", {
  skip("No test implemented yet")
  split <- sf_gambia %>%
    split_validation(
      mode = "k_means",
      k = 2L,
      n_train = 60L,
      k_close_pairs = 0L,
      prefix = "k_means"
    )
  split %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(paste0("k_means", 1:2)),
        ~ sum(.x == "train")))
  split$k_means1 == split$k_means2

  split <- sf_gambia %>%
    dplyr::mutate(phc = phc %>% as.factor) %>%
    split_validation(
      mode = "k_means",
      k = 2L,
      n_train = c(10L, 25L),
      group_var = "phc",
      k_close_pairs = 0L,
      prefix = "k_means"
    )
  split %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(paste0("k_means", 1:2)),
        ~ sum(.x == "train")))
  split$k_means1 == split$k_means2
  split %>%
    dplyr::filter(phc == "0") %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(paste0("k_means", 1:2)),
        ~ sum(.x == "train")))
  split %>%
    dplyr::filter(phc == "1") %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(paste0("k_means", 1:2)),
        ~ sum(.x == "train")))
})



test_that("mode = purposive works", {
  skip("No test implemented yet")
  n <-  20000
  dd <- tibble::tibble(a = 1:n,
                       purposive_variable1 = sort(rexp(n)),
                       purposive_variable2 = sort(rnorm(n, 20, 5)))

  plot_densities <- function(){
    purrr::map_dfr(paste0("valid_", 1:5),
                   function(name) (samp %>%
                                     dplyr::filter(.data[[name]] == "train") %>%
                                     dplyr::pull(purposive_variable1) %>%
                                     density())[c("x", "y")] %>%
                     tibble::as_tibble() %>%
                     dplyr::mutate(name = name)) %>%
      ggplot2::ggplot(ggplot2::aes(x,y, col = name))+
      ggplot2::geom_line()+
      ggplot2::geom_line(data = (dd$purposive_variable1 %>% density())[c("x", "y")] %>%
                           tibble::as_tibble(),
                         ggplot2::aes(x, y, col = "population"))
  }
  samp <- dd %>%
    split_validation("purposive_proportional",
                     k = 5L,
                     n_train = 1000L,
                     purposive_variable = "purposive_variable1")
  plot_densities()
  samp <- dd %>%
    split_validation("purposive_proportional",
                     k = 5L,
                     n_train = 1000L,
                     purposive_variable = "purposive_variable2")
  plot_densities()

  samp <- dd %>%
    split_validation("purposive_inverseproportional",
                     k = 5L,
                     n_train = 1000L,
                     purposive_variable = "purposive_variable1")
  plot_densities()
  samp <- dd %>%
    split_validation("purposive_inverseproportional",
                     k = 5L,
                     n_train = 1000L,
                     purposive_variable = "purposive_variable2")
  plot_densities()

  samp <- dd %>%
    split_validation("purposive_even",
                     k = 5L,
                     n_train = 1000L,
                     purposive_variable = "purposive_variable1")
  plot_densities()
  samp <- dd %>%
    split_validation("purposive_even",
                     k = 5L,
                     n_train = 1000L,
                     purposive_variable = "purposive_variable2")
  plot_densities()


  expect_error({
    dd %>%
      split_validation("purposive_proportional", k = 5L, prop_train = .8)
  })
})


