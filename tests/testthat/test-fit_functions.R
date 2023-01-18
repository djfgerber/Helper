test_that("get_map_boundary works", {
  # get_map_boundary("BGD")
  # get_map_boundary("BGD", crs = readr::read_rds("../bangladesh/Data/projection.rds"))
})

test_that("build_mesh works", {
  skip("no test implemented")
  mesh <- build_mesh(sf_gambia, country_id = "GMB", kappa = 8)
  mesh$n
  plot(mesh)
  points(sf_gambia %>% sf::st_coordinates(), col = "blue")
  mesh <- build_mesh(sf_gambia, country_id = "GMB", kappa = 5)
  mesh$n
  plot(mesh)
  points(sf_gambia %>% sf::st_coordinates(), col = "blue")
  mesh <- build_mesh(sf_gambia %>%
                       sf::st_transform(crs = "+proj=utm +zone=28 +datum=WGS84 +units=km +no_defs "),
                     country_id = "GMB",
                     kappa = 8/100,
                     crs = "+proj=utm +zone=28 +datum=WGS84 +units=km +no_defs")
  mesh$n
  plot(mesh)
  sf_gambia %>%
    sf::st_transform(crs = "+proj=utm +zone=28 +datum=WGS84 +units=km +no_defs") %>%
    sf::st_coordinates() %>%
    points(col = "blue")

})

test_that("build_mesh2 works", {
  skip("no test implemented")
  mesh <- build_mesh2(sf_gambia, range_guess = 0.25, country_id = "GMB")
  mesh$n
  mesh <- build_mesh2(sf_gambia, range_guess = 0.3, country_id = "GMB")
  mesh$n
  plot(mesh)
  points(sf_gambia %>%
           sf::st_coordinates(), col = "red")
  mesh <- build_mesh2(sf_gambia, range_guess = 0.3, country_id = "GMB", flag_reverse_interior = TRUE)
  mesh$n
  plot(mesh)
  points(sf_gambia %>%
           sf::st_coordinates(), col = "red")
  mesh <- build_mesh2(sf_gambia, range_guess = 1, convex = 0.5)
  mesh$n
  plot(mesh)
  points(sf_gambia %>% sf::st_coordinates(), col = "blue")
  expect_error(build_mesh2(
    sf_gambia,
    range_guess = 1,
    convex = 0.5,
    flag_reverse_interior = TRUE
  ))
  mesh <- build_mesh2(sf_gambia %>%
                       sf::st_transform(crs = "+proj=utm +zone=28 +datum=WGS84 +units=km +no_defs "),
                     country_id = "GMB",
                     range_guess = 110)
  mesh$n
  plot(mesh)
  sf_gambia %>%
    sf::st_transform(crs = "+proj=utm +zone=28 +datum=WGS84 +units=km +no_defs") %>%
    sf::st_coordinates() %>%
    points(col = "green")

  dd <- readr::read_rds("../bangladesh/data/ascaris_estimation.rds")
  mesh <- build_mesh2(dd, range_guess = 100, country_id = "BGD")
  mesh$n
  plot(mesh)
  dd %>% sf::st_coordinates() %>% points(col = "green")

  mesh <- build_mesh2(dd, range_guess = 200)
  mesh$n
  plot(mesh)
  dd %>% sf::st_coordinates() %>% points(col = "green")

  mesh <- build_mesh2(dd, range_guess = 100)
  mesh$n
  plot(mesh)
  dd %>% sf::st_coordinates() %>% points(col = "green")

})
