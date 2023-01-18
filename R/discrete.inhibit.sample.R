

#' From geosample package Chipeta et al.
#'
#' @param obj
#' @param size
#' @param delta
#' @param delta.fix
#' @param k
#' @param cp.criterion
#' @param zeta
#' @param ntries
#' @param poly
#'
#' @return
#' @export
#'
#' @examples
discrete.inhibit.sample <- function (obj, size, delta, delta.fix = FALSE, k = 0, cp.criterion = NULL,
          zeta, ntries = 10000)
{
  obj.origin <- obj
  if (!inherits(obj, "SpatialPointsDataFrame")) {
    if (!inherits(obj, "SpatialPoints")) {
      if (!inherits(obj, "sf") & !inherits(obj, "data.frame")) {
        stop("\n 'obj' must be of class 'sp' or 'sf'")
      }
    }
  }
  if (inherits(obj, "Spatial")) {
    obj <- sf::st_as_sf(obj)
  }
  if (any(!is.numeric(sf::st_coordinates(obj))))
    stop("\n non-numerical values in the coordinates")
  if (any(is.na(sf::st_geometry(obj)))) {
    warning("\n NA's not allowed in 'obj' coordinates")
    obj <- obj[complete.cases(obj), , drop = FALSE]
    warning("\n eliminating rows with NA's")
  }

  if (length(size) > 0) {
    if (!is.numeric(size) | size <= 0)
      stop("\n 'size' must be a positive integer")
    else orig.size <- size
  }
  if (length(k) > 0) {
    if (k > 0) {
      if (!is.numeric(k) | k < 0)
        stop("\n 'k' must be a positive integer >= 0")
      if (k > size/2)
        stop("\n 'k' must be between 0 and size/2")
      if (is.null(cp.criterion))
        stop("\n Close pairs selection criterion 'cp.criterion' must be provided")
      if (cp.criterion != "cp.zeta" & cp.criterion !=
          "cp.neighb")
        stop("\n 'cp.criterion' must be either 'cp.neighb' or 'cp.zeta'")
    }
  }
  if (length(delta) > 0) {
    if (!is.numeric(delta) | delta < 0)
      stop("\n 'delta' must be a positive integer >= 0")
  }
  if (delta == 0) {
    if (k > 0) {
      stop("\n close pairs not allowed for completely random sample")
    }
    else {
      res1 <- as.data.frame(unique(st_coordinates(obj)))
      N <- dim(res1)[1]
      index <- 1:N
      index.sample <- sample(index, size, replace = FALSE)
      xy.sample <- res1[index.sample, ]
      dim(xy.sample)
    }
  }else {
    delta.orig <- delta
    if (delta.fix == TRUE) {
      delta = delta
    }else {
      delta <- delta * sqrt(size/(size - k))
      delta
    }
    dsq <- delta * delta

    poly.shape <- sf::st_convex_hull(sf::st_union(obj))
    if (!is.infinite(size) && (size * pi * dsq/4 > as.numeric(sf::st_area(poly.shape))))
      stop("\n Polygon is too small to fit ", size,
           " points, with 'k' = ", k, " close pairs,",
           " at minimum separation ", round(delta,
                                            digits = 4))
    xnotiny <- function(a1, a2) {
      a1.vec <- apply(a1, 1, paste, collapse = "")
      a2.vec <- apply(a2, 1, paste, collapse = "")
      a1.without.a2.rows <- as.data.frame(a1[!a1.vec %in%
                                               a2.vec, ])
      return(a1.without.a2.rows)
    }
    res1 <- as.data.frame(unique(sf::st_coordinates(obj)))
    N <- dim(res1)[1]
    index <- 1:N
    index.sample <- sample(index, 1, replace = FALSE)
    xy.sample <- res1[index.sample, ]
    for (i in 2:size) {
      dmin <- 0
      iter <- 1
      while (dmin < dsq) {
        take <- sample(index, 1)
        iter <- iter + 1
        dvec <- (res1[take, 1] - xy.sample[, 1])^2 +
          (res1[take, 2] - xy.sample[, 2])^2
        dmin <- min(dvec)
        if (iter == ntries)
          break
      }
      xy.sample[i, ] <- res1[take, ]
      num <- dim(xy.sample)[1]
      if (iter == ntries && dim(xy.sample)[1] < size) {
        warning("\n For the given 'delta' and 'size', only ",
                num, " inhibitory sample locations placed out of ",
                size, ". Consider revising 'delta' and/or 'size'")
        break
      }
    }
  }
  if (k > 0) {
    k.origin <- k
    size <- nrow(unique(xy.sample))
    reduction <- ((orig.size - size)/orig.size)
    if (k > size/2) {
      k <- floor(k * (1 - reduction))
      warning("\n For the given parameters, only ",
              k, " close pairs could be placed out of ",
              k.origin)
    }
    take <- matrix(sample(1:size, 2 * k, replace = FALSE),
                   k, 2)
    xy.sample <- unique(xy.sample)
    if (cp.criterion == "cp.neighb") {
      for (j in 1:k) {
        take1 <- take[j, 1]
        take2 <- take[j, 2]
        xy1 <- as.numeric(c(xy.sample[take1, ]))
        dvec <- (res1[, 1] - xy1[1])^2 + (res1[, 2] -
                                            xy1[2])^2
        neighbour <- order(dvec)[2]
        xy.sample[take2, ] <- res1[neighbour, ]
      }
    }
    if (cp.criterion == "cp.zeta") {
      if (!is.numeric(zeta) | zeta < 0)
        stop("\n 'zeta' must be between > 0 and 'delta'/2")
      if (zeta < delta.orig * 0.005)
        stop("\n 'zeta' too small.")
      if (zeta > delta.orig/2)
        stop("\n 'zeta' must be between > 0 and 'delta'/2")
      for (j in 1:k) {
        take1 <- take[j, 1]
        take2 <- take[j, 2]
        xy1 <- as.numeric(c(xy.sample[take1, ]))
        dvec <- (res1[, 1] - xy1[1])^2 + (res1[, 2] -
                                            xy1[2])^2
        z.vec <- which(dvec > 0 & dvec <= zeta * 0.25)
        z.vec.pts <- (1:dim(res1)[1])[z.vec]
        avail.locs <- xnotiny(res1[z.vec, ], xy.sample)
        if (nrow(avail.locs) > 0) {
          rep.loc <- sample(1:dim(avail.locs)[1], 1,
                            replace = F)
          xy.sample[take2, ] <- avail.locs[rep.loc, ]
        }
        else {
          warning("\n One or more locations do not have\n                      eligible 'close pairs'")
          break
        }
      }
    }
  }
  xy.sample <- sf::st_as_sf(xy.sample, coords = c("X",
                                                  "Y"))
  sf::st_crs(xy.sample) <- sf::st_crs(obj)
  xy.sample <- obj[xy.sample, ]

  res <- list()
  res$unique.locs <- num
  res$delta = delta
  res$k <- k
  res$sample.locs = xy.sample
  sf::st_crs(res$sample.locs) <- sf::st_crs(obj)
  if (class(xy.sample)[1] != class(obj.origin)[1]) {
    res$sample.locs <- sf::as_Spatial(res$sample.locs, "Spatial")
  }
  return(res)
}



#' Title
#'
#' @param obj
#' @param size
#' @param delta
#' @param k
#'
#' @return
#' @export
#'
#' @examples
discrete_inhibit_sample_neighb <- function (obj,
                                     size,
                                     delta,
                                     k = 0)
{
  stopifnot(k > 0)
  sample_locations <-
    discrete.inhibit.sample(obj, (size - k), delta, delta.fix = TRUE, k = 0)$sample.locs
  sample_locations_ind <- obj %>%
    sf::st_intersects(sample_locations, sparse = FALSE) %>%
    apply(2, which)
  remaining_locations_temp <-
    remaining_locations <- obj[-sample_locations_ind,]
  remaining_locations_temp <- remaining_locations_temp %>%
    dplyr::mutate(eligible = TRUE)

  k_sample_locations <- sample_locations %>%
    dplyr::slice_sample(n = k)

  inds <- rep(NA, k)
  for (i in 1:k) {
    inds[i] <- k_sample_locations[i, ] %>%
      sf::st_nearest_feature(remaining_locations_temp %>%
                               dplyr::filter(eligible))

    remaining_locations_temp$eligible[inds[i]] <- FALSE
    }
  res <- list()

  res$sample.locs <- remaining_locations[inds, ] %>%
    dplyr::bind_rows(sample_locations)

  return(res)
}
