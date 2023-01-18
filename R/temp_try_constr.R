# rm(list = ls())
# library(geoR)
# library(tidyverse)
# data(gambia)
# dim(gambia)
# dim(unique(gambia[, c("x", "y")]))
# d <- group_by(gambia, x, y) %>%
#   summarize(total = n(),
#             positive = sum(pos),
#             prev = positive/total) %>%
#   ungroup()
# d <- d[1:5,]
# d
# library(sp)
# library(rgdal)
# sps  <- SpatialPoints(d[, c("x", "y")], proj4string = CRS("+proj=utm +zone=28"))
# spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))
# d[, c("long", "lat")] <- coordinates(spst)
# library(raster)
# r <- getData(name = 'alt', country = 'GMB', mask = TRUE)
# d$alt <- extract(r, d[, c("long", "lat")])
# library(INLA)
# coo <- cbind(d$long, d$lat)
# mesh <- inla.mesh.2d(loc = coo, max.edge = 10* c(0.1, 5), cutoff = 0.1)
# spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
# indexs <- inla.spde.make.index("s", spde$n.spde)
# A <- inla.spde.make.A(mesh = mesh, loc = coo)
# dp <- rasterToPoints(r)
# ra <- aggregate(r, fact = 5, fun = mean)
# dp <- rasterToPoints(ra)
# coop <- dp[, c("x", "y")]
# Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
# stk.e <- inla.stack(tag = "est",
#                     data = list(y = d$positive, numtrials = d$total),
#                     A = list(1, A),
#                     effects = list(data.frame(b0 = 1, cov = d$alt), s = indexs))
#
# #stack for prediction stk.p
# stk.p <- inla.stack(tag = "pred",
#                     data = list(y = NA, numtrials = NA),
#                     A = list(1, Ap),
#                     effects = list(data.frame(b0 = 1, cov = dp[, 3]), s = indexs))
#
# #stk.full has stk.e and stk.p
# stk.full <- inla.stack(stk.e, stk.p)
# formula <- y ~ 0 + b0 + cov + f(s, model = spde)
# res <- inla(formula, family = "binomial", Ntrials = numtrials,
#             control.family = list(link = "logit"),
#             data = inla.stack.data(stk.full),
#             control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))
# summary(res)
#
#
#
# # try several points ------------------------------------------------------
# rm(list = ls())
# library(geoR)
# library(tidyverse)
# data(gambia)
# dim(gambia)
# dim(unique(gambia[, c("x", "y")]))
# d <- group_by(gambia, x, y) %>%
#   summarize(total = n(),
#             positive = sum(pos),
#             prev = positive/total) %>%
#   ungroup()
# d <- d[1:5,]
# d <- d %>%
#   bind_rows(d %>%
#               rowwise() %>%
#               mutate(positive = rbinom(1, total, prev)) %>%
#               ungroup()
#   )
# library(sp)
# library(rgdal)
# sps  <- SpatialPoints(d[, c("x", "y")], proj4string = CRS("+proj=utm +zone=28"))
# spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))
# d[, c("long", "lat")] <- coordinates(spst)
# library(raster)
# r <- getData(name = 'alt', country = 'GMB', mask = TRUE)
# d$alt <- extract(r, d[, c("long", "lat")])
# library(INLA)
# coo <- cbind(d$long, d$lat)
# mesh <- inla.mesh.2d(loc = coo, max.edge = c(0.1, 5), cutoff = 0.01)
# spde1 <- inla.spde2.matern(mesh = mesh, alpha = 2)
# indexs <- inla.spde.make.index("s", spde$n.spde)
# A <- inla.spde.make.A(mesh = mesh, loc = coo)
# dp <- rasterToPoints(r)
# ra <- aggregate(r, fact = 5, fun = mean)
# dp <- rasterToPoints(ra)
# coop <- dp[, c("x", "y")]
# Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
# stk.e <- inla.stack(tag = "est",
#                     data = list(y = d$positive, numtrials = d$total),
#                     A = list(1, A),
#                     effects = list(data.frame(b0 = 1, cov = d$alt), s = indexs))
#
# #stack for prediction stk.p
# stk.p <- inla.stack(tag = "pred",
#                     data = list(y = NA, numtrials = NA),
#                     A = list(1, Ap),
#                     effects = list(data.frame(b0 = 1, cov = dp[, 3]), s = indexs))
#
# #stk.full has stk.e and stk.p
# stk.full <- inla.stack(stk.e, stk.p)
# formula <- y ~ 0 + b0 + cov + f(s, model = spde)
# res <- inla(formula, family = "binomial", Ntrials = numtrials,
#             control.family = list(link = "logit"),
#             data = inla.stack.data(stk.full),
#             control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))
# summary(res)
#
# # try contr = TRUE --------------------------------------------------------
# rm(list = ls())
# library(geoR)
# library(tidyverse)
# data(gambia)
# dim(gambia)
# dim(unique(gambia[, c("x", "y")]))
# d <- group_by(gambia, x, y) %>%
#   summarize(total = n(),
#             positive = sum(pos),
#             prev = positive/total) %>%
#   ungroup()
# d
# library(sp)
# library(rgdal)
# sps  <- SpatialPoints(d[, c("x", "y")], proj4string = CRS("+proj=utm +zone=28"))
# spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))
# d[, c("long", "lat")] <- coordinates(spst)
# library(raster)
# r <- getData(name = 'alt', country = 'GMB', mask = TRUE)
# d$alt <- extract(r, d[, c("long", "lat")])
# library(INLA)
# coo <- cbind(d$long, d$lat)
# mesh <- inla.mesh.2d(loc = coo, max.edge = c(0.1, 5), cutoff = 0.01)
# spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)
# indexs <- inla.spde.make.index("s", spde$n.spde)
# A <- inla.spde.make.A(mesh = mesh, loc = coo)
# dp <- rasterToPoints(r)
# ra <- aggregate(r, fact = 5, fun = mean)
# dp <- rasterToPoints(ra)
# coop <- dp[, c("x", "y")]
# Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
# stk.e <- inla.stack(tag = "est",
#                     data = list(y = d$positive, numtrials = d$total),
#                     A = list(1, A),
#                     effects = list(data.frame(b0 = 1, cov = d$alt), s = indexs))
#
# #stack for prediction stk.p
# stk.p <- inla.stack(tag = "pred",
#                     data = list(y = NA, numtrials = NA),
#                     A = list(1, Ap),
#                     effects = list(data.frame(b0 = 1, cov = dp[, 3]), s = indexs))
#
# #stk.full has stk.e and stk.p
# stk.full <- inla.stack(stk.e, stk.p)
# formula <- y ~ 0 + b0 + cov + f(s, model = spde)
# res <- inla(formula, family = "binomial", Ntrials = numtrials,
#             control.family = list(link = "logit"),
#             data = inla.stack.data(stk.full),
#             control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))
# summary(res)
