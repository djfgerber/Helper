# # #
# # #
# # #
# # # mesh <- build_mesh(sf_gambia, "GMB")
# # # sf_gambia_nb <- sf_gambia %>%
# # #   mutate(positives = round(prevalence * 30, 0) %>% as.integer())
# # # fitb <- sf_gambia %>%
# # #   fit_binomial_bayes_inla_spatial(
# # #     response = c("positives", "total"),
# # #     covariates = "age",
# # #     flag_intercept = TRUE,
# # #     flag_scale_vars = TRUE,
# # #     flag_non_spatial_re = TRUE,
# # #     mesh = mesh
# # #   )
# # #
# # # fitnb <- sf_gambia_nb %>%
# # #   fit_negative_binomial_bayes_inla_spatial(
# # #     response = "positives",
# # #     covariates = "age",
# # #     flag_intercept = TRUE,
# # #     flag_scale_vars = TRUE,
# # #     flag_non_spatial_re = TRUE,
# # #     mesh = mesh
# # #   )
# # #
# # # fitb$summary_fixed
# # # fitnb$summary_fixed
# # #
# # # fitb$summary_hyperpar
# # # fitnb$summary_hyperpar
# # #
# # # fitb <- sf_gambia[1:60,] %>%
# # #   fit_binomial_bayes_inla_spatial(
# # #     response = c("positives", "total"),
# # #     covariates = "age",
# # #     flag_intercept = TRUE,
# # #     flag_scale_vars = TRUE,
# # #     flag_non_spatial_re = TRUE,
# # #     mesh = mesh,
# # #     data_prediction = sf_gambia[61:65,]
# # #   )
# # #
# # # fitnb <- sf_gambia_nb[1:60,] %>%
# # #   fit_negative_binomial_bayes_inla_spatial(
# # #     response = "positives",
# # #     covariates = "age",
# # #     flag_intercept = TRUE,
# # #     flag_scale_vars = TRUE,
# # #     flag_non_spatial_re = TRUE,
# # #     mesh = mesh,
# # #     data_prediction = sf_gambia[61:65,]
# # #   )
# # #
# # # fitb$summary_fixed
# # # fitnb$summary_fixed
# # #
# # # fitb$summary_hyperpar
# # # fitnb$summary_hyperpar
# # #
# # # fitb$prediction
# # # fitnb$prediction
# #
# # library(INLA)
# n=100
# a = 1
# b = 1
# z = rnorm(n)
# eta = a + b*z
# p = 0.2
# Ntrials = sample(c(1,5,10,15), size=n, replace=TRUE)
# prob = exp(eta)/(1 + exp(eta))
# y = rbinom(n, size = Ntrials, prob = prob)
# is.zero = (y == 0)
# while(sum(is.zero) > 0)
# {
#   y[is.zero] = rbinom(sum(is.zero), size = Ntrials[is.zero], prob = prob[is.zero])
#   is.zero = (y == 0)
# }
# y[ rbinom(n, size=1, prob=p) == 1 ] = 0
# data = list(y=y,z=z)
# formula = y ~ 1+z
# result0 = inla(formula, family = "zeroinflatedbinomial0", data = data, Ntrials = Ntrials)
# summary(result0)
#
# result1 = inla(formula, family = "zeroinflatedbinomial1", data = data, Ntrials=Ntrials)
# summary(result1)
#
# #
# n = 100
# a = 0.5
# b = 1.5
# x1 = rnorm(n, sd = 0.5)
# eta.z = -a - b*x1
# z = rbinom(n, 1, inla.link.logit(eta.z, inverse=TRUE))
# n.y = sum(z)
# x2 = rnorm(n.y, sd = 0.5)
# eta.y = a + b*x2
# lambda = exp(eta.y)
# y = rpois(n.y, lambda)
# is.zero = (y == 0)
# while(sum(is.zero) > 0)
# {
#   y[is.zero] = rpois(sum(is.zero), lambda[is.zero])
#   is.zero = (y == 0)
# }
# Y = matrix(NA, n + n.y, 2)
# Y[1:n, 1] = z
# Y[n + 1:n.y, 2] = y
# form = Y ~ 0 + mu.z + mu.y + cov.z + cov.y
# ldat = list(
#   Y=Y,
#   mu.z=rep(1:0, c(n, n.y)),
#   mu.y=rep(0:1, c(n, n.y)),
#   cov.z=c(x1, rep(NA,n.y)),
#   cov.y=c(rep(NA, n), x2))
#
# res <- inla(form, data=ldat,
#             family=c('binomial', 'zeroinflatedpoisson0'),
#             control.family=list(
#               list(),
#               list(hyper = list(
#                 prob = list(
#                   initial = -20,
#                   fixed = TRUE)))))
# round(res$summary.fix, 4)
#
#
# sf_gambia
# y.z <- ifelse(sf_gambia_nb$positives> 0, 1, 0)
# y.y <- sf_gambia_nb$positives[sf_gambia_nb$positives > 0]
# ldat = list(
#   Y = cbind(c(y.z, rep(NA, length(y.y))),
#             c(rep(NA, length(y.z)), y.y)),
#   mu.z=rep(1:0, c(length(y.z), length(y.y))),
#   mu.y=rep(0:1, c(length(y.z), length(y.y))),
#   cov.z=c(sf_gambia_nb$netuse, rep(NA,length(y.y))),
#   cov.y=c(rep(NA,length(y.z)), sf_gambia_nb$netuse[sf_gambia_nb$positives > 0])
#   )
#
# res <- inla(form, data=ldat,
#             family=c('binomial', 'zeroinflatedpoisson0'),
#             control.family=list(
#               list(),
#               list(hyper = list(
#                 prob = list(
#                   initial = -20,
#                   fixed = TRUE)))))
# round(res$summary.fix, 4)
#
#
# y.z <- ifelse(sf_gambia_nb$positives> 0, 1, 0)
# y.y <- sf_gambia_nb$positives
# ldat = list(
#   Y = cbind(c(y.z, rep(NA, length(y.y))),
#             c(rep(NA, length(y.z)), y.y)),
#   mu.z=rep(1:0, c(length(y.z), length(y.y))),
#   mu.y=rep(0:1, c(length(y.z), length(y.y))),
#   cov.z=c(sf_gambia_nb$netuse, rep(NA,length(y.y))),
#   cov.y=c(rep(NA,length(y.z)), sf_gambia_nb$netuse)
# )
#
#
# res1 <- inla(form, data=ldat,
#             family=c('binomial', 'zeroinflatedpoisson1'),
#             control.family=list(
#               list(),
#               list(hyper = list(
#                 prob = list(
#                   initial = -20,
#                   fixed = TRUE)))))
# round(res1$summary.fix, 4)
#
#
# y.z <- ifelse(sf_gambia$positives> 0, 1, 0)
# y.y <- sf_gambia$positives[y.z > 0]
# ldat = list(
#   Y = cbind(c(y.z, rep(NA, length(y.y))),
#             c(rep(NA, length(y.z)), y.y)),
#   mu.z=rep(1:0, c(length(y.z), length(y.y))),
#   mu.y=rep(0:1, c(length(y.z), length(y.y))),
#   cov.z=c(sf_gambia_nb$netuse, rep(NA,length(y.y))),
#   cov.y=c(rep(NA,length(y.z)), sf_gambia_nb$netuse[y.z > 0]),
#   Ntrials = c(rep(1, nrow(sf_gambia)), sf_gambia$total[y.z>0])
# )
#
#
# res1 <- inla(form, data=ldat,
#              family=c('binomial', 'zeroinflatedbinomial0'),
#              Ntrials = ldat$Ntrials
#              # control.family=list(
#              #   list(),
#              #   list(hyper = list(
#              #     prob = list(
#              #       initial = -20,
#              #       fixed = TRUE))))
#              )
# round(res1$summary.fix, 4)
#
# y.z <- ifelse(sf_gambia$positives> 0, 1, 0)
# y.y <- sf_gambia$positives
# ldat = list(
#   Y = cbind(c(y.z, rep(NA, length(y.y))),
#             c(rep(NA, length(y.z)), y.y)),
#   mu.z=rep(1:0, c(length(y.z), length(y.y))),
#   mu.y=rep(0:1, c(length(y.z), length(y.y))),
#   cov.z=c(sf_gambia_nb$netuse, rep(NA,length(y.y))),
#   cov.y=c(rep(NA,length(y.z)), sf_gambia_nb$netuse),
#   Ntrials = c(rep(1, nrow(sf_gambia)), sf_gambia$total)
# )
#
#
# res2 <- inla(form, data=ldat,
#              family=c('binomial', 'zeroinflatedbinomial1'),
#              Ntrials = ldat$Ntrials
#              # control.family=list(
#              #   list(),
#              #   list(hyper = list(
#              #     prob = list(
#              #       initial = -20,
#              #       fixed = TRUE)))
#                # )
# )
# round(res2$summary.fix, 4)
#
#
#
#
