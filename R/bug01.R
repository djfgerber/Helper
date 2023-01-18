#
# dd <- read_rds("../merck/data/2020/temp_merck.rds")
# dd
#
# mesh <- read_rds("../merck/data/2020/temp_mesh_small.rds")
#
# dd %>%
#   filter(species == "S. haematobium") %>%
#   fit_inla2(response = c("positives", "sample_size"),
#             flag_intercept = TRUE,
#             covariates = c("aez"),
#             flag_non_spatial_re = TRUE,
#             flag_spatial_re = TRUE,
#             prior_pc_range = c(1,.7),
#             prior_pc_sigma = c(1,.99),
#             mesh = mesh,
#             family = "binomial")
#
# dd %>%
#   filter(species == "S. haematobium") %>%
#   fit_inla2(response = c("positives", "sample_size"),
#             flag_intercept = TRUE,
#             covariates = c("aez"),
#             flag_non_spatial_re = TRUE,
#             family = "binomial")
#
#
# dd %>%
#   filter(species == "S. haematobium") %>%
#   fit_inla2(response = c("positives", "sample_size"),
#             flag_intercept = TRUE,
#             covariates = c("aez"),
#             family = "binomial")
#


