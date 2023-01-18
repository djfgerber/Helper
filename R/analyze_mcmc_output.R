#
#
# document_mcmc <- function(chunk_name, rmd_name, output_dir){
#   output_dir <- "."
#   chunk_name <- "variable selection asc_cor"
#   rmd_name <- "3a-Estimation-Ascaris-Spike-Slap-Jags"
#   file_name <-
#     tempfile(pattern = "mcmc_output",
#              tmpdir = tempdir(),
#              fileext = ".Rmd")
#   sim <- read_simulation(chunk_name, rmd_name)
#
#   if(class(sim) == "runjags"){
#     units(sim$timetaken) <- "hours"
#     if(sim$summary == "Summary statistics are not stored internally when summarise=FALSE - see ?add.summary"){
#       sim_with_summary <- try(runjags::add.summary(sim))
#       if(class(sim_with_summary) == "try-error"){
#         summary_mcmc <- "No summary available. \n\n"
#       }else{
#         summary_mcmc <- "No summary available. \n\n"
#       }
#
#     }
#     paste0(
# "
# ---
# title: '1 Explore data'
# author: 'Daniel Gerber'
# date: ", format(Sys.Date(), "%B %d, %Y"), "
# output:
#   prettydoc::html_pretty:
#     toc: true
#     number_sections: true
#     theme: cayman
# editor_options:
#   chunk_output_type: console
# ---
#
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE,
#                       comment = '#>',
# fig.width = 10,
# dev = 'svg')
#     knitr::opts_knit$set(root.dir = '../')
#     library(tidyverse, warn.conflicts = FALSE)
# ```
#
# # Meta data
#
# ",
#
# paste0("Burnin = ", sim$burnin, "\n\n" ),
# paste0("Number of iterations = ", sim$sample, "\n\n"),
# paste0("Thinning = ", sim$thin, "\n\n"),
# paste0("Time taken = ", round(sim$timetaken,1), " hours \n\n"),
# "
#
# # Model
#
# ",
# paste0("```\n\n",sim$model %>% str_replace_all("\\n", "\n\n"),"```", "\n\n"),
#
# "
#
# # Summary
#
# ",
# paste0("```\n\n", summary_mcmc, "```", "\n\n")
#
#
# ) %>%
#       writeLines(file_name)
#     output_file_name <- paste0(rmd_name, "_", chunk_name)
#     rmarkdown::render(file_name,
#                       output_file = output_file_name,
#                       output_dir = output_dir)
#   }
#
#
# }
#
# # document_mcmc("a", "b")
#
# colnames(sim$mcmc[[1]])[1]
# sample <- sim$mcmc %>%
#   map(~ .x %>% unclass() %>% as_tibble()) %>%
#   bind_rows(.id = "chain") %>%
#   mutate(chain = chain %>% as_factor()) %>%
#   group_by(chain) %>%
#   slice(50000:100000) %>%
#   ungroup() %>%
#   rename_with(~.x %>% str_replace_all("\\[(\\d{1,3})\\]", "_\\1")) %>%
#   mutate(across(starts_with("ind_"),
#                 ~ .x %>% factor(levels = 0:1)),
#          across(starts_with("indb_"),
#                 ~ .x %>% factor(levels = 1:3))
#          )
#
# plot_ind <- function(sample, var = "ind_1"){
#   sample %>%
#     ggplot()+
#     geom_bar(aes_string(x = var, fill = "chain"), position = "dodge")+
#     scale_x_discrete(drop = FALSE)+
#     theme(legend.position = "none")
# }
# sample %>% plot_ind("ind_2")
# sample %>% plot_ind("indb_2")
# sample %>% plot_ind("ind_fac")
#
# plot_inds <- function(sample, starts_with, ncols = 3){
#   nn <- names(sample)
#   ind_nn <- nn[nn %>% str_detect(paste0("^", starts_with))]
#   plots <- ind_nn %>%
#     map(~ sample %>% plot_ind(.x))
#   gridExtra::grid.arrange(grobs = plots, ncol = ncols)
# }
#
# sample %>%
#   plot_inds("ind_")
# sample %>%
#   plot_inds("indb_", ncols = 5)
#
# nn <- names(sample)
#
# nn <- names(sample)
# ind_nn <- nn[nn %>% str_detect("b_", negate = TRUE)]
# ind_nn
#
# plot_coef <- function(sample, var = "b_1"){
#   sample %>%
#   ggplot()+
#   geom_density(aes_string(var, col = "chain"))
# }
#
# plot_coef(sample, "b_2")
# plot_coef(sample, "b_2")
#
# plot_coefs <- function(sample, starts_with = "b_", ncols = 3){
#   nn <- names(sample)
#   nn <- nn[nn %>% str_detect(paste0("^", starts_with))]
#   plots <- nn %>%
#     map(~ sample %>% plot_coef(.x))
#   gridExtra::grid.arrange(grobs = plots, ncol = ncols)
# }
# sample %>% plot_coefs(ncols = 5)
