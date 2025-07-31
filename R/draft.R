# # install.packages("tidyverse")
# library(tidyverse)
#
# # install.packages("devtools")
# library(devtools)
#
# # devtools::install_github("eliotmiller/clootl")
# library(clootl)
#
# # install.packages("ape")
# library(ape)
#
# # devtools::install_github("OleksiiDubovyk/FilterABM")
# library(FilterABM)
#
# ex_1 <- extractTree(species=c("Turdus migratorius","Setophaga dominica", "Setophaga ruticilla", "Sitta canadensis"),
#                       version=1.4)
# ex_2 <- extractTree(species=c("Turdus migratorius","Setophaga dominica", "Setophaga ruticilla", "Sitta canadensis", "Branta canadensis"),
#                     version=1.4)
# cophenetic.phylo(ex_1)
# cophenetic.phylo(ex_2)["Branta_canadensis", "Setophaga_dominica"]
# cophenetic.phylo(ex_2)["Setophaga_dominica", "Branta_canadensis"]
#
# rtree(n = 5) %>% cophenetic.phylo()
#
#
# # Test data
#
# regpool_ebd <- lapply(1:500, function(x){
#   FilterABM::init_meta(M = 450, env_mean_mc = 0, env_sd_mc = 10, cauchy = 5.62, trait_sds = 0.01, max_abun = 1e6)
# }) %>%
#   bind_rows() %>%
#   group_by(species) %>%
#   summarise(trait = mean(trait), abundance = round(mean(abundance)), trait_sd = mean(trait_sd)) %>%
#   ungroup() %>%
#   FilterABM::FilterABM_mc()
#
# df1_phyl <- ape::rtree(n = 450)
#
# lh = init_envt(env_mean_lh = 0, env_sd_lh = 3, npatch = 100, gradient = "correlated", rho = 0.95)
# # plot(lh)
# lc = draw_lcom(mc = regpool_ebd, lh = lh, nind = 100)
#
# runsim <- run_sim_(mc = regpool_ebd,
#                    lh = lh,
#                    lc = lc,
#                    nsteps = 1500,
#                    progress_bar = T,
#                    recruitment = 0.2, dispersal = 1.5,
#                    res_input = 10,
#                    age_crit = 8, mass_crit = 2,
#                    clustering = 1, dispersion = 1)
#
# # saveRDS(runsim1, "~/runsim1.rds")
#
# image(1:nrow(runsim$habitat) - 1,
#       runsim$coord$lh,
#       runsim$habitat,
#       xlab = "Time step", ylab = "Patch", main = "Resource per patch")
#
# ggplot() +
#   geom_bin_2d(aes(x = timestep, y = trait), data = runsim$lcs) +
#   scale_fill_gradientn(colours = terrain.colors(10)) +
#   geom_line(aes(x = timestep, y = trait), data = runsim$lcs %>% group_by(timestep) %>% summarise(trait = mean(trait))) +
#   xlab("Time step") + ylab("Trait") + theme(legend.position = "none")
#
# runsim$lcs %>%
#   group_by(timestep, patch) %>%
#   summarise(trait = mean(trait), .groups = "drop") %>%
#   ggplot(aes(x = timestep, y = patch, fill = trait)) +
#   scale_fill_gradientn(colours = terrain.colors(10)) +
#   geom_tile() +
#   xlab("Time step") + ylab("Patch") + labs(fill = "Mean\ntrait\nvalue") +
#   theme_bw()
#
# runsim$lcs %>%
#   group_by(timestep, patch) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   ggplot(aes(x = timestep, y = patch, fill = n)) +
#   scale_fill_gradientn(colours = terrain.colors(10)) +
#   geom_tile() +
#   xlab("Time step") + ylab("Patch") + labs(fill = "Individuals") +
#   theme_bw()
#
# runsim$lcs %>%
#   group_by(timestep, patch) %>%
#   summarise(S = length(unique(species)), .groups = "drop") %>%
#   ggplot(aes(x = timestep, y = patch, fill = S)) +
#   scale_fill_gradientn(colours = terrain.colors(10)) +
#   geom_tile() +
#   xlab("Time step") + ylab("Patch") + labs(fill = "Species") +
#   theme_bw()
#
# runsim$lcs %>%
#   filter(patch == 50) %>%
#   group_by(species, timestep) %>%
#   summarise(abundance = n()) %>%
#   ggplot(aes(x = timestep, y = species, fill = abundance)) +
#   geom_tile() +
#   scale_fill_gradientn(colours = terrain.colors(10)) +
#   xlab("Time step") + ylab("Species") +
#   theme_bw()
#
# runsim$lcs %>%
#   group_by(timestep) %>%
#   summarise(n = n()) %>%
#   ggplot(aes(x = timestep, y = n)) +
#   geom_line()
#
# runsim$lcs %>%
#   group_by(timestep) %>%
#   summarise(S = length(unique(species))) %>%
#   ggplot(aes(x = timestep, y = S)) +
#   geom_line()
#
#
#
# runsim$lcs %>%
#   filter(timestep == 1500) %>%
#   left_join(lh, by = "patch") %>%
#   ggplot(aes(x = env, y = trait)) +
#   geom_point() +
#   geom_smooth()
#
# runsim$lcs %>%
#   filter(timestep > 1000) %>%
#   distinct(species, trait, patch, timestep) %>%
#   group_by(species, trait, patch) %>%
#   summarise(n = n()) %>%
#   filter(n == max(n)) %>%
#   ungroup() %>%
#   left_join(lh, by = "patch") %>%
#   ggplot(aes(x = env, y = trait)) +
#   geom_point(alpha = 0.2, pch = ".") +
#   geom_smooth() +
#   geom_abline(slope = 1, intercept = 0, color = "darkblue", lty = 2)
#
# runsim$lcs %>%
#   filter(timestep > 1000) %>%
#   distinct(species, trait, patch, timestep) %>%
#   group_by(species, trait, patch) %>%
#   summarise(n = n()) %>%
#   filter(n == max(n)) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_density(aes(x = .data$trait, ggplot2::after_stat(count), fill = factor(.data$species)),
#                position = "stack", show.legend = F, na.rm = F)
#
# df1_com <- runsim$lcs %>%
#   filter(timestep > 1000) %>%
#   distinct(species, trait, patch, timestep) %>%
#   group_by(species, trait, patch) %>%
#   summarise(n = n()) %>%
#   filter(n == max(n)) %>%
#   ungroup() %>%
#   left_join(lh, by = "patch")
#
# df1_com <- df1_com %>%
#   select(-c(res, n))
#
# df1_env <- df1_com %>%
#   distinct(patch, env)
#
# df1_com <- df1_com %>%
#   select(-env) %>%
#   mutate(trait_cont = trait) %>%
#   select(-trait)
#
# df1_com <- df1_com %>%
#   group_by(species) %>%
#   mutate(trait_cat = mean(trait_cont)) %>%
#   ungroup() %>%
#   mutate(trait_cat = kmeans(x = trait_cat, centers = c(-4.5, -3, -1, 1.5, 3.5))$cluster)
#
# df1_com %>%
#   ggplot(aes(x = trait_cat, y = trait_cont, color = factor(trait_cat))) +
#   geom_jitter()
#
# df1_com <- df1_com %>%
#   group_by(species) %>%
#   mutate(mt = mean(trait_cont)) %>%
#   ungroup() %>%
#   mutate(trait_d1 = dnorm(x = mt, mean = -5),
#          trait_d2 = dnorm(x = mt, mean = -1.7),
#          trait_d3 = dnorm(x = mt, mean = 1.7),
#          trait_d4 = dnorm(x = mt, mean = 5)) %>%
#   mutate(mt = trait_d1 + trait_d2 + trait_d3 + trait_d4,
#          trait_d1 = (trait_d1/mt) %>% round(4),
#          trait_d2 = (trait_d2/mt) %>% round(4),
#          trait_d3 = (trait_d3/mt) %>% round(4),
#          trait_d4 = (trait_d4/mt) %>% round(4)) %>%
#   select(-mt)
#
# df1_com <- df1_com[sample(1:nrow(df1_com), size = 100000),]
#
# df1_com <- df1_com %>% mutate(species = paste0("t", species))
#
# usethis::use_data(df1_com, overwrite = T)
# usethis::use_data(df1_env)
# usethis::use_data(df1_phyl)
#
# df1_trait_cat_dist <- outer(c(-4.5, -3, -1, 1.5, 3.5), c(-4.5, -3, -1, 1.5, 3.5), '-') %>% abs()
# usethis::use_data(df1_trait_cat_dist, overwrite = T)
#
#
# #
#
# com1 <- df1_com %>% filter(patch == 25)
# com2 <- df1_com %>% filter(patch == 75)
#
# ggplot()+
#   geom_density(aes(x = trait_cont), data = com1, fill = "red", alpha = 0.3) +
#   geom_density(aes(x = trait_cont), data = com2, fill = "blue", alpha = 0.3)
#
# # cont trait
# outer(com1$trait_cont, com2$trait_cont, '-') %>% abs() %>% mean()
#
# # cat trait
# d <- expand.grid(com1$trait_cat, com2$trait_cat) %>%
#   as_tibble() %>%
#   group_by(Var1, Var2) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   ungroup()
# d$d <- sapply(1:nrow(d), function(i) df1_trait_cat_dist[unlist(d[i, "Var1"]), unlist(d[i, "Var2"])])
# sum((d$n/sum(d$n)) * d$d)
#
# # distr trait
# dtraits <- c("trait_d1", "trait_d2", "trait_d3", "trait_d4")
#
# abs(
#   (purrr::map_dfr(
#     seq_len(nrow(com2[, dtraits])), function(x) com1[, dtraits]
#     ) %>%
#      as.matrix()) -
#     (
#       com2[rep(1:nrow(com2), each = nrow(com1)), dtraits] %>%
#         as.matrix()
#       )
#   ) %>%
#   apply(1, sum) %>% mean()
#
#
# # test
#
# rpd <- rand_pair_diff(
#   comdata = df1_com %>%
#     mutate(n = 1),
#   # envdata = df1_env %>%
#   #   mutate(x = patch, y = 0, env = sample(env)),
#   envdata = df1_env %>%
#     mutate(x = patch, y = 0),
#   env_idx = "env",
#   com_idx = "patch",
#   trait_idx = "trait_cont",
#   taxa_idx = "species",
#   abun_idx = NULL,
#   x_idx = "x",
#   y_idx = "y",
#   trait_FUN = dist_cont,
#   phyl_FUN = dist_phyl,
#   phtree = df1_phyl
# )
#
# rpd %>% ggplot(aes(x = d_env, y = d_trait)) + geom_point() + geom_smooth(method = scam::scam, formula = y ~ s(x, bs = "mpi"))
# rpd %>% ggplot(aes(x = d_dist, y = d_trait)) + geom_point() + geom_smooth(method = scam::scam, formula = y ~ s(x, bs = "mpi"))
#
# rpd %>%
#   enfiltest::eftest_models() %>%
#   enfiltest::eftest_rankAIC()
#
# # model building
#
# rpd %>% mutate(across(all_of(c("d_dist", "d_env", "d_phyl", "d_trait")), scale)) %>%
#   ggplot(aes(x = d_env, y = d_trait)) +
#   geom_point() +
#   geom_smooth(method = scam::scam, formula = y ~ s(x, bs = "mpi")) +
#   geom_smooth(method = lm, color = "red", lty = 2, se = F)
#
# rpd %>%
#   eftest_models() %>%
#   eftest_rankAIC() %>%
#   eftest_stat()
#
# x <- eftest(
#   rep = 10, npairs = 100,
#   comdata = df1_com %>%
#     mutate(n = 1),
#   envdata = df1_env %>%
#     mutate(x = patch, y = 0),
#   env_idx = "env",
#   com_idx = "patch",
#   trait_idx = "trait_cont",
#   taxa_idx = "species",
#   abun_idx = NULL,
#   x_idx = "x",
#   y_idx = "y",
#   trait_FUN = dist_cont,
#   phyl_FUN = dist_phyl,
#   phtree = df1_phyl
# )
#
# x0 <- eftest(
#   rep = 10, npairs = 100,
#   comdata = df1_com %>%
#     mutate(n = 1),
#   envdata = df1_env %>%
#     mutate(x = patch, y = 0, env = sample(env)),
#   env_idx = "env",
#   com_idx = "patch",
#   trait_idx = "trait_cont",
#   taxa_idx = "species",
#   abun_idx = NULL,
#   x_idx = "x",
#   y_idx = "y",
#   trait_FUN = dist_cont,
#   phyl_FUN = dist_phyl,
#   phtree = df1_phyl
# )
#
# cor.var <- runif(500)
#
# x.var <- sapply(cor.var, function(i){
#   eftest(
#     rep = 1, npairs = 100,
#     comdata = df1_com %>%
#       mutate(n = 1, trait_cont = FilterABM::simcor(trait_cont, correlation = i)),
#     envdata = df1_env %>%
#       mutate(x = patch, y = 0),
#     env_idx = "env",
#     com_idx = "patch",
#     trait_idx = "trait_cont",
#     taxa_idx = "species",
#     abun_idx = NULL,
#     x_idx = "x",
#     y_idx = "y",
#     trait_FUN = dist_cont,
#     phyl_FUN = dist_phyl,
#     phtree = df1_phyl
#   )
# })
#
#
#
# # hpc testing
# library(tidyverse)
# library(FilterABM)
# library(enfiltest)
# library(foreach)
# library(doParallel)
#
# random_importance <- c(runif(1000), runif(1000), runif(1000))
# trait_types <- c(rep("cont", 1000), rep("cat", 1000), rep("dist", 1000))
#
# df_env <- enfiltest::df1_env %>%
#   mutate(x = patch, y = 0)
#
# df_trait_cat_dist <- enfiltest::df1_trait_cat_dist
# colnames(df_trait_cat_dist) <- 1:5
# rownames(df_trait_cat_dist) <- colnames(df_trait_cat_dist)
#
# df_phyl <- enfiltest::df1_phyl
#
# getdata_cat <- function(df_com){
#
#   tryCatch({
#     df_com <- df_com %>%
#       group_by(species) %>%
#       mutate(trait_cat = mean(trait_cont)) %>%
#       ungroup() %>%
#       mutate(trait_cat = kmeans(x = trait_cat, centers = c(-4.5, -3, -1, 1.5, 3.5))$cluster)
#
#     eftest(
#       rep = 1, npairs = 1000,
#       comdata = df_com,
#       envdata = df_env,
#       env_idx = "env",
#       com_idx = "patch",
#       trait_idx = "trait_cat",
#       taxa_idx = "species",
#       abun_idx = NULL,
#       x_idx = "x",
#       y_idx = "y",
#       trait_FUN = enfiltest::dist_cat,
#       phyl_FUN = enfiltest::dist_phyl,
#       phtree = df1_phyl,
#       dmatrix = df_trait_cat_dist
#     )
#   },
#   error = function(cond) {NA}
#   )
# }
#
# num_cores <- detectCores()
# cl <- makeCluster(num_cores - 1)
# registerDoParallel(cl)
#
# test <- foreach(i = c(1, 1001, 2001),
#                 .combine = c, .verbose = F, .packages = c("tidyverse", "FilterABM", "enfiltest")
# ) %dopar%{
#
#   df_com <- enfiltest::df1_com %>%
#     select(species, patch, trait_cont) %>%
#     mutate(n = 1,
#            trait_cont = FilterABM::simcor(trait_cont, ymean = mean(trait_cont), ysd = sd(trait_cont), correlation = random_importance[i]))
#
#   if (trait_types[i] == "cat"){
#
#     getdata_cat(df_com)
#
#   }else if (trait_types[i] == "cat"){
#
#     df_com <- df_com %>%
#       group_by(species) %>%
#       mutate(mt = mean(trait_cont)) %>%
#       ungroup() %>%
#       mutate(trait_d1 = dnorm(x = mt, mean = -5),
#              trait_d2 = dnorm(x = mt, mean = -1.7),
#              trait_d3 = dnorm(x = mt, mean = 1.7),
#              trait_d4 = dnorm(x = mt, mean = 5)) %>%
#       mutate(mt = trait_d1 + trait_d2 + trait_d3 + trait_d4,
#              trait_d1 = (trait_d1/mt) %>% round(4),
#              trait_d2 = (trait_d2/mt) %>% round(4),
#              trait_d3 = (trait_d3/mt) %>% round(4),
#              trait_d4 = (trait_d4/mt) %>% round(4)) %>%
#       select(-mt)
#
#     eftest(
#       rep = 1, npairs = 1000,
#       comdata = df_com,
#       envdata = df_env,
#       env_idx = "env",
#       com_idx = "patch",
#       trait_idx = c("trait_d1", "trait_d2", "trait_d3", "trait_d4"),
#       taxa_idx = "species",
#       abun_idx = NULL,
#       x_idx = "x",
#       y_idx = "y",
#       trait_FUN = enfiltest::dist_distr,
#       phyl_FUN = enfiltest::dist_phyl,
#       phtree = df1_phyl
#     )
#
#   }else{
#
#     eftest(
#       rep = 1, npairs = 1000,
#       comdata = df_com,
#       envdata = df_env,
#       env_idx = "env",
#       com_idx = "patch",
#       trait_idx = "trait_cont",
#       taxa_idx = "species",
#       abun_idx = NULL,
#       x_idx = "x",
#       y_idx = "y",
#       trait_FUN = enfiltest::dist_cont,
#       phyl_FUN = enfiltest::dist_phyl,
#       phtree = df1_phyl
#     )
#
#   }
#
#
#
# }
#
# stopCluster(cl)
#
# tibble(
#   importance = random_importance,
#   tr_type = trait_types,
#   test = test
# ) %>%
#   write_csv("./eft-test-results.csv")
#
# test <- read_csv("../hpc_eft_cor/eft-test-results0.csv") %>%
#   bind_rows(
#     read_csv("../hpc_eft_cor/eft-test-results1.csv")
#   ) %>%
#   filter(tr_type != "cat")
#
# test <- test %>%
#   bind_rows(
#     read_csv("../hpc_eft_cor/eft-test-results2.csv")
#   )
#
# test %>%
#   # filter(importance > 0.2) %>%
#   ggplot(aes(x = importance, y = test,
#              color = factor(tr_type, levels = c("cont", "cat", "dist"), labels = c("Continuous", "Categorical", "Distribution")))) +
#   geom_point(alpha = 0.2, stroke = 0) +
#   geom_smooth() +
#   labs(xlab = "Trait importance", ylab = "Test statistic", color = "Trait type") +
#   geom_vline(xintercept = c(1, 0.75, 0.5, 0.2, 0.05))
