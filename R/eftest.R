#' Pairwise differences between random communities
#'
#' @description
#' Estimating difference in topological location, environmental conditions, trait structure, and (if phylogeny provided) phylogenetic relatedness between random pairs of communities.
#'
#'
#' @param comdata Data frame containing the data on structure of communities: it should include community identifiers, species identities, trait values, and (optionally) species abundances.
#' @param envdata Data frame containing the data on environmental conditions: it should include community identifiers (values equivalent to those in \code{comdata}), environmental factor levels, and geographical coordinates (for large scale, make sure to transform the coordinate system).
#' @param npairs Integer. Number of random community pairs to pick; this function will not pick more than a square of number of communities in the data (e.g., all pairs will be unique).
#' @param env_idx String, the name of a column in \code{envdata} that contains the values of the \strong{environmental factor}.
#' @param com_idx String, the name of a column in \code{envdata} and \code{comdata} that contains \strong{community identifiers}. The column name should be the same in both data frames.
#' @param trait_idx String, the name of a column in \code{comdata} that contains the data on \strong{trait values}.
#' @param taxa_idx String, the name of a column in \code{comdata} that contains \strong{species identifiers}.
#' @param abun_idx \code{NULL} or string, the name of a column in \code{comdata} that contains \strong{species counts}. If \code{NULL}, all counts are assumed to be equal to one (e.g., when rows in \code{comdata} correspond to individuals rather than species).
#' @param x_idx String, the name of a column in \code{envdata} that contain the \strong{x-coordinate} of a community (e.g., longitude).
#' @param y_idx String, the name of a column in \code{envdata} that contain the \strong{y-coordinate} of a community (e.g., latitude).
#' @param trait_FUN Function, a distance function to assess trait-based distance between the communities. Options include \code{dist_cont}, \code{dist_cat}, \code{dist_distr}, or a manually written function.
#' @param phyl_FUN Function, a distance function to assess phylogeny-based distance between the communities. Options include \code{dist_phyl} or a manually written function. If missing but \code{phtree} is not, the \code{enfiltest::dist_phyl} is used by default. If both are missing, phylogenetic distance is not estimated.
#' @param phtree Phylogenetic tree (\code{ape} class "\code{phylo}") that includes all taxa from all communities in \code{comdata}. If missing but \code{phyl_FUN} is not, a random tree is generated. If both are missing, phylogenetic distance is not estimated.
#' @param ... Additional arguments for \code{trait_FUN}.
#'
#' @returns A tibble with identifiers of communities in a pair (\code{c1, c2}), topological (Euclidean) distance between the communities (\code{d_dist}), difference in environmental conditions (\code{d_env}), phylogenetic difference (\code{d_phyl}), and trait difference (\code{d_trait}).
#'
#' @importFrom ape rtree
#' @importFrom tidyr expand_grid
#'
#' @export
#'
#' @examples
#' test_comdata = df1_com %>% dplyr::mutate(n = 1)
#' test_envdata <- df1_env %>% dplyr::mutate(x = patch, y = 0)
#' rand_pair_diff(comdata = test_comdata,
#'   npairs = 10,
#'   envdata = test_envdata,
#'   env_idx = "env",
#'   com_idx = "patch",
#'   trait_idx = "trait_cont",
#'   taxa_idx = "species",
#'   abun_idx = NULL,
#'   x_idx = "x", y_idx = "y",
#'   trait_FUN = dist_cont, phyl_FUN = dist_phyl,
#'   phtree = df1_phyl)
rand_pair_diff <- function(comdata, envdata, npairs = 1000, env_idx, com_idx, trait_idx, taxa_idx, abun_idx = NULL, x_idx, y_idx, trait_FUN, phyl_FUN, phtree, ...){

  if (length(com_idx) != 1) stop("'com_idx' argument in rand_pair_diff() does NOT have length 1; only one community ID can be supplied\n", call. = FALSE)
  if (length(taxa_idx) != 1) stop("'taxa_idx' argument in rand_pair_diff() does NOT have length 1; only one taxa ID can be supplied\n", call. = FALSE)
  if (length(x_idx) != 1) stop("'x_idx' argument in rand_pair_diff() does NOT have length 1; only one x-coord varname can be supplied\n", call. = FALSE)
  if (length(y_idx) != 1) stop("'y_idx' argument in rand_pair_diff() does NOT have length 1; only one y-coord varname can be supplied\n", call. = FALSE)
  if (missing(phyl_FUN) & !missing(phtree)){
    warning("'phyl_FUN' was not supplied in rand_pair_diff(), set to enfiltest::dist_phyl")
    phyl_FUN = enfiltest::dist_phyl
  }
  taxa <- unlist(unique(comdata[,taxa_idx]))
  if (missing(phtree) & !missing(phyl_FUN)){
    warning("'phtree' was not supplied in rand_pair_diff(), set to a random phylogeny")
    phtree <- ape::rtree(n = length(taxa), tip.label = taxa)
  }
  if (nrow(envdata) != length(unlist(unique(envdata[,com_idx])))) stop("'envdata' in rand_pair_diff() must contain unique community ID (check distinct(envdata))\n")

  coms <- envdata[, com_idx] %>% unlist() %>% unname()

  # create index of output pairs
  pair_index <- tidyr::expand_grid(coms, coms)
  colnames(pair_index) <- c("c1", "c2")
  np <- nrow(pair_index)
  if (np < npairs){
    warning(paste0("in the data supplied into rand_pair_diff(), there are ", np, " possible pairs of communities, which is less than npairs=", npairs, "; output reduced to ", np, "\n"))
    out <- pair_index
  }else{
    out <- pair_index[sample(x = 1:np, size = npairs, replace = FALSE),]
  }
  npairs <- nrow(out)

  if (missing(phtree) & missing(phyl_FUN)){

    warning("both 'phyl_FUN' and 'phtree' arguments missing from rand_pair_diff(), phylogeny ignored\n")

    out_dist <- lapply(1:npairs, function(i){

      ci1 <- unlist(out[i, "c1"]) %>% unname()
      ci2 <- unlist(out[i, "c2"]) %>% unname()

      com1 <- comdata[unlist(comdata[,com_idx]) == ci1,]
      com2 <- comdata[unlist(comdata[,com_idx]) == ci2,]

      c(
        "d_dist" = sqrt(
          (unlist(envdata[unlist(envdata[,com_idx]) == ci1, x_idx]) - unlist(envdata[unlist(envdata[,com_idx]) == ci2, x_idx]))^2 +
            (unlist(envdata[unlist(envdata[,com_idx]) == ci1, y_idx]) - unlist(envdata[unlist(envdata[,com_idx]) == ci2, y_idx]))^2
        ) %>% unname(),
        "d_env" = abs(unlist(envdata[unlist(envdata[,com_idx]) == ci1, env_idx]) - unlist(envdata[unlist(envdata[,com_idx]) == ci2, env_idx])) %>% unname(),
        "d_trait" = rao_distance(com1, com2, trait_FUN, trait_idx = trait_idx, abun_idx = abun_idx, ...)
      )
    }) %>% bind_rows()

  }else{

    out_dist <- lapply(1:npairs, function(i){

      ci1 <- unlist(out[i, "c1"]) %>% unname()
      ci2 <- unlist(out[i, "c2"]) %>% unname()

      com1 <- comdata[unlist(comdata[,com_idx]) == ci1,]
      com2 <- comdata[unlist(comdata[,com_idx]) == ci2,]

      c(
        "d_dist" = sqrt(
          (unlist(envdata[unlist(envdata[,com_idx]) == ci1, x_idx]) - unlist(envdata[unlist(envdata[,com_idx]) == ci2, x_idx]))^2 +
            (unlist(envdata[unlist(envdata[,com_idx]) == ci1, y_idx]) - unlist(envdata[unlist(envdata[,com_idx]) == ci2, y_idx]))^2
        ) %>% unname(),
        "d_env" = abs(unlist(envdata[unlist(envdata[,com_idx]) == ci1, env_idx]) - unlist(envdata[unlist(envdata[,com_idx]) == ci2, env_idx])) %>% unname(),
        "d_phyl" = rao_distance(com1, com2, phyl_FUN, taxa_idx = taxa_idx, abun_idx = abun_idx, phtree = phtree),
        "d_trait" = rao_distance(com1, com2, trait_FUN, trait_idx = trait_idx, abun_idx = abun_idx, ...)
      )
    }) %>% bind_rows()

  }

  return(
    bind_cols(
      out,
      out_dist
    )
  )

}

#' Return a set of GAMs explaining difference in community trait structure
#'
#' @details
#' The null model is assumed to be a simple intercept model.
#'
#' For the term of spatial distance between community locations, a simple cubic regression spline function is used.
#'
#' For the terms corresponding to difference in environmental level and phylogenetic structure, a shape-constrained additive model is used with a monotonically increasing spline function.
#'
#' @param rpd A tibble, output of the \code{rand_pair_diff} function containing columns for spatial distance between communities (\code{d_dist}), difference in an environmental level (\code{d_env}), difference in trait structure (\code{d_trait}), and (optional) difference in phylogenetic structure (\code{d_phyl}).
#'
#' @returns A list of lists with two elements each: a vector indicating whether a spatial/environmental/phylogenetic distance term was included in the model and the \code{scam::scam} model object.
#'
#' @importFrom scam scam
#'
#' @export
#'
#' @examples
#' test_comdata = df1_com %>% dplyr::mutate(n = 1)
#' test_envdata <- df1_env %>% dplyr::mutate(x = patch, y = 0)
#' x = rand_pair_diff(comdata = test_comdata,
#'   npairs = 100,
#'   envdata = test_envdata,
#'   env_idx = "env",
#'   com_idx = "patch",
#'   trait_idx = "trait_cont",
#'   taxa_idx = "species",
#'   abun_idx = NULL,
#'   x_idx = "x", y_idx = "y",
#'   trait_FUN = dist_cont, phyl_FUN = dist_phyl,
#'   phtree = df1_phyl)
#' eftest_models(x)
eftest_models <- function(rpd){
  rpd <- rpd %>% mutate(across(all_of(c("d_dist", "d_env", "d_phyl", "d_trait")), scale))

  f_null <- scam::scam(formula = d_trait ~ 1, data = rpd)
  f_dist <- scam::scam(formula = d_trait ~ s(d_dist, bs = "cr"), data = rpd)
  f_env <- scam::scam(formula = d_trait ~ s(d_env, bs = "mpi"), data = rpd)
  f_dist_env <- scam::scam(formula = d_trait ~ s(d_env, bs = "mpi") + s(d_dist, bs = "cr"), data = rpd)
  if ("d_phyl" %in% colnames(rpd)){
    f_phyl <- scam::scam(formula = d_trait ~ s(d_phyl, bs = "mpi"), data = rpd)
    f_phyl_env <- scam::scam(formula = d_trait ~ s(d_env, bs = "mpi") + s(d_phyl, bs = "mpi"), data = rpd)
    f_phyl_dist_env <- scam::scam(formula = d_trait ~ s(d_env, bs = "mpi") + s(d_phyl, bs = "mpi") + s(d_dist, bs = "cr"), data = rpd)
  }

  if ("d_phyl" %in% colnames(rpd)){
    list(
      list(c("dist" = 0, "env" = 0, "phyl" = 0), f_null),
      list(c("dist" = 1, "env" = 0, "phyl" = 0), f_dist),
      list(c("dist" = 0, "env" = 1, "phyl" = 0), f_env),
      list(c("dist" = 1, "env" = 1, "phyl" = 0), f_dist_env),
      list(c("dist" = 0, "env" = 0, "phyl" = 1), f_phyl),
      list(c("dist" = 0, "env" = 1, "phyl" = 1), f_phyl_env),
      list(c("dist" = 1, "env" = 1, "phyl" = 1), f_phyl_dist_env)
    )
  }else{
    list(
      list(c("dist" = 0, "env" = 0, "phyl" = 0), f_null),
      list(c("dist" = 1, "env" = 0, "phyl" = 0), f_dist),
      list(c("dist" = 0, "env" = 1, "phyl" = 0), f_env),
      list(c("dist" = 1, "env" = 1, "phyl" = 0), f_dist_env)
    )
  }

}

#' Return a mean SCAM spline derivative
#'
#' @param fit A \code{scam::scam} model as constructed by \code{eftest_models()[[2]]}
#'
#' @returns A mean derivative
#'
#' @importFrom scam predict.scam
#' @importFrom stats density
#' @importFrom stats approx
#'
#' @export
eftest_gam_dx <- function(fit){
  A <- fit$model
  if ("d_env" %notin% colnames(A)){
    return(0)
  }else{
    env <- sort(A$d_env)

    if (("d_dist" %notin% colnames(A)) & ("d_phyl" %notin% colnames(A))){
      # env
      newDF <- data.frame(d_env = env )

    } else if (("d_dist" %in% colnames(A)) & ("d_phyl" %notin% colnames(A))){
      # env - dist
      newDF <- data.frame(d_env = env,
                          d_dist = mean(A$d_dist))

    } else if (("d_dist" %notin% colnames(A)) & ("d_phyl" %in% colnames(A))){
      # env - phyl
      newDF <- data.frame(d_env = env,
                          d_phyl = mean(A$d_phyl))

    } else if (("d_dist" %in% colnames(A)) & ("d_phyl" %in% colnames(A))){
      # env - dist - phyl
      newDF <- data.frame(d_env = env,
                          d_phyl = mean(A$d_phyl),
                          d_dist = mean(A$d_dist))

    }

    X0 <- scam::predict.scam(fit,
                  newDF,
                  type = "response")

    dn <- 0.001*(max(newDF$d_env) - min(newDF$d_env))

    newDF1 <- newDF
    newDF1$d_env <- newDF1$d_env - dn

    X1 <- scam::predict.scam(fit,
                  newDF1,
                  type = "response")
    dterm <- (X0 - X1)/dn

    dens_env <- density(x = newDF1$d_env)
    w_dens <- approx(dens_env$x, dens_env$y, xout = newDF1$d_env)$y
    w_dens <- w_dens/sum(w_dens)

    dx <- sum(dterm * w_dens)

    return(dx)
  }
}

#' Generate AIC table with a mean SCAM spline derivative
#'
#' @param models A list of \code{scam::scam} models as constructed by \code{eftest_models()}
#'
#' @returns A tibble with columns indicating terms in the models, AIC and ΔAIC, log-likelihood, estimated degrees of freedom, AIC weight, and a mean derivative for \code{d_env} SCAM term.
#'
#' @importFrom qpcR akaike.weights
#'
#' @export
#'
eftest_rankAIC <- function(models){
  trms <- lapply(models, function(m) m[[1]]) %>% bind_rows()
  aics <- numeric(0)
  lLs <- numeric(0)
  dfs <- numeric(0)
  denv_dx <- numeric(0)
  for (model in models){
    aics <- c(aics, model[[2]]$aic)
    lLs <- c(lLs, (scam::logLik.scam(model[[2]]) %>% unclass())[[1]])
    dfs <- c(dfs, sum(model[[2]]$edf))
    denv_dx <- c(denv_dx, eftest_gam_dx(model[[2]]))
  }
  aictable <- bind_cols(
    trms,
    tibble(
      AIC = aics %>% round(4),
      logLik = lLs %>% round(4),
      df = dfs %>% round(2),
      denv_dx = denv_dx %>% round(4)
    )
  )
  aictable$delta_AIC <- aictable$AIC - min(aictable$AIC) %>% round(4)
  aictable$weight <- qpcR::akaike.weights(aictable$AIC)$weights %>% round(4)
  aictable %>%
    arrange(.data$delta_AIC)
}

#' Get AIC-weighted SCAM spline derivative for env term
#'
#' @param efAICtable An AIC table generated by \code{eftest_rankAIC()}
#' @param aic_threshold Threshold of delta-AIC for the null model; if the null model is within this value of delta-AIC, the test score is automatically zero; default to delta-AIC = 2.
#'
#' @returns AIC-weighted SCAM spline derivative for env term
#' @export
eftest_stat <- function(efAICtable, aic_threshold = 2){
  null_daic <- efAICtable %>%
    filter(.data$dist == 0, .data$env == 0, .data$phyl == 0)
  null_daic <- null_daic$delta_AIC %>%
    unlist() %>% unname()
  if (null_daic < aic_threshold){
    0
  } else {
    sum(efAICtable$denv_dx * efAICtable$weight)
  }
}

#' Generate AIC-weighted SCAM spline mean derivative for the effect of an environmental variable on community trait structure given spatial and phylogenetic constraints
#'
#' @param comdata Data frame containing the data on structure of communities: it should include community identifiers, species identities, trait values, and (optionally) species abundances.
#' @param envdata Data frame containing the data on environmental conditions: it should include community identifiers (values equivalent to those in \code{comdata}), environmental factor levels, and geographical coordinates (for large scale, make sure to transform the coordinate system).
#' @param npairs Integer. Number of random community pairs to pick; this function will not pick more than a square of number of communities in the data (e.g., all pairs will be unique).
#' @param rep Positive integer, how many times to repeat the procedure (set a larger number to estimate uncertainity, but it is computationally expensive)
#' @param env_idx String, the name of a column in \code{envdata} that contains the values of the \strong{environmental factor}.
#' @param com_idx String, the name of a column in \code{envdata} and \code{comdata} that contains \strong{community identifiers}. The column name should be the same in both data frames.
#' @param trait_idx String, the name of a column in \code{comdata} that contains the data on \strong{trait values}.
#' @param taxa_idx String, the name of a column in \code{comdata} that contains \strong{species identifiers}.
#' @param abun_idx \code{NULL} or string, the name of a column in \code{comdata} that contains \strong{species counts}. If \code{NULL}, all counts are assumed to be equal to one (e.g., when rows in \code{comdata} correspond to individuals rather than species).
#' @param x_idx String, the name of a column in \code{envdata} that contain the \strong{x-coordinate} of a community (e.g., longitude).
#' @param y_idx String, the name of a column in \code{envdata} that contain the \strong{y-coordinate} of a community (e.g., latitude).
#' @param trait_FUN Function, a distance function to assess trait-based distance between the communities. Options include \code{dist_cont}, \code{dist_cat}, \code{dist_distr}, or a manually written function.
#' @param phyl_FUN Function, a distance function to assess phylogeny-based distance between the communities. Options include \code{dist_phyl} or a manually written function. If missing but \code{phtree} is not, the \code{enfiltest::dist_phyl} is used by default. If both are missing, phylogenetic distance is not estimated.
#' @param phtree Phylogenetic tree (\code{ape} class "\code{phylo}") that includes all taxa from all communities in \code{comdata}. If missing but \code{phyl_FUN} is not, a random tree is generated. If both are missing, phylogenetic distance is not estimated.
#' @param aic_threshold Threshold of delta-AIC for the null model; if the null model is within this value of delta-AIC, the test score is automatically zero; default to delta-AIC = 2.
#' @param ... Additional arguments for \code{trait_FUN}.
#'
#' @returns A vector of length \code{rep} with AIC-weighted SCAM spline derivative means for each run.
#' @export
eftest <- function(comdata, envdata, npairs = 1000, rep = 1, env_idx, com_idx, trait_idx, taxa_idx, abun_idx = NULL, x_idx, y_idx, trait_FUN, phyl_FUN, phtree, aic_threshold = 2, ...){

  if (rep < 1) stop("'rep' in eftest() must be a positive integer")

  rep <- round(rep)

  test_stats <- numeric(rep)

  for (i in 1:rep){

    x <- rand_pair_diff(
      comdata = comdata,
      envdata = envdata,
      npairs = npairs,
      env_idx = env_idx,
      com_idx = com_idx,
      trait_idx = trait_idx,
      taxa_idx = taxa_idx,
      abun_idx = abun_idx,
      x_idx = x_idx,
      y_idx = y_idx,
      trait_FUN = trait_FUN,
      phyl_FUN = phyl_FUN,
      phtree = phtree,
      ...
    )

    test_stats[i] <- x %>%
      eftest_models() %>%
      eftest_rankAIC() %>%
      eftest_stat(aic_threshold = aic_threshold)

  }

  return(test_stats)

}
