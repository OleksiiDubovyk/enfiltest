#' Distance function for a continuous trait
#'
#' @param df1 A dataset describing community one with at least one column corresponding to trait values
#' @param df2 A dataset describing community two with at least one column corresponding to trait values; \code{df1} and \code{df2} may have different length
#' @param trait_idx Character; column name of the column with trait values
#' @param abun_idx Character; column name of the column with species abundance values; leave \code{NULL} if rows in \code{df1} and \code{df2} represent individuals but not species
#'
#' @returns A mean absolute difference between all individual traits across communities.
#'
#' @import tidyverse
#' @importFrom stats rpois
#' @importFrom stats rnorm
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#'
#' @export
#'
#' @examples
#' df1 = data.frame(trait = rnorm(n = 3, mean = -1),
#'   n = rpois(n = 3, lambda = 5))
#' df2 = data.frame(trait = rnorm(n = 5, mean = 1),
#'   n = rpois(n = 5, lambda = 5))
#' dist_cont(df1, df2, trait_idx = "trait", abun_idx = "n")
dist_cont <- function(df1, df2, trait_idx, abun_idx = NULL){

  if (length(trait_idx) != 1) stop("'trait_idx' argument in dist_cont() does NOT have length 1; only one trait can be supplied\n", call. = FALSE)

  t1 <- df1[, trait_idx] %>% unlist() %>% unname()
  t2 <- df2[, trait_idx] %>% unlist() %>% unname()

  mx <- outer(t1, t2, '-') %>% abs()

  if (!is.null(abun_idx)){
    p1 <- (unlist(df1[, abun_idx])/sum(df1[, abun_idx])) %>% unname()
    p2 <- (unlist(df2[, abun_idx])/sum(df2[, abun_idx])) %>% unname()
    return(
      sum(mx * outer(p1, p2))
    )
  }else{
    return(
      mean(mx)
    )
  }

}

#' Distance function for a categorical trait
#'
#' @param df1 A dataset describing community one with at least one column corresponding to trait values
#' @param df2 A dataset describing community two with at least one column corresponding to trait values; \code{df1} and \code{df2} may have different length
#' @param trait_idx Character; column name of the column with trait values
#' @param abun_idx Character; column name of the column with species abundance values; leave \code{NULL} if rows in \code{df1} and \code{df2} represent individuals but not species; if not \code{NULL}, corresponding columns in \code{df1} and \code{df2} must be integer
#' @param dmatrix Square matrix; distance matrix between the values of the categorical trait; make sure that col/row-names are the same as all possible trait values
#'
#' @returns A mean distance (as specified in distance matrix) between all individual traits across the two communities.
#'
#' @export
#'
#' @examples
#' df1 = data.frame(trait = c(1, 2, 3, 1, 2, 3),
#'   n = rpois(n = 6, lambda = 5))
#' df2 = data.frame(trait = c(1, 1, 2, 3, 4),
#'   n = rpois(n = 5, lambda = 5))
#' dmatrix = matrix(c(0, 1, 2, 3, 1, 0, 1, 2, 2, 1, 0, 1, 3, 2, 1, 0),
#'   nrow = 4, byrow = TRUE)
#' colnames(dmatrix) = 1:4; rownames(dmatrix) = 1:4
#' dist_cat(df1, df2, trait_idx = "trait", abun_idx = "n",
#'   dmatrix = dmatrix)
dist_cat <- function(df1, df2, trait_idx, abun_idx = NULL, dmatrix){

  if (length(trait_idx) != 1) stop("'trait_idx' argument in dist_cat() does NOT have length 1; only one trait can be supplied\n", call. = FALSE)
  if (!setequal(colnames(dmatrix), rownames(dmatrix))) stop("colnames and ronwames of `dmatrix` argument in dist_cat() should be the same\n", call. = FALSE)

  t1 <- df1[, trait_idx] %>% unlist() %>% unname()
  t2 <- df2[, trait_idx] %>% unlist() %>% unname()

  if (any(unique(c(t1, t2)) %notin% colnames(dmatrix))) {
    extratraits <- unique(c(t1, t2))[unique(c(t1, t2)) %notin% colnames(dmatrix)]
    stop(paste0("some trait values from 'df1' or 'df2' are not indexed in 'dmatrix'; look for the following trait values:\n", paste(extratraits, collapse = ", "), "\n"))
  }

  if (!is.null(abun_idx)){
    p1 <- (unlist(df1[, abun_idx])) %>% unname()
    p2 <- (unlist(df2[, abun_idx])) %>% unname()

    t1 <- rep(t1, times = p1)
    t2 <- rep(t2, times = p2)
  }

  d <- expand.grid(t1, t2) %>%
    as_tibble() %>%
    group_by(.data$Var1, .data$Var2) %>%
    summarise(n = n(), .groups = "drop") %>%
    ungroup()
  d$d <- sapply(1:nrow(d), function(i) dmatrix[unlist(d[i, "Var1"]), unlist(d[i, "Var2"])])
  return(
    sum((d$n/sum(d$n)) * d$d)
  )

}

#' Distance function for a distributional trait
#'
#' @param df1 A dataset describing community one with at least one column corresponding to trait values
#' @param df2 A dataset describing community two with at least one column corresponding to trait values; \code{df1} and \code{df2} may have different length
#' @param trait_idx Character; column name of the column with trait values
#' @param abun_idx Character; column name of the column with species abundance values; leave \code{NULL} if rows in \code{df1} and \code{df2} represent individuals but not species
#'
#' @returns A mean sum of deviations across discrete distributions across all individuals in the two communities.
#'
#' @importFrom purrr map_dfr
#'
#' @export
#'
#' @examples
#' df1 = data.frame(n = rpois(n = 6, lambda = 5),
#'   t1 = c(1, 1, 1, 0.5, 0.5, 0.25),
#'     t2 = 1 - c(1, 1, 1, 0.5, 0.5, 0.25))
#' df2 = data.frame(n = rpois(n = 5, lambda = 5),
#'   t1 = c(0.1, 0.2, 0.5, 0.7, 0.8),
#'     t2 = 1 - c(0.1, 0.2, 0.5, 0.7, 0.8))
#' dist_distr(df1, df2, trait_idx = c("t1", "t2"), abun_idx = "n")
dist_distr <- function(df1, df2, trait_idx, abun_idx = NULL){

  t1 <- df1[,trait_idx]
  t2 <- df2[,trait_idx]

  if (!is.null(abun_idx)){
    p1 <- (unlist(df1[, abun_idx])/sum(df1[, abun_idx])) %>% unname()
    p2 <- (unlist(df2[, abun_idx])/sum(df2[, abun_idx])) %>% unname()
  } else {
    p1 <- rep(1, nrow(df1))/nrow(df1)
    p2 <- rep(1, nrow(df2))/nrow(df2)
  }

  p <- outer(p1, p2) %>% as.vector()

  d <- abs(
    (purrr::map_dfr(
      seq_len(nrow(t2)), function(x) t1
    ) %>%
      as.matrix()) -
      (
        t2[rep(1:nrow(t2), each = nrow(t1)),] %>%
          as.matrix()
      )
  ) %>%
    apply(1, sum)

  return(
    sum(d*p)
  )

}

#' Distance function for community phylogenies
#'
#' @param df1 A dataset describing community one with at least one column corresponding to trait values
#' @param df2 A dataset describing community two with at least one column corresponding to trait values; \code{df1} and \code{df2} may have different length
#' @param taxa_idx Character; column name of the column with taxa identities
#' @param abun_idx Character; column name of the column with species abundance values; leave \code{NULL} if rows in \code{df1} and \code{df2} represent individuals but not species; if not \code{NULL}, corresponding columns in \code{df1} and \code{df2} must be integer
#' @param phtree Phylogenetic tree (\code{ape} class "\code{phylo}") that includes all taxa from both communities
#'
#' @returns A phylogenetic distance between two communities.
#'
#' @importFrom ape keep.tip.phylo
#' @importFrom ape cophenetic.phylo
#' @importFrom ape rtree
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' df1 = data.frame(species = paste0("t", 1:6),
#'   n = rpois(n = 6, lambda = 5))
#' df2 = data.frame(species = paste0("t", 3:7),
#'   n = rpois(n = 5, lambda = 5))
#' phtree <- ape::rtree(10)
#' dist_phyl(df1, df2, taxa_idx = "species", abun_idx = "n",
#'   phtree = phtree)
dist_phyl <- function(df1, df2, taxa_idx, abun_idx = NULL, phtree){

  if (length(taxa_idx) != 1) stop("'taxa_idx' argument in dist_phyl() does NOT have length 1\n", call. = FALSE)

  if (!is(phtree, "phylo")){
    stop("'phtree' argument in dist_phyl() must be an object of 'phylo' class (see package 'ape')\n",
         call. = FALSE)
  }

  t1 <- df1[, taxa_idx] %>% unlist() %>% unname()
  t2 <- df2[, taxa_idx] %>% unlist() %>% unname()

  if (any(unique(c(t1, t2)) %notin% phtree$tip.label)) {
    extratraits <- unique(c(t1, t2))[unique(c(t1, t2)) %notin% phtree$tip.label]
    stop(paste0("some taxa from 'df1' or 'df2' are not indexed in the 'phtree'; look for the following taxa/tree tip labels:\n", paste(extratraits, collapse = ", "), "\n"))
  }

  pruned_tree <- ape::keep.tip.phylo(phtree, tip = unique(c(t1, t2)))

  dmatrix <- enfiltest::tree2phylmatr(pruned_tree)

  if (!is.null(abun_idx)){
    p1 <- (unlist(df1[, abun_idx])) %>% unname()
    p2 <- (unlist(df2[, abun_idx])) %>% unname()

    t1 <- rep(t1, times = p1)
    t2 <- rep(t2, times = p2)
  }

  d <- expand.grid(t1, t2) %>%
    as_tibble() %>%
    group_by(.data$Var1, .data$Var2) %>%
    summarise(n = n(), .groups = "drop") %>%
    ungroup()
  d$d <- sapply(1:nrow(d), function(i) dmatrix[unlist(d[i, "Var1"]), unlist(d[i, "Var2"])])
  return(
    sum((d$n/sum(d$n)) * d$d)
  )

}
