#' Get phylogenetic distances matrix from a phylogenetic tree
#'
#' @param phtree Phylogenetic tree, a 'phylo' class object (see 'ape' package)
#'
#' @description
#' A formal wrapper around the original \code{ape::cophenetic.phylo()} function.
#'
#'
#' @returns Phylogenetic distance matrix
#'
#' @import dplyr
#' @importFrom ape cophenetic.phylo
#' @importFrom ape rtree
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' ape::rtree(n = 5) %>% tree2phylmatr()
#'
tree2phylmatr <- function(phtree){

  if (!is(phtree, "phylo")){
    stop("'phtree' argument in tree2phylmatr() must be an object of 'phylo' class (see package 'ape')\n",
         call. = FALSE)
  }

  ape::cophenetic.phylo(phtree)
}

#' Wrapper around a distance function
#'
#' @description
#' Rao's quadratic entropy for a community is defined as \eqn{\sum_{m,n=1}^{S}d_{m,n} \cdot p_m p_n} where \eqn{m, n} are species, \eqn{d_{m,n}} is a distance between species \eqn{m} and \eqn{n}, and \eqn{p} is a weight of a species in the community (often calculated as the ratio of its abundance to the total abundance of all species).
#'
#' We can use a similar idea to estimate difference between two communities: for two communities with individuals indexed as {\eqn{i}} and \eqn{j}, respectively, expected distance between the two individuals randomly drawn from the two communities can be estimated as \eqn{mean(d_{i, j})}.
#' Depending on whether the distance is calculated for functional traits of different data types, or phylogenies, the distance function can take a form of Euclidean distance between trait values, Gower distance between categorical traits, branch lengths within a phylogenetic tree, etc.
#'
#' @param df1 A dataset describing community one with at least one column corresponding to trait values
#' @param df2 A dataset describing community two with at least one column corresponding to trait values; \code{df1} and \code{df2} may have different length
#' @param FUN Distance function; from this package, the options include \code{dist_cont()}, \code{dist_cat()}, \code{dist_distr()}, and \code{dist_phyl()}, but manually defined functions could also work (such functions must have a \code{df1} and \code{df2} arguments).
#' @param ... Additional arguments passed to \code{FUN}.
#'
#' @returns A distance between two communities.
#' @export
#'
#' @examples
#' df1 = tibble::tibble(trait = rnorm(n = 3, mean = -1),
#'   n = rpois(n = 3, lambda = 5))
#' df2 = tibble::tibble(trait = rnorm(n = 5, mean = 1),
#'   n = rpois(n = 5, lambda = 5))
#' rao_distance(df1, df2, FUN = enfiltest::dist_cont, trait_idx = "trait")
rao_distance <- function(df1, df2, FUN, ...){
  FUN(df1 = df1, df2 = df2, ...)
}
