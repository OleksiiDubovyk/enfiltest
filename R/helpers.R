#' Pair sampling without edge effects
#'
#' @param vec A vector
#'
#' @description
#' Pairwise sampling of a sample of values drawn from a uniform distribution will inadvertedly produce such a sample that an absolute difference across the value pairs will have a triangular distribution.
#' E.g., \code{x <- runif(10000); y <- sample(x, 10000) - sample(x, 10000); hist(abs(y))}.
#'
#' This function attempts to balance pairwise sampling in such a way that the differences across value pairs are distributed normally.
#' Note that the output is still skewed but not as triangular.
#'
#' @returns A pair of values
#'
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @importFrom graphics hist
#'
#' @export
#'
#' @examples
#' x <- runif(10000)
#' y <- sapply(1:1000, function(i) ucon(x))
#' hist(abs(y[1,] - y[2,]))
ucon <- function(vec){
  x1 <- sample(vec, size = 1)
  vec1 <- vec[vec != x1]
  d1 <- abs(vec1 - x1)
  w1 <- stats::dnorm(x = d1, mean = mean(d1), sd = sd(d1)) # give largest weights to intermediate distances
  x2 <- sample(vec1, size = 1, prob = w1)
  c(x1, x2)
}

#' A "not in" operator
#'
#' @param x vector or NULL: the values to be matched
#' @param y vector or NULL: the values to be matched against
#'
#' @returns An opposite of the \code{match} operator.
#' @export
`%notin%` <- function(x, y){!`%in%`(x, y)}

