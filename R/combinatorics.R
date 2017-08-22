#' variation 
#'
#' Variations are arrangements of selections of objects, where the order of the selected objects matters.
#' \code{variation} computes the number of \code{k}-element variations of \code{n} objects either with repetition 
#' or not.
#' 
#' \itemize{
#' \item{without repetition}{\eqn{V(n,k)=\frac{n!}{(n-k)!}}}
#' \item{with repetition}{\eqn{V(n,k)=n^k}}
#' }
#'
#' @param n numeric: number of elements to select from
#' @param k numeric: number of elements selected 
#' @param rep logical: with or without repetition (default: FALSE)
#'
#' @return numeric
#' @export
#'
#' @examples
#' variation(10, 3)
#' variation(10, 3, TRUE)
variation <- function (n, k, rep=F) {
	if (rep) n^k else prod((n-k+1):n)
}

#' combination
#' 
#' Combinations are arrangements of selections of objects, where the order of the selected objects does not matter.
#' \code{combination} computes the number of \code{k}-element combinations of \code{n} objects either with 
#' repetition or not.
#' 
#' \itemize{
#' \item{without repetition}{\eqn{C(n,k)=\choose{n}{k}}}
#' \item{with repetition}{\eqn{C(n,k)=\choose{n+k-1}{k}}}
#' }
#'
#' @param n numeric: number of elements to select from
#' @param k numeric: number of elements selected 
#' @param rep logical: with or without repetition (default: FALSE)
#'
#' @return numeric
#' @export
#'
#' @examples
#' combination(10, 3)
#' combination(10, 3, TRUE)
combination <- function (n, k, rep=F) {
	if (rep) choose(n+k-1,k) else choose(n,k)
}

#' permutation
#'
#' Permutations are arrangements of objects (with or without repetition), order does matter.
#' A permutation with repetition is an arrangement of objects, where some objects are repeated a 
#' prescribed number of times.
#'
#' \itemize{
#' \item{without repetition}{\eqn{P(n)=n!}}
#' \item{with repetition}{\eqn{P(n,k_1,k_2,...)=\frac{n!}{k_1!k_2!...}}}
#' }
#' 
#' @note If \eqn{k_1+k_2+... < n} then it assumed that for the rest the number of copies is always one.
#'
#' @param n numeric: number of elements to select from
#' @param ... numeric: number of repetitions copies
#'
#' @return numeric
#' @export
#'
#' @examples
#' permutation(10)
#' permutation(10, 5, 3) # equivalent to permutation(10, 5, 3, 1, 1)
#' permutation(10, 5, 3, 2) 
permutation <- function (n, ...) {
	args <- list(...)
	res  <- sum(log(1:n))
	grp  <- 0
	if (length(args)) grp <- unlist(lapply(args, function(g) {sum(log(1:g))})) 
	round(exp(res-sum(grp)))
}