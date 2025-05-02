#'
#' @param n,iter
#'
#' @returns histogram of uniform distribution of n sample means
#'
#' @export
#'
#'
#
myclt = function(n, iter) {
y = runif(n * iter, 0, 5)  # A

data = matrix(y, nr = n, nc = iter, byrow = TRUE)  # B

sm = apply(data, 2, mean)  # C

hist(sm, main = "Histogram of Sample Means", xlab = "Sample Means")

return(sm)
}
