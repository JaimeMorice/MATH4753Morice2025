#'get TSS function
#'
#' @param k  A quantitative vector
#'
#' @returns a scalar quantity of TSS value
#' @export
#'
#'
#'
getTSS <- function(k){
  sum((k-mean(k))^2)
}
