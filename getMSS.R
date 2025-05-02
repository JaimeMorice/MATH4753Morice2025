#' get MSS function
#'
#' @param x A quantitative vector
#'
#' @returns A scalar quantity of MSS value
#' @export
#'
#'
#
getMSS <- function(x){
  sum((fitted(x)-mean(x))^2)
}
