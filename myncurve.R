#'
#'
#' @param mu,sigma,a  scalar quantities
#'
#' @returns shaded area between curve and x-axis from -infinity to x=a,
#'          calculates area (probability, P(X<=a))
#'          //released to the command-line in a list.
#' @export
#'
#'
#'
myncurve <- function(mu,sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim=c(mu-3*sigma,mu+3*sigma))
  list(mu=mu,sigma=sigma)

  xcurve1 = seq(mu-3*sigma,a,length=1000)
  ycurve1 = dnorm(xcurve1,mean=mu,sd=sigma)

  polygon(c(xcurve1,a), c(ycurve1,0), col = "Blue")

  area =pnorm(a,mean=mu,sd=sigma)
  area = round(area,4)
  return(area)
}

