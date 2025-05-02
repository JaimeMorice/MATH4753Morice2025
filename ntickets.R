#'
#' @param N,gamma,p
#'
#' @returns list of nd,nc,N,p,gamma values
#' @returns plots of objective function (discrete distr) vs n and
#'  objective function (normal approx) vs n (n=number tickets sold)
#'
#'
#'
#' @export
#'
#'


ntickets <- function(N,gamma,p) {

  objectiveFunctionDiscrete <- function(n) {
    return(1-gamma-pbinom(N,n,p))
  }
  objectiveFunctionNormal <- function(n) {
    meanApprox <- n*p
    sdApprox <- sqrt(n*p*(1-p))
    return(1-gamma-pnorm(N,meanApprox,sdApprox))
  }
  nVals <- seq(1,1000,by=1)
  objectiveValsDiscrete <- sapply(nVals, objectiveFunctionDiscrete)
  objectiveValsNormal <- sapply(nVals, objectiveFunctionNormal)

  indDiscrete <- which.min(abs(objectiveValsDiscrete))
  nd <- nVals[indDiscrete]

  indNormal <- which.min(abs(objectiveValsNormal))
  nc <- nVals[indNormal]

  listWithParts <- list(nd=nd,nc=nc,N=N,p=p,gamma=gamma)

  print(listWithParts)

  plot(nVals,objectiveValsDiscrete, type = 'l', col = 'blue',
       main = "Objective Function vs n (Discrete Distribution)",
       xlab = "Number of Tickets Sold (n)",
       ylab = "Objective Function")
  abline(h=0,lty=2,col="red") #horizontal at y=0
  points(nVals[indDiscrete],objectiveValsDiscrete[indDiscrete],
         pch = 21, bg = "red", cex = 2) # minimum point

  plot(nVals,objectiveValsNormal,type='l',col="green",
       main = "Objective Function vs n (Normal Approximation)",
       xlab = "Number of Tickets Sold (n)",
       ylab = "Objective Function")
  abline(h=0,lty=2,col="red") #anotha one (horizontal at y=0)
  points(nVals[indNormal], objectiveValsNormal[indNormal],
         pch=21,bg="red", cex=2) # anotha one (min point)

  return(listWithParts)
}
