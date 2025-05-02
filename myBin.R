#' myBin function
#'
#' @param iter,n,p A quantitative vector
#'
#' @returns barplot for binned data
#' @export
#'
#' @examples
#' mybin(iter=100,n=3,p=0.7)

mybin=function(iter=100,n=10, p=0.5){ #making function mybin
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE) # make NA filled matrix
  succ=c() #vector holding num of successes in each trial
  for( i in 1:iter){ #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a sum from the sample
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n)) #Make successes table
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
