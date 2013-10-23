unif.gen <- function(n=2, N=1){

#Generation of rv U^(n) unif. distrib. on unit hypershpere
#Default is in 2 dimension (bivariate)
#From Solaro, 2004. Used to generate MEP (and here, only bivariate Laplace)

#as deafult, n=2 (ie, bivariate)
#N is number of rv to return
  
x = matrix(rnorm(n*N), N)

norm.vec <- function(y){y/sqrt(sum(y^2))}

unif = t(apply(x,1,norm.vec))

return(unif)
  
}