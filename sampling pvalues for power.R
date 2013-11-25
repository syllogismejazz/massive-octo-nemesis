#Sampling K-stat pvalues for
#Laplace Standard (mean=c(0,0) and var=Identity) 
# VERSUS
#Laplace mean=c(1,1) and var=4*Identity

sampling.pvalues.for.power <- function(m=10, gamma=0.5, repeats=10, c_N1=0.5, c_N2=0.5){
  
  p_values = 0
  
  #gamma = m / m+n so:
  n = ceiling((m/gamma)-m)
  
  #Using Replicate() instead of loop#
  p_values = replicate(repeats,K.statistic(rmvnorm(m,mean=c(0,0), sigma=diag(2)), rmvnorm(n, mean=c(-1,-1), sigma=4*diag(2)),c_N1,c_N2)$p_value)
  
  #for t
  #p_values = ...
  
  #for laplace
  #p_values = replicate(repeats,K.statistic(random.standard.bivariate.laplace(m), random.nonstandard.bivariate.laplace(n),c_N1,c_N2)$p_value)
  
  
  return(p_values)  
  
}