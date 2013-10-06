sampling.pvalues.from.mv.standard.normal<- function(m=10, gamma=0.5, repeats=10, c_N1=0.5, c_N2=0.5){

p_values = 0
mean = c(0,0)
sigma = diag(2)

#gamma = m / m+n so:
n = ceiling((m/gamma)-m)

#Using Replicate() instead of loop#
p_values = replicate(repeats,K.statistic(rmvnorm(m,mean,sigma), rmvnorm(n,mean,sigma),c_N1,c_N2)$p_value)

return(p_values)
	
}