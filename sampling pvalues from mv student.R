sampling.pvalues.from.mv.student<- function(m=10, gamma=0.5, repeats=10){

p_values = 0
mean = c(0,0)
sigma = diag(2)

#gamma = m / m+n so:
n = ceiling((m/gamma)-m)

#Using Replicate() instead of loop#
p_values = replicate(repeats,K.statistic(rmvt(m,sigma, df=4), rmvt(n,sigma, df=4),0.5,0.5)$p_value)


return(p_values)
	
}