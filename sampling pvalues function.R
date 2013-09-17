Sampling_pvalues_from_K_statistic<- function(repeats=10, m=10, n=10){

p_values = 0

for(i in 1:repeats){

	first.bivariate.sample=rmvnorm(m, mean=c(0,0), sigma=diag(2))
	second.bivariate.sample=rmvnorm(n, mean=c(0,0), sigma=diag(2))
	
	p_values[i]=(K.statistic(first.bivariate.sample, second.bivariate.sample, 0.5, 0.5))$p_value
		
}

return(p_values)
	
}