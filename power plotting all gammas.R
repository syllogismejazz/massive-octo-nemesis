#Plots the power of the K-stat test, with m as x-axis (m is the first sample size)
# ... as a function of the scalars c1 and c2
#
#Different curves by value of gamma (the relative size of the first sample)
#
#The power is only computed by getting p-values from K stat
#by comparing a Laplace Standard (mean=c(0,0) and var=Identity) with a Laplace mean=c(1,1) and var=4*Identity
#
power.plotting.all.gamma <- function(max.sample.size=25, increments=2, nb.of.pvalues.each=100, c_N1=0.5, c_N2=0.5){
  
  #Let gamma = m / m+n
  #gamma values of 0.1, 0.25, 0.5, 0.75, 0.9
  gamma1 = 0.1
  gamma2 = 0.25
  gamma3 = 0.5
  gamma4 = 0.75
  gamma5 = 0.9
  power.vector.gamma1=0
  power.vector.gamma2=0
  power.vector.gamma3=0
  power.vector.gamma4=0
  power.vector.gamma5=0
  
  #vector containing m: the first sample size(s)
  m.vector = seq(from=2, to=max.sample.size, by=increments)  
  
  p.values.for.gamma1 = lapply(m.vector, sampling.pvalues.for.power, gamma=gamma1, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
  p.values.for.gamma2 = lapply(m.vector, sampling.pvalues.for.power, gamma=gamma2, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
  p.values.for.gamma3 = lapply(m.vector, sampling.pvalues.for.power, gamma=gamma3, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
  p.values.for.gamma4 = lapply(m.vector, sampling.pvalues.for.power, gamma=gamma4, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
  p.values.for.gamma5 = lapply(m.vector, sampling.pvalues.for.power, gamma=gamma5, repeats=nb.of.pvalues.each, c_N1=c_N1, c_N2=c_N2)
  
  for(i in 1:length(m.vector)){
    power.vector.gamma1[i] = sum(p.values.for.gamma1[[i]] <= 0.05) / nb.of.pvalues.each
    power.vector.gamma2[i] = sum(p.values.for.gamma2[[i]] <= 0.05) / nb.of.pvalues.each
    power.vector.gamma3[i] = sum(p.values.for.gamma3[[i]] <= 0.05) / nb.of.pvalues.each
    power.vector.gamma4[i] = sum(p.values.for.gamma4[[i]] <= 0.05) / nb.of.pvalues.each
    power.vector.gamma5[i] = sum(p.values.for.gamma5[[i]] <= 0.05) / nb.of.pvalues.each
  }  
  
  color.vector = gray.colors(5, start = 0, end = 0.4, gamma = 2.2)
  
  plot(m.vector, power.vector.gamma1, ylim=c(0, 1), xlab="m (first sample size)", ylab="Power (at alpah=0.05)",main="Power each for 2000 pvalues (Laplace (0,1) vs Non-Std Laplace)", sub="(c_N1=0.05 and c_N2=0.1)", lty=1, col=color.vector[1], type="l")
  
  lines(m.vector,power.vector.gamma2,lty=2,col=color.vector[2])
  lines(m.vector,power.vector.gamma3, lty=3, col=color.vector[3])
  lines(m.vector,power.vector.gamma4, lty=4, col=color.vector[4])
  lines(m.vector,power.vector.gamma5, lty=5, col=color.vector[5])
  abline(h=0.8, v=0, col = "red")
  
  legend("topright", legend=c("= 0.1","= 0.25", "= 0.5", "= 0.75", "= 0.9"),lty=1:5, col=color.vector, bty="n")
  
}