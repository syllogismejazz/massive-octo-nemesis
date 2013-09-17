D.Stats.Against.Sample.Size <- function(max.sample.size=100, increments=5, nb.of.pvalues.each=100){


  #Samples 2000 p-values for each sample size ... by increments
  
  x.vector = seq(from=increments, to=max.sample.size, by=increments)	
  D.vector=0
  
  for(i in 1:length(x.vector)){
    pvalues.sample = Sampling_pvalues_from_K_statistic(nb.of.pvalues.each, i*increments, i*increments)
    D.vector[i]=ks.test(unique(pvalues.sample), "punif")$statistic
  }
  
  plot(x.vector, D.vector, type="l")
  
  return(list(Sample_Sizes=x.vector, D_Statistics=D.vector))
}