compute_correlation<-function(df)
{
  ##function to compute the correlation between two vectors of word counts.
  ##returns teh value of the correlation and the confidence intervals
  counts1<-unlist(df$count.x)
  counts2<-unlist(df$count.y)
  if(length(counts1)==0|length(counts2)==0) ##checking that the two counts vectors are not empty
  {correl=c(0,0,0,0)
  return(correl)}
  else if(sum(counts2)==0|sum(counts1)==0) ##checking that the two counts vectors are not empty
  {correl=c(0,0,0,0)
  return(correl)}
  else
  {correl<-cor.test(counts1,counts2)
  return(c(correl$estimate,correl$p.value,correl$conf.int))}
}
