count_string<-function(vector,wordlist)
{
  ##function to count the number of words in vector which appear also in word list
  vector<-as.data.frame(vector)
  return(sum(vector$Freq[vector$Var1 %in% wordlist]))
}
