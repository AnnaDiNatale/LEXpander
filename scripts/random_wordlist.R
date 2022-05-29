random_wordlist<-function(method,dic_name){
	library(dplyr)
  ##Function to compute baseline methods (random words from the space)
  ##method is one of the methods to use ('lexpander', 'glove','fasttext','empath_new','wordnet' for English and
  ##'lexpander_deu', 'glove_deu','fasttext_deu','empath_new_deu','odenet' for German)
  ##dic_name is 'EV' or '2007deu' or '2015en'
  ##loading needed datasets
  if(method=='lexpander')
  {method<-'freedict'}
  else if(method=='lexpander_deu')
  {method<-'freedict_deu'}
  if(method=='glove')
  {cos_sim<-readRDS('datasets/glove_matrix.Rda')}  #loading the similarity matrix from GloVe
  else if(method=='glove_deu')
  {cos_sim<-readRDS('datasets/glovedeu_matrix.Rda')}
  else if(method=='fasttext')
  {cos_sim<-readRDS('datasets/fasttext/cos_sim_fasttext_en_25000_ngrams.Rda')}  #loading the similarity from fastText
  else if(method=='fasttext_deu')
  {cos_sim<-readRDS('datasets/fasttext/cos_sim_fasttext_de_25000_ngrams.Rda')}
  else if (method=='wordnet')
  {network<-readRDS('datasets/WordNet.Rda')}
  else if (method=='odenet')
  {network<-readRDS('datasets/odenet_neigh.Rda')}
  else if(method=='empath')
  {}##we do not load anything
  else if(method=='empath_new')
  {vec<-readRDS('datasets/fasttext/wiki.en.vec.ngrams.Rda')}
  else if(method=='empath_new_deu')
  {vec<-readRDS('datasets/fasttext/wiki.de.vec.ngrams.Rda')}
  else
  {network<-readRDS(paste0('datasets/',method,'.Rda'))} ##loading the colexification network
  res<-readRDS(paste0('results/new/',dic_name,'_',method,'.Rda')) ##loading the results to get the average number of words

  for (cat in sort(unique(dic$id))) ##loop on the categories
  {
    print(cat)
    if(dic_name!='EV')
    {
    for (perc in seq(0.1,0.9,by=0.1))
    {
      size<-round(res$length[res$perc==as.character(perc) & res$cat_id==cat]) ##number of words to choose at random
    
      if(size>0)
      {
      
       expanded<-data.frame(stringsAsFactors = F) #initializing expanded wordlist
       if(method=='glove'|method=='fasttext'|method=='glove_deu'|method=='fasttext_deu')
       {
        neigh<-sample(rownames(cos_sim),size)
        expanded<-rbind(expanded,data.frame(word=tolower(neigh),stringsAsFactors = F)) ##adding words to the expanded wordlist
        rownames(expanded)<-seq(1,nrow(expanded))
       }
        else if(method=='empath_new'|method=='empath_new_deu')
        {
          neigh<-sample(rownames(vec),size)
          expanded<-rbind(expanded,data.frame(word=tolower(neigh),stringsAsFactors = F))
        }
        else ##LEXpander and wordnet
        {
          words_netw<-union(network$from_word,network$to_word)
          neigh<-sample(words_netw,size)
          expanded<-rbind(expanded,data.frame(word=tolower(neigh),stringsAsFactors = F))
        }
      }
        expanded<-unique(expanded)
        return(expanded) ##returns the random wordlist
      
    }
    }
    else
    {

        size<-round(res$length[res$cat_id==cat]) ##number of words to choose at random
        
        if(size>0)
        {
          
          expanded<-data.frame(stringsAsFactors = F) #initializing expanded wordlist
          if(method=='glove'|method=='fasttext'|method=='glove_deu'|method=='fasttext_deu')
          {
            neigh<-sample(rownames(cos_sim),size)
            expanded<-rbind(expanded,data.frame(word=tolower(neigh),stringsAsFactors = F)) ##adding words to the expanded wordlist
            rownames(expanded)<-seq(1,nrow(expanded))
          }
          else if(method=='empath_new'|method=='empath_new_deu')
          {
            neigh<-sample(rownames(vec),size)
            expanded<-rbind(expanded,data.frame(word=tolower(neigh),stringsAsFactors = F))
          }
          else ##LEXpander and wordnet
          {
            words_netw<-union(network$from_word,network$to_word)
            neigh<-sample(words_netw,size)
            expanded<-rbind(expanded,data.frame(word=tolower(neigh),stringsAsFactors = F))
          }
        }
        expanded<-unique(expanded)
        return(expanded) ##returns the random wordlist
      
    }
    
  }
}
