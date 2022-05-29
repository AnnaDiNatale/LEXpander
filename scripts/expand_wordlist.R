expand_wordlist_EV<-function(method){
  ##Function to expand EV wordlists
  ##method is one of the methods to use ('lexpander', 'glove','fasttext','empath_new','wordnet')

  ##loading needed datasets
  dic<-readRDS('datasets/EV_all.Rda') ##EV dataset
  if(method=='lexpander')
  {method<-'freedict'}
  if(method=='glove')
  {cos_sim<-readRDS('datasets/glove_matrix.Rda')}  #loading the similarity matrix from GloVe
  else if(method=='fasttext')
  {cos_sim<-readRDS('datasets/fasttext/cos_sim_fasttext_en_25000_ngrams.Rda')}  #loading the similarity from fastText
  else if (method=='wordnet')
  {network<-readRDS('datasets/WordNet.Rda')}
  else if(method=='empath')
  {}##we do not load anything
  else if(method=='empath_new')
  {vec<-readRDS('datasets/fasttext/wiki.en.vec.ngrams.Rda')
    cos_sim<-readRDS('datasets/fasttext/cos_sim_fasttext_en_25000_ngrams.Rda')
    norm_matrix<-readRDS('datasets/fasttext_norm_matrix_en.Rda')}
  else
  {network<-readRDS(paste0('datasets/',method,'.Rda'))} ##loading the colexification network

  
  for (cat in sort(unique(dic$id))) ##loop on the categories
  {
    print(cat)
    words<-dic$word[dic$id==cat] ##words in said category
   
   
    expanded<-data.frame(stringsAsFactors = F) #initializing expanded word list
    if(method=='glove'|method=='fasttext')
    {
      for(i in words)
      {
        ind<-which(rownames(cos_sim)==i) #selecting the index of the word
        neigh<-unique(rownames(cos_sim[which(cos_sim[ind,]>=0.5),])) #selecting the neighbors (words with cosine similarity >=0.5)
        neigh<-neigh[!neigh%in%words] ##deleting seed words
        expanded<-rbind(expanded,data.frame(word=tolower(neigh),stringsAsFactors = F)) ##adding words to the expanded word list
      }
    }
    else if(method=='empath_new')
    {
      s<-colSums(vec[rownames(vec)%in%words,]) ##sum of the vectors of the seed words
      norm_sum<-norm(s)
      ind<-which(rownames(cos_sim)%in%words) ##index of words in the cosine similarity matrix
      cos_sim_new<-cos_sim[ind,]
      t<-t(apply(cos_sim_new,1,FUN=normsum,norm_matrix=norm_matrix))
      fin<-colSums(t)/norm_sum
      neigh<-unique(names(fin[which(fin>=0.5)])) #selecting the neighbors
      neigh<-neigh[!neigh%in%words] ##deleting seed words
      expanded<-rbind(expanded,data.frame(word=tolower(neigh),stringsAsFactors = F)) ##adding words to the expanded word list
    }
      else if(method=='empath')
      {
          writeLines(words,"results/wordlists/wordlist.txt", sep=",") #save the selection of words
          wd<-getwd()
          py_run_file("scripts/python/empath_script.py") #run the Python script to use the Empath API
          setwd(wd)
          neigh<-scan("results/empath_resultfiles/wordlist.txt",character(),quote="") #reading the results back in R
          neigh<-neigh[!neigh%in%words] ##delete the seed words from the output
          neigh<-gsub("_"," ",neigh) ##some words have underscore
          expanded<-rbind(expanded,data.frame(word=unique(tolower(neigh)),stringsAsFactors = F)) #adding the neighbors to the expanded word list
      }
      else ##LEXpander
      {
          for (i in words)
          {
            neigh<-union(network$to_word[network$from_word==i],network$from_word[network$to_word==i]) #neighbors in the colexification network
            neigh<-neigh[!neigh%in%words] ##delete the seed words from the output
            expanded<-rbind(expanded,data.frame(word=neigh,stringsAsFactors = F)) #adding the neighbors to the expanded wordlist
          }
      }
    
    expanded<-rbind(expanded,data.frame(word=words,stringsAsFactors = F)) ##adding the seed words

  }
  return(expanded)
}