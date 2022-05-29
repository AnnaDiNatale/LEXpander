cor_on_texts<-function(dataset,method)
{
  ##function to compute the word counts relative to the wordlists on different text corpora
  ##dataset is either '2015en' for LIWC in English or 'EV'
  ##method can be 'fasttext', 'freedict' for LEXpander, 'glove', 'empath', 'empath_new' for Empath 2.0, 'wordnet', 'union' for the union of LEXpander and fasttext
  ##'LIWC2015en' for the English LIWC
  
  bc<-readRDS('datasets/bc_counts.Rda') ##loading texts from the Brown corpus
  tweets<-readRDS('datasets/tweets_counts.Rda') ##loading single tweets posted in one day
  coha<-readRDS('datasets/coha_long_counts.Rda') ##loading the COHA dataset
  reddit_family<-readRDS('datasets/family_counts.Rda')
  reddit_home<-readRDS('datasets/Home_counts.Rda')
  reddit_2X<-readRDS('datasets/TwoXChromosomes_counts.Rda')
  reddit_antiwork<-readRDS('datasets/antiwork_counts.Rda')
  
  all_counts<-data.frame(stringsAsFactors = F) #initializing vector of results
  if(dataset!=method) ##we consider the expanded word lists
  {
      ##retrieving the word lists expanded via the chosen method
      filenames_wl <- list.files("results/wordlists_expanded", full.names=TRUE)
      filenames_wl<-filenames_wl[grep(dataset,filenames_wl)]
      filenames_wl<-filenames_wl[grep(method,filenames_wl)]
      
      for(wl in filenames_wl) ##loop on the word lists
      {
        wordlist<-readLines(wl) ##importing the expanded word list
        print(wl)
        t<-gsub(paste0("results/wordlists_expanded/wordlist_",dataset,"_"),"",wl)
        t<-gsub(".txt","",t)
        t<-strsplit(t,"_")
        if(method!='empath_new'&dataset!='EV')
        {
          category<-t[[1]][2]
          threshold<-gsub("th","",t[[1]][3])
          nrepetitions<-gsub("rep","",t[[1]][4])
        }
        else if (method=='empath_new'&dataset!='EV')
        {
          category<-t[[1]][3]
          threshold<-gsub("th","",t[[1]][4])
          nrepetitions<-gsub("rep","",t[[1]][5])
        }
        else if(method!='empath_new'&dataset=='EV')
        {
          category<-t[[1]][2]
          threshold<-0
          nrepetitions<-0
        }
        else if (method=='empath_new'&dataset=='EV')
        {
          category<-t[[1]][3]
          threshold<-0
          nrepetitions<-0
        }

        count<-apply(as.matrix(bc),MARGIN=1,count_string,wordlist=wordlist) ##counting words in texts from the Brown corpus
        count_twitter<-apply(as.matrix(tweets),MARGIN=1,count_string,wordlist=wordlist) ##counting words in the single tweets
        count_coha<-apply(as.matrix(coha),MARGIN=1,count_string,wordlist=wordlist) ##counting words in texts from COHA

        all_counts<-rbind(all_counts,data.frame(dataset="brown",cat=category,method=method,th=threshold,
                                                 rep=nrepetitions,count=I(list(count)),stringsAsFactors = F))
        all_counts<-rbind(all_counts,data.frame(dataset="daily_tweets",cat=category,method=method,th=threshold,
                                                  rep=nrepetitions,count=I(list(count_twitter)),stringsAsFactors = F))
        all_counts<-rbind(all_counts,data.frame(dataset="coha",cat=category,method=method,th=threshold,
                                                rep=nrepetitions,count=I(list(count_coha)),stringsAsFactors = F))
       ##counts on the reddit datasets
        count<-apply(as.matrix(reddit_home),MARGIN=1,count_string,wordlist=wordlist)
        all_counts<-rbind(all_counts,data.frame(dataset="reddit_home",cat=category,method=method,th=threshold,
                                                rep=nrepetitions,file=wl,count=I(list(count)),stringsAsFactors = F))
        count<-apply(as.matrix(reddit_family),MARGIN=1,count_string,wordlist=wordlist)
        all_counts<-rbind(all_counts,data.frame(dataset="reddit_family",cat=category,method=method,th=threshold,
                                                rep=nrepetitions,file=wl,count=I(list(count)),stringsAsFactors = F))
        count<-apply(as.matrix(reddit_antiwork),MARGIN=1,count_string,wordlist=wordlist)
        all_counts<-rbind(all_counts,data.frame(dataset="reddit_antiwork",cat=category,method=method,th=threshold,
                                                rep=nrepetitions,file=wl,count=I(list(count)),stringsAsFactors = F))
        count<-apply(as.matrix(reddit_2X),MARGIN=1,count_string,wordlist=wordlist)
        all_counts<-rbind(all_counts,data.frame(dataset="reddit_TwoXChromosomes",cat=category,method=method,th=threshold,
                                                rep=nrepetitions,file=wl,count=I(list(count)),stringsAsFactors = F))
        
      }
    
  }
  else if (method == dataset) ##if we take the original wordlists
  {
    original_wl<-readRDS(paste0('datasets/nowildcards/',dataset,'_all.Rda'))  ##original LIWC wordlist
    for (cat in unique(sort(original_wl$id)))  ##loop on the categories
    {
      print(cat)
      wordlist<-original_wl$word[original_wl$id==cat] ##wordlist in the original lexicon relative to the category chosen
      count<-apply(as.matrix(bc),MARGIN=1,count_string,wordlist=wordlist) ##counts on the Brown Corpus
      all_counts<-rbind(all_counts,data.frame(dataset="brown",cat=cat,method=dataset,th=0,rep=0,count=I(list(count)),stringsAsFactors = F))
      count_twitter<-apply(as.matrix(tweets),MARGIN=1,count_string,wordlist=wordlist) ##counts on the single tweets
      all_counts<-rbind(all_counts,data.frame(dataset="daily_tweets",cat=cat,method=dataset,th=0,rep=0,count=I(list(count_twitter)),stringsAsFactors = F))
      count_coha<-apply(as.matrix(coha),MARGIN=1,count_string,wordlist=wordlist) ##counts on texts of the COHA corpus
      all_counts<-rbind(all_counts,data.frame(dataset="coha",cat=cat,method=dataset,th=0,rep=0,count=I(list(count_coha)),stringsAsFactors = F))
      count<-apply(as.matrix(reddit_home),MARGIN=1,count_string,wordlist=wordlist)
      all_counts<-rbind(all_counts,data.frame(dataset="reddit_home",cat=cat,method=dataset,th=0,rep=0,count=I(list(count)),stringsAsFactors = F))
      count<-apply(as.matrix(reddit_family),MARGIN=1,count_string,wordlist=wordlist)
      all_counts<-rbind(all_counts,data.frame(dataset="reddit_family",cat=cat,method=dataset,th=0,rep=0,count=I(list(count)),stringsAsFactors = F))
      count<-apply(as.matrix(reddit_antiwork),MARGIN=1,count_string,wordlist=wordlist)
      all_counts<-rbind(all_counts,data.frame(dataset="reddit_antiwork",cat=cat,method=dataset,th=0,rep=0,count=I(list(count)),stringsAsFactors = F))
      count<-apply(as.matrix(reddit_2X),MARGIN=1,count_string,wordlist=wordlist)
      all_counts<-rbind(all_counts,data.frame(dataset="reddit_TwoXChromosomes",cat=cat,method=dataset,th=0,rep=0,count=I(list(count)),stringsAsFactors = F))
    }
  }

  return(all_counts)
}
