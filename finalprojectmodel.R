library(tm)
predict0 <-function(input,profanity,unigramDF, bigramDF, trigramDF, maxResults = 3) {
  
  input <- removePunctuation(input)#remove punctuation
  input <- removeNumbers(input)#remove number
  input <- tolower(input)#to lower case
  #input <- stemDocument(input)#hmmm, get rid?
  input <- stripWhitespace(input)
  input <- input[grepl('[[:alpha:]]',input)]
  
  
  
  if(input == ''|input == "na na") stop ('Warning: Just input something')#figure out a more graceful way of exiting
  
  seektri<-grepl(paste0("^",input,"$"),trigramDF$bigram)#match input to bigram component of trigram
  subtri<-trigramDF[seektri,]#retrieve matched trigram if present, retrieve header if not present
  input2 <- unlist(strsplit(input," "))[2]# retrieve matched trigram if present,retrieve header if not present
  seekbi <- grepl(paste0("^",input2,"$"),bigramDF$unigram)#match input to unigram component of bigram
  subbi <- bigramDF[seekbi,]#retrieve matched bigram if present, retrive header if not present
  unigramDF$s <- unigramDF$freq/nrow(unigramDF)*0.16#weighted Good-Turing probabability of unigram
  useuni <- unigramDF[order(unigramDF$s,decreasing = T),]#ordered weighted Good-Turing probability
  useunia <- useuni[1:maxResults,]#top-MaxResults of weighted Good-Turing probability
  
  if (sum(seektri) == 0) {
    if(sum(seekbi)==0)
    {
      return(head(unigramDF[order(unigramDF$freq,decreasing = T),1],maxResults))#return top-maXResults for unigram
    }
    subbi$s <- 0.4*subbi$freq/sum(seekbi)#weighted Good-Turing probability of bigram
    names <- c(subbi$name,useunia$unigram)
    score <- c(subbi$s,useunia$s)
    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
    predictWord <- predictWord[order(predictWord$score,decreasing = T),]
    # in case replicated
    final <- unique(predictWord$next_word)
    final <- setdiff(final,profanity)
    final <- final[grepl('[[:alpha:]]',final)]
    return(final[1:maxResults])
  } 
  subbi$s <- 0.4*subbi$freq/sum(seekbi)#weighted Good-Turing probability of bigram
  subtri$s <- subtri$freq/sum(subtri$freq)
  names <- c(subtri$name,subbi$name,useunia$unigram)
  score <- c(subtri$s,subbi$s,useunia$s)
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  # in case replicated
  final <- unique(predictWord$next_word)
  final <- final[1:maxResults]
  final <- setdiff(final,profanity)
  final <- final[grepl('[[:alpha:]]',final)]        
  return(final)
}
