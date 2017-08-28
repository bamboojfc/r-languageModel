tokenize = function(s){
  s <- tolower(s)
  s <- gsub(pattern="[[:punct:]]\\s+"," ",s)
  tokens <- unlist(strsplit(s, '\\s+', perl=TRUE))
  
  count <- 0
  tokens <- unlist(lapply(tokens,FUN = function(t){
    #replace last character of each token with "" if it's a punctuation
    t <- t[1]
    count <- 0
    repeat{
      lastChar <- substr(t,nchar(t),nchar(t))
      if(grepl(lastChar, pattern = "[[:punct:]]")){
        t <- substr(t, 1, nchar(t)-1)
        count <- 1
      }else{
        break
      }
    }
    tokens[length(tokens)][1] <- t
  }))
  
  return(tokens)
}

tableToDataframe = function(tb){
  tb.matrix <- as.matrix(tb)
  tb.table <- data.frame(tb.matrix)
  names(tb.table) <- c("freq")
  return(tb.table)
}

createDiscount = function(nGram.df){
  nGram.df$discount <- rep(1, length(nGram.df))
  
  # Calculate the discount coefficient.
  # We only consider n-grams that have 0 < frequency <= k (5). Larger than 5: "Reliable enough".
  for(i in 5:1){
    currRTimes <- i
    nextRTimes <- currRTimes + 1
    
    currN <- nrow(nGram.df[nGram.df$freq==currRTimes,])
    nextN <- nrow(nGram.df[nGram.df$freq==nextRTimes,])
    
    currd <- (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
    
    nGram.df[nGram.df$freq==currRTimes,"discount"] <- currd
  }
  
  return(nGram.df)
}

#Do this with each document
#get content from each document
s <- "This is intouch intouch intouch. I'm Nannapas Banluesombatkul!. Nannapas Nannapas Nannapas
How are you?! you you
Best regards, intouch intouch intouch Nannapas Banluesombatkul Nannapas"

#tokenize content into tokens
tokens <- tokenize(s)
unigrams.freq <- sort(table(tokens), decreasing=T)
unigrams.df <- tableToDataframe(unigrams.freq)
unigrams.table <- createDiscount(unigrams.df)

#prepare to create bigram
tokens2 <- c(tokens[-1], "STOP")
bigrams <- paste(tokens, tokens2)
bigrams <- bigrams[!grepl(bigrams, pattern = "STOP")]
bigrams.freq <- sort(table(bigrams), decreasing=T)
bigrams.df <- tableToDataframe(bigrams.freq)
bigrams.table <- createDiscount(bigrams.df)

#prepare to create trigram
tokens3 <- c(tokens2[-1], "STOP")
trigrams <- paste(tokens, tokens2, tokens3)
trigrams <- trigrams[!grepl(trigrams, pattern = "STOP")]
trigrams.freq <- sort(table(trigrams), decreasing=T)
trigrams.df <- tableToDataframe(trigrams.freq)
trigrams.table <- createDiscount(trigrams.df)
