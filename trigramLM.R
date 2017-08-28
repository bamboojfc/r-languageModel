q.count = function(l){
  NC <- length(tokens)**2 - length(bigrams)
  
  if(length(l)==1){
    c <- unigrams.freq[l]
    if(is.na(c)) c <- 0
    if(c!=0){
      NC <- length(unigrams.freq[unigrams.freq==c])
    }
    return((c+1)*(length(unigrams.freq[unigrams.freq==(c+1)])/NC))
  }else if(length(l)==2){
    c <- bigrams.freq[paste(l,collapse = " ")]
    if(is.na(c)) c <- 0
    if(c!=0){
      NC <- length(bigrams.freq[bigrams.freq==c])
    }
    return((c+1)*(length(bigrams.freq[unigrams.freq==(c+1)])/NC))
  }else if(length(l)==3){
    c <- trigrams.freq[paste(l,collapse = " ")]
    if(is.na(c)) c <- 0
    if(c!=0){
      NC <- length(trigrams.freq[trigrams.freq==c])
    }
    return((c+1)*(length(trigrams.freq[trigrams.freq==(c+1)])/NC))
  }
}

trigrams.prob = function(query){
  prob <- 1
  if(length(query)>=3){
    for(i in 3:(length(query))){
      prob <- prob * (q.count(c(query[i-2],query[i-1],query[i]))/q.count(c(query[i-2],query[i-1])))
    }
  }else{
    prob <- 0
  }
  return(prob)
}

bigrams.prob = function(query){
  prob <- 1
  for(i in 2:(length(query))){
    prob <- prob * (q.count(c(query[i-1],query[i]))/q.count(query[i-1]))
  }
  return(prob)
}

unigrams.prob = function(query){
  prob <- 1
  for(i in 1:(length(query))){
    prob <- prob * (q.count(query[i-1])/length(tokens))
  }
  return(prob)
}

markov = function(query){
  prob <- 0.6*trigrams.prob(query) + 0.3*bigrams.prob(query) + 0.1*unigrams.prob(query)
  return(prob)
}

tokenize = function(s){
  s <- tolower(s)
  s <- gsub(pattern="[[:punct:]]\\s+"," STOP ",s)
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
  
  if(tokens[length(tokens)][1]!="STOP"){
    tokens <- c(tokens,'STOP')
  } 
  return(tokens)
}

#Do this with each document
#get content from each document
s <- "This is intouch. intouch intouch intouch I'm Nannapas Banluesombatkul!. Nannapas Banluesombatkul!. 
How are you?!
Best regards, Nannapas Banluesombatkul, Nannapas"

#tokenize content into tokens
tokens <- tokenize(s)
unigrams.freq <- sort(table(tokens), decreasing=T)

#prepare to create bigram
tokens2 <- c(tokens[-1], "STOP")
bigrams <- paste(tokens, tokens2)
bigrams <- bigrams[bigrams!="STOP STOP"]
bigrams.freq <- sort(table(bigrams), decreasing=T)

#prepare to create trigram
tokens3 <- c(tokens2[-1], "STOP")
trigrams <- paste(tokens, tokens2, tokens3)
trigrams <- trigrams[trigrams!="STOP STOP STOP"]
trigrams.freq <- sort(table(trigrams), decreasing=T)

#markov process to calculate how much probability can this query be in this language 
#(how much related to this doc)
query <- "nannapas banluesombatkul"
query.tokens <- tokenize(query)
prob.query <- markov(query.tokens)
