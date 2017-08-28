trigrams.prob = function(firstWord, secondWord, thirdWord){
  
  text2 <- paste(c(firstWord,secondWord),collapse = " ")
  text3 <- paste(c(firstWord,secondWord,thirdWord),collapse = " ")
  
  ## count three words
  c <- length(trigrams[trigrams==text3])
  if(c>5){
    count.threeWords <- c
  }else{
    trigram.table <- table(trigrams)
    if(c==0){
      Nc <- length(tokens)**2 - length(trigrams)
    }else{
      Nc <- length(trigram.table[trigram.table==c])
    }
    Nc1 <- length(trigram.table[trigram.table==(c+1)])
    count.threeWords <- (c+1)*(Nc1/Nc)
  }
  
  ## count two words
  c <- length(bigrams[bigrams==text2])
  
  if(c>5){
    count.twoWords <- c
  }else{
    bigram.table <- table(bigrams)
    if(c==0){
      Nc <- length(tokens)**2 - length(bigrams)
    }else{
      Nc <- length(bigram.table[bigram.table==c])
    }
    Nc1 <- length(bigram.table[bigram.table==(c+1)])
    count.twoWords <- (c+1)*(Nc1/Nc)
  }
  
  prob <- count.threeWords / count.twoWords
  return(prob)
}

bigrams.prob = function(firstWord, secondWord){
  
  text <- paste(c(firstWord,secondWord),collapse = " ")
  c <- length(bigrams[bigrams==text])
  
  if(c>5){
    count.twoWords <- c
  }else{
    bigram.table <- table(bigrams)
    if(c==0){
      Nc <- length(tokens)**2 - length(bigrams)
    }else{
      Nc <- length(bigram.table[bigram.table==c])
    }
    Nc1 <- length(bigram.table[bigram.table==(c+1)])
    count.twoWords <- (c+1)*(Nc1/Nc)
  }
  
  count.firstWord <- length(tokens[tokens==firstWord])
  prob <- count.twoWords / count.firstWord
  
  return(prob)
}

unigrams.prob = function(query){
  numberOfQ <- length(tokens[tokens==query])
  prob <- numberOfQ/length(tokens[tokens!="STOP"])
  
  return(prob)
}

markov.trigrams = function(query){
  prob<-1
  multiplied <- FALSE
  
  for(i in 1:length(query)){
    p <- 1
    if(i==1){
      p <- unigrams.prob(query[i])
    }else if(i==2){
      p <- bigrams.prob(query[i-1], query[i])
    }else{
      p <- trigrams.prob(query[i-2], query[i-1], query[i])
    }
    
    if(!is.na(p)){
      if(p!=0){
        prob <- prob*p
        multiplied <- TRUE
      }
    }
  }
  
  if(!multiplied){
    prob <- 0
  }
  return(prob)
}

markov.bigrams = function(query){
  prob<-1
  multiplied <- FALSE
  for(i in 1:length(query)){
    p <- 1
    if(i==1){
      p <- unigrams.prob(query[i])
    }else{
      p <- bigrams.prob(query[i-1], query[i])
    }
    
    if(!is.na(p)){
      if(p!=0){
        prob <- prob*p
        multiplied <- TRUE
      }
    }
  }
  
  if(!multiplied){
    prob <- 0
  }
  return(prob)
}

markov.unigrams = function(query){
  prob <- unigrams.prob(query[1])
  return(prob)
}

createNGramAndCalProbs = function(d,query,column.name){
  i <- 1
  for(s in d[,column.name]){
    ### tokenize content into tokens
    tokens <<- tokenize(s)
    
    ### create bigrams
    tokens2 <- c(tokens[-1], "STOP")
    bigrams <- paste(tokens, tokens2)
    bigrams <<- bigrams[!grepl(bigrams, pattern = "STOP STOP")]
    
    ### create trigrams
    tokens3 <- c(tokens2[-1], "STOP")
    trigrams <- paste(tokens, tokens2, tokens3)
    trigrams <<- trigrams[!grepl(trigrams, pattern = "STOP STOP")]
    
    ### markov process to calculate how much probability can this query be in this language
    ### (how much related to this doc)
    query.tokens <- tokenize(query)
    
    ## choose N-Gram probs calculation based on length of query.tokens
    if(length(query.tokens)-1==1){
      prob.query <- markov.unigrams(query.tokens)
    }else if(length(query.tokens)-1==2){
      prob.query <- markov.bigrams(query.tokens)
    }else{
      prob.query <- markov.trigrams(query.tokens)
    }
    d$prob[i] <- d$prob[i]+prob.query
    
    i<-i+1
  }
  
  return(d)
}

tokenize = function(s){
  s <- tolower(s)
  
  s <- gsub(pattern="[[:punct:]]\\s+"," STOP ",s)
  tokens <- unlist(strsplit(s, '\\s+', perl=TRUE))
  
  tokens <- unlist(lapply(tokens,FUN = function(t){
    #replace last character of each token with "" if it's a punctuation
    t.tmp <- t
    
    repeat{
      lastChar <- substr(t,nchar(t),nchar(t))
      if(grepl(lastChar, pattern = "[[:punct:]]")){
        t <- substr(t, 1, nchar(t)-1)
      }else{
        break
      }
    }
    tokens[tokens==t.tmp] <- t
  }))
  
  if(length(tokens)!=0){
    if(tokens[length(tokens)]!="STOP"){
      tokens <- c(tokens,'STOP')
    } 
  }
  return(tokens)
}

cleanData = function(d){
  ### remove html tag
  d <- gsub("<[^>]+>", "", d)
  d <- gsub("\u2028", "", d)
  ### remove html special character
  d <- gsub("&#?[a-zA-Z0-9];", "", d)
  ### unicode to ascii
  Encoding(d) <- "latin1"
  d <- iconv(d, "latin1", "ASCII", sub="")
  ### replace ( ) [ ] with space
  d <- gsub(pattern = "\\]|\\[", " ", d)
  d <- gsub(pattern = "[()]", " ", d)
  ### remove \n
  d <- gsub(pattern = "\n", " ", d)
  ### separate camel case
  d <- gsub("([a-z])([A-Z])", "\\1 \\L\\2", d, perl = TRUE)
  return(d)
}


### get content from each document by read csv to dataframe
path <- "C:/Users/U6063152/Documents/RScripts/data/"
filename <- paste(path,"database.csv",sep = "")
documents.df <- read.csv(filename)
documents.df.tmp <- documents.df

### clean data
documents.df$title <- cleanData(documents.df$title)
documents.df$body <- cleanData(documents.df$body)

### prepare for store calculated probs
documents.df$prob <- rep(0, nrow(documents.df))

### Do this with each document (both title and body)
# 1. create unigram bigram and trigram
# 2. calculate prob with search keyword
search.keyword <- "eikonDesktop"
search.keyword <- cleanData(search.keyword)
documents.df <- createNGramAndCalProbs(documents.df,search.keyword,"title")
documents.df <- createNGramAndCalProbs(documents.df,search.keyword,"body")

### rank documents
documents.df <- documents.df[order(documents.df$prob,decreasing = TRUE),]
documents.df.top <- documents.df[1:100,]
documents.df.top <- documents.df.top[documents.df.top$prob!=0,]

### new method to aggregate by authors and updaters
MAX_AUTHOR <- 24
combine.link <- c()
combine.name <- c()
for(i in 0:MAX_AUTHOR){
  col.link <- paste("authors.",i,".link",sep = "")
  author <- as.character(documents.df.top[[col.link]])
  combine.link <- c(combine.link, author[author!=""])
  col.author <- paste("authors.",i,".name",sep = "")
  author <- as.character(documents.df.top[[col.author]])
  combine.name <- c(combine.name, author[author!=""])
}
combine.df <- data.frame(combine.link, combine.name)
combine.df.unique <- unique.data.frame(combine.df)

result.df <- data.frame(author.link = character(), author.name=character(), id=character(), title=character(), body=character(), prob=numeric(), stringsAsFactors = FALSE)
for(c in as.character(combine.df.unique$combine.link)){
  ### c is a staff
  ## select only document includes author or updater == c from documents.df.top
  docs <- c()
  for(i in 0:21){
    col.name <- paste("authors.",i,".link",sep = "")
    docs <- c(docs,as.character(documents.df.top[documents.df.top[[col.name]]==c,]$X_id))
  }
  docs <- unique(docs) #list of c's docs
  
  ### combine dataframe of c and document list to result
  author.name <- as.character(combine.df.unique[combine.df.unique["combine.link"]==c,]$combine.name)
  for(d in docs){
    tmp.df <- data.frame(c, author.name, d, as.character(documents.df.top[documents.df.top["X_id"]==d,]$title), as.character(documents.df.top[documents.df.top["X_id"]==d,]$body), documents.df.top[documents.df.top["X_id"]==d,]$prob)
    names(tmp.df) <- c("author.link", "author.name", "id", "title", "body", "prob")
    result.df <- rbind(result.df, tmp.df)
  }
}

### write results to csv
filename <- paste(path,"results-LM-",search.keyword,"-list.csv",sep = "")
write.csv(file = filename, x = result.df)

### group docs by author
library(plyr)
result.df.count <- count(result.df, "author.link")
result.df.sum <- aggregate(prob ~ author.link,result.df,sum)
result.merge <- merge(result.df.count, result.df.sum, by="author.link")

### map author.name with author.link
names(combine.df.unique) <- c("author.link","author.name")
result.merge <- merge(result.merge, combine.df.unique, by="author.link")
result.merge <- result.merge[order(result.merge$freq, result.merge$prob, decreasing = TRUE),]
row.names(result.merge) <- 1:nrow(result.merge)

### write results to csv with aggregated
filename <- paste(path,"results-LM-",search.keyword,".csv",sep = "")
write.csv(file = filename, x = result.merge)