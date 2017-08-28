############ prepare all tokens from all documents ##############
format(Sys.time(), "START : %a %b %d %X %Y")

### get content from each document by read csv to dataframe
path <- "C:/Users/U6063152/Documents/RScripts/data/"
filename <- paste(path,"database.csv",sep = "")
documents.df <- read.csv(filename)
documents.df.tmp <- documents.df

format(Sys.time(), "AFTER READ CSV : %a %b %d %X %Y")

### clean data
documents.df$title <- cleanData(documents.df$title)
documents.df$body <- cleanData(documents.df$body)
documents.df.clean <- documents.df

format(Sys.time(), "AFTER CLEAN DATA : %a %b %d %X %Y")

tokens.title <- list()
bigrams.title <- list()
trigrams.title <- list()
tokens.body <- list()
bigrams.body <- list()
trigrams.body <- list()

tmp <- sapply(1:nrow(documents.df), FUN = function(i){
  
  s <- documents.df[i,]
  
  ### title ###
  tokens <- tokenize(s$title)
  tokens.title[[length(tokens.title)+1]] <<- list(tokens)
  
  tokens2 <- c(tokens[-1], "STOP")
  bigrams <- paste(tokens, tokens2)
  bigrams <- bigrams[!grepl(bigrams, pattern = "STOP STOP")]
  bigrams.title[[length(bigrams.title)+1]] <<- list(bigrams)
  
  tokens3 <- c(tokens2[-1], "STOP")
  trigrams <- paste(tokens, tokens2, tokens3)
  trigrams <- trigrams[!grepl(trigrams, pattern = "STOP STOP")]
  trigrams.title[[length(trigrams.title)+1]] <<- list(trigrams)
  
  ### body ###
  tokens <- tokenize(s$body)
  tokens.body[[length(tokens.body)+1]] <<- list(tokens)
  
  tokens2 <- c(tokens[-1], "STOP")
  bigrams <- paste(tokens, tokens2)
  bigrams <- bigrams[!grepl(bigrams, pattern = "STOP STOP")]
  bigrams.body[[length(bigrams.body)+1]] <<- list(bigrams)
  
  tokens3 <- c(tokens2[-1], "STOP")
  trigrams <- paste(tokens, tokens2, tokens3)
  trigrams <- trigrams[!grepl(trigrams, pattern = "STOP STOP")]
  trigrams.body[[length(trigrams.body)+1]] <<- list(trigrams)
})

format(Sys.time(), "AFTER PREPARE ALL TOKENS : %a %b %d %X %Y")

#################################################################

format(Sys.time(), "START SEARCHING : %a %b %d %X %Y")

### setting search.keyword
search.keyword <- "Veracode"
search.keyword <- cleanData(search.keyword)

### clear old search results and prepare for store calculated probs
documents.df <- documents.df.clean
documents.df$prob <- rep(0, nrow(documents.df))

### Do this with each document (both title and body)
# 1. create unigram bigram and trigram
# 2. calculate prob with search keyword
documents.df$prob <- createNGramAndCalProbs(documents.df,search.keyword)

format(Sys.time(), "AFTER CAL PROB : %a %b %d %X %Y")

### rank documents
documents.df <- documents.df[order(documents.df$prob,decreasing = TRUE),]
if(nrow(documents.df)<100){
  documents.df.top <- documents.df[1:nrow(documents.df),]
}else{
  documents.df.top <- documents.df[1:100,]
}
documents.df.top <- documents.df.top[documents.df.top$prob!=0,]

if(nrow(documents.df.top)>0){
  ### new method to aggregate by authors and updaters
  MAX_AUTHOR <- 24
  combine.link <- c()
  combine.name <- c()
  combine.status <- c()
  for(i in 0:MAX_AUTHOR){
    col.link <- paste("authors.",i,".link",sep = "")
    author <- as.character(documents.df.top[[col.link]])
    combine.link <- c(combine.link, author[author!=""&!is.na(author)])
    
    col.author <- paste("authors.",i,".name",sep = "")
    author <- as.character(documents.df.top[[col.author]])
    combine.name <- c(combine.name, author[author!=""&!is.na(author)])
    
    col.status <- paste("authors.",i,".status",sep = "")
    status <- as.character(documents.df.top[[col.status]])
    combine.status <- c(combine.status, status[status!=""&!is.na(status)])
  }
  combine.df <- data.frame(combine.link, combine.name, combine.status)
  combine.df.unique <- unique.data.frame(combine.df)
  combine.df.unique <- combine.df.unique[!is.na(combine.df.unique$combine.link)&!is.na(combine.df.unique$combine.name),]
  
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
    docs <- docs[!is.na(docs)]
    
    ### combine dataframe of c and document list to result
    author.name <- as.character(combine.df.unique[combine.df.unique["combine.link"]==c,]$combine.name)
    for(d in docs){
      tmp.df <- data.frame(c, author.name, d, as.character(documents.df.top[documents.df.top["X_id"]==d,]$title), as.character(documents.df.top[documents.df.top["X_id"]==d,]$body), documents.df.top[documents.df.top["X_id"]==d,]$prob)
      names(tmp.df) <- c("author.link", "author.name", "id", "title", "body", "prob")
      result.df <- rbind(result.df, tmp.df)
    }
  }
  
  ### write results to csv
  search.keyword.filename <- gsub(pattern="[^[:alnum:] ]","",search.keyword)
  search.keyword.filename <- gsub(pattern=" ","-",search.keyword.filename)
  filename <- paste(path,"results-LM-",search.keyword.filename,"-list.csv",sep = "")
  write.csv(file = filename, x = result.df)
  
  ### group docs by author
  library(plyr)
  result.df.count <- count(result.df, "author.link")
  result.df.sum <- aggregate(prob ~ author.link,result.df,sum)
  result.merge <- merge(result.df.count, result.df.sum, by="author.link")
  
  ### map author.name with author.link
  names(combine.df.unique) <- c("author.link","author.name","author.status")
  result.merge <- merge(result.merge, combine.df.unique, by="author.link")
  result.merge <- result.merge[order(result.merge$freq, result.merge$prob, decreasing = TRUE),]
  row.names(result.merge) <- 1:nrow(result.merge)
  
  ### write results to csv with aggregated
  filename <- paste(path,"results-LM-",search.keyword.filename,".csv",sep = "")
  write.csv(file = filename, x = result.merge)
  
  ### filter only activated
  ### write results to csv with aggregated
  filename <- paste(path,"results-LM-",search.keyword.filename,"-active.csv",sep = "")
  write.csv(file = filename, x = result.merge[result.merge$author.status=="True",])
  
  format(Sys.time(), "FINISH : %a %b %d %X %Y")
  print(paste(search.keyword, " : complete"))
  
}else{
  print(paste(search.keyword, " : 0 result"))
}

#################################################################


trigrams.prob = function(tokens, bigrams, trigrams, firstWord, secondWord, thirdWord){
  text2 <- paste(c(firstWord,secondWord),collapse = " ")
  text3 <- paste(c(firstWord,secondWord,thirdWord),collapse = " ")
  return((length(trigrams[trigrams==text3])+0.001)/(length(bigrams[bigrams==text2])+0.001))
}

bigrams.prob = function(tokens, bigrams, firstWord, secondWord){
  text <- paste(c(firstWord,secondWord),collapse = " ")
  return((length(bigrams[bigrams==text])+0.001) / (length(tokens[tokens==firstWord])+0.001))
}

unigrams.prob = function(tokens, query){
  numberOfQ <- length(tokens[tokens==query])
  return((numberOfQ+0.001) / (length(tokens[tokens!="STOP"])+0.001))
}

markov.trigrams = function(tokens, bigrams, trigrams, query){
  prob <- unigrams.prob(tokens, query[1])
  prob <- prob * bigrams.prob(tokens, bigrams, query[1], query[2])
  
  for(i in 3:length(query)){
    prob <- prob*trigrams.prob(tokens, bigrams, trigrams, query[i-2], query[i-1], query[i])
  }
  
  return(prob)
}

markov.bigrams = function(tokens, bigrams, query){
  prob <- unigrams.prob(tokens, query[1])
  for(i in 2:length(query)){
    prob <- prob * bigrams.prob(tokens, bigrams, query[i-1], query[i])
  }
  return(prob)
}

markov.unigrams = function(tokens, query){
  prob <- unigrams.prob(tokens, query[1])
  return(prob)
}

isAllUnseen = function(tokens, query){
  for(q in query){
    if(length(tokens[tokens==q&q!="STOP"])!=0){
      return(FALSE)
    }
  }
  return(TRUE)
}

createNGramAndCalProbs = function(d,query){
  prob.query <- 0
  query.tokens <- tokenize(query)
  len.q <- length(query.tokens)-1
  
  prob <- sapply(1:nrow(d), FUN = function(i){
    prob.query <- 0
    
    ### tokenize content into tokens
    t.title <- unlist(tokens.title[[i]])
    t.bigrams.title <- unlist(bigrams.title[[i]])
    t.trigrams.title <- unlist(trigrams.title[[i]])
    
    t.body <- unlist(tokens.body[[i]])
    t.bigrams.body <- unlist(bigrams.body[[i]])
    t.trigrams.body <- unlist(trigrams.body[[i]])
    
    if(length(t.title)!=0&&length(t.title)!=length(t.title[t.title=="STOP"])&&length(t.body)!=0&&length(t.body)!=length(t.body[t.body=="STOP"])){
      
      ## check if all tokens in query unseen in this doc's title
      if(!isAllUnseen(t.title, query.tokens)){
        ### markov process to calculate how much probability can this query be in this language
        ### (how much related to this doc)
        
        ## choose N-Gram probs calculation based on length of query.tokens
        if(len.q==1){
          prob.query <- markov.unigrams(t.title, query.tokens)
        }else if(len.q==2){
          prob.query <- markov.bigrams(t.title, t.bigrams.title, query.tokens)
        }else{
          prob.query <- markov.trigrams(t.title, t.bigrams.title, t.trigrams.title, query.tokens)
        }
      }
      
      ## check if all tokens in query unseen in this doc's body
      if(!isAllUnseen(t.body, query.tokens)){
        if(len.q==1){
          prob.query <- prob.query+markov.unigrams(t.body, query.tokens)
        }else if(len.q==2){
          prob.query <- prob.query+markov.bigrams(t.body, t.bigrams.body, query.tokens)
        }else{
          prob.query <- prob.query+markov.trigrams(t.body, t.bigrams.body, t.trigrams.body, query.tokens)
        }
      }
    }
    prob.query
  })
  
  return(prob)
}

tokenize = function(s){
  s <- tolower(s)
  s <- gsub(pattern="[[:punct:]]+\\s+|[[:punct:]]+$"," STOP ",s)
  tokens <- unlist(strsplit(s, '\\s+', perl=TRUE))
  tokens <- gsub(tokens, pattern = "^[[:punct:]]*", replacement = "")
  if(length(tokens)!=0){
    if(tokens[length(tokens)]!="STOP"){
      tokens <- c(tokens,'STOP')
    } 
  }
  return(tokens[tokens!=""])
}

cleanData = function(d){
  ### remove html tag
  d <- gsub("<[^>]+>", "", d)
  d <- gsub("\u2028", "", d)
  ### remove html special character
  d <- gsub("&#?[a-zA-Z0-9];", "", d)
  ### unicode to ascii
  library(stringi)
  d <- stri_trans_general(d,"Latin-ASCII")
  ### replace ( ) [ ] with space
  d <- gsub(pattern = "\\]|\\[", " ", d)
  d <- gsub(pattern = "[()]", " ", d)
  ### remove \n
  d <- gsub(pattern = "\n", " ", d)
  ### separate camel case
  d <- gsub("([a-z])([A-Z])", "\\1 \\L\\2", d, perl = TRUE)
  return(d)
}
