ptm <- proc.time()

#read csv to dataframe
filename <- "C:/Users/U6063152/Documents/RScripts/data/data.csv" 
documents.df = read.csv(filename)

#clean data
documents.df$body <- gsub("<[^>]+>", "", documents.df$body)
documents.df$body <- gsub("\u2028", "", documents.df$body)
Encoding(documents.df$body) <- "latin1"
documents.df$body <- iconv(documents.df$body, "latin1", "ASCII", sub="")

library(tm)
corpus <- Corpus(VectorSource(documents.df$body))
corpus.tmp <- corpus

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, tolower)

#create a TDM with TF-IDF
library(SnowballC)
corpus.tdm <- TermDocumentMatrix(corpus, control = list(weighting=weightTfIdf, stemming = T))
corpus.tdm.rm <- removeSparseTerms(corpus.tdm, 0.99)
inspect(corpus.tdm.rm) #view TDM

#convert TDM to dataframe
corpus.matrix <- as.matrix(corpus.tdm.rm)
corpus.matrix <- t(corpus.matrix)
corpus.dataframe <- data.frame(corpus.matrix)
remove(corpus.tdm.rm)
remove(corpus.matrix)

proc.time() - ptm
ptm <- proc.time()

#search with search keyword
KEYWORD <- "eikon desktop"
corpus <- Corpus(VectorSource(KEYWORD))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stemDocument)
corpus <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
search.keyword <- unlist(strsplit(corpus[[1]],split = " "))
remove(corpus)

#normalize search keyword like data in TDM
if(exists("result.df")) remove(result.df)
for(s in search.keyword){
  if(s %in% colnames(corpus.dataframe)){
    if(!exists("result.df")) {
      result.df <- data.frame(corpus.dataframe[[s]])
    }else{
      result.df <- cbind(result.df,corpus.dataframe[[s]])
    }
  }
}

#sum the TF-IDF value of all columns (including in search keyword)
result.df.sum <- rowSums(result.df)
remove(result.df)
result.df.sum <- data.frame(result.df.sum)
result.df.sum <- cbind(result.df.sum,documents.df$contentId,documents.df$author.jive.username)
result.sorted <- result.df.sum[order(result.df.sum$result.df.sum, decreasing = TRUE),]
result.sorted <- result.sorted[1:100,]
names(result.sorted) <- c('sum','contentId','username')
  
#groupby author's username
library(plyr)
#count.df <- data.frame(table(result.sorted$username))
count.df <- count(result.sorted, 'username')
count.sorted <- count.df[order(count.df$freq, decreasing = TRUE),]
View(count.sorted)

proc.time() - ptm
