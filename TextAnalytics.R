#read csv to dataframe
filename <- "C:/Users/u6036584/Documents/R/data.csv" 
documents.df = read.csv(filename)

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
corpus <- tm_map(corpus, stemDocument)
corpus <- TermDocumentMatrix(corpus)
corpus <- removeSparseTerms(corpus, 0.95)
inspect(corpus) #view TDM

corpus.matrix <- as.matrix(corpus)
corpus.matrix <- t(corpus.matrix)
corpus.dataframe <- data.frame(corpus.matrix)
corpus.dataframe$contentId <- documents.df$contentId

#y = prcomp(corpus)
#y$sdev^2
#screeplot(y,type="lines")

wss <- (nrow(corpus.dataframe)-1)*sum(apply(corpus.dataframe,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(corpus.dataframe,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

cluster <- kmeans(corpus.dataframe,6)
library(fpc)
plotcluster(corpus.dataframe, cluster$cluster)

#write to csv
documents.df$cluster <- cluster$cluster
write.csv(file='result.csv', x=documents.df)
