#setwd("C:/Users/U6063152/Documents/RScripts")

#read csv to dataframe
filename <- "C:/Users/U6063152/Documents/RScripts/data/data.csv" 
doc.df = read.csv(filename)

doc.df$body <- gsub("<[^>]+>", "", doc.df$body)

library(tm)
cp <- cp(VectorSource(doc.df$body))
cp <- TermDocumentMatrix(cp)
cp.tmp <- cp
cp <- removeSparseTerms(cp, 0.95)
inspect(cp) #view TDM

cp.matrix <- as.matrix(cp)
cp.matrix <- t(cp.matrix)
cp.dataframe <- data.frame(cp.matrix)
cp.dataframe$contentId <- doc.df$contentId
cp.dataframe$author <- doc.df$author
cp.dataframe$updater <- doc.df$updater

#y = prcomp(cp)
#y$sdev^2
#screeplot(y,type="lines")

wss <- (nrow(cp.dataframe)-1)*sum(apply(cp.dataframe,2,var))
for (i in 2:50) wss[i] <- sum(kmeans(cp.dataframe,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
