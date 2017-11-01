
topic <- 3  
m <- 10
n <- 3
l <- 10

#dir <- commandArgs(trailingOnly = TRUE)

#---------1.LSA--------------------------------------

#setwd()
label <- c('title', 'newdescp', 'url')

news = read.table("News.txt", header = F, col.names = label, sep="\t", stringsAsFactors = F, fill=T, encoding="UTF-8")

library(tm)
tdm = TermDocumentMatrix(Corpus(VectorSource(news$newdescp)),
                         control = list(removeNumbers = T,
                                        removePunctuation = T,
                                        stopwords = T))

library(slam)
word.count = as.array(rollup(tdm, 2))
word.order = order(word.count, decreasing = T)
fre.qword = word.order[1:(dim(tdm)[1])/3]
#row.names(tdm[freq.word,])

library(lsa)
news.lsa = lsa(tdm,topic)

library(GPArotation)
tk = Varimax(news.lsa$tk)$loadings


#---------2.choose articles of each topic----------------------------------------

#library(KoNLP)

for(i in 1:topic){
  tt <- paste("topic", i,".txt")      
  #tt <- paste("topic", i,".xlsx")  
  importance = order(abs(tk[, i]), decreasing = T) 
  query <- names(tk[importance[1:l], i])
  #print(names(tk[importance[1:l], i]))           
  
  docs <- news$newdescp
  #titles <- news$title
  #names(docs) <- paste(titles, sep="")
  
  for(j in 1:l){
    docs[m+1] <- paste(docs[m+1],query[j])
  }
  docs.corp <- Corpus(VectorSource(docs))
  
  konlp_tokenizer <- function(doc){
    extractNoun(doc)
  }
  
  tdmat <- TermDocumentMatrix(docs.corp, control=list(tokenize=konlp_tokenizer,
                                                      weighting = function(x) weightTfIdf(x, TRUE),
                                                      wordLengths=c(1,Inf)))
  
  tdmatmat <- as.matrix(tdmat)
  
  norm_vec <- function(x) {x/sqrt(sum(x^2))}
  tdmatmat <- apply(tdmatmat, 2, norm_vec)
  
  docord <- t(tdmatmat[,m+1]) %*% tdmatmat[,1:m]
  
  orders <- data.frame(docs=docs[-m],scores=t(docord) ,stringsAsFactors=FALSE)
  orders[order(docord, decreasing=T),]

  carr <- ""
  
  for(k in 1:n){
    carr[k] <- docs[(order(docord, decreasing=T)[k])]
  }

  write.table(carr, tt, row.names = F, col.names = F) 
}

