
topic <- 3 # num of topic 
m <- 100   # num of articles
#n <- 1     # num of selected articles per topic
l <- 10    # num of verb in one topic

#----------------------1. find 'topic' number of big issues-------------------------------------

label <- c('title', 'newdescp')

news = read.table("output.txt", header = F, col.names = label, sep="\t", stringsAsFactors = F, fill=T, encoding="UTF-8")

library(tm)
tdm = TermDocumentMatrix(Corpus(VectorSource(news$newdescp)),
                         control = list(removeNumbers = T,
                                        removePunctuation = T,
                                        stopwords = T))

library(slam)
word.count = as.array(rollup(tdm, 2))
word.order = order(word.count, decreasing = T)
fre.qword = word.order[1:(dim(tdm)[1])/3]

library(lsa)
news.lsa = lsa(tdm,topic)

library(GPArotation)
tk = Varimax(news.lsa$tk)$loadings


#---------2.choose one article that has the highest similarity with each topic---------------------

carr = ""  # array of contents of news articles
narr = ""  # array of titles of news articles

# for each topic
for(i in 1:topic){
  tt <- paste("topic", i,".txt")      
  importance = order(abs(tk[, i]), decreasing = T) 
  query <- names(tk[importance[1:l], i])        
  
  docs <- news$newdescp
  titles <- news$title
  names(docs) <- paste(titles, sep="")
  
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
  
  index = 1
  narr[i] <- names(docs[(order(docord, decreasing=T)[index])])
  carr[i] <- docs[(order(docord, decreasing=T)[index])]

  # eliminate overlapping case
  if(i>1){
    for(k in 1:i-1){
      if(narr[i] %in% c(narr[k])){
        index = index + 1
        narr[i] <- names(docs[(order(docord, decreasing=T)[index])])
        carr[i] <- docs[(order(docord, decreasing=T)[index])]
      }
    }
  }
  
  cat(narr[i],"\n",carr[i],"\n", file=tt, append=F)
}


