setwd('C:/Users/DK/Documents/R')
label <- c('rl','id','newdescp','nil','option3','oururl')

news = read.csv("News2.csv", stringsAsFactors = F)

library(tm)
tdm = TermDocumentMatrix(Corpus(VectorSource(news$newdescp)),
                         control = list(removeNumbers = T,
                                        removePunctuation = T,
                                        stopwords = T))

library(slam)
word.count = as.array(rollup(tdm, 2))
word.order = order(word.count, decreasing = T)
freq.word = word.order[1:1000]
#row.names(tdm[freq.word,])

library(lsa)
news.lsa = lsa(tdm,30)

library(GPArotation)
tk = Varimax(news.lsa$tk)$loadings

for(i in 1:30){
  print(i)
  importance = order(abs(tk[, i]), decreasing = T)
  print(tk[importance[1:10], i])
}