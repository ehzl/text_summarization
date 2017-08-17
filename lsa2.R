# library(KoNLP) 무슨 패키지지..

setwd('C:/Users/DK/summarization')
label <- c('newdescp')

news = read.csv("News4.csv", header = F, col.names = label, stringsAsFactors = F)

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
#454단어 -> 상위 200개의 단어

library(lsa)
news.lsa = lsa(tdm,3)
#여섯개의 기사 -> 3개의 차원(주제)

library(GPArotation)
tk = Varimax(news.lsa$tk)$loadings

for(i in 1:3){
  print(i)
  importance = order(abs(tk[, i]), decreasing = T) # 첫번째 값이 +일지 -일지 몰라서 abs 
  print(tk[importance[1:10], i])
}
# i번째 차원에서 늘어나는/줄어나는 단어
# 기사가 많고 그 안에 비슷한 내용의 기사가 많으면 기사들이 어떤 내용을 말하는지 더 명확하게 나타날듯
