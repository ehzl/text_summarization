
topic <- 3  #찾아낸 기사들에서 몇 개의 주제를 뽑아낼지
m <- 10  #총 기사 갯수
n <- 3  #주제와 관련 깊은 뉴스 몇개 뽑아낼건지
l <- 10 #한 토픽이 몇 개의 단어로 구성될지

#---------1. LSA로 수많은 기사들에서 가장 핫한 topic개의 이슈 뽑아냄--------------------------------------

setwd('C:/Users/DK/summarization')
label <- c('title', 'newdescp', 'url')

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
#n개의 단어 중 상위 n/3개의 단어

library(lsa)
news.lsa = lsa(tdm,topic)
#n개의 기사 -> topic개의 차원(주제)로 정리

library(GPArotation)
tk = Varimax(news.lsa$tk)$loadings

#for(i in 1:3){
#  print(i)
#  importance = order(abs(tk[, i]), decreasing = T) # 첫번째 값이 +일지 -일지 몰라서 abs 
#  #print(tk[importance[1:10], i])
#  print(names(tk[importance[1:10], i]))
#}
# i번째 차원에서 늘어나는/줄어나는 단어
# 기사가 많고 그 안에 비슷한 내용의 기사가 많으면 기사들이 어떤 내용을 말하는지 더 명확하게 나타날듯



#---------2. 뽑아낸 각 주제에 가장 연관성 높은 기사 n개씩 골라냄----------------------------------------

library(KoNLP)

for(i in 1:topic){
  cat("topic #", i, "")
  importance = order(abs(tk[, i]), decreasing = T) # 첫번째 값이 +일지 -일지 몰라서 abs 
  query <- names(tk[importance[1:l], i])
  print(names(tk[importance[1:l], i]))
  
  docs <- news$newdescp
  titles <- news$title
  names(docs) <- paste(titles, sep="")
  
  for(j in 1:l){
    docs[m+1] <- paste(docs[m+1],query[j])
  }
  docs.corp <- Corpus(VectorSource(docs))
  
  #색인어 추출함수 
  konlp_tokenizer <- function(doc){
    extractNoun(doc)
  }
  
  # weightTfIdf 함수 말고 다른 여러 함수들이 제공되는데 관련 메뉴얼을 참고하길 바란다. 
  tdmat <- TermDocumentMatrix(docs.corp, control=list(tokenize=konlp_tokenizer,
                                                      weighting = function(x) weightTfIdf(x, TRUE),
                                                      wordLengths=c(1,Inf)))
  
  tdmatmat <- as.matrix(tdmat)
  
  # 벡터의 norm이 1이 되도록 정규화 
  norm_vec <- function(x) {x/sqrt(sum(x^2))}
  tdmatmat <- apply(tdmatmat, 2, norm_vec)
  
  # 문서 유사도 계산 
  docord <- t(tdmatmat[,m+1]) %*% tdmatmat[,1:m]
  
  #검색 결과 리스팅 
  orders <- data.frame(docs=docs[-m],scores=t(docord) ,stringsAsFactors=FALSE)
  orders[order(docord, decreasing=T),]
  
  for(k in 1:n){
    #print(order(docord, decreasing=T)[i])
    print(names(docs[(order(docord, decreasing=T)[k])]))
  }
}

#write.table()
