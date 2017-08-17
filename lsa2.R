# library(KoNLP) ���� ��Ű����..

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
#454�ܾ� -> ���� 200���� �ܾ�

library(lsa)
news.lsa = lsa(tdm,3)
#�������� ��� -> 3���� ����(����)

library(GPArotation)
tk = Varimax(news.lsa$tk)$loadings

for(i in 1:3){
  print(i)
  importance = order(abs(tk[, i]), decreasing = T) # ù��° ���� +���� -���� ���� abs 
  print(tk[importance[1:10], i])
}
# i��° �������� �þ��/�پ�� �ܾ�
# ��簡 ���� �� �ȿ� ����� ������ ��簡 ������ ������ � ������ ���ϴ��� �� ��Ȯ�ϰ� ��Ÿ����