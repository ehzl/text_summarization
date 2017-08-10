setwd('C:/Users/DK/Documents/R')
label <- c('article', 'url','id','newdescp','nil', 'option3', 'oururl')

news = read.csv("News3.csv", header = F, col.names = label, stringsAsFactors = F)

library(tm)
tdm = TermDocumentMatrix(Corpus(VectorSource(news$newdescp)),
                         control = list(removeNumbers = T,
                                        removePunctuation = T,
                                        stopwords = T))

library(slam)
word.count = as.array(rollup(tdm, 2))
word.order = order(word.count, decreasing = T)
freq.word = word.order[1:(dim(tdm)[1])/3]
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