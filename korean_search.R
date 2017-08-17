library(tm)
library(KoNLP)

docs <- 
  c("����� ������ ���̳� �װ��� ���� ���ؼ��� ������ ���� ������ �� ��Ⱑ �־�� �Ѵ�.", 
    "������ ����� ��ü�� �����̴�.",
    "������ �����ڷ� ���ϴ� ������ ���.",
    "ģ���� �� ����� ��ü�� ��� �ϳ��� ��ȥ�̴�.",
    "�帣�� ������ ������ ���ٸ�, �ٴٰ� �Ǿ ��ٷ���.",
    "���� �Ҹ� ��� ���߿� ������ ����̶�.",
    "���� ������ ����� ���� ����ϴ� ����̴�.",
    "��� ��� ���")

#���ǻ� �˻�� �־��ش�. 
query <- "������ �ִ� ���"

names(docs) <- paste("doc", 1:length(docs), sep="")
docs <- c(docs, query=query)
docs.corp <- Corpus(VectorSource(docs))

#���ξ� �����Լ� 
konlp_tokenizer <- function(doc){
  extractNoun(doc)
}

# weightTfIdf �Լ� ���� �ٸ� ���� �Լ����� �����Ǵµ� ���� �޴����� �����ϱ� �ٶ���. 
tdmat <- TermDocumentMatrix(docs.corp, control=list(tokenize=konlp_tokenizer,
                                                    weighting = function(x) weightTfIdf(x, TRUE),
                                                    wordLengths=c(1,Inf)))

tdmatmat <- as.matrix(tdmat)

# ������ norm�� 1�� �ǵ��� ����ȭ 
norm_vec <- function(x) {x/sqrt(sum(x^2))}
tdmatmat <- apply(tdmatmat, 2, norm_vec)

# ���� ���絵 ��� 
docord <- t(tdmatmat[,9]) %*% tdmatmat[,1:8]

#�˻� ��� ������ 
orders <- data.frame(docs=docs[-9],scores=t(docord) ,stringsAsFactors=FALSE)
orders[order(docord, decreasing=T),]