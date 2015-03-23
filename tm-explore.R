## exploring R libraries for text analysis and NLP

#if you need to install Rgraphviz:
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

require("tm")
require("RWeka")

setwd("~/Projects/Capstone/textprediction")

#read all three files into the news Corpus
corp<-VCorpus(DirSource("data"))

#quick look to verify read worked correctly
inspect(corp[1:2])
meta(corp[[1]])

#clean corpus in prep for tokenizing
corp<-tm_map(corp,removePunctuation)
#corp<-tm_map(corp,removeNumbers)
#corp<-tm_map(corp,stripWhitespace)
#corp<-tm_map(corp,content_transformer(tolower))

#tokenize corpus
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm<-TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
tdm <- TermDocumentMatrix(corp, control = list(tokenize = words))

freqTerms<-findFreqTerms(tdm,100,Inf)

plot(tdm,terms=freqTerms[1:20],weighting=TRUE,corThreshold = 0.7)


data("crude")
crude[[1]]
(f <- content_transformer(function(x, pattern) gsub(pattern, "", x)))
tm_map(crude, f, "[[:digit:]]+")[[1]]

