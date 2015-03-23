## exploring R libraries for text analysis and NLP

#if you need to install Rgraphviz:
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

require("tm")
require("RWeka")
require("gsubfn")
require("ggplot2")
require("qdap")
require("stringr")

setwd("~/Projects/Capstone/textprediction")

news <- readLines("data/en_US.news.txt",n=-1,encoding="ISO-8859-1")
blogs<- readLines("data/en_US.blogs.txt",n=-1,encoding="ISO-8859-1")
twitter<- readLines("data/en_US.twitter.txt",n=-1,encoding="ISO-8859-1")

#### clean imported text and create corpus  #####################################################

# find non-ascii chars and replace with empty
grep("I_WAS_NOT_ASCII", iconv(news, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
news_<-iconv(news, "latin1", "ASCII", sub="")
grep("I_WAS_NOT_ASCII", iconv(news_, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))


# read cleaned text into a Corpus to use with tm functions
corp<-VCorpus(VectorSource(news))
#writeCorpus(corp)

#quick look to verify read worked correctly
print(corp)
inspect(corp[1:2])
#meta(corp[[100]])

# use tm library to decaptalize, scrub, stem and remove terms
corp_ <- tm_map(corp, tolower)  # Make lowercase
corp_ <- tm_map(corp_, removePunctuation, preserve_intra_word_dashes = TRUE)
corp_ <- tm_map(corp_, removeWords, stopwords("english"))
corp_ <- tm_map(corp_, removeWords, c("shit", "damn", "fuck"))
corp_ <- tm_map(corp_, stemDocument)  # Stem words

#tokenize the Corpus (x-grams)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm<-TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))

#tokeinze the Corpus (words)
#tdm <- TermDocumentMatrix(corp, control = list(tokenize = words))

freqTerms<-findFreqTerms(tdm,100,Inf)

#plot(tdm,terms=freqTerms,weighting=FALSE,corThreshold = 0.7)


### IF NEEDED AGAIN ##########################################################

#news<-multigsub("^[\x20-\x7F]+$","",news)

#str_detect(news,"Â")
#str_detect(news,"\u0094")
#str_detect(news,"^[\x20-\x7F]+$")
#str_detect(news,"[\x93]")
#remove control characters

#### Quiz 1 stuff ############################################################
#max(nchar(news))
#max(nchar(blogs))
#max(nchar(twitter))

#love<-grepl("love",twitter)
#hate<-grepl("hate",twitter)
#sum(love)/sum(hate)

#twitter[grep("biostats",twitter)]

#length(grep("A computer once beat me at chess, but it was no match for me at kickboxing",twitter))

#remove extra whitespace, quotes, spaces before commas
#news_ <- scrubber(news_)
#strip digits and force all chars to lower case
#news_ <- strip(news_,char.keep=NULL,apostrophe.remove=FALSE)

# ***remove stopwords -- ASSUME STOPWORDS NEEDED FOR GOOD TEXT PREDICT
#news_ <- rm_stopwords(news_,tm::stopwords("english"),separate=FALSE,apostrophe.remove=FALSE)
