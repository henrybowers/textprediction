## Capstone - Exploratory Analysis of Data Sets

require("tm")
require("RWeka")
require("gsubfn")
require("ggplot2")
require("qdap")
require("stringr")
require("slam")
require("plyr")
#require("plyr")
#require('wordnet')

setwd("~/Projects/Capstone/textprediction")

news <- readLines("data/en_US.news.txt",n=-1,encoding="ISO-8859-1")
blogs<- readLines("data/en_US.blogs.txt",n=-1,encoding="ISO-8859-1")
twitter<- readLines("data/en_US.twitter.txt",n=-1,encoding="ISO-8859-1")

print(paste("Lines of text: ",length(news)))
newsWC<-wc(news,digit.remove=FALSE)
print(paste("Total words: ",sum(newsWC,na.rm=TRUE)))
print(paste("Mean words per line: ",round(mean(newsWC,na.rm=TRUE),1)))
print(paste("Max words per line: ",round(max(newsWC,na.rm=TRUE),1)))
print(paste("Min words per line: ",round(max(newsWC,na.rm=TRUE),1)))
