## creating unigram and bigram data sets with frequencies

#if you need to install Rgraphviz:
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

require("tm")
require("RWeka")
require("gsubfn")
require("ggplot2")
require("qdap")
require("stringr")
require("slam")
require("stringr")
#require("plyr")
#require('wordnet')

setwd("~/Projects/Capstone/textprediction")

blogs<- readLines("data/en_US.blogs.txt",n=-1,encoding="ISO-8859-1")

text<-blogs

#### sammple x elements to reduce  size of data set ####

#red<-seq(1,length(text),by=100)
#text<-text[red]

#remove(news,blogs,twitter)

#### clean imported text and create corpus  #####################################################

# find non-ascii chars and remove them
text_<-iconv(text, "ISO-8859-1", "ASCII", sub="byte")
textASCII<-gsub("<[0-9|a-z][0-9|a-z]>","",text_)
remove(blogs,text,text_)

# remove quotes and forward slashes (not covered by removePunctuation)
textASCII<-gsub("\"","",textASCII)
textASCII<-gsub("/","",textASCII)

#more tests
grep("\"",textASCII)
grep("/",textASCII)

#replace sequences of "-" with whitespace
textASCII<-gsub("-+"," ",textASCII)

#remove extra whitespace, quotes, spaces before commas
textASCII <- scrubber(textASCII)
#strip digits and force all chars to lower case
textASCII <- strip(textASCII,char.keep=NULL,apostrophe.remove=FALSE)

### DOES NOT WORK - Weka tokenizer operates on char vectors, not vectors of strings
blogwords<-NGramTokenizer(textASCII,Weka_control(min = 1, max = 1,delimiters=" \r\n\t.,;:\"()?!"))

