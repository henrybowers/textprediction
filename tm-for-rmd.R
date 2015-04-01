## exploring R libraries for text analysis and NLP

#if you need to install Rgraphviz:
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

require("tm",quietly=TRUE,warn.conflicts=FALSE)
require("RWeka",quietly=TRUE,warn.conflicts=FALSE)
require("gsubfn",quietly=TRUE,warn.conflicts=FALSE)
require("ggplot2",quietly=TRUE,warn.conflicts=FALSE)
require("qdap",quietly=TRUE,warn.conflicts=FALSE)
require("stringr",quietly=TRUE,warn.conflicts=FALSE)
require("slam",quietly=TRUE,warn.conflicts=FALSE)
require("stringr",quietly=TRUE,warn.conflicts=FALSE)
#require("plyr")
#require('wordnet')

setwd("~/Projects/Capstone/textprediction")

news <- readLines("data/en_US.news.txt",n=-1,encoding="ISO-8859-1")
blogs<- readLines("data/en_US.blogs.txt",n=-1,encoding="ISO-8859-1")
twitter<- readLines("data/en_US.twitter.txt",n=-1,encoding="ISO-8859-1")

text<-c(news,blogs,twitter)

#### sammple x elements to reduce  size of data set ####

red<-seq(1,length(text),by=100)
text<-text[red]

#remove(news,blogs,twitter)

#### clean imported text and create corpus  #####################################################

# find non-ascii chars and remove them
text_<-iconv(text, "ISO-8859-1", "ASCII", sub="byte")
textASCII<-gsub("<[0-9|a-z][0-9|a-z]>","",text_)
#test
#grep("<[0-9|a-z][0-9|a-z]>",text_)
#grep("<[0-9|a-z][0-9|a-z]>",textASCII)

# remove quotes and forward slashes (not covered by removePunctuation)
textASCII<-gsub("\"","",textASCII)
textASCII<-gsub("/","",textASCII)

#more tests
#grep("\"",textASCII)
#grep("/",textASCII)

#replace sequences of "-" with whitespace
textASCII<-gsub("-+"," ",textASCII)

# read cleaned text into a Corpus to use with tm functions
corp<-VCorpus(VectorSource(textASCII))
#writeCorpus(corp)

#quick look to verify read worked correctly
#print(corp)
#inspect(corp[1:10])
#meta(corp[[100]])

# use tm library to decaptalize, scrub, stem and remove terms
corp<-tm_map(corp, content_transformer(tolower))  # Make lowercase 
#corp<-tm_map(corp,content_transformer(stemmer)) #Breaks x-grams over 1
corp<-tm_map(corp, removeNumbers) #remove numbers
#corp<-tm_map(corp, removePunctuation, preserve_intra_word_dashes = TRUE)
#corp<-tm_map(corp, removeWords, stopwords("english"))
corp<-tm_map(corp, removeWords, c("shit", "damn", "fuck"))
corp<-tm_map(corp, stripWhitespace)

#just in case, coerce VCorp to ensure TermDocumentMatrix recognizes it
#corp<-tm_map(corp,PlainTextDocument)

#### Tokenize the Corpus ######################################################

tokenize<-function(corp,n=1,cut=0) {
        BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n,
                                                                      delimiters=" \r\n\t.,;:\"()?!"))
        tdm<-TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
        v<-row_sums(tdm)
        termTable<-data.frame(term=names(v),freq=v,row.names = NULL,stringsAsFactors=FALSE)
        termTable[termTable$freq>cut,]
}


### Tokenize the Corpus (1-grams)

termFreqTable<-tokenize(corp,1,2)
#termFreqTable<-tokenize(corp,1)

#freqTerms<-findFreqTerms(tdm,40000,Inf)

### Tokenize the Corpus (bi-grams)

termFreqTable2<-tokenize(corp,2,4)
#termFreqTable2<-tokenize(corp,2)

#freqTerms2<-findFreqTerms(tdm2,5000,Inf)

termFreqTable3<-tokenize(corp,3,2)
#termFreqTable3<-tokenize(corp,3)

### Explore x-grams ##########################################################

stats<-function(freq) {
        print(paste("Num = ",length(freq)))
        print(paste("Mean = ",round(mean(freq),1)))
        print(paste("Standard Dev = ",round(sd(freq),1)))
        print(paste("max frequency = ",max(freq)))
        print(paste("min frequency = ",min(freq)))
        
        hist(log(freq))
        rug(jitter(log(freq),amount=0.1),col="red")
        #boxplot(freq)
        
}

termFreqTable<-termFreqTable[order(-termFreqTable$freq),]
termFreqTable2<-termFreqTable2[order(-termFreqTable2$freq),]
termFreqTable3<-termFreqTable3[order(-termFreqTable3$freq),]
