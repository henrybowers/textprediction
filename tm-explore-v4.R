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
require("slam")
require("stringr")
#require("plyr")
#require('wordnet')

setwd("~/Projects/Capstone/textprediction")

news <- readLines("data/en_US.news.txt",n=-1,encoding="ISO-8859-1")
blogs<- readLines("data/en_US.blogs.txt",n=-1,encoding="ISO-8859-1")
twitter<- readLines("data/en_US.twitter.txt",n=-1,encoding="ISO-8859-1")

text<-c(news,blogs,twitter)

#### sammple x elements to reduce  size of data set ####

red<-seq(1,length(text),by=50)
text<-text[red]

#remove(news,blogs,twitter)

#### clean imported text and create corpus  #####################################################

# find non-ascii chars and remove them
text_<-iconv(text, "ISO-8859-1", "ASCII", sub="byte")
textASCII<-gsub("<[0-9|a-z][0-9|a-z]>","",text_)
#test
grep("<[0-9|a-z][0-9|a-z]>",text_)
grep("<[0-9|a-z][0-9|a-z]>",textASCII)

# make some space
remove(news,blogs,twitter,text,text_)

# remove quotes and forward slashes (not covered by removePunctuation)
textASCII<-gsub("\"","",textASCII)
textASCII<-gsub("/","",textASCII)

#more tests
grep("\"",textASCII)
grep("/",textASCII)

#replace sequences of "-" with whitespace
textASCII<-gsub("-+"," ",textASCII)

# read cleaned text into a Corpus to use with tm functions
corp<-VCorpus(VectorSource(textASCII))

#quick look to verify read worked correctly
print(corp)
inspect(corp[1:10])
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

termFreqTable<-tokenize(corp,1)
#termFreqTable<-tokenize(corp,1)

#freqTerms<-findFreqTerms(tdm,40000,Inf)

### Tokenize the Corpus (bi-grams)

termFreqTable2<-tokenize(corp,2)
#termFreqTable2<-tokenize(corp,2)

#freqTerms2<-findFreqTerms(tdm2,5000,Inf)

termFreqTable3<-tokenize(corp,3)
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
        boxplot(freq)
        
}

# get distribution stats

stats(termFreqTable$freq)

stats(termFreqTable2$freq)

stats(termFreqTable3$freq)

# review most frequent x-grams

termFreqTable<-termFreqTable[order(-termFreqTable$freq),]
termFreqTable2<-termFreqTable2[order(-termFreqTable2$freq),]
termFreqTable3<-termFreqTable3[order(-termFreqTable3$freq),]

print("Most frequent 1-grams:")
head(termFreqTable[order(-termFreqTable$freq),])
print("Most frequent bigrams:")
head(termFreqTable2[order(-termFreqTable2$freq),])
print("Most frequent trigrams:")
head(termFreqTable3[order(-termFreqTable3$freq),])


### Build first-order markov model ###########################################
l<-str_extract_all(termFreqTable2$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2")
sFreqTable2<-cbind(termFreqTable2,temp)


#NOT BEING USED RIGHT NOW
get_prob<-function (t,s,i) {
        totFreq<-sum(t[t$s1==s,]$freq)
        prob<-t[i,]$freq/totFreq
}

sFreqTable2$prob<-0.0

for(i in 1:nrow(sFreqTable2)) {
        totFreq<-sum(sFreqTable2[sFreqTable2$s1==sFreqTable2[i,]$s1,]$freq)
        sFreqTable2[i,]$prob<-sFreqTable2[i,]$freq/totFreq
}

sFreqTable2<-sFreqTable2[order(-sFreqTable2$freq),]

# test model

bi_next_word <- function(markov,w) {
        matches<-markov[markov$s1==w,]
        matches[matches$freq==max(matches$freq),]$s2
}


bi_next_word(sFreqTable2,"can")
bi_next_word(sFreqTable2,"will")
bi_next_word(sFreqTable2,"can't")


### Build second-order markov model ##########################################
l<-str_extract_all(termFreqTable3$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2","s3")
sFreqTable3<-cbind(termFreqTable3,temp)

sFreqTable3$prob<-0.0

totFreq<-sum(sFreqTable3[sFreqTable3$s1==sFreqTable3[1,]$s1 & 
                     sFreqTable3$s2==sFreqTable3[1,]$s2,]$freq)

for(i in 1:nrow(sFreqTable3)) {
        totFreq<-sum(sFreqTable3[sFreqTable3$s1==sFreqTable3[i,]$s1 & 
                                         sFreqTable3$s2==sFreqTable3[i,]$s2,]$freq)
        sFreqTable3[i,]$prob<-sFreqTable3[i,]$freq/totFreq
}

sFreqTable3<-sFreqTable3[order(-sFreqTable3$freq),]

tri_next_word <- function(markov,w) {
        l<-as.vector(str_extract_all(w,"[^[:blank:]]+")[[1]])
        matches<-markov[markov$s1==l[1] & markov$s2==l[2],]
        matches[matches$freq==max(matches$freq),]$s3
}

tri_next_word(sFreqTable3,"going to")
tri_next_word(sFreqTable3,"case of")
tri_next_word(sFreqTable3,"me the")
tri_next_word(sFreqTable3,"going to")


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
