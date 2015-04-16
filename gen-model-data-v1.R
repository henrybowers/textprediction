## exploring R libraries for text analysis and NLP

#if you need to install Rgraphviz:
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

#require("tm")
require("RWeka")
#require("gsubfn")
require("ggplot2")
require("plyr")
require("qdap")
require("stringr")
require("slam")

setwd("~/Projects/Capstone/textprediction")

news <- readLines("../data/en_US.news.txt",n=-1,encoding="ISO-8859-1")
blogs<- readLines("../data/en_US.blogs.txt",n=-1,encoding="ISO-8859-1")
twitter<- readLines("../data/en_US.twitter.txt",n=-1,encoding="ISO-8859-1")

Text<-c(news,blogs,twitter)

#### sammple x elements to reduce  size of data set ####

red<-seq(1,length(Text),by=50)
text<-Text[red]


#### clean imported text and create corpus  #####################################################

# find non-ascii chars and remove them
text_<-iconv(text, "ISO-8859-1", "ASCII", sub="byte")
textASCII<-gsub("<[0-9|a-z][0-9|a-z]>","",text_)
#test
#grep("<[0-9|a-z][0-9|a-z]>",text_)
#grep("<[0-9|a-z][0-9|a-z]>",textASCII)

# make some space
remove(news,blogs,twitter,text,text_)

# remove quotes and forward slashes (not covered by removePunctuation)
textASCII<-gsub("\"","",textASCII)
textASCII<-gsub("/","",textASCII)

#more tests
#grep("\"",textASCII)
#grep("/",textASCII)

#replace sequences of "-" with whitespace
textASCII<-gsub("-+"," ",textASCII)

#remove extra whitespace, quotes, spaces before commas
#textASCII <- scrubber(textASCII)
#strip digits and force all chars to lower case
textASCII <- strip(textASCII,char.keep="'",apostrophe.remove=FALSE)



#### Tokenize the Corpus ######################################################

#v<-NGramTokenizer(textASCII, Weka_control(min = 1, max = 1,delimiters=" \r\n\t.,;:\"()?!"))
#termFreqTable<-as.data.frame(table(v),stringsAsFactors=FALSE)
#names(termFreqTable)<-c("term","freq")
#termFreqTable<-termFreqTable[termFreqTable$freq>1,]
#termFreqTable<-termFreqTable[order(-termFreqTable$freq),]


v<-NGramTokenizer(textASCII, Weka_control(min = 2, max = 2,delimiters=" \r\n\t.,;:\"()?!"))
termFreqTable2<-as.data.frame(table(v),stringsAsFactors=FALSE)
names(termFreqTable2)<-c("term","freq")
termFreqTable2<-termFreqTable2[termFreqTable2$freq>1,]
termFreqTable2<-termFreqTable2[order(-termFreqTable2$freq),]

v<-NGramTokenizer(textASCII, Weka_control(min = 3, max = 3,delimiters=" \r\n\t.,;:\"()?!"))
termFreqTable3<-as.data.frame(table(v),stringsAsFactors=FALSE)
names(termFreqTable3)<-c("term","freq")
termFreqTable3<-termFreqTable3[termFreqTable3$freq>1,]
termFreqTable3<-termFreqTable3[order(-termFreqTable3$freq),]

#v<-NGramTokenizer(textASCII, Weka_control(min = 1, max = 3,delimiters=" \r\n\t.,;:\"()?!"))
#termFreqTableN<-as.data.frame(table(v),stringsAsFactors=FALSE)
#names(termFreqTableN)<-c("term","freq")
#termFreqTableN<-termFreqTableN[termFreqTableN$freq>1,]
#termFreqTableN<-termFreqTableN[order(-termFreqTableN$freq),]

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

#stats(termFreqTable$freq)

stats(termFreqTable2$freq)

stats(termFreqTable3$freq)

# review most frequent x-grams

#print("Most frequent 1-grams:")
#head(termFreqTable[order(-termFreqTable$freq),])
print("Most frequent bigrams:")
head(termFreqTable2[order(-termFreqTable2$freq),])
print("Most frequent trigrams:")
head(termFreqTable3[order(-termFreqTable3$freq),])
#print("Most frequent n-grams:")
#head(termFreqTableN[order(-termFreqTableN$freq),])

## Create training/testing sets ###############################################

#write.csv(termFreqTable,"termFreqTable.csv", row.names = FALSE)
write.csv(termFreqTable2,"termFreqTable2.csv", row.names = FALSE)
write.csv(termFreqTable3,"termFreqTable3.csv", row.names = FALSE)
#write.csv(termFreqTableN,"termFreqTableN.csv", row.names = FALSE)





## Create final test set (repeats code above - would be cleaner to weave into

news <- readLines("../data/en_US.news.txt",n=-1,encoding="ISO-8859-1")
blogs<- readLines("../data/en_US.blogs.txt",n=-1,encoding="ISO-8859-1")
twitter<- readLines("../data/en_US.twitter.txt",n=-1,encoding="ISO-8859-1")

text<-c(news,blogs,twitter)

#### sammple different set of elements for testing ####

red<-seq(1,length(text),by=500)
text<-text[red]


#### clean imported text and create corpus  #####

# find non-ascii chars and remove them
text_<-iconv(text, "ISO-8859-1", "ASCII", sub="byte")
textASCII<-gsub("<[0-9|a-z][0-9|a-z]>","",text_)
#test
#grep("<[0-9|a-z][0-9|a-z]>",text_)
#grep("<[0-9|a-z][0-9|a-z]>",textASCII)

# make some space
remove(news,blogs,twitter,text,text_)

# remove quotes and forward slashes (not covered by removePunctuation)
textASCII<-gsub("\"","",textASCII)
textASCII<-gsub("/","",textASCII)

#more tests
#grep("\"",textASCII)
#grep("/",textASCII)

#replace sequences of "-" with whitespace
textASCII<-gsub("-+"," ",textASCII)

#remove extra whitespace, quotes, spaces before commas
#textASCII <- scrubber(textASCII)
#strip digits and force all chars to lower case
textASCII <- strip(textASCII,char.keep="'",apostrophe.remove=FALSE)



#### Tokenize the Corpus 
# for final test set reduce frequency threshold for keepers

v<-NGramTokenizer(textASCII, Weka_control(min = 3, max = 3,delimiters=" \r\n\t.,;:\"()?!"))
termFreqTableT<-as.data.frame(table(v),stringsAsFactors=FALSE)
names(termFreqTableT)<-c("term","freq")
termFreqTableT<-termFreqTableT[termFreqTableT$freq>1,]
termFreqTableT<-termFreqTableT[order(-termFreqTableT$freq),]


# write final test set to file
write.csv(termFreqTableT,"termFreqTableT.csv", row.names = FALSE)


## Calc probs ( ngram model)###################################################3

# prob x | y = count(y,x) / count(y)

# predict on last word
# test code - IGNORE
#text<-"and I'd"
#l<-tolower(unlist(str_extract_all(text,"[^[:blank:]]+")))
#biCount<-termFreqTable2[termFreqTable2$term==tolower(text),]$freq
#uniCount<-termFreqTable[termFreqTable$term==l[1],]$freq
#iCount/uniCount

text<-"and I'd"
l<-tolower(unlist(str_extract_all(text,"[^[:blank:]]+")))
uniCount<-termFreqTable[termFreqTable$term==l[1],]$freq

probs<-vector(mode="numeric",length=nrow(termFreqTable))
for(i in 1:length(probs)) {
  biGram<-paste(l[1],termFreqTable[i,]$term)
  #if(biGram=="i have") print(paste("***** i have is",i,"*******"))
  biCount<-termFreqTable2[termFreqTable2$term==biGram,]$freq
  biCount<-ifelse(length(biCount)==0,0,biCount)
  probs[i]<-biCount/uniCount
}
mostLikely<-probs==max(probs)
termFreqTable[mostLikely,]$term



# predict on last 2 words

#BEGIN TEST
text<-tolower("and I'd")
termFreqTable3[grep("i'd",termFreqTable3$term),]
l<-tolower(unlist(str_extract_all(text,"[^[:blank:]]+")))
uniCount<-termFreqTable2[termFreqTable2$term==text,]$freq
triGram<-paste(l[1],l[2],"sleep")
#if(biGram=="i have") print(paste("***** i have is",i,"*******"))
triCount<-termFreqTable3[termFreqTable3$term==triGram,]$freq
triCount<-ifelse(length(triCount)==0,0,triCount)
triCount/uniCount
#### END TEST

text<-tolower("and I'd")
l<-tolower(unlist(str_extract_all(text,"[^[:blank:]]+")))
uniCount<-termFreqTable2[termFreqTable2$term==text,]$freq

probs<-vector(mode="numeric",length=nrow(termFreqTable))
for(i in 1:length(probs)) {
  triGram<-paste(l[1],l[2],termFreqTable[i,]$term)
  #if(biGram=="i have") print(paste("***** i have is",i,"*******"))
  triCount<-termFreqTable3[termFreqTable3$term==triGram,]$freq
  triCount<-ifelse(length(triCount)==0,0,triCount)
  probs[i]<-triCount/uniCount
}
mostLikely<-probs==max(probs)
termFreqTable[mostLikely,]$term


