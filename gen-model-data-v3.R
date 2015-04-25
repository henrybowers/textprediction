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

#### config training set ######################

#set min frequency to keep (floor+1)
floor <- 4

#set corpus sample size
sampleSize <- 5000

#set config tag for output filenames
sizeLabel <- paste("_",sampleSize/1000,"K_",floor,sep="")

news <- readLines("../data/en_US.news.txt",n=-1,encoding="ISO-8859-1")
blogs<- readLines("../data/en_US.blogs.txt",n=-1,encoding="ISO-8859-1")
twitter<- readLines("../data/en_US.twitter.txt",n=-1,encoding="ISO-8859-1")

Text<-c(news,blogs,twitter)

#### sammple x elements to reduce  size of data set ####
set.seed(39955)
#red<-seq(1,length(Text),by=10)
text<-sample(Text,sampleSize)


#### clean imported text and create corpus  #####################################################

# find non-ascii chars and remove them
text_<-iconv(text, "ISO-8859-1", "ASCII", sub="byte")
textASCII<-gsub("<[0-9|a-z][0-9|a-z]>","",text_)
#test
#grep("<[0-9|a-z][0-9|a-z]>",text_)
#grep("<[0-9|a-z][0-9|a-z]>",textASCII)

# make some space
remove(text,text_)

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

v<-NGramTokenizer(textASCII, Weka_control(min = 1, max = 1,delimiters=" \r\n\t.,;:\"()?!"))
termFreqTable<-as.data.frame(table(v),stringsAsFactors=FALSE)
names(termFreqTable)<-c("term","freq")
termFreqTable<-termFreqTable[termFreqTable$freq>floor,]
termFreqTable<-termFreqTable[order(-termFreqTable$freq),]


v<-NGramTokenizer(textASCII, Weka_control(min = 2, max = 2,delimiters=" \r\n\t.,;:\"()?!"))
termFreqTable2<-as.data.frame(table(v),stringsAsFactors=FALSE)
names(termFreqTable2)<-c("term","freq")
termFreqTable2<-termFreqTable2[termFreqTable2$freq>floor,]
termFreqTable2<-termFreqTable2[order(-termFreqTable2$freq),]

v<-NGramTokenizer(textASCII, Weka_control(min = 3, max = 3,delimiters=" \r\n\t.,;:\"()?!"))
termFreqTable3<-as.data.frame(table(v),stringsAsFactors=FALSE)
names(termFreqTable3)<-c("term","freq")
termFreqTable3<-termFreqTable3[termFreqTable3$freq>floor,]
termFreqTable3<-termFreqTable3[order(-termFreqTable3$freq),]

v<-NGramTokenizer(textASCII, Weka_control(min = 4, max = 4,delimiters=" \r\n\t.,;:\"()?!"))
termFreqTable4<-as.data.frame(table(v),stringsAsFactors=FALSE)
names(termFreqTable4)<-c("term","freq")
termFreqTable4<-termFreqTable4[termFreqTable4$freq>floor,]
termFreqTable4<-termFreqTable4[order(-termFreqTable4$freq),]


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

stats(termFreqTable$freq)

stats(termFreqTable2$freq)

stats(termFreqTable3$freq)

stats(termFreqTable4$freq)

# review most frequent x-grams

print("Most frequent 1-grams:")
head(termFreqTable[order(-termFreqTable$freq),])
print("Most frequent bigrams:")
head(termFreqTable2[order(-termFreqTable2$freq),])
print("Most frequent trigrams:")
head(termFreqTable3[order(-termFreqTable3$freq),])
print("Most frequent 4-grams:")
head(termFreqTable4[order(-termFreqTable4$freq),])


#decompose ngrams to facilitate prob calcs

l<-str_extract_all(termFreqTable2$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2")
termFreqTable2<-cbind(termFreqTable2,temp)
remove(temp)

l<-str_extract_all(termFreqTable3$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2","s3")
termFreqTable3<-cbind(termFreqTable3,temp)
remove(temp)

l<-str_extract_all(termFreqTable4$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2","s3","s4")
termFreqTable4<-cbind(termFreqTable4,temp)
remove(temp)


# calc n-gram probs from frequencies

# first unigrams
termFreqTable$prob <- termFreqTable$freq/sum(termFreqTable$freq)

# then bigrams
uni<-vector(mode="numeric",length=nrow(termFreqTable2))
for(i in 1:nrow(termFreqTable2)) {
    uni[i] <- termFreqTable[termFreqTable$term==termFreqTable2[i,]$s1,]$freq
}

termFreqTable2$prob <- termFreqTable2$freq/uni

# then trigrams
uni<-vector(mode="numeric",length=nrow(termFreqTable3))
for(i in 1:nrow(termFreqTable3)) {
    uni[i] <- termFreqTable2[termFreqTable2$s1==termFreqTable3[i,]$s1 & termFreqTable2$s2==termFreqTable3[i,]$s2,]$freq
}

termFreqTable3$prob <- termFreqTable3$freq/uni

#then quadgrams
uni<-vector(mode="numeric",length=nrow(termFreqTable4))
for(i in 1:nrow(termFreqTable4)) {
    uni[i] <- termFreqTable3[termFreqTable3$s1==termFreqTable4[i,]$s1 & termFreqTable3$s2==termFreqTable4[i,]$s2 & termFreqTable3$s3==termFreqTable4[i,]$s3,]$freq
}

termFreqTable4$prob <- termFreqTable4$freq/uni

## persist ngram models to csv files ###############################################

write.csv(termFreqTable,paste("termFreqTable",sizeLabel,".csv",sep=""), row.names = FALSE)
write.csv(termFreqTable2,paste("termFreqTable2",sizeLabel,".csv",sep=""), row.names = FALSE)
write.csv(termFreqTable3,paste("termFreqTable3",sizeLabel,".csv",sep=""), row.names = FALSE)
write.csv(termFreqTable4,paste("termFreqTable4",sizeLabel,".csv",sep=""), row.names = FALSE)

####################################################################################
####################################################################################

## Create final test set (repeats code above - would be cleaner to weave into

#### sammple different set of elements for testing ####

#use different seed then training data to introduce unseen unigrams
set.seed(39901)
#red<-seq(1,length(Text),by=10)
text<-sample(Text,100000)

#### clean imported text and create corpus  #####

# find non-ascii chars and remove them
text_<-iconv(text, "ISO-8859-1", "ASCII", sub="byte")
textASCII<-gsub("<[0-9|a-z][0-9|a-z]>","",text_)
#test
#grep("<[0-9|a-z][0-9|a-z]>",text_)
#grep("<[0-9|a-z][0-9|a-z]>",textASCII)

# make some space
remove(text,text_)

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

v<-NGramTokenizer(textASCII, Weka_control(min = 4, max = 4,delimiters=" \r\n\t.,;:\"()?!"))
termFreqTableT<-as.data.frame(table(v),stringsAsFactors=FALSE)
names(termFreqTableT)<-c("term","freq")
termFreqTableT<-termFreqTableT[termFreqTableT$freq>9,]
termFreqTableT<-termFreqTableT[order(-termFreqTableT$freq),]

l<-str_extract_all(termFreqTableT$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2","s3","s4")
termFreqTableT<-cbind(termFreqTableT,temp)
remove(temp)

# write final test set to file (sample size params,floor, file name stays constant)
write.csv(termFreqTableT,paste("termFreqTableT_100K_4",".csv",sep=""), row.names = FALSE)

