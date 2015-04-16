### prediction model - Capstone - 
### References ###
# sgt-gale.pdf

require(stringr)
require(caret)

#setwd()
setwd("~/R-projects/Capstone/textprediction")

#termFreqTable <- read.csv("model-data/termFreqTable.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable2 <- read.csv("model-data/termFreqTable2.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable3 <- read.csv("model-data/termFreqTable3.csv",stringsAsFactors=FALSE,header=TRUE)
#termFreqTableN <- read.csv("model-data/termFreqTableN.csv",stringsAsFactors=FALSE,header=TRUE)
#termFreqTableT <- read.csv("model-data/termFreqTableT.csv",stringsAsFactors=FALSE,header=TRUE)

#decompose ngrams for faster response time
l<-str_extract_all(termFreqTable3$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2","s3")
termFreqTable3<-cbind(termFreqTable3,temp)
remove(temp)

l<-str_extract_all(termFreqTableT$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2","s3")
termFreqTableT<-cbind(termFreqTableT,temp)
remove(temp)


l<-str_extract_all(termFreqTable2$term,"[^[:blank:]]+")
temp<-as.data.frame(do.call(rbind,l),stringsAsFactors = FALSE)
names(temp)<-c("s1","s2")
termFreqTable2<-cbind(termFreqTable2,temp)
remove(temp)



tri_next_word <- function(markov,l1,l2) {
  matches<-markov[markov$s1==l1 & markov$s2==l2,]
  matches<-matches[order(-matches$freq),]
  matches$s3
}


bi_next_word <- function(markov,l1) {
  matches<-markov[markov$s1==l1,]
  matches<-matches[order(-matches$freq),]
  matches$s2
}

#kernel
w<-"my big"
l<-tolower(unlist(str_extract_all(w,"[^[:blank:]]+")))
system.time({
matches<-tri_next_word(termFreqTable3,l[1],l[2])
if(is.na(matches[1])) matches<-bi_next_word(termFreqTable2,l[2])
})

bi_next_word(termFreqTable2,l[2])

#test model
predResults<-vector(mode = "character",length = nrow(termFreqTableT))
triFault<-vector(length=nrow(termFreqTableT))
for(i in 1:nrow(termFreqTableT)) {
  matches<-tri_next_word(termFreqTable3,termFreqTableT[i,]$s1,termFreqTableT[i,]$s2)
  triFault[i]<-ifelse(is.na(matches[1]),1,0)
  if(is.na(matches[1])) matches<-bi_next_word(termFreqTable2,termFreqTableT[i,]$s2)
  predResults[i]<-matches[1]
}

perf<-table(predResults==termFreqTableT$s3)
termFreqTableT[which(predResults!=termFreqTableT$s3),]
