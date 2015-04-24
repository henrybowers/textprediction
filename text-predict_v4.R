### prediction model - Capstone - 

require(stringr)

#setwd()
#setwd("~/R-projects/Capstone/textprediction")
setwd("~/Projects/Capstone/textprediction")


termFreqTable <- read.csv("model-data/termFreqTable_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable2 <- read.csv("model-data/termFreqTable2_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable3 <- read.csv("model-data/termFreqTable3_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable4 <- read.csv("model-data/termFreqTable4_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTableT <- read.csv("model-data/termFreqTableT_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)


#save tableframes for fast import later
#save(termFreqTable,"termFreqTable",file="termFreqTable.RData")
#save(termFreqTable2,"termFreqTable2",file="termFreqTable2.RData")
#save(termFreqTable3,"termFreqTable3",file="termFreqTable3.RData")
#save(termFreqTable4,"termFreqTable4",file="termFreqTable4.RData")


quad_next_word <- function(markov,l1,l2,l3) {
        matches<-markov[markov$s1==l1 & markov$s2==l2 & markov$s3==l3,]
        matches<-matches[order(-matches$prob),]
        matches$s4
}


tri_next_word <- function(markov,l1,l2) {
  matches<-markov[markov$s1==l1 & markov$s2==l2,]
  matches<-matches[order(-matches$prob),]
  matches$s3
}


bi_next_word <- function(markov,l1) {
  matches<-markov[markov$s1==l1,]
  matches<-matches[order(-matches$prob),]
  matches$s2
}

#lever to adjust freq floor in test data set
#termFreqTableT<-termFreqTableT[termFreqTableT$freq>5,]


#test model
predResults<-vector(mode = "character",length = nrow(termFreqTableT))
quadFault<-vector(length=nrow(termFreqTableT))
for(i in 1:nrow(termFreqTableT)) {
        
  matches<-quad_next_word(termFreqTable4,termFreqTableT[i,]$s1,termFreqTableT[i,]$s2,termFreqTableT[i,]$s3)

  quadFault[i]<-ifelse(is.na(matches[1]),1,0)
  
  if(is.na(matches[1])) matches<-tri_next_word(termFreqTable3,termFreqTableT[i,]$s2,termFreqTableT[i,]$s3)
  
  if(is.na(matches[1])) matches<-bi_next_word(termFreqTable2,termFreqTableT[i,]$s3)
  
  predResults[i]<-ifelse(is.na(matches[1]),"NO MATCH",matches[1])
}

perf<-table(predResults==termFreqTableT$s4)
paste("Accuracy =",round(perf[2]/(perf[1]+perf[2]),digits = 2))
paste("Unknown quadgrams =",sum(quadFault))
head(termFreqTableT[which(predResults!=termFreqTableT$s4),])
head(predResults[which(predResults!=termFreqTableT$s4)])

head(termFreqTableT[which(quadFault>0),])

testResults<-cbind(termFreqTableT,predResults)

