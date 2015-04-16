### prediction model - Capstone - 
### References ###
# sgt-gale.pdf

require(stringr)
require(caret)

#setwd()
setwd("~/R-projects/Capstone/textprediction")

termFreqTable <- read.csv("model-data/termFreqTable.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable2 <- read.csv("model-data/termFreqTable2.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable3 <- read.csv("model-data/termFreqTable3.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTableN <- read.csv("model-data/termFreqTableN.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTableT <- read.csv("model-data/termFreqTableT.csv",stringsAsFactors=FALSE,header=TRUE)


#N <- 0
#freq<-vector(mode="integer",length=max(termFreqTable$freq))

#for (i in 1:length(freq)) {
#  freq[i] <- nrow(subset(termFreqTable,freq==i))
#  N <- N + i*freq[i]
#}

## trade off observed n-grams against memory size, response time and perplexit
#termFreqTable3<-termFreqTable3[1:100,]
#termFreqTable2<-termFreqTable2[1:100,]


N <- sum(termFreqTable$freq) ### count of words observed
V <- nrow(termFreqTable)     ### number of unique words 


Pt <- vector(mode = "double",length=V)
Pb <- vector(mode = "double",length=V)

inputString <- "without a"

l<-tolower(unlist(str_extract_all(inputString,"[^[:blank:]]+")))
Bigram <- paste(l[1],l[2])
Unigram <- l[2]
cBigram <- termFreqTable2[termFreqTable2$term==Bigram,]$freq
cUnigram <- termFreqTable[termFreqTable$term==Unigram,]$freq

for (i in 1:V) {
  Trigram <- paste(Bigram,termFreqTable[i,]$term)
  cTrigram <- termFreqTable3[termFreqTable3$term==Trigram,]$freq
  Pt[i]<- ifelse(length(cTrigram),cTrigram/cBigram,0)
}

for (i in 1:V) {
  Bigram <- paste(Unigram,termFreqTable[i,]$term)
  cBigram <- termFreqTable2[termFreqTable2$term==Bigram,]$freq
  Pb[i]<- ifelse(length(cBigram),cBigram/cUnigram,0)
}

if(max(Pt)) {
  mostLikely <- which(Pt==max(Pt))
  predicted <- paste(l[1],l[2],termFreqTable[mostLikely,]$term)
  Tbest <- ifelse(length(predicted) > 1, predicted[sample(1:length(predicted),1)],predicted[1])
  Tbest
}

if(max(Pb)) {
  mostLikely <- which(Pb==max(Pb))
  predicted <- paste(l[1],l[2],termFreqTable[mostLikely,]$term)
  Bbest <- ifelse(length(predicted) > 1, predicted[sample(1:length(predicted),1)],predicted[1])
  Bbest
}

if(max(Pt)|max(Pb)) {
  P <- Pt + Pb
  mostLikely <- which(P==max(P))
  predicted <- paste(l[1],l[2],termFreqTable[mostLikely,]$term)
  Pbest <- ifelse(length(predicted) > 1, predicted[sample(1:length(predicted),1)],predicted[1])
  Pbest
  
}


# Create train and test sets #################3

set.seed(7685)

#set up cross-validation sets - split training set into training and testing
trainIndex <- createResample(termFreqTable3,times=nrow(termFreqTable3),list=FALSE)
training <- termFreqTable3[trainIndex[,1]==1,]
testing <- termFreqTable3[trainIndex[,2]==2,]






######## IN CASE I NEED IT ################################################
#set cross-validation method to use during training
ctrl <- trainControl(method = "repeatedcv", repeats = 5)

#### build random forest ####
set.seed(1599)
modfit <- train(classe ~ . ,method="rf",data=training,trControl = ctrl)
varImpPlot(modfit$finalModel)

#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Random Forest - Class Error ", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#predict on test
predResults<- predict(modfit,testing)
confusionMatrix(predResults,testing$classe)

### build decision tree ###
set.seed(1599)
modfit <- train(classe ~ . ,method="rpart",data=training,trControl = ctrl)

#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Classification Tree", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#predict on testing
predResults<- predict(modfit,testing)
confusionMatrix(predResults,testing$classe)
