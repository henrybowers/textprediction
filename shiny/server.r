### Building Data Products - Shiny Project - Predictor

#load libraries
require(stringr)

#load ngram models
termFreqTable <- read.csv("model-data/termFreqTable_unigram_prob_used_10-4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable2 <- read.csv("model-data/termFreqTable2_unigram_prob_used_10-4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable3 <- read.csv("model-data/termFreqTable3_unigram_prob_used_10-4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable4 <- read.csv("model-data/termFreqTable4_unigram_prob_used_10-4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTableT <- read.csv("model-data/termFreqTableT_unigram_prob_used_10-4.csv",stringsAsFactors=FALSE,header=TRUE)

#define helper functions

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



shinyServer( function(input, output) {
                
    output$outputText <-  renderText({
                
        l<-tolower(unlist(str_extract_all(input$inputText,"[^[:blank:]]+")))
        if(length(l)>3) l<-c(l[length(l)-2],l[length(l)-1],l[length(l)])
        
        switch(length(l),
        {
                # predict bigram
                matches<-bi_next_word(termFreqTable2,l[1])
                ifelse(is.na(matches[1]),"**HUH?**",matches[1])
        },
        {
                # predict trigram
                matches<-tri_next_word(termFreqTable3,l[1],l[2])
                if(is.na(matches[1])) matches<-bi_next_word(termFreqTable2,l[2])
                ifelse(is.na(matches[1]),"**HUH?**",matches[1])
        },
        {
                # predict quadgram
                matches<-quad_next_word(termFreqTable4,l[1],l[2],l[3])
                if(is.na(matches[1])) matches<-tri_next_word(termFreqTable3,l[2],l[3])
                if(is.na(matches[1])) matches<-bi_next_word(termFreqTable2,l[3])
                ifelse(is.na(matches[1]),"**HUH?**",matches[1])
        }
        )
                
               
        })
    
    output$parsed <- renderText({
        l<-tolower(unlist(str_extract_all(input$inputText,"[^[:blank:]]+")))
        if(length(l)>3) l<-c(l[length(l)-2],l[length(l)-1],l[length(l)])
        l
    })
                
        }
)
