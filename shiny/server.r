### Building Data Products - Shiny Project - Predictor

#load libraries
require(stringr)

#load ngram models
termFreqTable <- read.csv("model-data/termFreqTable_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable2 <- read.csv("model-data/termFreqTable2_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable3 <- read.csv("model-data/termFreqTable3_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)
termFreqTable4 <- read.csv("model-data/termFreqTable4_50K_4.csv",stringsAsFactors=FALSE,header=TRUE)


#define helper functions

quad_next_word <- function(markov,l1,l2,l3) {
        matches<-markov[markov$s1==l1 & markov$s2==l2 & markov$s3==l3,]
        matches<-matches[order(-matches$prob),]
}


tri_next_word <- function(markov,l1,l2) {
        matches<-markov[markov$s1==l1 & markov$s2==l2,]
        matches<-matches[order(-matches$prob),]
}


bi_next_word <- function(markov,l1) {
        matches<-markov[markov$s1==l1,]
        matches<-matches[order(-matches$prob),]
}


shinyServer( function(input, output) {


    matches <- reactive ({
        l<-tolower(unlist(str_extract_all(input$inputText,"[^[:blank:]]+")))
        if(length(l)>3) l<-c(l[length(l)-2],l[length(l)-1],l[length(l)])
        
        switch(length(l),
            {
                # predict bigram
                temp<-bi_next_word(termFreqTable2,l[1])
                temp
            },
            {
                # predict trigram
                temp<-tri_next_word(termFreqTable3,l[1],l[2])
                if(!nrow(temp))temp<-bi_next_word(termFreqTable2,l[2])
                temp
            },
            {
                # predict quadgram
                temp<-quad_next_word(termFreqTable4,l[1],l[2],l[3])
                if(!nrow(temp)) temp<-tri_next_word(termFreqTable3,l[2],l[3])
                if(!nrow(temp)) temp<-bi_next_word(termFreqTable2,l[3])
                temp
            }
        )       

    })
    
    output$outputText <-  renderText({
        temp<-matches()
        s<-temp[1,]$term
        ifelse(!is.na(s),str_extract(s,"[^[:blank:]]+$"),"***HUH?***")
    })
    
    output$parsed <- renderText({
        l<-tolower(unlist(str_extract_all(input$inputText,"[^[:blank:]]+")))
        if(length(l)>3) l<-c(l[length(l)-2],l[length(l)-1],l[length(l)])
        l
        })
    
    output$goodMatches <- renderTable({
        head(matches()[-1,])
        })
                
    }
)
