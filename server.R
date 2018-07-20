#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(gridExtra)  # for neatly arranging plots 
library(wordcloud) 
library(RColorBrewer)

# 1. File upload and Unzipping 
shinyServer(function(input, output) {

#    browser()
    output$zipfiletblfmt <- renderTable({
        if(is.null(input$uploadzipfile)) 
            {return ()}
        input$uploadzipfile })

    
    observeEvent(input$unzip,
        {output$unzipped <- 
        renderTable({unzip(input$uploadzipfile$datapath)}) }
        )

#    observeEvent(input$preview,{    
#        fileReadFun <- reactive({
#
#            my_wd <- getwd()
#            fileForAnalysisName <- paste0(my_wd,"/",input$uploadzipfile,".txt")
#            fileCon <- file(description = fileForAnalysisName, open = "r",blocking = FALSE, method = "internal")
#            fileTxt <-  readLines(con = fileCon)
#            fileTxt
#            return(renderText(fileTxt))
#            output$fileForAnalysis <- return(renderText(fileTxt))
#            close(con = fileCon)
#        })
#        output$fileForAnalysis <- renderTable(fileReadFun())
#    })
    observeEvent(input$preview,{    
        fileReadFun <- reactive({
            my_wd <- getwd()
            fileForAnalysisName <- paste0(my_wd,"/",input$uploadzipfile,".txt")
            fileCon <- file(description = fileForAnalysisName, open = "r",blocking = FALSE, method = "internal")
#           fileTxt <-  readLines(con = fileCon)
            fileTxt <- read.table(con = fileCon)
            fileTxt
#            return(renderText(fileTxt))
#            output$fileForAnalysis <- return(renderText(fileTxt))
            close(con = fileCon)
            
#            fileForAnalysisName <- input$uploadzipfile
#            if (is.null(fileForAnalysisName))
#                {return(NULL)}
#            read.table(fileForAnalysisName$datapath)
#            source("NLP.R")
        })
        output$fileForAnalysis <- renderTable(fileReadFun())
    })
    
    
    output$predictedWords <- renderText({ 
        input$my_terms})


#    print(d)
    
    wordFreqReactive <- reactive({
        input$wordFreq
    })
    
    output$myBarPlot <- renderPlot({
        # to plot word frequencies
         barplot(d[1:wordFreqReactive(),]$freq, las = 2, names.arg = d[1:wordFreqReactive(),]$word,
                             col ="lightgreen", main ="Most frequent words",
                             ylab = "Word frequencies") })
        
        #        print(myBarPlot)
    output$myTable <-renderTable({
#  to display word frequency table **************************************************
        head(d,wordFreqReactive()) })
#       head(d,input$wordFreq) })
#  to display the word cloud ***********************************************
    output$myWordCloud <- renderPlot({
        withProgress({setProgress(message = "Generating word cloud..") 
            set.seed(03072018)
            wordcloud(words = d$word, freq = d$freq, min.freq = 30,
                      max.words=wordFreqReactive(), random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))
            
    }) })
})
