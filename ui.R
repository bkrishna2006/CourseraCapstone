#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

#shinyUI(fillPage(
    # Application title
    titlePanel("Swiftkey-JHU Word Prediction Application"),
    mainPanel(
        tabsetPanel(type = "tabs",
                   tabPanel(title = "1. File Upload ", 
                             fileInput("uploadzipfile","Select the zip file :", multiple = F, accept = ".zip"),
                             tableOutput("zipfiletblfmt"), # to display the details of the uploaded unzipped file
                             actionButton("unzip", "Unzip the file"),   # to unzip the file
                             tableOutput("unzipped"), # to display the path and name of the unzipped file 
                             actionButton("preview", "Preview the contents"),
                             verbatimTextOutput("fileForAnalysis"),
                             actionButton("analyze", "Analyze")
                    ),
                    tabPanel(title = "2. Text Prediction", 
                             textInput("my_terms","Enter the text here: ", width = '400px', placeholder="your text here.."),
                             verbatimTextOutput("predictedWords")
#                             plotOutput("myBarPlot"),
#                             plotOutput("myWordCloud"),
#                             tableOutput("myTable")
                    ),
                    tabPanel(title = "3. Word Freq. Analysis",
                             h3("List of most frequently used words"),
                             sliderInput("wordFreq","Select the # of words to display:", 
                                         min = 3, max = 10, value = 5 ),
#                             reactivePlot("myTable"),
                             tableOutput("myTable")
#                             plotOutput("myBarPlot")
                    ),
                    tabPanel(title = "4. Word Cloud", 
#                             sliderInput("wordCloudFreq","Select the # of words to display:", 
#                                         min = 3, max = 50, value = 25 ),
                             plotOutput("myWordCloud")
                    )
        )
    ))
)

