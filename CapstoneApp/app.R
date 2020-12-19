
library(shiny)
library(data.table)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Next Word Predictor"),
    h4("Coursera Capstone Project"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "usr.input",
                        label = "Enter your Text",
                        width = 500),
            textOutput(outputId =  "ldng.indicator"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("prdct.first")
        )
    )
)

# Prediction Logic
server <- function(input, output, session) {

    prdct.word <- function(str.input, dt.1gram, dt.2gram, dt.3gram, dt.4gram){
        ## Core Prediction Function
        ## uses very simple back-off function, i.e. if no n-gram is found use a (n-1)-gram etc
        
        prdct.4gram <- function(splt.input, dt.4gram){
            l.input <- length(splt.input)
            return(dt.4gram[first == splt.input[l.input-2] & second == splt.input[l.input-1] & third == splt.input[l.input]])
        }
        
        prdct.3gram <- function(splt.input, dt.3gram){
            l.input <- length(splt.input)
            return(dt.3gram[first == splt.input[l.input-1] & second == splt.input[l.input]])
        }
        
        prdct.2gram <- function(splt.input, dt.2gram){
            l.input <- length(splt.input)
            return(dt.2gram[first == splt.input[l.input]])
        }
        prdct.1gram <- function(splt.input, dt.1gram){
            return(dt.1gram[1:10])
        }
        
        
        splt.input <- unlist(strsplit(str.input, " "))
        print(splt.input)
        l.input <- length(splt.input)
        print(l.input)
        
        res <- data.table()
        suc <- TRUE
        
        if (l.input >= 3){
            res <- prdct.4gram(splt.input, dt.4gram)
            if (nrow(res) == 0){suc <- FALSE}
            else {suc <- TRUE}
        }
        if (l.input == 2 | suc == FALSE){
            res <- prdct.3gram(splt.input, dt.3gram)
            if (nrow(res) == 0){suc <- FALSE}
            else {suc <- TRUE}
        }
        if (l.input == 1 | suc == FALSE){
            res <- prdct.2gram(splt.input, dt.2gram)
            if (nrow(res) == 0){suc <- FALSE}
            else {suc <- TRUE}
        }
        if (l.input == 0 | suc == FALSE){
            res <- prdct.1gram(splt.input, dt.1gram)
        }
        
        return(res)
        
    }
    
    ## load data for word prediction:
    output$ldng.indicator <- renderText("Please Wait! -- Fetching Data (0/4) done..")
    df.1gram <- readRDS("df.1gram.rds")
    output$ldng.indicator <- renderText("Please Wait! -- Fetching Data (1/4) done..")
    df.2gram <- readRDS("df.2gram.rds")
    output$ldng.indicator <- renderText("Please Wait! -- Fetching Data (2/4) done..")
    df.3gram <- readRDS("df.3gram.rds")
    output$ldng.indicator <- renderText("Please Wait! -- Fetching Data (3/4) done..")
    df.4gram <- readRDS("df.4gram.rds")
    output$ldng.indicator <- renderText("Ready!")
    
    # i <- 1
    # output$ldng.indicator <- renderText({
    #     if (i == 1){
    #         paste("Please Wait! -- Fetching Data (0/4) done..")
    #         df.1gram <- readRDS("df.1gram.rds")
    #         i <- i+1
    #     }
    #     if (i == 2){
    #         ##paste("Please Wait! -- Fetching Data (1/4) done..")
    #         df.2gram <- readRDS("df.2gram.rds")
    #         i <- i+1
    #     }
    #     if (i == 3){
    #         ##paste("Please Wait! -- Fetching Data (2/4) done..")
    #         df.3gram <- readRDS("df.3gram.rds")
    #         i <- i+1
    #     }
    #     if (i == 4){
    #         ##paste("Please Wait! -- Fetching Data (3/4) done..")
    #         df.4gram <- readRDS("df.4gram.rds")
    #         i <- i+1
    #     }
    #     if (i == 5){
    #         ##paste("READY!")
    #     }
    # })
    
    re.prdct <- reactive({prdct.word(input$usr.input, df.1gram, df.2gram, df.3gram, df.4gram)})
    
    output$prdct.first <- renderDataTable({
        ## predict the next word
        re.prdct()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
