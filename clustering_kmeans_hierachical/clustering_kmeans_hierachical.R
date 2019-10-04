#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Explain somewhere whats happening and what the tool is doing!!

library(shiny)
# Generate data as 3 separated groups
x1 <- 80+rnorm(20, mean=20, sd=10)
  y1 <- 80+rnorm(20, mean=20, sd=10)
  x2 <- 80+rnorm(20, mean=70, sd=10)
  y2 <- 80+rnorm(20, mean=20, sd=10)
  x3 <- 80+rnorm(20, mean=40, sd=10)
  y3 <- 80+rnorm(20, mean=120, sd=10)

# Merge them all in a single dataset
  x <- c(x1,x2,x3)
  y <- c(y1,y2,y3)
  i <- seq(1, length(y))
  data_def <- data.frame(i,x,y)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("K Mean Clustering - 2Dim"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        fileInput("file1", "Choose local CSV",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
        ),
       textInput("file_online", "Choose online CSV", "https://people.sc.fsu.edu/~jburkardt/data/csv/faithful.csv"),
       
       radioButtons("dataset", "Dataset:",
               c("Default" = "default",
                 "Local" = "local",
                 "Online" = "online")),
      tags$hr(),
      sliderInput("k",
                        "Number of clusters:",
                        min = 1,
                        max = 10,
                        value = 3)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
        output$distPlot <- renderPlot({
        # Choose Dataset (local, online, default, generate)
        inFile <- input$file1
        data <- switch(input$dataset,
               default = data_def,
               local = read.csv(file=inFile$datapath, header=TRUE, sep=","),
               online = read.csv(url(input$file_online), header=TRUE, sep=","),
               data_def)  
          
        
        #if (!is.null(inFile))
        #  data <- read.csv(file=inFile$datapath, header=TRUE, sep=",")
        
        #data prep, only numeric
        data <- data[ , purrr::map_lgl(data, is.numeric)]
        data <- data[2:3]
        # Run K-means
        km.out <- kmeans(data,input$k,nstart =50)
        plot (data, col =(km.out$cluster+1),
        main ="K-Means Clustering" ,
        xlab =names(data)[1] , ylab =names(data)[2] ,
        pch =20 , cex =2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
