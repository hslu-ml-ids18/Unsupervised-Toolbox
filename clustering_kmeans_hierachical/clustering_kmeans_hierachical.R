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
  h1("Machine Learning 2 - Unsupervised Toolbox"),
  "by",
  strong("Giuliano Ardesi, Lisa Becker, Anastasiia Chebatarova, Axel Kandel, Alexander Kushe"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        fileInput("file_local", "Choose local CSV",
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
       
       uiOutput("selected_input_x_col"),
       uiOutput("selected_input_y_col"),
       

      sliderInput("k",
                        "Number of clusters:",
                        min = 1,
                        max = 20,
                        value = 3),
      sliderInput("tree_h",
                  "Height of tree:",
                  min = 1,
                  max = 20,
                  value = 3)
        ),
      

      
        # Show a plot of the generated distribution
        mainPanel(
          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs",
                      tabPanel("DataSet", br(), verbatimTextOutput("summary"), verbatimTextOutput("strucutre"), tableOutput("view")),
                      tabPanel("K-means", plotOutput("k_cluster"), plotOutput("k_cluster_total") ),
                      tabPanel("Heatmap", plotOutput("heatmap")),
                      tabPanel("Tree", plotOutput("tree"))
          )

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
          # Return the requested dataset ----
          datasetInput <- reactive({
            switch(input$dataset,
                   local = read.csv(file=input$file_local$datapath, header=TRUE, sep=","),
                   online = read.csv(url(input$file_online), header=TRUE, sep=","),
                   data_def)
          })
          
          selectedData <- reactive({
            datasetInput()[, c(input$xcol, input$ycol)]
          })
          
          #output$selected_input_xclol <- renderText({names(datasetInput())})
          ### This will create the dynamic dropdown list ###
          

          
          output$selected_input_x_col <- renderUI({
            selectInput('xcol', 'X Variable', names(datasetInput()))
          })
          output$selected_input_y_col <- renderUI({
            selectInput('ycol', 'Y Variable', names(datasetInput()), selected=names(datasetInput())[[2]])
          })
          
          output$heatmap <- renderPlot({
            data_scaled <- as.matrix(scale(datasetInput()))
            heatmap(data_scaled,
                    col = topo.colors(200, alpha=0.5),
                    Colv=F, scale="none")
          })
          
          output$tree <- renderPlot({
            data_scaled <- as.matrix(scale(datasetInput()))
            hc1= hclust(dist(data_scaled))
            cutree(hc1, k = 3)
            plot(hc1)
            
          })
  
         output$k_cluster <- renderPlot({
          
          # read local, online or defautl dataset
          data <- datasetInput()
          
          # Data prep, only numeric
          data <- data[ , purrr::map_lgl(data, is.numeric)]
          
          # Choose column 2 and 3, skipping column 1, assuming its an index.
          data <- selectedData()
    
          # Run K-means Plot
          km.out <- kmeans(data,input$k,nstart =50)
          plot (data, col =(km.out$cluster+1),
          main ="K-Means Clustering" ,
          xlab =names(data)[1] , ylab =names(data)[2] ,
          pch =20 , cex =2)
          points(km.out$centers, pch = 4, cex = 4, lwd = 4)
      
        })
        
        output$k_cluster_total <- renderPlot({    
          #read dataset
          data <- datasetInput()
          
          # Run Total within-cluster sum of squares Plot
          List_y <- list()
          List_x <- list()
          for (k in seq(1,input$k)){
            km.out <- kmeans(data,k,nstart =50)
            List_y[k] <- km.out$tot.withinss
            List_x[k] <- k
          }
          plot(List_x, List_y,
          typ = "b",
          xlab ="Number of Clusters", 
          ylab ="Total within-cluster sum of squares")
        })
        
        # Generate a summary of the dataset
        output$summary <- renderPrint({
          data <- datasetInput()
          summary(data)
        })
        
        # Show the first "n" observations
        #output$view <- renderTable({
        #  input$show_all = "head"
        #})
        #  head(datasetInput())
        
        # Return the requested dataset ----
        output$view <- renderTable({
          head(datasetInput())
        })

        
        # Show the first "n" observations
        output$strucutre <- renderPrint({
          str(datasetInput())
        })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
