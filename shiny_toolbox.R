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
      sliderInput("Perplexity",
                  "Number of Perplexitys:",
                  min = 2,
                  max = 100,
                  value = 10),
      sliderInput("Epsilon",
                  "Number of Epsilons:",
                  min = 2,
                  max = 100,
                  value = 5),
      sliderInput("Iteration",
                  "Number of Iterations:",
                  min = 2,
                  max = 20,
                  value = 5),
      headerPanel('k-means clustering'),
      sidebarPanel(
        selectInput('xcol', 'X Variable', names(data)),
        selectInput('ycol', 'Y Variable', names(data),
                    selected=names(data)[[2]]),
        numericInput('clusters', 'Cluster count', 3,
                     min = 1, max = 9)
      ),
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
      )
          ),
    
     # Show a plot of the generated distribution
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("DataSet", br(), verbatimTextOutput("summary"), verbatimTextOutput("strucutre"), tableOutput("view")),
                  tabPanel("PCA", plotOutput("pcaplot", width = "1200px"), plotOutput("pca_variance_plot", width = "1200px")),
                  tabPanel("t-SNE", plotOutput("tsne_plot", height = "800px")),
                  tabPanel("K-means", plotOutput("k_cluster"), plotOutput("k_cluster_total") ),
                  tabPanel("Absolutely-positioned panel", plotOutput("heatmap", height = "800px", width = "1200px")),
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
  
  
  
  
  rownames(data) = raw[,1]
  
  # Get mean and variance in every column of the dataset
  apply(data,2,mean)
  apply(data,2,var)
  
  ### Principal Component Analysis
  
  output$pca_variance_plot <- renderPlot({
  
  #Computing PCA
  scaled_data = as.matrix(scale(data))
  data.prc <- prcomp(scaled_data)
  names(data.prc)
  
  #Standard deviation and means of the variables that were used for scaling prior to implementing PCA:
  data.prc$center
  data.prc$scale
  
  
  # Rotation matrix provides the principal component of the loadings.
  dim(data.prc$rotation)
  data.prc$rotation
  # x matrix provides the principal component of the scores.
  dim(data.prc$x)
  data.prc$x
  

  
  #Get standard deviation and variance of PCA
  data.prc$sdev
  data.prc_var = data.prc$sdev^2
  data.prc_var
  
  # In order to compute the proportion of variance explained by each principal component (variance explained 
  #by each principal component / total variance explained by all four principal components)
  pve=data.prc_var/sum(data.prc_var)
  pve
  plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')
  pca_variance_plot <- plot(cumsum(pve),xlab="PrincipalComponent",ylab="Cumulative Proportion of Variance
Explained",ylim=c(0,1),type='b')
  })
  
  #Biplot
  output$pcaplot <- renderPlot({
    biplot(data.prc, scale=0)})
    
  
  
  # Create a tsna plot of the dataset
  output$tsne_plot <- renderPlot({
    # Define a function to run tsna with input variables
    ###t-Distributed Stochastic Neighbor Embedding (tSNE)
    library(Rtsne)
    # Use table row names to label the datapoint later in the plot:
    data_label<-as.factor(rownames(data))
    
    #remove duplicates:
    data_unique <- unique(data)
    
    #  Run tSNE:
    tSNEdata <- as.matrix(scale(data_unique))
    tsne <- Rtsne(tSNEdata, dims = 2,
                  perplexity= input$Perplexity, verbose=TRUE,
                  max_iter = input$Iteration)
    plot(tsne$Y)
    text(tsne$Y, labels=data_label) })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

