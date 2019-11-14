#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Prep work ==================================================================

# Required packages
library(shiny)
library(kohonen)
library(ggplot2)
library(Rtsne)

# Generate data as 3 separated groups

x1 <- 80 + rnorm(20, mean = 20, sd = 10)
y1 <- 80 + rnorm(20, mean = 20, sd = 10)
z1 <- 85 + rnorm(20, mean = 25, sd = 20)
x2 <- 80 + rnorm(20, mean = 70, sd = 10)
y2 <- 80 + rnorm(20, mean = 20, sd = 10)
z2 <- 60 + rnorm(20, mean = 55, sd = 15)
x3 <- 80 + rnorm(20, mean = 40, sd = 10)
y3 <- 80 + rnorm(20, mean = 120, sd = 10)
z3 <- 70 + rnorm(20, mean = 75, sd = 5)

# Merge them all in a single dataset
x <- c(x1, x2, x3)
y <- c(y1, y2, y3)
z <- c(z1, z2, z3)
i <- seq(1, length(y))
data_def <- data.frame(x, y, z)

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual'),
                      glue = NULL) {

  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      #  href = "#", class = "tip", `data-toggle` = "popover",
      href = "#",
      class = "btn btn-default",
      `data-toggle` = "popover",
      title = title,
      `data-content` = content,
      # added this parameter
      `data-html` = TRUE,
      #
      `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      glue,
      icon("question")
    ),
    # CB added for popup width control
    tags$style(type='text/css', ".popover { width: 400px; relative; left: 320px !important; }")
    # end add
  )
}


# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lato:700&display=swap');
                    body {background-color: #c2d6d6;
                    }
                    h1 {
                    font-family: 'Lato', sans-serif;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #50;
                    }
                    strong {
                    font-family: Helvetica, Arial;
                    font-weight: bold;
                    color: #50;
                    }

                    "))
    ),
  # Format the application title
  h1("Machine Learning 2 - Unsupervised Toolbox"),
  "by",
  strong("Giuliano Ardesi, Lisa Becker, Anastasiia Chebatarova, Axel Kandel, Alexander Kusche"),

  # Create sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabs==1",
                       helpPopup("Import a data file",
                                 "
                                 You can load a coma separated file (CSV) to be used in this Shiny App.
                                 You can either load a file from your local machine or
                                 by providing a direct link to your file. You will then
                                 have to select which 'Dataset Source' to use, by default the Old Faithful Geyser Data
                                 https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/faithful.html is used to populate this Shiny App.
                                 Finally you can define the separator used in your CSV file, by default it is set to a comma ','
                                 ",
                                 placement='bottom',
                                 trigger='click',
                                 glue = "About this tab"
                       ),
                       tags$hr(),

                       fileInput("file_local",
                                 "Choose local CSV",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                       ),
                       textInput("file_online",
                                 "Choose online CSV",
                                 "https://people.sc.fsu.edu/~jburkardt/data/csv/faithful.csv"),

                       radioButtons("dataset",
                                    "Dataset Source:",
                                    c("Default" = "default",
                                      "Local" = "local",
                                      "Online" = "online")),

                       textInput(inputId = "separator",
                                 label = "Choose a separator for your file",
                                 value = ","),

                       tags$hr()),
      conditionalPanel(condition = "input.tabs==2",
                       helpPopup("PCA",
                                 paste("
                                 What is it?
                                 It is an unsupervised, linear, non-parametric method
                                 When to use it?
                                 PCA is used first in exploration of multidimensional data.  
                                       "),
                                 placement='bottom',
                                 trigger='click',
                                 glue = "About this tab"
                       )
      ),
      conditionalPanel(condition = "input.tabs==3",
                       helpPopup("T-SNE",
                                 paste("
                                 What is it?
                                 T-SNE is an ansupervised, non-linear, parametric
                                 When to use it?
                                 Exploration & visualization of data, well-suited for
                                 high-dimensional data. 
                                 method for dimensionality reduction.
                                 You can drag the slide inputs to increase and decrease the values
                                 for: Perplexity, Epsilons, Iterations
                                       "),
                                 placement='bottom',
                                 trigger='click',
                                 glue = "About this tab"
                       ),
                       tags$hr(),
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

                       tags$hr()),
      conditionalPanel(condition = "input.tabs==4",
                       helpPopup("K-means",
                                 "
                                 What is it?
                                 K-means is an ansupervised, parametric method (need to pre-specify K number of clusters).
                                 When to use it?
                                 First exploration of multidimensional data (few assumptions needed, i.e., K)
                                 In this tab you can select which variable should be used for
                                 the Y and X axis in K-means clustering. You can also select
                                 into how many clusters you want to split your data into.
                                 The 'X' in the graph represent the center of each cluster.
                                 ",
                                 placement='bottom',
                                 trigger='click',
                                 glue = "About this tab"
                       ),
                       tags$hr(),
                       uiOutput("selected_input_x_col"),
                       uiOutput("selected_input_y_col"),

                       sliderInput("k",
                                   "Number of clusters:",
                                   min = 1,
                                   max = 20,
                                   value = 3)),
      conditionalPanel(condition = "input.tabs==5",
                       helpPopup("Heatmap",
                                 "
                                 What is it?
                                 Unsupervised, non-parametric method (no need labelled data)
                                 'Better' than K-means clustering, no need to specify K number of clusters a priori (goes through all K’s)
                                 When to use it?
                                 First exploration of multidimensional data (no assumptions needed).!
                                ",
                                 placement='bottom',
                                 trigger='click',
                                 glue = "About this tab"
                       )
      ),
      conditionalPanel(condition = "input.tabs==6",
                       helpPopup("SOM - Self-Organizing Maps",
                                 "
                                  What is it?
                                  Unsupervised, nonlinear, parametric method
                                  Type of artificial neural network
                                  Somewhat similar to K-means (SOMs with a small number of nodes behave similar to K-means) Somewhat similar to PCA (can be considered a nonlinear generalization of PCA)
                                  When to use it?
                                  For data visualization of high-dimensional data
                                 ",
                                 placement='bottom',
                                 trigger='click',
                                 glue = "About this tab"
                       )),
      conditionalPanel(condition = "input.tabs==7",
                       helpPopup("Tree",
                                 "
                                 What is it?
                                 Unsupervised, non-parametric method (no need labelled data)
                                 'Better' than K-means clustering, no need to specify K number of clusters a priori (goes through all K’s)
                                 When to use it?
                                 First exploration of multidimensional data (no assumptions needed).
                                   ",
                                 placement='bottom',
                                 trigger='click',
                                 glue = "About this tab"
                       ),
                       tags$hr(),
                       sliderInput("tree_h",
                                   "Height of tree:",
                                   min = 1,
                                   max = 20,
                                   value = 3),
                       sliderInput("tree_k",
                                   "Cut of tree:",
                                   min = 1,
                                   max = 20,
                                   value = 3)
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # Output: Tabset w/ plot, summary, and table
      tabsetPanel(type = "tabs", id = "tabs",
                  tabPanel("DataSet", value=1, br(), verbatimTextOutput("summary"), verbatimTextOutput("strucutre"), verbatimTextOutput("view_table"), tableOutput("view")),
                  tabPanel("PCA", plotOutput("pcaplot"), value=2, plotOutput("pca_variance_plot")),
                  tabPanel("t-SNE", value=3 , plotOutput("tsne_plot")),
                  tabPanel("K-Means", value=4, plotOutput("k_cluster"), plotOutput("k_cluster_total") ),
                  tabPanel("HC-Heatmap", value=5, plotOutput("heatmap")),
                  tabPanel("HC-Tree", value=7, plotOutput("tree"), verbatimTextOutput("tree_cut")),
                  tabPanel("SOM", value = 6, plotOutput("som"), plotOutput("som_cluster"))
      )

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Return the requested dataset
  datasetInput <- reactive({
    switch(
      input$dataset,
      local = read.csv(
        file = input$file_local$datapath,
        header = TRUE,
        sep = input$separator
      ),
      online = read.csv(
        url(input$file_online),
        header = TRUE,
        sep = input$separator
      ),
      data_def
    )
  })

  selectedData <- reactive({
    datasetInput()[, c(input$xcol, input$ycol)]
  })

  
  
  # Create a dynamic dropdown list
  output$selected_input_x_col <- renderUI({
    selectInput('xcol', 'X Variable', names(datasetInput()))
  })
  output$selected_input_y_col <- renderUI({
    selectInput('ycol',
                'Y Variable',
                names(datasetInput()),
                selected = names(datasetInput())[[2]])
  })

# Heatmap (Hirarchical Clustering) ==================================================================
  output$heatmap <- renderPlot({

    data <- datasetInput()

    # Keep only the numerical data
    data <- data[, purrr::map_lgl(data, is.numeric)]
    
    data_scaled <- as.matrix(scale(data))
    heatmap(
      data_scaled,
      col = topo.colors(200, alpha = 0.5),
      Colv = F,
      scale = "none",
      main = "Heatmap"
    )
  })

# Tree (Hirarchical Clustering) ==================================================================  
  output$tree <- renderPlot({

    data <- datasetInput()

    # Keep only the numerical data
    data <- data[ , purrr::map_lgl(data, is.numeric)]

    data_scaled <- scale(data)
    hc1= hclust(dist(data_scaled))


    plot(hc1)

  })

  # Generate a summary of the dataset
  output$tree_cut <- renderText({

    data <- datasetInput()

    # Keep only the numerical data
    data <- data[, purrr::map_lgl(data, is.numeric)]
    
    data_scaled <- scale(data)
    hc1 = hclust(dist(data_scaled))
    
    cutree(hc1, k = input$tree_k, h = input$tree_h)
  })
  
# K-means Clustering ==================================================================
  output$k_cluster <- renderPlot({

    # Read local, online or default dataset
    data <- datasetInput()

    # Keep only the numerical data
    data <- data[, purrr::map_lgl(data, is.numeric)]
 
    # Choose column 2 and 3, skipping column 1 (assuming its an index)
    data <- selectedData()

    # Run K-means plot
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

    # Keep only the numerical data
    data <- data[ , purrr::map_lgl(data, is.numeric)]

    # Run total within-cluster sum of squares plot
    List_y <- list()
    List_x <- list()
    for (k in seq(1, input$k)) {
      km.out <- kmeans(data, k, nstart = 50)
      List_y[k] <- km.out$tot.withinss
      List_x[k] <- k
    }
    plot(List_x,
         List_y,
         typ = "b",
         xlab = "Number of Clusters",
         ylab = "Total within-cluster sum of squares",
         main = "Number of Clusters vs. Total within-cluster sum of squares")

  })

  # Generate a summary of the dataset
  output$summary <- renderPrint({
    data <- datasetInput()
    print("Summary of dataset")
    summary(data)
    
  })

  # Return the requested dataset
  output$view <- renderTable({
    head(datasetInput())
  })

  # Show the first "n" observations
  output$strucutre <- renderPrint({
    print("Strucutre of dataset")
    str(datasetInput())
  })
  
  # Show the first "n" observations
  output$view_table <- renderPrint({
    print("Data as Table")
  })
  
  
#Principal Component Analysis (PCA) =============================================================
  output$pca_variance_plot <- renderPlot({

    # Read local, online or default dataset
    data <- datasetInput()

    # apply() function allows to apply a function, mean()/var(), to each row (1) or column (2) of the data set
    apply(data,2,mean)
    apply(data,2,var)

    # Compute PCA.
    # scale=TRUE to scale the variables to have standard deviation = 1 pr.out=prcomp(USArrests,scale=TRUE)
    pr.out=prcomp(USArrests,scale=TRUE)

    # scale=TRUE to scale the variables to have standard deviation = 1 pr.out=prcomp(USArrests,scale=TRUE)
    names(pr.out)

    # Access all stuff computed by PCA.
    # Means and standard deviations of the variables that were used for scaling prior to implementing PCA. pr.out$center
    pr.out$scale
    # Rotation matrix provides the principal component of the loadings.
    dim(pr.out$rotation)
    pr.out$rotation
    # x matrix provides the principal component of the scores. dim(pr.out$x)
    pr.out$x
    # Biplot, scale=0 ensures that the arrows are scaled to represent the loadings; other values for scale give slightly different biplots with different interpretations.
    biplot(pr.out,scale=0)

    # This is to make the figure look like in the book, mirrored, for some reason, it does not change the significance.
    pr.out$rotation=-pr.out$rotation
    pr.out$x=-pr.out$x
    biplot(pr.out,scale=0)
    # From PCA, get the standard deviation, and variance.
    pr.out$sdev
    pr.var=pr.out$sdev^2
    #pr.var
    # In order to compute the proportion of variance explained by each principal component (variance explained by each principal component / total variance explained by all four principal components)
    pve=pr.var/sum(pr.var)
    #pve
    plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')
    plot(cumsum(pve),xlab="PrincipalComponent",ylab="Cumulative Proportion of Variance
Explained", main="PrincipalComponent vs. Cumulative Proportion of Variance
Explained", ylim=c(0,1),type='b')
  })

  #Biplot
  output$pcaplot <- renderPlot({

    # read local, online or default dataset
    data <- datasetInput()

    scaled_data <- scale(data)
    data.prc <- prcomp(scaled_data)
    biplot(data.prc, scale=0, main="PC1 vs. PC2")

    })

# T-distributed stochastic nearest neighbor (T-Sne) ==================================================================
  # Create a tsne plot of the dataset
  output$tsne_plot <- renderPlot({


    # Read local, online or default dataset
    data <- datasetInput()

    # Keep only the numerical data
    data <- data[, purrr::map_lgl(data, is.numeric)]
    
    # Use table row names to label the datapoint later in the plot
    data_label <- as.factor(rownames(data))
    
    # Remove duplicates
    data_unique <- unique(data)
    
    # Run tSNE
    tSNEdata <- as.matrix(scale(data_unique))
    tsne <- Rtsne(
      tSNEdata,
      dims = 2,
      perplexity = input$Perplexity,
      verbose = TRUE,
      max_iter = input$Iteration
    )
    tsneplot <- plot(tsne$Y, type = "p", main="t-SNE Plot")
  })
  
  output$som <- renderPlot({
    # Read local, online or default dataset
    data <- datasetInput()
    
    # Keep only the numerical data
    data <- data[, purrr::map_lgl(data, is.numeric)]
    
    # For plotting evaluation against colorcode # category (~ classification solution)
    row_label <- as.factor(rownames(data))
    colors <- c("red", "black", "blue")
    colors <- colors[as.numeric(data$Item)]
    data_train_matrix <- as.matrix(scale(data))

    
    # Self-Organizing Maps (SOM) =============================================================
    
    # Define the neuronal grid
    som_grid <- somgrid(xdim = 4,
                        ydim = 4,
                        topo = "hexagonal")
    
    # Train the model
    som_model <-
      som(
        data_train_matrix,
        grid = som_grid,
        rlen = 1000,
        alpha = c(0.05, 0.01),
        keep.data = TRUE
      )
    
    # Check training progress
    # Plot shows the progress over time. If the curve is continually decreasing, more iterations are required.
    plot(som_model, type = "changes")
    
    # Node Counts
    # Visualise the count of how many samples are mapped to each node on the map.
    plot(som_model, type = "count")
    plot(som_model, type = "mapping",
         col = colors[row_label])
    plot(
      som_model,
      type = "mapping",
      labels = (rownames(data)),
      col = colors[row_label]
    )
    
    # U-Matrix / Neighbor Matrix
    #The U-Matrix can be used to identify clusters within the SOM map. Visualizing the distance between each node and its neighbors
    plot(som_model,
         type = "property",
         property = getCodes(som_model, 1)[, 2]) # ??? Make a loop over all the variables ?
    
    
    #Codes/ Weight vectors
    # By visualising the weight vectors across the map, we can see patterns in the distribution of samples and variables.
    plot(som_model, type = "codes")
    
    #SOM Heatmap
    # A SOM heatmap allows the visualisation of the distribution of a single variable across the map.
    plot(
      som_model,
      type = "property",
      property = getCodes(som_model),
      main = names(som_model$data)
    )
    
    # Clustering
    # Clustering can be performed on the SOM nodes to isolate groups of samples with similar metrics.
    tree <-
      as.dendrogram(hclust(dist(as.numeric(
        unlist(som_model$codes)
      ))))
    plot(tree, ylab = "Height (h)")
    
    # Use hierarchical clustering to cluster the codebook vectors
    som_cluster <-
      cutree(hclust(dist(as.numeric(
        unlist(som_model$codes)
      ))), h = 2)
    
    # Plot these results:
    pretty_palette <- c("#1f77b4",
                        '#ff7f0e',
                        '#2ca02c',
                        '#d62728',
                        '#9467bd',
                        '#8c564b',
                        '#e377c2')
    plot(
      som_model,
      type = "mapping",
      labels = (rownames(data)),
      bgcol = pretty_palette[som_cluster],
      col = colors[row_label]
    )
    add.cluster.boundaries(som_model, som_cluster)
    
  })

}

# Run the application
shinyApp(ui = ui, server = server)
