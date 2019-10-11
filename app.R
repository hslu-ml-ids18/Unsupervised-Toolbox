#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput('tsne_plot'),
           #  k-means clustering plot
           plotOutput('plot1'),
           # Absolutely-positioned panels
           plotOutput('plot2', height = "800px", width = "900px")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        data[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
        output$plot2 <- renderPlot({
            mtscaled <- as.matrix(scale(data))
            heatmap(mtscaled,
                    col = topo.colors(200, alpha=0.5),
                    Colv=F, scale="none")
        })
    
    
    # Create a PCA plot of the dataset
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
