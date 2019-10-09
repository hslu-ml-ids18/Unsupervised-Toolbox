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
                         value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           #placeholder for data plot
           plotOutput("data_plot")
        )
    )
)

# Define a function to run tsna with input variables
###t-Distributed Stochastic Neighbor Embedding (tSNE)
library(Rtsne)
# Use table row names to label the datapoint later in the plot:
data_label<-as.factor(rownames(data))

#remove duplicates:
data_unique <- unique(data)

# Run tSNE:
# tSNEdata <- as.matrix(scale(data_unique))
# tsne <- Rtsne(tSNEdata, dims = 2,
#               perplexity= sliderInput$Perplexity, verbose=TRUE,
#               max_iter = sliderInput$Iteration)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Create a PCA plot of the dataset 
    output$data_plot <- tsneplot
    

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

