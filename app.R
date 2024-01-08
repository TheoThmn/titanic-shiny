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
    titlePanel("Titanic Überlebensstatistik"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("mosaicPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        data    <- read.csv2("titanic_data.csv", sep=",", dec=".")
        # bins <- seq(min(data), max(data), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(data$Age, breaks = input$bins, col = 'darkgray', border = 'white',
             xlab = 'Alter der Passagiere',
             main = 'Histogramm der Altersverteilung der Passagiere')

    })

    output$mosaicPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        data    <- read.csv2("titanic_data.csv", sep=",", dec=".")
        # bins <- seq(min(data), max(data), length.out = input$bins + 1)

        head(data)
        # convert 0 and 1 to boolean
        data$Survived <- as.logical(data$Survived)
        # draw the histogram with the specified number of bins
        
        prop_survival_class <- prop.table(table(data$Survived, data$Pclass ), 1)
        library(knitr)
        kable(prop_survival_class)
        mosaicplot(prop_survival_class,
             main = 'Überlebensstatistik')

    })

}

# Run the application 
shinyApp(ui = ui, server = server)
