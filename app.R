#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    run with shiny::runApp('app.R')
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "Titanic Überlebensstatistik",
    tabPanel(
      "Daten verstehen",
      sidebarLayout(
        sidebarPanel(
          sliderInput("gruppen",
            "Größe der Altersgruppen:",
            min = 5,
            max = 30,
            value = 20,
            step = 5
          )
        ),
        mainPanel(
          plotOutput("survivalClassBarplot"),
          "Altersverteilung der Passagiere",
          plotOutput("altersverteilungHist"),
          tableOutput("altersverteilungRelHäufigkeiten")
        )
      )
    ),
    navbarMenu(
      "zweidimensionale Darstellungen",
      tabPanel(
        "nach Überleben",
        sidebarLayout(
          sidebarPanel(
            # dropdown menu for second dimension of age, class etc.
            selectInput(
              "additionalDimension",
              "zweite Dimension:",
              choices = c(
                "Alter",
                "Klasse",
                "Geschlecht",
                "Kinder und Eltern an Bord",
                "Geschwister und Ehepartner an Bord",
                "Hafen der Einschiffung",
                "Ticketpreis"
              ),
              multiple = FALSE
            ),
            uiOutput("slider")
          ),
          mainPanel(
            plotOutput("two_dim_mosaic")
          )
        )
      ),
      tabPanel(
        "Kombination zweier Merkmale",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "firstDimension",
              "erste Dimension:",
              choices = c(
                "Alter",
                "Klasse",
                "Kinder und Eltern an Bord",
                "Geschwister und Ehepartner an Bord",
                "Ticketpreis"
              ),
              multiple = FALSE
            ),
            selectInput(
              "secondDimension",
              "zweite Dimension:",
              choices = c(
                "Alter",
                "Klasse",
                "Kinder und Eltern an Bord",
                "Geschwister und Ehepartner an Bord",
                "Ticketpreis"
              ),
              multiple = FALSE
            )
          ),
          mainPanel(
            plotOutput("scatterplot")
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(knitr)
  data <- read.csv2("titanic_data.csv", sep = ",", dec = ".")
  data$Survived <- as.logical(data$Survived)


  ######################################

  output$survivalClassBarplot <- renderPlot({
    # convert 0 and 1 to boolean
    data$Survived <- as.logical(data$Survived)

    # draw the histogram with the specified number of bins
    prop_survival_class <- prop.table(table(data$Survived, data$Pclass), 1)
    library(knitr)
    kable(prop_survival_class)
    barplot(prop_survival_class,
      main = "Überlebensstatistik"
    )
  })

  output$altersverteilungRelHäufigkeiten <- renderTable({
    breaks <- seq(0, 100, input$gruppen)
    Altersgruppen <- cut(data$Age, breaks = breaks)
    prop.table(table(Altersgruppen)) * 100
  })

  output$altersverteilungHist <- renderPlot({
    # Histogramm der Altersgruppen
    breaks <- seq(0, 100, input$gruppen)
    hist(
      data$Age,
      main = "Histogramm der Altersverteilung der Passagiere",
      xlab = "Alter", ylab = "Häufigkeit",
      breaks = breaks
    )
  })

  ######################################


  data_selected_dim <- reactive({
    cabin_letter <- gsub("[0-9]", "", data$Cabin)
    switch(input$additionalDimension,
      "Alter" = cut(data$Age, breaks = 8),
      "Klasse" = data$Pclass,
      "Geschlecht" = data$Sex,
      "Kinder und Eltern an Bord" = data$Parch,
      "Geschwister und Ehepartner an Bord" = data$SibSp,
      "Hafen der Einschiffung" = data$Embarked,
      "Ticketpreis" = cut(data$Fare, breaks = 15),
    )
  })

  output$two_dim_mosaic <- renderPlot({
    mosaicplot(
      table(
        data_selected_dim(),
        data$Survived
      ),
      main = "Überlebensstatistik",
      xlab = input$additionalDimension,
      ylab = "Überlebt",
      color = TRUE
    )
  }) 


  
  ######################################
  data_first_selected_dim <- reactive({
    cabin_letter <- gsub("[0-9]", "", data$Cabin)
    switch(input$firstDimension,
      "Alter" = data$Age,
      "Klasse" = data$Pclass,
      "Kinder und Eltern an Bord" = data$Parch,
      "Geschwister und Ehepartner an Bord" = data$SibSp,
      "Ticketpreis" = data$Fare,
    )
  })

  data_second_selected_dim <- reactive({
    cabin_letter <- gsub("[0-9]", "", data$Cabin)
    switch(input$secondDimension,
      "Alter" = data$Age,
      "Klasse" = data$Pclass,
      "Kinder und Eltern an Bord" = data$Parch,
      "Geschwister und Ehepartner an Bord" = data$SibSp,
      "Ticketpreis" = data$Fare,
    )
  })

  output$scatterplot <- renderPlot(
    plot(
      data_first_selected_dim(),
      data_second_selected_dim(),
      main = "Überlebensstatistik",
      xlab = input$firstDimension,
      ylab = input$secondDimension
    )
  )
}

# Run the application
shinyApp(ui = ui, server = server)
