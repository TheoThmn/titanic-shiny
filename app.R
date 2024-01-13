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
    tabPanel(
      "zweidimensionale Visualisierungen",
      sidebarLayout(
        sidebarPanel(
          # dropdown menu for second dimension of age, class etc.
          selectInput(
            "secondDimension",
            "zweite Dimension:",
            choices = c("Alter", "Klasse", "Geschlecht", "Kinder und Eltern an Bord", "Geschwister und Ehepartner an Bord", "Hafen der Einschiffung", "Ticketpreis", "Kabine"),
            multiple = FALSE
          ),
          uiOutput("slider")
        ),
        mainPanel(
          plotOutput("two_dim_mosaic")
        )
      )
    ),
    navbarMenu(
      "Alter",
      tabPanel(
        "Kontingenztabellen",
          mainPanel(
            "nach Überleben",
            tableOutput("kontingenztabellen"),
          )
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(knitr)
  data <- read.csv2("titanic_data.csv", sep = ",", dec = ".")

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
      breaks = breaks,
      beside = TRUE
    )
  })

  ######################################


  data_selected_dim <- reactive({
    cabin_letter <- gsub("[0-9]", "", data$Cabin)
    switch(input$secondDimension,
           "Alter" = cut(data$Age, breaks = 8),
           "Klasse" = data$Pclass,
           "Geschlecht" = data$Sex,
            "Kinder und Eltern an Bord" = data$Parch,,
            "Geschwister und Ehepartner an Bord" = data$SibSp,
            "Hafen der Einschiffung" = data$Embarked,
            "Ticketpreis" = cut(data$Fare, breaks = 15),
            "Kabine" = cabin_letter,          )
  })

  output$two_dim_mosaic <- renderPlot(
    mosaicplot(
      table(
        data_selected_dim(),
        data$Survived
      ),
      main = "Überlebensstatistik",
      xlab = input$secondDimension,
      ylab = "Überlebt",
      color = TRUE   
    )
  )
  output$kontingenztabellen <- renderTable({
    head(data)

    # convert 0 and 1 to boolean
    data$Survived <- as.logical(data$Survived)

    # draw the histogram with the specified number of bins
    prop_survival_class <- prop.table(table(data$Survived, data$Pclass), 1)
    table(prop_survival_class)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
