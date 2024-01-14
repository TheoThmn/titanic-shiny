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

options_nominal <- c(
  "Geschlecht",
  "Klasse",
  "Einstiegshafen"
)
options_kardinal <- c(
  "Alter",
  "Kinder und Eltern an Bord",
  "Geschwister und Ehepartner an Bord",
  "Ticketpreis"
)
options <- c(options_nominal, options_kardinal)

switch_options <- function(data, selected_option, breaks = NULL) {

  cabin_letter <- gsub("[0-9]", "", data$Cabin)

  switch(selected_option,
    "Alter" = data$Age,
    "Klasse" = data$Pclass,
    "Geschlecht" = data$Sex,
    "Kinder und Eltern an Bord" = data$Parch,
    "Geschwister und Ehepartner an Bord" = data$SibSp,
    "Einstiegshafen" = data$Embarked,
    "Ticketpreis" = data$Fare,
    "Kabinenbuchstabe" = cabin_letter
  )
}


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
          plotOutput("survival"),
          plotOutput("altersverteilungHist"),
          plotOutput("survivalClassBarplot"),
        )
      )
    ),
    navbarMenu(
      "zweidimensionale Darstellungen",
      tabPanel(
        "nach Überleben",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "additionalDimension",
              "zweite Dimension:",
              choices = options,
              multiple = FALSE
            ),
            uiOutput("slider")
          ),
          mainPanel(
            plotOutput("two_dim_mosaic"),
            plotOutput("abhängigkeit")
          )
        )
      ),
      tabPanel(
        "Kombination im Streudiagramm",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "first_dimension_scatter",
              "erste Dimension:",
              choices = options_kardinal,
              multiple = FALSE
            ),
            selectInput(
              "second_dimension_scatter",
              "zweite Dimension:",
              choices = options_kardinal,
              multiple = FALSE
            )
          ),
          mainPanel(
            plotOutput("scatterplot"),
          )
        )
      ),
      tabPanel(
        "Kombination im Mosaikdiagramm",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "first_dimension_mosaic",
              "erste Dimension:",
              choices = options_nominal,
              multiple = FALSE
            ),
            selectInput(
              "second_dimension_mosaic",
              "zweite Dimension:",
              choices = options_nominal,
              multiple = FALSE
            )
          ),
          mainPanel(
            plotOutput("mosaicplot")
          )
        )
      )
    ),
    tabPanel(
      "Kontingenzmaße",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "contingencyDimensions",
            "Merkmale:",
            choices = options,
            multiple = TRUE,
            selected = c("Alter", "Klasse")
          )
        ),
        mainPanel(
          plotOutput("contingency")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(knitr)
  library(DescTools)
  data <- read.csv2("titanic_data.csv", sep = ",", dec = ".")
  data$Survived <- as.logical(data$Survived)


  ######################################

  output$survival <- renderPlot({
    barplot(
      prop.table(table(data$Survived)),
      main = "Überlebensstatistik",
      ylab = "Überlebt",
      xlab = "Relative Häufigkeit",
      names.arg = c("Gestorben", "Überlebt"),
      horiz = TRUE,
      col = c("#ff000053", "#00ff003e")
    )
  })

  output$survivalClassBarplot <- renderPlot({
    prop_survival_class <- prop.table(table(data$Survived, data$Pclass), 1)
    library(knitr)
    kable(prop_survival_class)
    barplot(prop_survival_class,
      main = "Kabinenklasse mit Anteilen der Überlebenden",
    )
  })


  output$altersverteilungHist <- renderPlot({
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
    switch_options(data, input$additionalDimension)
  })


  ######################################

  output$two_dim_mosaic <- renderPlot({
    mosaicplot(
      table(
        data_selected_dim(),
        data$Survived
      ),
      main = paste(
        "Mosaikplot Überleben in Abhängigkeit von",
        input$additionalDimension
      ),
      xlab = input$additionalDimension,
      ylab = "Überlebt",
      color = TRUE
    )
  })

  ######################################

  data_first_selected_dim <- reactive({
    switch_options(data, input$first_dimension_scatter)
  })
  data_second_selected_dim <- reactive({
    switch_options(data, input$second_dimension_scatter)
  })

  output$scatterplot <- renderPlot(
    plot(
      data_first_selected_dim(),
      data_second_selected_dim(),
      main = paste(
        "Streudiagramm von",
        input$first_dimension_scatter,
        "und",
        input$second_dimension_scatter
      ),
      xlab = input$first_dimension_scatter,
      ylab = input$second_dimension_scatter
    ),
  )

#######################################


  data_first_selected_dim_mosaic <- reactive({
    switch_options(data, input$first_dimension_mosaic)
  })

  data_second_selected_dim_mosaic <- reactive({
    switch_options(data, input$second_dimension_mosaic)
  })

  output$mosaicplot <- renderPlot({
    mosaicplot(
      table(
        data_first_selected_dim_mosaic(),
        data_second_selected_dim_mosaic()
      ),
      main = paste(
        "Mosaikplot",
        input$first_dimension_mosaic,
        "in Abhängigkeit von",
        input$second_dimension_mosaic
      ),
      xlab = input$first_dimension_mosaic,
      ylab = input$second_dimension_mosaic,
      color = TRUE
    )
  })

  #######################################


  output$contingency <- renderPlot({
    cramer_v_values <- sapply(input$contingencyDimensions, function(dim) {
      selected_dim <- switch_options(data, dim)
      CramerV(data$Survived, selected_dim)
    })

    barplot(
      cramer_v_values,
      xlab = "Cramer V",
      ylab = "Dimension",
      main = "Kontingenzmaße vom Überleben (Cramer V)",
      xlim = c(0, 1),
      horiz = TRUE
    )
  })

  ######################################
}

# Run the application
shinyApp(ui = ui, server = server)
