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
library(knitr)
library(DescTools)
library(data.table)

options_up_to_ordinal <- c(
  "Geschlecht",
  "Klasse",
  "Einstiegshafen",
  "Kabinenbuchstabe",
  "Kinder und Eltern an Bord",
  "Geschwister und Ehepartner an Bord",
  "Familienmitglieder an Bord"

)
options_stetig <- c(
  "Alter",
  "Ticketpreis"
)
options <- c(options_up_to_ordinal, options_stetig)

switch_options <- function(data, selected_option, breaks = NULL) {

  cabin_letters <- gsub("[0-9]", "", data$Cabin)
  cabin_letter <- substr(cabin_letters, 1, 1)

  if (is.null(breaks)) {
    cut_alter <- data$Age
    cut_fare <- data$Fare
    cut_sibsp <- data$SibSp
    cut_parch <- data$Parch
  } else {
    cut_alter <- cut(
      data$Age,
      breaks = breaks,
      dig.lab = 2
    )
    cut_fare <- cut(data$Fare, breaks = breaks)
    cut_sibsp <- cut(data$SibSp, breaks = breaks)
    cut_parch <- cut(data$Parch, breaks = breaks)
  }

  switch(selected_option,
    "Alter" = cut_alter,
    "Klasse" = data$Pclass,
    "Geschlecht" = data$Sex,
    "Kinder und Eltern an Bord" = cut_parch,
    "Geschwister und Ehepartner an Bord" = cut_sibsp,
    "Familienmitglieder an Bord" = cut_sibsp + cut_parch,
    "Einstiegshafen" = data$Embarked,
    "Ticketpreis" = cut_fare,
    "Kabinenbuchstabe" = cabin_letter,
    "Überlebt" = data$Survived
  )
}

######################################################
##################### UI #############################
######################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "Titanic Überlebensstatistik",
    navbarMenu(
      "Startseite",
      tabPanel(
        "Tabelle",
        sidebarLayout(
          sidebarPanel(
            h4("Tabelle der Daten"),
            p("Survived: Überlebt (1) oder Gestorben (0)"),
            p("Pclass: Kabinenklasse (1 = 1. Klasse, 2 = 2. Klasse, 3 = 3. Klasse)"),
            p("SibSp: Anzahl der Geschwister und Ehepartner an Bord"),
            p("Parch: Anzahl der Kinder und Eltern an Bord"),
            p("Ticket: Ticketnummer"),
            p("Fare: Ticketpreis"),
            p("Cabin: Kabinennummer"),
            p("Embarked: Einstiegshafen (C = Cherbourg, Q = Queenstown, S = Southampton)"),
            selectInput(
              "survived",
              "Überlebt",
              choices = c("Überlebt" = TRUE, "Gestorben" = FALSE),
              multiple = TRUE,
            ),
            selectInput(
              "pclass",
              "Klasse",
              choices = c(
                "1. Klasse" = "1",
                "2. Klasse" = "2",
                "3. Klasse" = "3"
              ),
              multiple = TRUE,
            ),
            selectInput(
              "sex",
              "Geschlecht",
              choices = c("männlich" = "male", "weiblich" = "female"),
              multiple = TRUE,
            ),
            sliderInput("fares",
              "Ticketpreis",
              min = 0,
              max = 513,
              value = c(0, 513),
              step = 1
            ),
            selectInput(
              "harbor",
              "Einstiegshafen",
              choices = c(
                "Cherbourg" = "C",
                "Queenstown" = "Q",
                "Southampton" = "S"
              ),
              multiple = TRUE
            )
          ),
          mainPanel(
            dataTableOutput("dataTable")
          )
        )
      ),
      tabPanel(
        "Übersicht",
        sidebarLayout(
          sidebarPanel(
            sliderInput("gruppen",
              "Größe der Altersgruppen",
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
      tabPanel(
        "Zusammenfassungen",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "selected_dimension",
              "Merkmal",
              choices = c("Überlebt", options_up_to_ordinal),
              multiple = FALSE
            )
          ),
          mainPanel(
            verbatimTextOutput("summary"),
            tableOutput("proportions_and_absolute_numbers")
          )
        )
      )
    ),

    navbarMenu(
      "Kombination von Merkmalen",
      tabPanel(
        "nach Überleben",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "additional_dimension",
              "zweites Merkmal",
              choices = options,
              multiple = FALSE
            ),
            uiOutput("slider"),
            uiOutput("checkbox_agegroup_only"),
          ),
          mainPanel(
            plotOutput("two_dim_mosaic"),
            plotOutput("barplot_on_survival"),
            "(Kardinalskalierte Merkmale werden gerundet)",
            htmlOutput("zusätzliche_bilder"),
            tableOutput("contingency_table")
          )
        )
      ),
      tabPanel(
        "Kombination im Mosaikdiagramm",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "first_dimension_mosaic",
              "erstes Merkmal",
              choices = options_up_to_ordinal,
              multiple = FALSE,
              selected = "Einstiegshafen"
            ),
            selectInput(
              "second_dimension_mosaic",
              "zweites Merkmal",
              choices = options_up_to_ordinal,
              multiple = FALSE,
              selected = "Klasse"
            )
          ),
          mainPanel(
            plotOutput("mosaicplot")
          )
        )
      ),
      tabPanel(
        "Kombination im Streudiagramm",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "first_dimension_scatter",
              "erstes Merkmal",
              choices = options_stetig,
              multiple = FALSE
            ),
            selectInput(
              "second_dimension_scatter",
              "zweites Merkmal",
              choices = rev(options_stetig),
              multiple = FALSE
            )
          ),
          mainPanel(
            plotOutput("scatterplot"),
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
            "Merkmale",
            choices = options,
            multiple = TRUE,
            selected = c(
              "Ticketpreis",
              "Geschlecht",
              "Alter",
              "Klasse"
            )
          )
        ),
        mainPanel(
          plotOutput("contingency")
        )
      )
    )
  )
)

######################################################
##################### Server #########################
######################################################


# Define server logic required to draw a histogram
server <- function(input, output) {

  data <- read.csv2("titanic_data.csv", sep = ",", dec = ".")
  data$Survived <- as.logical(data$Survived)

  ################# Startseite #####################

  output$survival <- renderPlot({
    barplot(
      prop.table(table(data$Survived)),
      main = "Überlebensstatistik",
      ylab = "Schicksal",
      xlab = "Relative Häufigkeit",
      names.arg = c("Gestorben", "Überlebt"),
      horiz = TRUE,
      beside = FALSE,
      col = c("#b9b9b953", "#00ff003e")
    )
  })

  output$survivalClassBarplot <- renderPlot({
    prop_survival_class <- prop.table(table(data$Survived, data$Pclass), 1)
    library(knitr)
    kable(prop_survival_class)
    barplot(prop_survival_class,
      main = "Kabinenklasse mit Anteilen der Überlebenden",
      ylab = "Relative Häufigkeit",
      xlab = "Klasse",
      legend = c("Gestorben", "Überlebt"),
      col = c("#b9b9b953", "#00ff003e")
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

  ################# Tabelle #####################

  output$dataTable <- renderDataTable({
    filtered_data <- data
    if (!is.null(input$survived)) {
      filtered_data <- filtered_data[data$Survived %in% input$survived, ]
    }
    if (!is.null(input$fares)) {
      filtered_data <- filtered_data[data$Fare >= input$fares[1], ]
      filtered_data <- filtered_data[data$Fare <= input$fares[2], ]
    }
    if (!is.null(input$sex)) {
      filtered_data <- filtered_data[data$Sex %in% input$sex, ]
    }
    if (!is.null(input$pclass)) {
      filtered_data <- filtered_data[data$Pclass %in% input$pclass, ]
    }
    if (!is.null(input$harbor)) {
      filtered_data <- filtered_data[data$Embarked %in% input$harbor, ]
    }
    filtered_data

  })

  ################# Summary einzelner Merkmale #####################

  ## as in https://stackoverflow.com/a/47206071
  output$summary <- renderPrint({
    this_data <- switch_options(data, input$selected_dimension)
    summary(this_data)
  })

  output$proportions_and_absolute_numbers <- renderTable({

    this_data <- switch_options(data, input$selected_dimension)
    h_table <- table(this_data)
    p_table <- prop.table(table(this_data))
    combined_table <- cbind(h_table, p_table)
    combined_table <- data.frame(RowNames = rownames(combined_table), combined_table)
    colnames(combined_table) <- c("Merkmal", "Häufigkeit", "relative Häufigkeit")
    combined_table$Häufigkeit <- as.integer(combined_table$Häufigkeit)
    combined_table
  })


  ############### X unter der Bedingung Überleben #######################

  output$slider <- renderUI({
    if (input$additional_dimension %in% options_stetig) {
      sliderInput(
        "breaks",
        "Anzahl der Gruppen:",
        min = 2,
        max = 30,
        value = 8,
        step = 1
      )
    }
  })

  output$zusätzliche_bilder <- renderText(
    if(input$additional_dimension == "Kabinenbuchstabe") {
      '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/8/84/Titanic_cutaway_diagram.png/800px-Titanic_cutaway_diagram.png" alt="Titanic cutaway diagram" width="500" height="500">'
    }
  )


  output$two_dim_mosaic <- renderPlot({
    filtered_data <- data
    if (
        input$additional_dimension == "Kinder und Eltern an Bord" || 
        input$additional_dimension == "Geschwister und Ehepartner an Bord" ||
        input$additional_dimension == "Familienmitglieder an Bord") {
      if (input$only_selected_agegroup == "Nur Kinder einbeziehen") {
        filtered_data <- subset(filtered_data, Age <= 18)
      } else if (input$only_selected_agegroup == "Nur Erwachsene einbeziehen") {
        filtered_data <- subset(filtered_data, Age > 18)
      }
    }
    mosaic_data <- switch_options(filtered_data, input$additional_dimension, breaks = input$breaks)
    mosaicplot(
      table(
        mosaic_data,
        filtered_data$Survived
      ),
      main = paste(
        "Mosaikplot Überleben in Abhängigkeit von",
        input$additional_dimension
      ),
      xlab = input$additional_dimension,
      ylab = "Überlebt",
      color = TRUE
    )
  })

  output$checkbox_agegroup_only <- renderUI({
    altersfiltergruppen <- c("Alle einbeziehen", "Nur Kinder einbeziehen", "Nur Erwachsene einbeziehen")
    if (input$additional_dimension == "Kinder und Eltern an Bord" ||
    input$additional_dimension == "Geschwister und Ehepartner an Bord" ||
    input$additional_dimension == "Familienmitglieder an Bord") {
      radioButtons(
        "only_selected_agegroup",
        "Nur Kinder oder Erwachsene einbeziehen (Kinder: Age < 18)",
        choices = altersfiltergruppen,
        selected = NULL,
      )
    }
  })

  output$barplot_on_survival <- renderPlot({
    filtered_data <- data
    if (
        input$additional_dimension == "Kinder und Eltern an Bord" || 
        input$additional_dimension == "Geschwister und Ehepartner an Bord" ||
        input$additional_dimension == "Familienmitglieder an Bord") {
      if (input$only_selected_agegroup == "Nur Kinder einbeziehen") {
        filtered_data <- subset(filtered_data, Age <= 18)
      } else if (input$only_selected_agegroup == "Nur Erwachsene einbeziehen") {
        filtered_data <- subset(filtered_data, Age > 18)
      }
    }
    this_data <- switch_options(filtered_data, input$additional_dimension, breaks = input$breaks)
    this_survival <- factor(filtered_data$Survived, levels = c(TRUE, FALSE))
    barplot(
      prop.table(table(this_survival, this_data), 2),
      main = paste(
        "Überlebensstatistik in Abhängigkeit von",
        input$additional_dimension
      ),
      ylab = "Relative Häufigkeit von Überleben",
      xlab = input$additional_dimension,
      beside = FALSE,
      col = c("#00ff003e", "#b9b9b953"),
      legend = (c("Überlebt", "Gestorben"))
    )
  })

  output$contingency_table <- renderTable(
    {
      this_data <- switch_options(data, input$additional_dimension, breaks = input$breaks)
      this_survival <- data$Survived
      frame <- as.data.frame.matrix(prop.table(table(this_data, this_survival), 1))
      colnames(frame) <- c("Gestorben", "Überlebt")
      frame
    },
    rownames = TRUE
  )

  ################ Streudiagramme ######################

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

  ################### Mosaikplots ####################

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

  ################## Kontingenzmaße #####################

  output$contingency <- renderPlot({
    cramer_v_values <- sapply(input$contingencyDimensions, function(dim) {
      selected_dim <- switch_options(data, dim)
      CramerV(data$Survived, selected_dim)
    })

    barplot(
      cramer_v_values,
      xlab = "Cramer V",
      ylab = "Merkmale",
      main = "Kontingenzmaße vom Überleben (Cramer V)",
      xlim = c(0, 1),
      horiz = TRUE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)