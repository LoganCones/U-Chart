library(shiny)
library(readxl)
library(qicharts2)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("Quality Control Charts"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = c(".xlsx")),
      radioButtons("chartType", "Select Chart Type:", choices = c("C Chart", "U Chart")),
      conditionalPanel(
        condition = "input.chartType == 'C Chart'",
        selectInput("countColumn", "Select Count Column:", NULL),
        selectInput("dateColumn", "Select Date Column:", NULL)
      ),
      conditionalPanel(
        condition = "input.chartType == 'U Chart'",
        selectInput("countColumn", "Select Count Column:", NULL),
        selectInput("sampleSizeColumn", "Select Sample Size Column:", NULL),
        selectInput("dateColumn", "Select Date Column:", NULL)
      ),
      actionButton("createChartBtn", "Create Chart")
    ),
    
    mainPanel(
      plotOutput("chart"),
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveValues(chart = NULL, table = NULL)
  
  observeEvent(input$file, {
    file <- input$file
    if (!is.null(file)) {
      df <- read_excel(file$datapath)
      updateSelectInput(session, "countColumn", choices = colnames(df))
      updateSelectInput(session, "dateColumn", choices = colnames(df))
      updateSelectInput(session, "sampleSizeColumn", choices = colnames(df))
    }
  })
  
  observeEvent(input$createChartBtn, {
    req(input$file, input$countColumn, input$dateColumn)
    
    df <- read_excel(input$file$datapath)
    y <- df[[input$countColumn]]
    x <- df[[input$dateColumn]]
    
    if (input$chartType == "C Chart") {
      chart <- qic(y, x, chart = "C", title = input$countColumn, empty = TRUE)
    } else if (input$chartType == "U Chart") {
      req(input$sampleSizeColumn)
      n <- df[[input$sampleSizeColumn]]
      chart <- qic(y, x, n, chart = "U", title = input$countColumn, empty = TRUE)
    }
    
    data$chart <- chart
    
    # Generate datatable
    data$table <- data.frame(x = x, y = y, UCL = chart$limits$UCL, stringsAsFactors = FALSE)
    data$table$above_UCL <- data$table$y > data$table$UCL
    
    output$chart <- renderPlot({
      if (is.null(data$chart))
        return()
      
      plot(data$chart, empty = TRUE)
    })
    
    output$table <- renderDataTable({
      datatable(data$table, options = list(pageLength = 10), rownames = FALSE) %>%
        formatStyle(
          "y",
          backgroundColor = styleInterval(data$table$y, c(0, data$table$UCL)),
          color = "white",
          background = c("transparent", "red")
        )
    })
  })
}

shinyApp(ui, server)
