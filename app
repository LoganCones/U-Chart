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
        selectInput("cCountColumn", "Select Count Column:", NULL),
        selectInput("cDateColumn", "Select Date Column:", NULL)
      ),
      conditionalPanel(
        condition = "input.chartType == 'U Chart'",
        selectInput("uCountColumn", "Select Count Column:", NULL),
        selectInput("uSampleSizeColumn", "Select Sample Size Column:", NULL),
        selectInput("uDateColumn", "Select Date Column:", NULL)
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
      updateSelectInput(session, "cCountColumn", choices = colnames(df))
      updateSelectInput(session, "cDateColumn", choices = colnames(df))
      updateSelectInput(session, "uCountColumn", choices = colnames(df))
      updateSelectInput(session, "uSampleSizeColumn", choices = colnames(df))
      updateSelectInput(session, "uDateColumn", choices = colnames(df))
    }
  })
  
  observeEvent(input$createChartBtn, {
    req(input$file)
    
    df <- read_excel(input$file$datapath)
    
    if (input$chartType == "C Chart") {
      req(input$cCountColumn, input$cDateColumn)
      
      y <- df[[input$cCountColumn]]
      x <- df[[input$cDateColumn]]
      
      chart <- qic(y, x, chart = "C", main = input$cCountColumn, empty = TRUE)
    } else if (input$chartType == "U Chart") {
      req(input$uCountColumn, input$uSampleSizeColumn, input$uDateColumn)
      
      y <- df[[input$uCountColumn]]
      n <- df[[input$uSampleSizeColumn]]
      x <- df[[input$uDateColumn]]
      
      chart <- qic(y, x, n, chart = "U", main = input$uCountColumn, empty = TRUE)
    }
    
    data$chart <- chart
    
    # Generate datatable
    if (!is.null(chart)) {
      data$table <- data.frame(x = x, y = y, UCL = chart$limits$UCL, stringsAsFactors = FALSE)
      data$table$above_UCL <- data$table$y > data$table$UCL
    }
    
    output$chart <- renderPlot({
      if (is.null(data$chart))
        return()
      
      plot(data$chart, empty = TRUE)
    })
    
    output$table <- renderDataTable({
      if (!is.null(data$table)) {
        datatable(data$table, options = list(pageLength = 10), rownames = FALSE) %>%
          formatStyle(
            "y",
            backgroundColor = styleInterval(data$table$y, c(0, data$table$UCL)),
            color = "white",
            background = ifelse(data$table$y > data$table$UCL, "red", "transparent")
          )
      }
    })
  })
}

shinyApp(ui, server)
