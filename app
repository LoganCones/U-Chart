library(shiny)
library(readxl)
library(qicharts2)

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
      plotOutput("chart")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$file, {
    file <- input$file
    if (!is.null(file)) {
      df <- read_excel(file$datapath)
      updateSelectInput(session, "countColumn", choices = colnames(df))
      updateSelectInput(session, "dateColumn", choices = colnames(df))
      updateSelectInput(session, "sampleSizeColumn", choices = colnames(df))
    }
  })
  
  output$chart <- renderPlot({
    req(input$createChartBtn)
    
    if (input$createChartBtn == 0) {
      return()  # Return an empty plot if the button hasn't been clicked yet
    }
    
    req(input$file, input$countColumn, input$dateColumn)
    
    df <- read_excel(input$file$datapath)
    y <- df[[input$countColumn]]
    x <- df[[input$dateColumn]]
    
    if (input$chartType == "C Chart") {
      chart <- qic(y, x, chart = "C", title = input$countColumn)
    } else if (input$chartType == "U Chart") {
      req(input$sampleSizeColumn)
      n <- df[[input$sampleSizeColumn]]
      chart <- qic(y, x, n, chart = "U", title = input$countColumn)
    }
    
    plot(chart, empty = TRUE)  # Generate an empty plot
  })
}

shinyApp(ui, server)
