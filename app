library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# UI definition
ui <- fluidPage(
  titlePanel("U and C Chart Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Upload CSV file"),
      radioButtons("chartType", "Chart Type",
                   choices = c("C Chart", "U Chart"),
                   selected = "C Chart")
    ),
    mainPanel(
      plotOutput("chart"),
      dataTableOutput("controlTable")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Read data from uploaded file
  data <- reactive({
    req(input$dataFile)
    read.csv(input$dataFile$datapath)
  })
  
  # Generate C Chart
  cChart <- reactive({
    data() %>%
      ggplot(aes(x = date, y = count_defects / count_total_defects)) +
      geom_line() +
      geom_hline(yintercept = mean(.$count_defects / .$count_total_defects),
                 color = "red", linetype = "dashed") +
      labs(x = "Date", y = "C Chart", title = "C Chart")
  })
  
  # Generate U Chart
  uChart <- reactive({
    data() %>%
      ggplot(aes(x = date, y = rate)) +
      geom_line() +
      geom_hline(yintercept = mean(.$rate),
                 color = "red", linetype = "dashed") +
      labs(x = "Date", y = "U Chart", title = "U Chart")
  })
  
  # Calculate control limits
  controlLimits <- reactive({
    c_avg <- mean(data()$count_defects)
    n_total <- sum(data()$count_total_defects)
    k <- length(unique(data()$date))
    m <- c_avg
    u_hat <- sum(data()$count_defects) / n_total
    
    lower_limit <- u_hat - 3 * sqrt(u_hat / n_total)
    upper_limit <- u_hat + 3 * sqrt(u_hat / n_total)
    
    data.frame(Lower_Limit = lower_limit, Upper_Limit = upper_limit)
  })
  
  # Render the selected chart
  output$chart <- renderPlot({
    if (input$chartType == "C Chart") {
      print(cChart())
    } else {
      print(uChart())
    }
  })
  
  # Render the control limits table
  output$controlTable <- renderDataTable({
    controlLimits()
  })
}

# Run the Shiny app
shinyApp(ui, server)
