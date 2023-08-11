# app.R
library(shiny)

# Load shared data and function from global.R
source("global.R")

# UI
ui <- fluidPage(
  titlePanel("Simple Shiny App"),
  sidebarLayout(
    sidebarPanel(
      # Sidebar content (if any)
    ),
    mainPanel(
      tableOutput("data_table"),
      textOutput("average_score")
    )
  )
)

# Server
server <- function(input, output) {
  output$data_table <- renderTable({
    sample_data
  })
  
  output$average_score <- renderText({
    avg <- calculate_average(sample_data)
    paste("Average Score:", avg)
  })
}

# Run the app
shinyApp(ui, server)
