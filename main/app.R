library(shiny)
library(shinythemes)
library(dygraphs)
library(data.table)
library(DT)
library(tidyverse)

# Good theme options: "lumem", "spacelab", "cerulean"...
# There's an shiny app dedicated to test theme options,
# https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/

checkboxes <- data.frame()

# Front-end
ui <- fluidPage(theme = shinytheme("lumen"),
  titlePanel("Wavexpress"),
  sidebarLayout(
    sidebarPanel(
      # Display "Browse" button
      fileInput("file_1", "Choose a txt file:",
                accept = c(".txt")
      ),
      # Display table with checkboxes
      dataTableOutput("myTable")
    ),
    mainPanel(
      # Graph window
      dygraphOutput("dygraph_plot_1")
    )
  )
)

server <- function(input, output, checkboxes) {
  # Sends the graph object to the output
  output$dygraph_plot_1 <- renderDygraph({
    # Silently leave the function in case there's no input file
    req(input$file_1)
    
    # Read file as tab separated values
    data <- read.delim2(input$file_1$datapath)  # Skip headers
    
    # Extract the headers (variable names)
    headers <- colnames(read.delim2(input$file_1$datapath))[-1]
    
    # Print the result (including any errors caught)
    print(result)
    
    
    # Plots the graph
    dygraph(dplyr::select(data, everything()), main = "EMG Signal") %>% dyRangeSelector() %>%
      dyAxis("x", label = "Time (s)")%>%
      dyAxis("y", label = "Amplitude (mV)")
  })
  
  # Proxy to update the myTable
  proxy <- dataTableProxy('myTable')
  
  # Sends the "checkbox" table to the output
  output$myTable <- renderDataTable({
    # Silently leave the function in case there's no input file
    req(input$file_1)
    
    # Get file name and headers
    filename <- input$file_1$name
    headers <- colnames(read.delim2(input$file_1$datapath))[-1]
    
    # Create an empty dataframe with the specified column names and a single row of "OK" values
    checkboxes <- data.frame(matrix(as.character(icon("ok", lib = "glyphicon")), 
                                    ncol = length(headers), nrow = 1))
    
    # Set the column names
    colnames(checkboxes) <- headers
    
    # Set the row name
    rownames(checkboxes) <- filename
    
    # The reactive version of the data
    tableData <- reactiveValues(checkboxes = checkboxes)
    
    # Update the table when clicked
    observeEvent(req(input$myTable_cells_selected), {
      req(input$file_1)
      if(input$myTable_cells_selected[2]) {
        tableData$checkboxes[input$myTable_cells_selected] =
          ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
                 as.character(icon("ok", lib = "glyphicon")), NA)
        
        # Send proxy (no need to refresh whole table)
        replaceData(proxy, tableData$checkboxes)
      }
    })
    
    # finally calls the object
    checkboxes
    
    },
    
    # These are options to make the table look like checkboxes
    selection = list(mode = "single", target = 'cell'), 
    options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      dom = "t", ordering = F
    ),
    escape = F
  )
}

shinyApp(ui, server)