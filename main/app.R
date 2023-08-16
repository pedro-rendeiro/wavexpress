library(shiny)
library(shinythemes)
library(dygraphs)
library(data.table)
library(DT)
library(tidyverse)

# Load shared data and function from global.R
# source("global.R")

# Good theme options: "lumem", "spacelab", "cerulean"...
# There's an shiny app dedicated to test theme options,
# https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/

# Front-end
ui <- fluidPage(theme = shinytheme("lumen"),
  titlePanel("Wavexpress"),
  sidebarLayout(
    sidebarPanel(
      # Display "Browse" button
      fileInput("files", "Choose a txt file:",
                accept = c(".txt"),
                multiple = TRUE
      ),
      # Display tables with checkboxes
      uiOutput("myTable")
    ),
    mainPanel(
      # UI Test
      uiOutput("dygraphPlot")
    )
  )
)

server <- function(input, output) {
  # Variable to save the state of the checkboxes
  v.marked <<- c(rep(T,10))
  
  # Render graphs after a new input
  output$dygraphPlot <- renderUI({
    purrr::map(file_data(), \(file) {
      # Silently leave the function in case there's no input file
      # req(file$data)
      
      # Limit length according to the number of attributes of the input file
      length(v.marked) <<- length(file$headers)
      
      # Consider "Time" (always True) as the 1st element
      new_v.marked <- c(TRUE, v.marked)
      
      # Get indices
      cols_to_plot <- which(new_v.marked)
      print(cols_to_plot)
      
      # Plots the graph
      renderDygraph({
        dygraph(dplyr::select(file$data, all_of(cols_to_plot)), 
              main = file$filename) %>% 
        dyRangeSelector() %>%
        dyAxis("x", label = "Time (s)") %>%
        dyAxis("y", label = "Amplitude (mV)")
      })
    })
  })
  
  # Combined reactive expression for file input, data, headers, and filename
  file_data <- reactive({
    req(input$files)
    file_list <- apply(input$files, 1, function(file) {
      data <- read.delim2(file[["datapath"]])
      headers <- colnames(data)[-1]
      filename <- file[["name"]]
      
      # Create an empty dataframe with the specified column names and a single row of "OK" values
      checkboxes <- data.frame(matrix(as.character(icon("ok", lib = "glyphicon")),
                                      ncol = length(headers), nrow = 1))
      
      # Set the column names
      colnames(checkboxes) <- headers
      
      # Set the row name
      rownames(checkboxes) <- filename
      
      # Create dataTable object
      tableID <- paste("table_", filename, sep="")
      DT::dataTableOutput(tableID)
      
      # Create a list of file info
      list(data = data, headers = headers, filename = filename, checkboxes = checkboxes)
    })
  })
  
  output$myTable <- renderUI({
    # Silently leave the function in case there's no input file
    # req(input$files)
    
    purrr::map(file_data(), \(file) {
      # Proxy to update the myTable
      proxy <- dataTableProxy(file$tableID)
      
      # The reactive version of the data
      tableData <- reactiveValues(checkboxes = file$checkboxes)
      
      # Update the table when clicked
      # (selected = clicked)
      # (marked = "OK" sign)
      observeEvent(req(input$myTable_cells_selected), {
        # req(input$files)
        
        # Variable to store selected cells
        cell <- input$myTable_cells_selected[2]
        
        # Invert logic value of the selected cell
        v.marked[cell] <<- !v.marked[cell]
        
        # Debugging
        cat("v.marked: ")
        print(v.marked)
        
        # If one of the cells has been selected
        if(cell) {
          tableData$checkboxes[input$myTable_cells_selected] =
            ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
                   as.character(icon("ok", lib = "glyphicon")), NA)
          
          # Send proxy (no need to refresh whole table)
          replaceData(proxy, tableData$checkboxes)
        }
        
        # Render graphs after a change on the table checkboxes
        output$dygraphPlot <- renderUI({
          purrr::map(file_data(), \(file) {
            # Silently leave the function in case there's no input file
            # req(file$data)
            
            # Limit length according to the number of attributes of the input file
            length(v.marked) <<- length(file$headers)
            
            # Consider "Time" (always True) as the 1st element
            new_v.marked <- c(TRUE, v.marked)
            
            # Get indices
            cols_to_plot <- which(new_v.marked)
            print(cols_to_plot)
            
            # Plots the graph
            renderDygraph({
              dygraph(dplyr::select(file$data, all_of(cols_to_plot)), 
                      main = file$filename) %>% 
                dyRangeSelector() %>%
                dyAxis("x", label = "Time (s)") %>%
                dyAxis("y", label = "Amplitude (mV)")
            })
          })
        })
        
      })
      
      var_name <- paste("output", file$tableID, sep="$")
      
      get(var_name) <- renderDataTable(
        {file$checkboxes},
        # These are options to make the table look like checkboxes
        selection = list(mode = "single", target = 'cell'), 
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          dom = "t", ordering = F
        ),
        escape = F
      )
      
    })
  })
  
  # Sends the "checkbox" table to the output
  # renderDataTable({
  #   
  # 
  #   
  #   
  #   
  # 
  #   # Finally calls the object
  #   checkboxes},
  #   
  #   
  # )
}

shinyApp(ui, server)