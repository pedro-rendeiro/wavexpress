library(shiny)
library(shinythemes)
library(dygraphs)
library(data.table)
library(DT)
library(tidyverse)

# Load shared data and function from global.R
source("global.R")

# Good theme options: "lumem", "spacelab", "cerulean"...
# There's an shiny app dedicated to test theme options,
# https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/

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

server <- function(input, output) {
  # Sends the graph object to the output
  output$dygraph_plot_1 <- renderDygraph({
    # Silently leave the function in case there's no input file
    req(input$file_1)

    # Read file as tab separated values
    data <- read.delim2(input$file_1$datapath)  # Skip headers

    # Extract the headers (variable names)
    col_names <- colnames(read.delim2(input$file_1$datapath))

    headers <- col_names[-1]

    # Limit length acording to number of attributes of the input file
    length(v.marked) <<- length(headers)

    # Debugging
    cat("v.marked:", v.marked, "\n")

    # Consider "Time" (always True) as the 1st element
    new_v.marked <- c(TRUE, v.marked)

    # Get indices
    cols_to_plot <- which(new_v.marked)
    print(cols_to_plot)

    # Plots the graph
    dygraph(dplyr::select(data, all_of(cols_to_plot)), main = "EMG Signal") %>% dyRangeSelector() %>%
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
    # (selected = clicked)
    # (marked = "OK" sign)
    observeEvent(req(input$myTable_cells_selected), {
      req(input$file_1)
      
      # Varible to store selected cells
      cell <- input$myTable_cells_selected[2]
      
      # Invert logic value of the selected cell
      v.marked[cell] <<- !v.marked[cell]
      
      # Debugging
      print(v.marked)
      
      # If one of the cells has been selected
      if(cell) {
        tableData$checkboxes[input$myTable_cells_selected] =
          ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
                 as.character(icon("ok", lib = "glyphicon")), NA)
        
        # Send proxy (no need to refresh whole table)
        replaceData(proxy, tableData$checkboxes)
      }
      
      output$dygraph_plot_1 <- renderDygraph({
        # Silently leave the function in case there's no input file
        req(input$file_1)
        
        # Read file as tab separated values
        data <- read.delim2(input$file_1$datapath)  # Skip headers
        
        # Extract the headers (variable names)
        col_names <- colnames(read.delim2(input$file_1$datapath))
        
        headers <- col_names[-1]
        
        # Limit length acording to number of attributes of the input file
        length(v.marked) <<- length(headers)
        
        # Debugging
        cat("v.marked:", v.marked, "\n")
        
        # Consider "Time" (always True) as the 1st element
        new_v.marked <- c(TRUE, v.marked)
        
        # Get indices
        cols_to_plot <- which(new_v.marked)
        print(cols_to_plot)
        
        # Plots the graph
        dygraph(dplyr::select(data, all_of(cols_to_plot)), main = "EMG Signal") %>% dyRangeSelector() %>%
          dyAxis("x", label = "Time (s)")%>%
          dyAxis("y", label = "Amplitude (mV)")
      })
      
    })
  
    # Finally calls the object
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