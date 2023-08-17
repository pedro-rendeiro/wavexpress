library(shiny)
library(shinythemes)
library(dygraphs)
library(data.table)
library(DT)
library(tidyverse)

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
      # Display table with checkboxes
      dataTableOutput("myTable")
    ),
    mainPanel(
      # UI Test
      uiOutput("moreControls")
    )
  )
)

server <- function(input, output) {
  # Proxy to update the myTable
  proxy <- dataTableProxy('myTable')
  
  # Render graphs after a new input
  output$moreControls <- renderUI({
    graphs <- purrr::map(file_data(), \(file) {
      # Silently leave the function in case there's no input file
      req(file$data)

      # Plots the graph
      renderDygraph({
        dygraph(file$data, 
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
    file_list <- apply(input$files, 1, function(read) {
      data <- read.delim2(read[["datapath"]])
      headers <- colnames(data)[-1]
      filename <- read[["name"]]
      list(data = data, headers = headers, filename = filename)
    })
  })
  
  
  # Sends the "checkbox" table to the output
  output$myTable <- renderDataTable({
    # Silently leave the function in case there's no input file
    req(input$files)
    
    # Create an empty dataframe with the specified column names and a single row of "OK" values
    checkboxes <- data.frame(matrix(as.character(icon("ok", lib = "glyphicon")), 
                                    ncol = length(file_data()[[1]]$headers), nrow = 1))
    
    # Set the column names
    colnames(checkboxes) <- file_data()[[1]]$headers

    # Set the row name
    rownames(checkboxes) <- file_data()[[1]]$filename
    
    # The reactive version of the data
    tableData <- reactiveValues(checkboxes = checkboxes)
    
    # If a cell is selected, enter here
    observeEvent(req(input$myTable_cells_selected), {
      req(input$files)
      
      # Variable to store selected cells
      cell <- input$myTable_cells_selected[2]

      tableData$checkboxes[input$myTable_cells_selected] =
        ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
               as.character(icon("ok", lib = "glyphicon")), NA)

      # Send proxy (no need to refresh whole table)
      replaceData(proxy, tableData$checkboxes)
      
      control_list <- purrr::map_lgl(as.character(tableData$checkboxes[1,]), \(var) {
        ifelse(is.na(var), FALSE, TRUE)
      })
      
      # Render graphs after a change on the table checkboxes
      output$moreControls <- renderUI({
        graphs <- purrr::map(file_data(), \(file) {
          # Silently leave the function in case there's no input file
          req(file$data)
          
          # Get indices
          cols_to_plot <- which(c(TRUE, control_list))
          
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
  
    # Finally calls the object
    checkboxes},
    
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