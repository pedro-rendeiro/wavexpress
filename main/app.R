library(shiny)
library(shinythemes)
library(dygraphs)
library(data.table)
library(DT)
library(dplyr)
library(purrr)

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
      # dataTableOutput("controlTable")
      uiOutput("controlTable")
    ),
    mainPanel(
      # UI Test
      uiOutput("dgPlots")
    )
  )
)

server <- function(input, output) {
  
  
  # Combined reactive expression for file input, data, headers, and filename
  file_data <- reactive({
    req(input$files)
    file_list <- apply(input$files, 1, function(read) {
      data <- read.delim2(read[["datapath"]])
      headers <- colnames(data)[-1]
      filename <- read[["name"]]
      checkboxes <- data.frame(matrix(as.character(icon("ok", lib = "glyphicon")), 
                        ncol = length(headers), nrow = 1))
      colnames(checkboxes) <- headers
      rownames(checkboxes) <- filename
      tabledata <- reactiveValues(checkboxes = checkboxes)
      
      # Proxy to update the controlTable
      dataTableOutput(filename)
      proxy <- dataTableProxy(filename)
      
      
      list(data = data, headers = headers, filename = filename, 
           checkboxes = checkboxes, tabledata = tabledata, proxy = proxy)
    })
  })
  
  # Render graphs after a new input
  output$dgPlots <- renderUI({
    purrr::map(file_data(), \(file) {
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
  
  # Sends the "checkbox" table to the output
  output$controlTable <- renderUI({
    # Silently leave the function in case there's no input file
    # req(input$files)
    
    print("Hey! :D")
    
    purrr::map(file_data(), \(file) {
      # If a cell is selected, enter here
      observeEvent(req(input$controlTable_cells_selected), {
        # Variable to store selected cells
        cell <- input$controlTable_cells_selected[2]
  
        file$tabledata$checkboxes[input$controlTable_cells_selected] =
          ifelse(is.na(file$tabledata$checkboxes[input$controlTable_cells_selected]),
                 as.character(icon("ok", lib = "glyphicon")), NA)
  
        # Send proxy (no need to refresh whole table)
        replaceData(file$proxy, file$tabledata$checkboxes)
        
        control_list <- purrr::map_lgl(as.character(file$tabledata$checkboxes[1,]), \(var) {
          ifelse(is.na(var), FALSE, TRUE)
        })
        
        # Render graphs after a change on the table checkboxes
        output$dgPlots <- renderUI({
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
      
      renderDataTable({
        file$checkboxes},
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
}

shinyApp(ui, server)