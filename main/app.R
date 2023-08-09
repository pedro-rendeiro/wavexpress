library(shiny)
library(shinythemes)
library(dygraphs)
library(data.table)
library(DT)

# Good theme options: "lumem", "spacelab", "cerulean"...
# There's an shiny app dedicated to test theme options,
# https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/

ui <- fluidPage(theme = shinytheme("lumen"),
  titlePanel("Wavexpress"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_1", "Choose a txt file:",
                accept = c(".txt")
      ),
      dataTableOutput("myTable")
    ),
    mainPanel(
      dygraphOutput("dygraph_plot_1")
    )
  )
)

server <- function(input, output) {

  # READ AND DISPLAY FILE
  # Read the input file and render the dygraph plot
  output$dygraph_plot_1 <- renderDygraph({
    req(input$file_1)
    
    # Read file as tab separated values
    values <- read.delim2(input$file_1$datapath)  # WITH headers
    # df_1.1 <- read.delim2(input$file_1$datapath, header = FALSE)  # NO headers
    
    headers = colnames(read.delim2(input$file_1$datapath))
    
    cat(values[1,1])
    cat("\n")
    
    cat(headers)
    cat("\n")
    
    values_df <- data.frame(time=values[,1], values[,2]) #criação de um dataframe para armazenamento dos dados
    
    #https://www.rdocumentation.org/packages/dygraphs/versions/1.1.1.6/topics/dyAxis
    dygraph(values_df,main = "EMG Signal") %>% dyRangeSelector() %>%
      dyAxis("x", label = "Time (s)")%>%
      dyAxis("y", label = "Amplitude")
  })
  
  # READ FROM MATRIX CHECKBOX AND UPDATE TABLE
  # The proxy to update the DT
  proxy <- dataTableProxy('myTable')
  
  # The initial data for the checkboxes
  checkboxes <- data.frame(
    Name = c(NA, as.character(icon("ok", lib = "glyphicon"))),
    Time = c(NA, as.character(icon("ok", lib = "glyphicon"))),
    Budget = c(as.character(icon("ok", lib = "glyphicon")), NA),
    row.names = paste("Project", 1:2)
  )
  
  # The reactive version of the data
  tableData <- reactiveValues(checkboxes = checkboxes)
  
  # Update the table when clicked
  observeEvent(req(input$myTable_cells_selected), {
    if(input$myTable_cells_selected[2]) {
      tableData$checkboxes[input$myTable_cells_selected] =
        ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
               as.character(icon("ok", lib = "glyphicon")), NA)
  
      # Send proxy (no need to refresh whole table)
      replaceData(proxy, tableData$checkboxes)
    }
  })
  
  # The "checkbox" table
  output$myTable <- renderDataTable({
    
    checkboxes
    
  }, 
  # These are options to make the table look like checkboxes
  selection = list(mode = "single", target = 'cell'), 
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    dom = "t", ordering = F
  ),
  escape = F)
  
}

shinyApp(ui, server)
