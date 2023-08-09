library(shiny)
library(shinythemes)
library(dygraphs)
library(data.table)

# Good theme options: "lumem", "spacelab", "cerulean"...
# There's an shiny app dedicated to test theme options,
# https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/

ui <- fluidPage(theme = shinytheme("lumen"),
  titlePanel("Interface"),
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
    
    # Read the data from the uploaded file
    file_data <- fread(input$file_1$datapath, sep = "\t")
    
    # Read file as tab separated values WITH headers
    df_1.1 <- read.delim2(input$file_1$datapath)
    # Read file as tab separated values WITHOUT headers
    # df_1.1 <- read.delim2(input$file_1$datapath, header = FALSE)
    
    df_1.2 <- data.frame(time=df_1.1[,1], df_1.1[,2]) #criação de um dataframe para armazenamento dos dados
    
    #https://www.rdocumentation.org/packages/dygraphs/versions/1.1.1.6/topics/dyAxis
    dygraph(df_1.2,main = "EMG Signal") %>% dyRangeSelector() %>%
      dyAxis("x", label = "Time (s)")%>%
      dyAxis("y", label = "Amplitude")
  })
  
  # READ FROM MATRIX CHECKBOX AND UPDATE TABLE
  # The proxy to update the DT
  proxy <- dataTableProxy('myTable')
  
  # The initial data for the checkboxes
  checkboxes = data.frame(
    Name = c(NA, as.character(icon("ok", lib = "glyphicon"))),
    Time = c(NA, as.character(icon("ok", lib = "glyphicon"))),
    Budget = c(as.character(icon("ok", lib = "glyphicon")), NA),
    row.names = paste("Project", 1:2)
  )
  
  # The reactive version of the data
  tableData = reactiveValues(checkboxes = checkboxes)
  
  # Update the table when clicked
  observeEvent(req(input$myTable_cells_selected), {
    tableData$checkboxes[input$myTable_cells_selected] =
      ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
             as.character(icon("ok", lib = "glyphicon")), NA)
    
    # Send proxy (no need to refresh whole table)
    replaceData(proxy, tableData$checkboxes)
    
  })
  
  # The "checkbox" table
  output$myTable = renderDataTable({
    
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
