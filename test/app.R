library(shiny)
library(DT)

# BASIC CHECKBOX 1
ui1 <- fluidPage(
  checkboxGroupInput("variable", "Variables to show:",
                     c("Cylinders" = "cyl",
                       "Transmission" = "am",
                       "Gears" = "gear")),
  tableOutput("data")
)

server1 <- function(input, output, session) {
  output$data <- renderTable({
    mtcars[, c("mpg", input$variable), drop = FALSE]
  }, rownames = TRUE)
}

# BASIC CHECKBOX 2
ui2 <- fluidPage(
  checkboxGroupInput("icons", "Choose icons:",
                     choiceNames =
                       list(icon("calendar"), icon("bed"),
                            icon("cog"), icon("bug")),
                     choiceValues =
                       list("calendar", "bed", "cog", "bug")
  ),
  textOutput("txt")
)

server2 <- function(input, output, session) {
  output$txt <- renderText({
    icons <- paste(input$icons, collapse = ", ")
    paste("You chose", icons)
  })
}


# MATRIX CHECKBOX 1,
# https://community.rstudio.com/t/shiny-app-matrix-checkbox/108251/3
ui3 <- fluidPage(
  dataTableOutput("myTable")
)

server3 <- function(input, output, session) {
  
  #The proxy to update the DT
  proxy <- dataTableProxy('myTable')
  
  #The initial data for the checkboxes
  checkboxes = data.frame(
    Name = c(NA, as.character(icon("ok", lib = "glyphicon"))),
    Time = c(NA, as.character(icon("ok", lib = "glyphicon"))),
    Budget = c(as.character(icon("ok", lib = "glyphicon")), NA),
    row.names = paste("Project", 1:2)
  )
  
  #The reactive version of the data
  tableData = reactiveValues(checkboxes = checkboxes)
  
  #Update the table when clicked
  observeEvent(req(input$myTable_cells_selected), {
    tableData$checkboxes[input$myTable_cells_selected] =
      ifelse(is.na(tableData$checkboxes[input$myTable_cells_selected]),
             as.character(icon("ok", lib = "glyphicon")), NA)
    
    #Send proxy (no need to refresh whole table)
    replaceData(proxy, tableData$checkboxes)
    
  })
  
  #The "checkbox" table
  output$myTable = renderDataTable({
    
    checkboxes
    
  }, 
  #These are options to make the table look like checkboxes
  selection = list(mode = "single", target = 'cell'), 
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    dom = "t", ordering = F
  ),
  escape = F)
  
}

shinyApp(ui3, server3)