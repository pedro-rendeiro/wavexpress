library(shiny)
library(dygraphs)
library(data.table)

ui <- fluidPage(
  titlePanel("Interface"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a txt file:",
                accept = c(".txt")
      )
    ),
    mainPanel(
      dygraphOutput("dygraph_plot")
    )
  )
)

server <- function(input, output) {

  # Read the input file and render the dygraph plot
  output$dygraph_plot <- renderDygraph({
    req(input$file)
    
    # Read the data from the uploaded file
    file_data <- fread(input$file$datapath, sep = "\t")
    
    # Lendo o arquivo texto na forma de uma tabela
    df2 <- read.table(input$file$datapath, sep = "\t", header = FALSE, dec = ",")
    
    df2.1 <- data.frame(time=df2[,1], df2[,2]) #criação de um dataframe para armazenamento dos dados
    
    #https://www.rdocumentation.org/packages/dygraphs/versions/1.1.1.6/topics/dyAxis
    dygraph(df2.1,main = "EMG Signal") %>% dyRangeSelector() %>%
      dyAxis("x", label = "Time (s)")%>%
      dyAxis("y", label = "Amplitude")
  })
}

shinyApp(ui, server)
