library(shiny)
library(dygraphs)
library(data.table)

ui <- fluidPage(
  titlePanel("Interface"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_1", "Choose a txt file:",
                accept = c(".txt")
      ),
      fileInput("file_2", "Choose a txt file:",
                accept = c(".txt")
      )
    ),
    mainPanel(
      dygraphOutput("dygraph_plot_1"),
      dygraphOutput("dygraph_plot_2")
    )
  )
)

server <- function(input, output) {

  # Read the input file and render the dygraph plot
  # FILE 1
  output$dygraph_plot_1 <- renderDygraph({
    req(input$file_1)
    
    # Read the data from the uploaded file
    file_data <- fread(input$file_1$datapath, sep = "\t")
    
    # Lendo o arquivo texto na forma de uma tabela
    df_1.1 <- read.table(input$file_1$datapath, sep = "\t", header = FALSE, dec = ",")
    
    df_1.2 <- data.frame(time=df_1.1[,1], df_1.1[,2]) #criação de um dataframe para armazenamento dos dados
    
    #https://www.rdocumentation.org/packages/dygraphs/versions/1.1.1.6/topics/dyAxis
    dygraph(df_1.2,main = "EMG Signal") %>% dyRangeSelector() %>%
      dyAxis("x", label = "Time (s)")%>%
      dyAxis("y", label = "Amplitude")
  })
  
  output$dygraph_plot_2 <- renderDygraph({
    # FILE 2
    req(input$file_2)
    
    # Read the data from the uploaded file
    file_data <- fread(input$file_2$datapath, sep = "\t")
    
    # Lendo o arquivo texto na forma de uma tabela
    df_2.1 <- read.table(input$file_2$datapath, sep = "\t", header = FALSE, dec = ",")
    
    df_2.2 <- data.frame(time=df_2.1[,1], df_2.1[,2]) #criação de um dataframe para armazenamento dos dados
  
    #https://www.rdocumentation.org/packages/dygraphs/versions/1.1.1.6/topics/dyAxis
    dygraph(df_2.2,main = "EMG Signal") %>% dyRangeSelector() %>%
      dyAxis("x", label = "Time (s)")%>%
      dyAxis("y", label = "Amplitude")
  })
  
}

shinyApp(ui, server)
