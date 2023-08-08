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
      )
    ),
    mainPanel(
      dygraphOutput("dygraph_plot_1")
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
  
}

shinyApp(ui, server)
