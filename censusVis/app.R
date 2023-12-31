library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with
               information from the 2010 US Census."),
      
      selectInput("select", "Choose a variable to display", 
                  choices = list("Percent White" = 1, 
                                 "Percent Black" = 2,
                                 "Percent Hispanic" = 3, 
                                 "Percent Asian" = 4),
                  selected = 1),
      
      sliderInput("slider1", "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)),
    ),
    mainPanel()
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)