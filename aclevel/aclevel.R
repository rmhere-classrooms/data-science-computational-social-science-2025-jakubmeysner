library(shiny)
library(bslib)
library("lubridate") 

# Define UI for app that draws a history of AC in a flat ----
ui <- page_sidebar(
  # App title ----
  title = "Napięcie w mieszkaniu",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of hours ----
    sliderInput(
      inputId = "hours",
      label = "Liczba ostatnich godzin:",
      min = 1,
      max = 120,
      value = 24
    )
  ),
  # Output: AC level plot ----
  plotOutput(outputId = "acPlot")
)

# Define server logic required to draw a plot ----
server <- function(input, output) {
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$hours) change
  # 2. Its output type is a plot
  
  output$acPlot <- renderPlot({
    
    AClog <- read.csv2(url("http://bergplace.org/share/aclevel.txt"), sep=" ")
    names(AClog) <- c("Timestamp", "Napięcie")
    AClog$Napięcie <- as.numeric(AClog$Napięcie)
    
    AClog <- tail(AClog, n=3600*input$hours)
    
    AClog$Timestamp <- as_datetime(AClog$Timestamp)
    
    plot(AClog, col = "#007bc2", border = "white",
         xlab = "Czas (UTC)", ylab="Napięcie (V)", cex=0.7,
         main = "Historia wartości napięcia")
  })
}

shinyApp(ui = ui, server = server)