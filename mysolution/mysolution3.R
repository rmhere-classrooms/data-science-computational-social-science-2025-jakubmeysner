library(shiny)
library(bslib)
library("lubridate")
library(igraph)

df <- read.csv2("https://bergplace.org/share//out.radoslaw_email_email", skip = 2, sep = " ")[, 1:2]
colnames(df) <- c("from", "to")
g <- graph_from_data_frame(df, directed = TRUE)

g <- simplify(g)
summary(g)

cnt_ij <- as.data.frame(table(df$from, df$to), stringsAsFactors = FALSE)
colnames(cnt_ij) <- c("from", "to", "cnt_ij")
cnt_ij <- cnt_ij[cnt_ij$cnt_ij > 0,]
cnt_i <- aggregate(cnt_ij ~ from, data = cnt_ij, sum)
colnames(cnt_i)[2] <- "cnt_i"
w <- merge(cnt_ij, cnt_i, by = "from")
w$wij <- w$cnt_ij / w$cnt_i
edges <- as_data_frame(g, what = "edges")
E(g)$weight <- w$wij[match(paste(edges$from, edges$to), paste(w$from, w$to))]
strength(g, mode = "out", weights = E(g)$weight)


################
# SHINYAPP WIP #
################

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

    AClog <- read.csv2(url("http://bergplace.org/share/aclevel.txt"), sep = " ")
    names(AClog) <- c("Timestamp", "Napięcie")
    AClog$Napięcie <- as.numeric(AClog$Napięcie)

    AClog <- tail(AClog, n = 3600 * input$hours)

    AClog$Timestamp <- as_datetime(AClog$Timestamp)

    plot(AClog, col = "#007bc2", border = "white",
         xlab = "Czas (UTC)", ylab = "Napięcie (V)", cex = 0.7,
         main = "Historia wartości napięcia")
  })
}

shinyApp(ui = ui, server = server)
