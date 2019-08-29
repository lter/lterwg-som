library(shiny)
library(dplyr)

#runExample("10_download")

#Load your SOM data
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-08-27.csv", as.is=T)
unique(tarball$network)

#Shiny UI ftn
ui <- fluidPage(
  
  # App title ----
  titlePanel("Filtering SOM Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("network_filter", "Filter by network:",
                  choices = c("DIRT", "NutNet", "LTER", "NEON")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("table")
      
    )
    
  )
)

#Shiny server ftn
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    tarball %>% filter(network == input$network_filter)
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
