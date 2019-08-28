#Modified from https://shiny.rstudio.com/gallery/telephones-by-region.html

# Get the tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-04-29.csv")

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Title"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("analyte", "Analyte:", 
                  choices=colnames(tarball)),
      hr(),
      helpText("info")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("myPlot")  
    )
    
  )
)
