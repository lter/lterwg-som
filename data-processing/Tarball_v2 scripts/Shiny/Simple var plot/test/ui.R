#Modified from https://shiny.rstudio.com/gallery/telephones-by-region.html

# Get the tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-04-29.csv")

# Vector of numeric analyte names
num.analytes <- colnames(as.data.frame(select_if(tarball, is.numeric)))
str.analytes <- colnames(as.data.frame(select_if(tarball, is.factor)))

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("SOM PLOT"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput('x', 'Analyte:', choices = num.analytes,
                  selected = "lyr_soc"),
      selectInput('y', 'By:', choices = str.analytes, 
                  selected = "google_dir")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("myPlot")  
    )
    
  )
)
