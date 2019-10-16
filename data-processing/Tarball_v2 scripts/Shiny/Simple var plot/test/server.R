library(dplyr)
library(ggplot2)
library(tidyverse)

# Get the tarball
tarball <- read.csv("C:/Users/drkpi/Desktop/tmp/SOM/somCompositeData_2019-04-29.csv")

# Vector of numeric analyte names
num.analytes <- colnames(as.data.frame(select_if(tarball, is.numeric)))
str.analytes <- colnames(as.data.frame(select_if(tarball, is.factor)))

#Try to setup Shiny app to change lyr_soc to different var with drop down list
ggplot(tarball, aes(y=lyr_soc, x=google_dir)) + geom_point() + theme(axis.text.x = element_text(angle = 90))


# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$myPlot <- renderPlot({
    
    # Render a plot here
    ggplot(tarball, aes_string(y=input$y, x=input$x)) + geom_point() + theme(axis.text.x = element_text(angle = 90))
    
  })
}