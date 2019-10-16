#Modified from https://shiny.rstudio.com/gallery/telephones-by-region.html



# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)


# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    barplot(WorldPhones[,input$region]*1000, 
            main="title", #input$region,
            ylab="Number of Telephones",
            xlab="Year")
  })
}



