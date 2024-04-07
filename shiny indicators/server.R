
library(shiny)
library(dplyr)

function(input, output, session) {
  output$distPlot <- renderPlot({
    data <- all_data_eng
    
    if (input$region != "Russia") {
      data <- data[data$Region == input$region, ]
    } else {
      # Group by 'Month' and calculate the mean for numeric columns
      data <- data %>% 
        group_by(Month) %>% 
        summarise(across(where(is.numeric), mean, na.rm = TRUE))
    }
    
    # Check if the resulting data frame has no rows
    if (nrow(data) == 0) {
      plot.new()
      title(main = "No data", col.main = "red")
      return()
    }
    
    # Fetch the plot title from the 'indicatorTitles' based on user input
    plotTitle <- indicatorTitles[input$indicator]
    
    # Generate the appropriate plot based on the 'type' of analysis selected by the user
    if (input$type == "Value distribution") {
      boxplot(data[[input$indicator]], main = paste("Distribution of", plotTitle, "in", input$region), col = input$colorInput, ylab = plotTitle)
    } else {
      # Assuming each row corresponds to a consecutive month
      months <- 1:nrow(data)
      plot(months, data[[input$indicator]], type = "l", main = paste(plotTitle, "over time in", input$region), col = input$colorInput, xlab = "Month", ylab = plotTitle, xaxt = 'n')
      points(months, data[[input$indicator]], pch = 19, col = input$colorInput)
      axis(1, at = months, labels = month.abb, las = 2)  # Use abbreviated month names, with labels perpendicular to the axis
    }
  })
}
