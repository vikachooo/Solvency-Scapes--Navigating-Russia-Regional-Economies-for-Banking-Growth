
library(shiny)

# Assuming 'regions' and 'indicatorTitles' are defined within this file or in a global.R file within the same directory
shinyUI(fluidPage(
  selectInput("region", "Region:", choices = regions, selected = "Russia"),
  selectInput("indicator", "Indicator:", choices = names(indicatorTitles), selected = names(indicatorTitles)[1]),
  selectInput("type", "Type:", choices = c("Value distribution", "Time trend"), selected = "Time trend"),
  textInput("colorInput", "Enter Color Code:", value = "#3498db"),
  plotOutput("distPlot")
))
