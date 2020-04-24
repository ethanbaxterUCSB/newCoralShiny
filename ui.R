library(shiny)

#Create UI
ui <- fluidPage(
  #Title
  titlePanel("New Coral Shiny App"),
  sidebarLayout(
    #Run Button
    sidebarPanel(actionButton(inputId="run", label="Run")),
    mainPanel(dataTableOutput("parsTest"))
  )
)