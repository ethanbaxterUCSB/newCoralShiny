#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Newton's Law of Cooling, dT/dt = -h(T-Te)"),

    # Sidebar with a slider inputs
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "h1", label = "h1 [0,1]:", min = 0, max = 1, value = 0.01, step = 0.001),
            numericInput(inputId = "h2", label = "h2 [0,1]:", min = 0, max = 1, value = 0.00, step = 0.001),
            sliderInput(inputId = "time", label = "model length:", min = 1, max = 1000, value = 500),
            sliderInput(inputId = "T_e", label = "Environment temp:", min = 0, max = 1000, value = 100),
            sliderInput(inputId = "T_o1", label = "Initial object1 temp:", min = 0, max = 1000, value = 150),
            sliderInput(inputId = "T_o2", label = "Initial object2 temp:", min = 0, max = 1000, value = 150)
        ),
        mainPanel(
            plotOutput("plot"),
            dataTableOutput("temps")
        )
    )
)

server <- function(input, output) {
    #model for newton's law of cooling
    DE <- function(h=h, T_e=T_e, T_o=T_o) {-1*h*(T_o-T_e)}
    
    #Generate frame
    temps <- reactive({
        time <- c(1:input$time)
        frame <- data.frame(time=time, T_o1=input$T_o1, T_o2=input$T_o2)
        
        for(t in 2:input$time){
            prevTemp1 <- frame$T_o1[t-1]
            prevTemp2 <- frame$T_o2[t-1]
            frame$T_o1[t] <- prevTemp1+DE(input$h1, input$T_e, prevTemp1)
            frame$T_o2[t] <- prevTemp2+DE(input$h2, input$T_e, prevTemp2)
        }
        frame
    })
    
    #Output plot
    output$plot <- renderPlot({
        plot(temps()$time,temps()$T_o1, type="l", col="blue")
        lines(temps()$time, temps()$T_o2, col="red")
        })
    
    #Output frame
    output$temps <- renderDataTable(temps())
}

# Run the application 
shinyApp(ui = ui, server = server)
