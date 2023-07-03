#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source('./fromDB.R')
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = 'cal',
            label = 'Calibrateur',
            choices = calibrateurs()$designation
          ),
          selectInput(
            inputId = 
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(h1("Hello Shiny"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
