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


# Interface
ui <- fluidPage(

  # Titre
  titlePanel("Traitement de données TAM"),

  # Menu
  sidebarLayout(
    
    sidebarPanel(
      
      #Modèles analyseurs
      selectInput(
        inputId = 'mod_ana',
        label = 'Modèle analyseur',
        selectize = F,
        choices = getModeles()
      ),
      
      #Analyseurs
      selectInput(
        inputId = 'ana',
        label = 'Analyseur',
        selectize = F,
        choices = 'Analyseurs'
      ),
      
      #Type des tests
      selectInput(
        inputId = 'type',
        label = 'Type de test',
        selectize = F,
        choices = getTypes()
      ),
      
      #Test à sélectionner
      selectInput(
        inputId = 'test',
        label = 'Test séléctionné',
        selectize = F,
        choices = 'Test'
      ),
    ),
    
    mainPanel(
      textOutput(outputId = "test")
    )
  )

  # Graphe des consignes envoyées
    
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$mod_ana,{
    updateSelectInput(session, "ana", choices = getAnalyseurs(input$mod_ana))
  })
  
  output$test <- renderText({input$mod_ana})
}

# Run the application 
shinyApp(ui = ui, server = server)
