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
    titlePanel("Dashboard data TAM"),

    # Menu
    sidebarLayout(
        # Sélection de l'analyseur
        sidebarPanel(
          selectInput(
            inputId = 'cal',
            label = 'Calibrateur',
            choices = calibrateurs()$designation,
            selected = 'NAUSINOOS'
          ),
          
          # Sélection du test réalisé
          selectInput(
            inputId = 'test_real',
            label = 'Test réalisé',
            choices = paste(
              testDone('NAUSINOOS')$debut,
              testDone('NAUSINOOS')$test
            )
          ),
          
          checkboxInput(
            inputId = 'curveType',
            label = 'Inverser la courbe'
          )
        ),

        # Graphe des consignes envoyées
        mainPanel(plotOutput('consignes'))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Si on change de calibrateur
  observeEvent(input$cal, {
    
    # On charge les tests qui y sont associés
    calibrateur <- input$cal
    
    testChoices <- paste(
      testDone(calibrateur)$id_testrealise,
      testDone(calibrateur)$debut,
      testDone(calibrateur)$test
    )
    

    if(length(testChoices) == 0){
      testChoices <- "Pas de test"
    }
    
    updateSelectInput(session, 'test_real',
      choices = testChoices)
    
  })
  
  
  output$consignes <- renderPlot(
    plot(
      getConsignes(input$test_real),
      type = "s"
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
