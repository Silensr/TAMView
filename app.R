library(shiny)


# Interface
ui <- fluidPage(

  # Titre
  titlePanel("Résultats de test TAM"),

  # Menu
  sidebarLayout(
    
    sidebarPanel(
      
      # Modèles analyseurs
      selectInput(
        inputId = 'mod_ana',
        label = 'Modèle analyseur',
        selectize = F,
        choices = getModeles()
      ),

      # Analyseurs
      selectInput(
        inputId = 'ana',
        label = 'Analyseur',
        selectize = F,
        choices = 41
      ),

      # Type des tests
      selectInput(
        inputId = 'type',
        label = 'Type de test',
        selectize = F,
        choices = getTypes()
      ),
      
      # Test à sélectionner
      selectInput(
        inputId = 'test',
        label = 'Test séléctionné',
        selectize = F,
        choices = 2
      )
    ),
    
    mainPanel(
      div(
        id = "resultat",
        # h2("Consignes"),
        # plotOutput("consignes"),
        h2("Mesures"),
        DT::DTOutput("mesures"),
        div(id = 'crit')
      )
    )
  )
)

server <- function(input, output, session) {
  
  #Changement de la liste des analyseurs en fonction du modèle
  observeEvent(input$mod_ana,{
    updateSelectInput(session, "ana", choices = getAnalyseurs(input$mod_ana))
  })
  
  #Changement de la liste des tests en fonction 
  observeEvent(input$ana, {
    updateSelectInput(session, 'test', choices = getTests(input$ana, input$type))
  })
  
  observeEvent(input$type, {
    updateSelectInput(session, 'test', choices = getTests(input$ana, input$type))
  })
  
  output$consignes <- renderPlot(
    plot(
      getConsignes(input$test)$horodatage,
      getConsignes(input$test)$consigne,
      type = "s",
      xlab = "Horodatage",
      ylab = "Concentration"
    )
  )
  
  output$mesures <- DT::renderDataTable(
    getDataTable(
      getMesures(
        input$test,
        input$ana
      ),
      input$type
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
