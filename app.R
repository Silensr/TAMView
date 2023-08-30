library(shiny)
library(ggplot2)


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
      h2("Résultats"),
      div(
        id = "resultat",
        tabsetPanel(
          type="tabs",
          tabPanel(
            "Tableau des mesures",
            DTOutput("mesures")
          ),
          tabPanel(
            "Mesures",
            plotOutput("mesuresPlot")
          ),
          tabPanel(
            "Consignes",
            h3("Consignes éxecutées"),
            plotOutput("consignes")
          )
        ),
        uiOutput("crit")
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
  
  # Tracé des consignes
  output$consignes <- renderPlot(
    ggplot(getConsignes(input$test), aes(horodatage, consigne)) +
      geom_step()
  )
  
  #Visualisation des critères de performances
  observeEvent(input$test, {
    output$crit <- renderUI({
      
      ui = getCritComp(
        input$type,
        get_values(
          getMesures(
            input$test,
            input$ana
          ),
          input$type
        ),
        getCriteres(
          input$ana,
          input$type
        )
      )
    })
  })
  
  # Tableau de mesure choisis
  output$mesures <- renderDataTable(
    getDataTable(
      getMesures(
        input$test,
        input$ana
      ),
      input$type
    )
  )
  
  output$mesuresPlot <- renderPlot(
    ggplot(
      getMesures(input$test, input$ana) %>%
        pivot_longer(
          cols = starts_with("cn"),
          names_to = "canal",
          values_to = "mesure"
        ),
      aes(moment, mesure)
    ) + geom_point(aes(colour = factor(canal)))
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
