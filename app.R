library(shiny)

source('./fromDB.R')


# Interface
ui <- fluidPage(

  # Titre
  titlePanel("Rapport de test TAM"),

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
        choices = 41
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
      )
    ),
    
    mainPanel(
      div(
        h2("Consignes"),
        plotOutput("consignes"),
        h2(Mesures),
        div(id = "Mesures")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$mod_ana,{
    updateSelectInput(session, "ana", choices = getAnalyseurs(input$mod_ana))
  })
  
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
      type = "s"
    )
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
