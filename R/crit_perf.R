library(tidyverse)
library(shiny)

getCritComp <- function(type, data, critPerf) {
  if(type == 5) {
    return(
      createComponents(
        crit_rdt(data),
        critPerf
      )
    )
  }
}

createComponents <- function(critList, critPerf) {
  crit_ui <- tagList(
    h2("Critères de performances")
  )
  
  for(i in critList){
    
    crit <- critPerf %>% filter(i$nom_crit == nom)
    
    tagAppendChild(
      crit_ui,
      div(
        span(paste(crit$intitule, ":")),
        p(i$value), 
        br(),
        span("Critère de performance : "), 
        p(crit$valeur),
        span("Validité :"),
        strong(
          p(
            ifelse(!xor(crit$superieur, i$value >= crit$valeur), "V", "X")
          )
        )
      )
    )
  }
  
  return(crit_ui)
  
}

crit_rdt <- function(data) {
  mes <- data %>%
    remove_rownames() %>%
    select(-n_mesure) %>%
    group_by(ordre) %>%
    summarise(
      m_NO = mean(NO),
      m_NOx = mean(NOx)
    ) %>%
    select(-ordre)
  
  rdt1 <-(slice(mes, 3) - slice(mes, 2)) %>%
    transmute(rdt = m_NOx / m_NO) %>%
    pull(rdt)
  
  rdt1 <- (1 - rdt1) * 100
  
  
  rdt2 <-(slice(mes, 6) - slice(mes, 5)) %>%
    transmute(rdt = m_NOx / m_NO) %>%
    pull(rdt)
  
  rdt2 <- (1 - rdt2) * 100

  return(
    list(
      Ec1 = list(value = rdt1, nom_crit="RDT_four"),
      Ec2 = list(value = rdt2, nom_crit="RDT_four")
    )
  )
}

