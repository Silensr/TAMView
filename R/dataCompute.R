library(dplyr)
library(tibble)


getDataTable <- function(mesures, type_test){
  if(type_test == 1) {
    return(
      tablRepeta(mesures)
    )
  }
}


tablRepeta <- function(mesures){
  mes <- mesures %>%
    filter(consigne == 0) %>%
    select(c1) %>%
    add_column(
      c2 = mesures %>%
        filter(consigne != 0) %>%
        pull(c1) 
    ) %>%
    select(c1,c2)
  
  crit <- mes %>%
    summarise(c1 = mean(c1), c2 = mean(c2)) %>%
    bind_rows(
      mes %>% summarise(c1 = sd(c1), c2 = sd(c2))
    ) 
    
  
  mes <- mes %>% bind_rows(crit)
  return(
    DT::datatable(
      {mes},
      rownames = c(paste("Mesure", 1:10), "Moyenne", "Ecart-Type"),
      
      colnames = mesures %>%
        distinct(consigne) %>%
        pull(consigne),
      
      options = list(
        pageLength = 20,
        dom = ''
      )
    )
  )
}