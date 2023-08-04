library(dplyr)
library(tibble)

getDataTreated <- function(type, mesures){
  return(
    switch(type,
      Répétabilité = tablRepeta(mesures)
    )
  )
}



tablRepeta <- function(mesures){
  mes <- mesures %>%
    filter(consigne == 0) %>%
    select(c1)
    
  conc <- mes %>%
    add_column(
      c2 = mesures %>%
        filter(consigne != 0) %>%
        pull(c1) 
    )
  print(conc)
  
}