library(tidyverse)


getDataTable <- function(mesures, type_test){
  if(type_test == 1) {
    return(tablRepeta(mesures))
  } else if(type_test == 3) {
    return(tabl_linea(mesures))
  }
}

new_round <- function(x){
  return(round(x, digits = 2))
}

prep_table <- function(data){
  return(
    data %>% 
      group_by(consigne) %>%
      bind_cols(
        data %>%
          group_by(consigne, i_mesure) %>%
          transmute(
            m1 = mean(c1),
            m2 = mean(c2),
            m3 = mean(c3)
          )
      ) %>% 
      rename(consigne = 'consigne...4', i_mesure = 'i_mesure...6') %>%
      select(-'consigne...9',-'i_mesure...10')
      
  )
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
      mes %>% summarise(
        c1 = round(sd(c1), 2), 
        c2 = round(sd(c2), 2)
      )
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


tabl_linea <- function(mesures) {
  mes <- mesures %>%

    # On crée des noms de ligne pour les mesures
    mutate(nom = paste("Mesure", n_mesure, sep = "")) %>%
    
    # On ne sélectionne que ce qui nous intéresse
    select(nom, consigne, c1) %>%
    
    # On pivote le tableau, avec les consignes en guise de noms de lignes.
    # On ne les convertis pas en row_names tout de suite, on a des calculs à faire
    pivot_wider(
      names_from = nom,
      values_from = c1,
    ) %>%
    
    # Calcul des moyennes et des écart-types pour chaque consigne
    rowwise() %>%
    mutate(
      mu = mean(c_across(-1)),
      sd = sd(c_across(-1))
    ) 
  
  # Calcul des résidus
  res <- resid(lm(consigne~mu, data = mes))
  
  # J'ai choisis d'utiliser car mutate ne sembalit pas marcher
  mes <- mes %>% 
    cbind(Résidus = res) %>%
    mutate(Résidus_relatifs= 100 * res/mu) %>%
    remove_rownames() %>%
    column_to_rownames(var = "consigne") %>%
    t() %>%
    mutate_if(is.numeric(), round, digits=3)
  
  # On passe le dataframe dans DT avant de le renvoyer
  return(
    DT::datatable(
      mes,
      rownames = c(
        paste("Mesure", 1:5),
        "Moyenne",
        "Ecart-Type",
        "Résidu",
        "Résidu relatif"
      ),
      options = list(
        dom = '',
        digits = 2
      )
    )
  )
}


