library(dplyr)
library(tidyr)
library(tibble)
library(DT)


# Cette fonction pour obtenir les tableaux en composants shiny
getDataTable <- function(mesures, type_test) {
  if(type_test == 1) {
    return(tablRepeta(mesures))
  } else if(type_test == 3) {
    return(tabl_linea(mesures))
  } else if(type_test == 5) {
    return(
      datatable(
        tabl_rdt(mesures),
        options = list(dom = "ltp")
      )
    )
  }
}

# Celle-ci pour obtenir le dataframe
get_values <- function(mesures, type_test) {
  if(type_test == 1) {
    return(tablRepeta(mesures))
  } else if(type_test == 3) {
    return(tabl_linea(mesures))
  } else if(type_test == 5) {
    return(tabl_rdt(mesures))
  }
}


#Cette fonction prépare les tables en 
prep_table <- function(data) {
  return(
    data %>% 
      group_by(ordre, n_mesure) %>%
      mutate_at(c("cn1","cn2","cn3"), ~na_if(., 0)) %>%
      summarise(
        m1 = mean(cn1, na.rm = T),
        m2 = mean(cn2, na.rm = T),
        m3 = mean(cn3, na.rm = T)
      )
  )
}

# Traitement pour le tableau de répétabilité
tablRepeta <- function(mesures) {
  mes <- mesures %>%
    filter(consigne == 0) %>%
    select(cn1) %>%
    add_column(
      cn2 = mesures %>%
        filter(consigne != 0) %>%
        pull(cn1) 
    ) %>%
    select(cn1,cn2)
  
  crit <- mes %>%
    summarise(cn1 = mean(cn1), cn2 = mean(cn2)) %>%
    bind_rows(
      mes %>% summarise(
        cn1 = round(sd(cn1), 2), 
        cn2 = round(sd(cn2), 2)
      )
    ) 
    
  
  mes <- mes %>% bind_rows(crit)
  return(
    datatable(
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

# Traitement pour le tableau de linéarité
tabl_linea <- function(mesures) {
  mes <- mesures %>%

    # On crée des noms de ligne pour les mesures
    mutate(nom = paste("Mesure", n_mesure, sep = "")) %>%
    
    # On ne sélectionne que ce qui nous intéresse
    select(nom, consigne, cn1) %>%
    
    # On pivote le tableau, avec les consignes en guise de noms de lignes.
    # On ne les convertie pas en row_names tout de suite, on a des calculs à faire
    pivot_wider(
      names_from = nom,
      values_from = cn1,
    ) %>%
    
    # Calcul des moyennes et des écart-types pour chaque consigne
    rowwise() %>%
    mutate(
      mu = mean(c_across(-1)),
      sd = sd(c_across(-1))
    ) 
  
  # Calcul des résidus
  res <- abs(resid(lm(consigne~mu, data = mes)))
  
  # Calcul des résidus relatifs
  mes <- mes %>% 
    cbind(Résidus = res) %>%
    
    # On calcul les résidus relatifs sauf pour 0
    mutate(Résidus_relatifs= ifelse(consigne == 0, NA, 100 * res/mu)) %>%
    remove_rownames() %>%
    column_to_rownames(var = "consigne") %>%
    t()
  
  datat <- datatable(
    mes,
    rownames = c(
      paste("Mesure", 1:5),
      "Moyenne",
      "Ecart-Type",
      "Résidu",
      "Résidu relatif"
    ),
    options = list(
      dom = ''
    )
  ) %>% formatRound(columns = 1:6) 
    
  
  
  # On passe le dataframe dans DT avant de le renvoyer
  return(
    datat
  )
}

# Traitement pour le tableau de rendement de conversion
tabl_rdt <- function(mesures) {
  return(
    mesures %>%
      prep_table() %>%
      mutate(nom = paste("Mesure", ordre, n_mesure, sep="_")) %>%
      column_to_rownames(var = "nom") %>%
      mutate(
        NO = round(m1, 3),
        NOx = round(m2, 3),
        NO2 = round(m3, 3)
      ) %>%
      select(-m1, -m2, -m3)
  )
}
