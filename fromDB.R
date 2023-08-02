library(RPostgres)
library(DBI)
library(stringr)


connect <- function(){
  
  con <- dbConnect(
    Postgres(),
    host='dublin',
    port=5432,
    user='metro',
    password='metro@dublin',
    dbname='metrologie'
  )
  
  return(con)
}

getModeles <- function(){
  con <- connect()
  
  req <- dbSendQuery(con, "SELECT nom, id_modele FROM tam.modele_analyseur;")
  
  rep <- dbFetch(req)
  
  dbClearResult(req)
  dbDisconnect(con)
  
  vec <- rep$id_modele
  names(vec) <- rep$nom
  
  
  return(vec)
}

getAnalyseurs <- function(modele) {
  con <- connect()
  
  req <- dbSendQuery(con, paste(
    "SELECT designation, id_analyseur FROM tam.analyseur where modele=",
    modele,
    ";"
  ))
  rep <- dbFetch(req)
  
  dbClearResult(req)
  dbDisconnect(con)
  
  vec <- rep$id_analyseur
  names(vec) <- rep$designation
  
  return(vec)
}

getTypes <- function(){
  con <- connect()
  
  req <- dbSendQuery(con, "SELECT designation, id_type_test from tam.type_test")
  
  rep <- dbFetch(req)
  
  dbClearResult(req)
  dbDisconnect(con)
  
  vec <- rep$id_type_test
  names(vec) <- rep$designation
  
  return(vec)
}

getTests <- function(ana_id, type_id){
  con <- connect()
  
  res <- dbSendQuery(
    con, 
    paste(
      "SELECT debut, id_testrealise FROM tam.test_done_view WHERE id_type_test=",
      type_id,
      " and ana1=", ana_id," or ana2=", ana_id, " or ana3=", ana_id, ";",
      sep = ""
    )
  )
  
  rep <- dbFetch(res)
  
  dbClearResult(res)
  dbDisconnect(con)
  
  vec <- rep$id_testrealise
  names(vec) <- rep$debut
  
  return(vec)
}

getConsignesFromTest <- function(test_id){
  con <- connect()
  
  res <- dbSendQuery(
    con,
    paste(
      "SELECT consigne, horodatage FROM tam.consignes_view WHERE id_test_realise=",
      test_id,
      ";",
      sep = ""
    )
  )
  
  rep <- dbFetch(res)
  
  dbClearResult(res)
  dbDisconnect(con)
  
  return(rep)
}

getMesures <- function(id_test, id_analyseur){
  con <- connect()
  
  res <- dbSendQuery(
    con,
    paste(
      "SELECT n_mesure, c1, c2, c3  FROM tam.mesures_view WHERE",
      "test_realise=",id_test,
      "and id_analyseur=",id_analyseur,";"
    )
  )
  
  
  
  rep <- dbFetch(res)
  
  dbClearResult(res)
  dbDisconnect(con)
  
  return(rep)
}
  
# formatDate <- function(d){
#   return(
#     paste(day(d),' ',month(d, label = T),' ', hour(d),':',minute(d), sep = "")
#   )
# }


