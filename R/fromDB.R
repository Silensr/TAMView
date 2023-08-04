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

getConsignes <- function(test_id){
  con <- connect()
  
  res <- dbSendQuery(
    con,
    paste(
      "SELECT consigne, horodatage FROM tam.consignes_view WHERE id_testrealise=",
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
      "SELECT c1, c2, c3, consigne, i_mesure, ordre, id_etape FROM tam.mesures_view WHERE",
      "test_realise=",id_test,
      "and id_analyseur=",id_analyseur,";"
    )
  )
  
  rep <- dbFetch(res)
  
  dbClearResult(res)
  dbDisconnect(con)
  
  return(rep)
}


getGaz <- function(id_analyser) {
  con <- connect()

  res <- dbSendQuery(
    con,
    "select gaz.molecule as gaz from tam.analyseur
  	join tam.modele_analyseur on modele_analyseur.id_modele = modele
  	join tam.lien_modele_gaz on lien_modele_gaz.id_modele = modele_analyseur.id_modele
  	join tam.gaz on gaz.id_gaz = lien_modele_gaz.id_gaz
  	where id_analyseur =", id_analyser, ";"
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


