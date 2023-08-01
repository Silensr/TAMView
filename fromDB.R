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
  
# formatDate <- function(d){
#   return(
#     paste(day(d),' ',month(d, label = T),' ', hour(d),':',minute(d), sep = "")
#   )
# }


