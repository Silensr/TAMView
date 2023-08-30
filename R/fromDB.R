library(RPostgres)
library(DBI)



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
      " and (ana1=", ana_id," or ana2=", ana_id, " or ana3=", ana_id, ");",
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
      " ORDER BY horodatage;",
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
      "SELECT cn1, cn2, cn3, consigne,n_mesure, i_mesure, ordre, id_etape, moment FROM tam.mesures_view WHERE",
      "test_realise=",id_test,
      "AND id_analyseur=",id_analyseur,
      "ORDER BY moment;"
    )
  )
  
  rep <- dbFetch(res)
  
  dbClearResult(res)
  dbDisconnect(con)
  
  return(rep)
}


getGaz <- function(id_analyseur) {
  con <- connect()

  res <- dbSendQuery(
    con,
    paste(
      "select gaz.molecule as gaz from tam.analyseur
    	join tam.modele_analyseur on modele_analyseur.id_modele = modele
    	join tam.lien_modele_gaz on lien_modele_gaz.id_modele = modele_analyseur.id_modele
    	join tam.gaz on gaz.id_gaz = lien_modele_gaz.id_gaz
    	where id_analyseur =", id_analyseur, ";"
    )
    
  )

  rep <- dbFetch(res)

  dbClearResult(res)
  dbDisconnect(con)
  
  return(rep)
}

getCriteres <- function(id_analyseur, id_type_test) {
  con <- connect()
  
  gaz <- getGaz(id_analyseur)$gaz
  
  
  rep <- dbSendQuery(
    con,
    paste(
      "select nom, valeur, superieur, norme_nom, intitule from tam.critere_view",
      "where gaz in ('", paste0(gaz,  collapse = "','"),"')",
      "and type_test =", id_type_test,
      ";"
    )
  )
  
  res <- dbFetch(rep)
  dbClearResult(rep)
  
  dbDisconnect(con)
  
  return(res)
}

  