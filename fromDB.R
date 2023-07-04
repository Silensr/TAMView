library(RPostgres)
library(DBI)
library(stringr)


con <- dbConnect(
  Postgres(),
  host='dublin',
  port=5432,
  user='metro',
  password='metro@dublin',
  dbname='metrologie'
)

calibrateurs <- function(){
  req <- dbSendQuery(con, "SELECT designation FROM tam.calibrateur")
  
  return(dbFetch(req))
}

formatDate <- function(d){
  return(
    paste(day(d),' ',month(d, label = T),' ', hour(d),':',minute(d), sep = "")
  )
}

testDone <- function(calibrateur){
  if(is.null(calibrateur)) {calibrateur <- 'NAUSINOOS'}
  req <- dbSendQuery(
    con, 
    paste(
      "SELECT * FROM tam.test_done_view WHERE calibrateur='",
      calibrateur,
      "';",
      sep = ""
    )
  )
  
  rep <- dbFetch(req)
  
  return(rep)
}

getConsignes <- function(selectResponse){
  id <- str_split_i(selectResponse, " ", 1)
  
  
  req <- dbSendQuery(
    con,
    paste(
      "SELECT horodatage, consigne FROM tam.consignes_view WHERE id_testrealise=",
      id,
      ";",
      sep = ""
    )
  )
  
  print(dbFetch(req))
}

getConsignes("3 2023-06-30 15:17:54 Répétabilité O3")