library(RPostgres)
library(DBI)


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
    paste(day(d),' ', hour(d),':',minute(d), sep = "")
  )
}

testDone <- function(calibrateur){
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
  
  rep$debut <- formatDate(rep$debut)
  rep$fin <- formatDate(rep$fin)
  
  return(rep)
}

