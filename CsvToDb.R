library(DBI)
library(RPostgres)


LinO3 <- read.csv2('LinO3.csv')

LinO3 <- subset(LinO3, !(Position %in% c("A","S")))

LinO3$datetime <- lubridate::dmy_hm(LinO3$datetime)

con <- dbConnect(
  Postgres(),
  host="dublin",
  port=5432,
  dbname="metrologie",
  user="metro",
  password="metro@dublin"
)

plot(LinO3$datetime, LinO3$c1)

mes <- c()

for(i in 1:nrow(LinO3)) {
  mes <- c(mes,as.numeric(sub("M","", LinO3[i,"Position"])))
}

LinO3$mes <- mes

LinO3$Phase <- factor(LinO3$Phase)

levels(LinO3) <- c(3,4,5,6,7,8)

for(i in 1:nrow(LinO3)){
  dbSendQuery(
    con,
    paste(
      "INSERT INTO tam.mesure VALUES(",
      paste(i,LinO3[i,'c1'], LinO3[i,'c2'],LinO3[i,'c3'], LinO3[i,'Cycle'],LinO3[i,'Phase'], 18,  sep = ','),
      ",",
      "'",
      LinO3[i,'datetime'],
      "'",
      ");",
      sep = ""
    )
  )
}

dbDisconnect(con)