

standard_con<- function(default=NULL){

#dbpath = "C:/Users/daniel.woodrich/Desktop/database/lab_data_exp.db"
#dbpath = "//161.55.120.117/NMML_AcousticsData/Working_Folders/test database/lab_data.db"
dbpath = Sys.getenv("DBPATH")  

if(dbpath ==""){ #useful when running from
  dontlock=TRUE
  dbpath = default
}else{
  dontlock=FALSE
}

#print(dbpath)

con <- dbConnect(RSQLite::SQLite(),dbpath)
dbClearResult(dbSendQuery(con, "PRAGMA foreign_keys = ON"))
dbClearResult(dbSendQuery(con, "PRAGMA busy_timeout = 60000")) #will attempt to wait for up to one minute before error

if(dontlock==FALSE){
dbClearResult(dbSendQuery(con,"PRAGMA locking_mode = EXCLUSIVE"))
dbClearResult(dbSendQuery(con,"BEGIN EXCLUSIVE"))
}

return(con)
}