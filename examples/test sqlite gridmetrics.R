if(F){
  library(RSQLite)

  #create sqlite database
  dir_metrics="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\grid_metrics_sql"
  db <- dbConnect(SQLite(), dbname=file.path(dir_metrics,"2017_June_08.db"),sychronous = "on")
  RSQLite::dbClearResult(RSQLite::dbSendQuery(db, "PRAGMA journal_mode=WAL;"))
  #dbDisconnect(db)

  #load a gridmetrics file
  setwd("C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\grid_metrics_sql")
  files=list.files(pattern="csv$",full.names=F)

  tab_out="gridmetrics_temp"

  for(i in 1:length(files)){

    dati=read.csv(files[i])
    dbWriteTable(db,tab_out,dati,append=T)

  }


  library(parallel)
  clus=makeCluster(4)

  f_write=function(x,tab_out,lock.name){

    ll = lock(lock.name)

    dati=read.csv(x)
    dbWriteTable(db,tab_out,dati,append=T)

    unlock(ll)
  }
  f_write2=function(x,tab_out,lock.name){

    #ll = lock(lock.name)

    dati=read.csv(x)
    dbWriteTable(db,tab_out,dati,append=T)

    #unlock(ll)
  }

  clusterEvalQ(clus,
    {
      require(RSQLite)
      library(flock)
      dir_metrics="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\grid_metrics_sql"
      db <- dbConnect(SQLite(), dbname=file.path(dir_metrics,"2017_June_08.db"),sychronous = "off")
      RSQLite::dbClearResult(RSQLite::dbSendQuery(db, "PRAGMA busy_timeout=5000000;"))

      }
    )

  lock.name=tempfile()

  system.time(parLapply(clus,files,f_write,tab_out,lock.name ))
  system.time(lapply(files,f_write2,tab_out ))

  dbGetQuery(db, "delete from gridmetrics_temp;")
  dbGetQuery(db, "select count (*) from gridmetrics_temp;")

}
