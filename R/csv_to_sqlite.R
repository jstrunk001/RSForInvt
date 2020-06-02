#'@title
#'  load csvs into database
#'
#'@description
#'  point at a directory and load fusion derived tabular csv data into an sqlite database
#'
#'@details
#'  <Delete and Replace>
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 1/18/2018 header added \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'

#'@param db sqlite database connection
#'@param csv_folder location of csv files
#'@param tb_summary output name of summary table of csv files
#'@param tb_csv table to dump csv files into
#'@param project name of project
#'@param resolution resolution of input data
#'@param units unites of resolution
#'@param proj4 proj4 string of coordinate data
#'@param notes any relevant notes
#'@param skip_loaded should csv files which are already loaded be skipped
#'@param n_load max number of files to load
#'@param use_col_classes read colClasses from first file, and apply to remaining files - big read speedup

#'
#'@return
#'  NULL
#'
#'@examples
#'  library(lasR)
#'  dir_csv="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics1\\gridmetrics_csv\\"
#'    dir_sqlite="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\sqlite\\"
#'    dir.create(dir_sqlite)
#'
#'    require(RSQLite)
#'    db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics2.db"),sychronous = "off")
#'
#'
#'    csv_to_sqlite(db=db
#'                  ,csv_folder=dir_csv
#'    )
#'
#'@import RSQLite
#'
#'@export
#
#'@seealso \code{\link{run_gridmetrics}}\cr \code{\link{lasR_project}}\cr


csv_to_sqlite=function(
                       db_csvOut
                       ,csv_folder
                       ,tb_summary="gm_summary"
                       ,tb_csv="gm"
                       ,project="naip_2015"
                       ,resolution="66"
                       ,units="feet"
                       ,proj4=""
                       ,notes=""
                       ,skip_loaded=T
                       ,n_load=NA
                       ,use_col_classes=T

                       ){

  if(class(db_csvOut) == "SQLiteConnection") db_in = db_csvOut
  if(class(db_csvOut) == "character") db_in = dbConnect( RSQLite::SQLite(), db_csvOut )

  if(interactive()) requireNamespace("RSQLite")

  csv_files=list.files(csv_folder,full.names=T,pattern="[.]csv")

  #check for existence of tables
  tb_exist=dbListTables(db_in)

  #prep data table - get column names
  dat0=read.csv(csv_files[1])
  names(dat0)=gsub("elev","ht",gsub("[.]+","_",gsub("^x[.]","",tolower(names(dat0)))))
  names0=names(dat0)
  col_classes=sapply(dat0,class)


  if(!tb_csv %in% tb_exist | !skip_loaded ){

    dbWriteTable(db_in,tb_csv,dat0[0,],overwrite = TRUE,append=F)
    dat1=dbGetQuery(db_in,paste("select * from ",tb_csv,"limit 50"))
    names1=names(dat1)
  }else{

    dat1=dbGetQuery(db_in,paste("select * from ",tb_csv,"limit 50"))
    names1=names(dat1)
  }

  if(!is.na(n_load)){

    csv_files=csv_files[1:min(n_load,length(csv_files))]

  }

  #prep / read summary table
  if(!tb_summary %in% tb_exist | !skip_loaded){

    summ0=data.frame(date=as.character(Sys.Date()),file="",nrows=0,status="")
    dbWriteTable(db_in,tb_summary,summ0[0,],overwrite = TRUE,append=F)

  }else{

    summ0=dbReadTable(db_in,tb_summary)
    files_loaded=basename(csv_files) %in% basename(summ0$file[summ0$status == "completed"])
    csv_files=csv_files[!files_loaded]

  }

  if(length(csv_files)==0) return()

  #prep process
  errs=list()
  n_files=length(csv_files)

  for(i in 1:n_files){

    if(use_col_classes) dati=try(read.csv(csv_files[i],colClasses = col_classes))
    if(!use_col_classes) dati=try(read.csv(csv_files[i]))

    if(!class(dati)=="try-error"){

      #match names / number of cols
      dati=dati[,1:length(names1)]
      names(dati)=names1
      dati=dati[ dati[,"total_all_returns"] > -1 , ]

      #write data
      if(nrow(dati)>0){
         err_i=try(dbWriteTable(db_in,tb_csv,dati,append=T))

        #write summary record
        if(!class(err_i) == "try-error"){
          summ_i=data.frame(date=as.character(Sys.Date()),file=csv_files[i],nrows=nrow(dati),status="completed")
          try(dbWriteTable(db_in,tb_summary,summ_i,append=T))
        }
      }
    }
    print(paste(csv_files[i],", file",i,"from",n_files,"files at",Sys.time()))
  }


  if(class(db_csvOut) == "character") dbDisconnect(db_in)

  }




# if(F){
#
#   library(lasR)
#   dir_csv="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics1\\gridmetrics_csv\\"
#   dir_sqlite="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\sqlite\\"
#   dir.create(dir_sqlite)
#
#   require(RSQLite)
#   db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics2.db"),sychronous = "off")
#
#
#   csv_to_sqlite(db=db
#                 ,csv_folder=dir_csv
#                 )
#
# }
