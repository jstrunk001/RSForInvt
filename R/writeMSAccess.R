#'@title
#'  Write data to MS access table - must be 32 bit windows
#'
#'@description
#'  handles some column name / type conversion issues
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 3/27/2020 Created \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param db access database
#'@param  nm table name
#'@param  dat data to write
#'@param  dropTarget T/F drop target before writing ?
#'@param  doDebug  T/F enter browser() at beginning of function run
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import RODBC plyr odbc lubridate
#'
#'@export
#
#'@seealso \code{\link{sqlSave}}\cr \code{\link{dbWritetable}}\cr

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))


writeMSAccess_RODBC = function(db
                         , nm
                         , dat
                         , dropTarget=F
                         , doDebug=F
                         ){

  require(RODBC)

  if(doDebug) browser()
  nm_temp = paste(nm,"XXXX",sep="")

  res0a = odbcQuery(db, paste("drop table",nm_temp))
  res0b = odbcQuery(db, paste("select * into", nm_temp, "from",nm))
  res0c = odbcQuery(db, paste("delete from",nm_temp))

  #tab_tempxxx = sqlFetch(db,nm_temp, max=1)
  tab_temp = sqlFetch(db,nm, max=1)
  dat_in = (plyr::rbind.fill(tab_temp[0,], dat))[,names(tab_temp)]

  #type conversion
  type_compare = sapply(dat_in,class) == sapply(tab_temp,class)
  if(mean(type_compare)<1){
    dat_in[,sapply(tab_temp,is.numeric)] = sapply(dat_in[,sapply(tab_temp,is.numeric)],as.numeric)
    dat_in[,sapply(tab_temp,is.integer)] = sapply(dat_in[,sapply(tab_temp,is.integer)],as.integer)
    dat_in[,sapply(tab_temp,is.character)] = sapply(dat_in[,sapply(tab_temp,is.character)],as.character)
    dat_in[,sapply(tab_temp,is.factor)] = sapply(dat_in[,sapply(tab_temp,is.factor)],as.factor)
  }

  res0d = sqlSave(db,dat_in,nm_temp,rownames = F,append=T)

  if(!dropTarget){

    res1 = odbcQuery(db, paste("insert into", nm, "select * from", nm_temp))
    res2 = odbcQuery(db, paste("drop table",nm_temp))
    return( c(dropTemp = res0a , createTemp=res0b , truncateTemp=res0c , loadTemp = res0d , selectInto=res1 ,tempDelete = res2 ))

  }
  if(dropTarget){

    res1 = odbcQuery(db, paste("drop table",nm))
    res2 = odbcQuery(db, paste("select * into", nm, "from",nm_temp))
    res3 = odbcQuery(db, paste("drop table",nm_temp))
    return( c(dropTemp = res0a , createTemp=res0b , truncateTemp=res0c , loadTemp = res0d , dropTarget = res1, selectInto=res2 ,tempDelete = res3 ))

  }

}


writeMSAccess_odbc = function(db
                         , nm
                         , dat
                         , dropTarget=F
                         , doDebug=F
){

  # require(odbc)
  # require(DBI)
  # require(lubridate)

  if(doDebug) browser()
  nm_temp = paste(nm,"XXXX",sep="")

  res0a = try(dbSendQuery(db, paste("drop table",nm_temp)) , silent = T)
  res0b = try(dbSendQuery(db, paste("select * into", nm_temp, "from",nm)) , silent = T)
  res0c = try(dbSendQuery(db, paste("delete from",nm_temp)) , silent = T)

  #tab_tempxxx = sqlFetch(db,nm_temp, max=1)
  tab_temp = dbGetQuery(db,paste("select top 1 * from",nm))
  dat_in = (plyr::rbind.fill(tab_temp[0,], dat))[,names(tab_temp)]

  #type conversion
  type_dat = sapply(dat_in,class)
  type_fps = sapply(tab_temp,class)
  #type_compare = type_dat  == type_fps

  #if(mean(type_compare)<1){
  if(T){

    dat_in[,sapply(tab_temp,is.POSIXct)] = sapply(dat_in[,sapply(tab_temp,is.POSIXct)],as.POSIXct)
    dat_in[,sapply(tab_temp,is.numeric)] = sapply(dat_in[,sapply(tab_temp,is.numeric)],as.numeric)
    dat_in[,sapply(tab_temp,is.integer)] = sapply(dat_in[,sapply(tab_temp,is.integer)],as.integer)
    dat_in[,sapply(tab_temp,is.character)] = sapply(dat_in[,sapply(tab_temp,is.character)],as.character)
    dat_in[,sapply(tab_temp,is.factor)] = sapply(dat_in[,sapply(tab_temp,is.factor)],as.factor)

  }

  res0d = try(DBI::dbWriteTable(db,nm_temp,dat_in[,],rownames = F,append=T, overwrite=F))

  if(class(res0d) == "try-error"){

    #DBI uses e.g. "DOUBLE PRECISION" and MS Access uses "DOUBLE"
    # sql_create = gsub(" PRECISION","",odbc::sqlCreateTable(db, nm_temp, dat))
    # res0d =dbSendQuery(db,sql_create)

    try(dbSendQuery(db, paste("drop table",nm_temp)) , silent = T)
    cols=5:6
    sql_create = gsub(" PRECISION","",odbc::sqlCreateTable(db, nm_temp, dat))
    res0e =dbSendQuery(db,sql_create)
    res0d = try(DBI::dbWriteTable(db,nm_temp,dat[,cols,drop=F],append=T))



    for(i in 1:nrow(dat_in)){
      res0d = try(DBI::dbWriteTable(db,nm_temp,dat[i,],append=T))
    }

  }

  if(!dropTarget){

    res1 = dbSendQuery(db, paste("insert into", nm, "select * from", nm_temp))
    res2 = dbSendQuery(db, paste("drop table",nm_temp))
    return( c(dropTemp = res0a , createTemp=res0b , truncateTemp=res0c , loadTemp = res0d , selectInto=res1 ,tempDelete = res2 ))

  }
  if(dropTarget){

    res1 = dbSendQuery(db, paste("drop table",nm))
    res2 = dbSendQuery(db, paste("select * into", nm, "from",nm_temp))
    res3 = dbSendQuery(db, paste("drop table",nm_temp))
    return( c(dropTemp = res0a , createTemp=res0b , truncateTemp=res0c , loadTemp = res0d , dropTarget = res1, selectInto=res2 ,tempDelete = res3 ))

  }

}
