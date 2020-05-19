#'@title
#'  Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)
#'
#'@description
#'  Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)
#'
#'@details
#'  Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/19/2020 Function created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param dfTL data.frame with tree records
#'@param region (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#'@param forest (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#'@param district (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#'@param regionNm (optional) column name in DFTL:provide region, forest, district for every tree in dfTL
#'@param forestNm (optional) column name in DFTL:provide region, forest, district for every tree in dfTL
#'@param districtNm (optional) column name in DFTL: provide region, forest, district for every tree in dfTL
#'@param spcdNm (required) column name in DFTL: USFS species code
#'@param dll_64  path to 64bit dll
#'@param dll_32  path to 64bit dll
#'@param load_dll T/F should dll be loaded (in case it is already loaded)
#'@param dll_func_voleq name of volume equation chooser function call in NVEL .dll
#'
#'@return
#'  reformatted tree list with with a new column volume equation codes for NVEL - "voleq"
#'
#'@examples
#'
#'
#'         #grab list of species
#'         if(!"dfSpp" %in% ls()){
#'           library(RSQLite)
#'           db0 = dbConnect(RSQLite::SQLite(), "code/BiomassEqns.db")
#'           dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
#'           dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
#'           dbDisconnect(db0)
#'         }
#'
#'         #build a fake tree list
#'         if(!"dfSpp" %in% ls()){
#'           set.seed=111
#'           nfake=length(unique(dfCoeff$species_code))
#'
#'           df_fake = data.frame(
#'             trid=1:(nfake)
#'             ,region = 6
#'             ,forest = "01"
#'             ,district = "01"
#'             ,dbh=10*abs(rnorm(nfake))
#'             ,ht=100*abs(rnorm(nfake))
#'             ,spcd = unique(dfCoeff$species_code)#'     sample(c("a","b","c","d") , nfake , T)
#'           )
#'
#'         }
#'
#'         #get volumes
#'         NVEL_voleq( dfTL = df_fake )
#'
#'
#'
#'
#'@import some_package,some_package2
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))


NVEL_voleq = function(

  dfTL = list(NA, data.frame(spcd=201, dbh=5 ,ht=5 ,region = 0 , forest = "01", district = "01") )[[1]]

  #optional, but these supercede values in dfTL columns regionNm,forestNm, districtNm
  ,region = NA
  ,forest = NA
  ,district = NA
  ,spcd = NA

  #optional, provide region, forest, district for every tree
  ,regionNm = "region"
  ,forestNm = "forest"
  ,districtNm = "district"
  ,spcdNm = "spcd"

  ,dll_64 = 'code/VolLibDll20190514/vollib-64bits/vollib.dll'
  ,dll_32 = 'code/VolLibDll20190514/vollib-32bits/vollib.dll'
  ,dll_func_voleq = "getvoleq_r"
  ,load_dll = T

){

  #load dll if needed
  if(load_dll) .load_dll(dll_64,dll_32,dll_func )

  #figure out how we are generating volume equations
  if(class(dfTL) == "logical"){

    if( is.na(region[1]) | is.na(forest[1]) | is.na(district[1]) | is.na(spcd[1]) ) warning("dfTL not provided, and missing region,forest,district, or spcd - generic equation(s) likely returned")

    dfTL = data.frame(region=region,forest=forest,district=district,spcd=spcd)

    regionNm = c("region")
    forestNm = c("forest")
    districtNm = c("district")
    spcdNm = c("spcd")

  }

  #get volume equation
  #catch warning about portabiliyt of passing a char vector
  defaultW <- getOption("warn")
  options(warn = -1)
  dfTL[,"voleq"] = mapply(.fn_fortran_voleq,dfTL[,regionNm],dfTL[,forestNm],dfTL[,districtNm],dfTL[,spcdNm] ,MoreArgs=list(dll_func_voleq=dll_func_voleq), SIMPLIFY = T)
  options(warn = defaultW)

  return(dfTL)

}
#call fortran
.fn_fortran_voleq = function(region,forest,district,species,dll_func_voleq){
  .Fortran(dll_func_voleq,as.integer(region),as.character(forest),as.character(district),as.integer(species),as.character("          "),as.integer(0))[[5]]
}



if(F){


  if(!"dfSpp" %in% ls()){
    library(RSQLite)
    db0 = dbConnect(RSQLite::SQLite(), "code/BiomassEqns.db")
    dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
    dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
    dbDisconnect(db0)
  }

  #if(!"dfSpp" %in% ls()){
  if(f){
    set.seed=111
    nfake=length(unique(dfCoeff$species_code))

    df_fake = data.frame(
      trid=1:(nfake)
      ,region = 6
      ,forest = "01"
      ,district = "01"
      ,dbh=10*abs(rnorm(nfake))
      ,ht=100*abs(rnorm(nfake))
      ,spcd = unique(dfCoeff$species_code)# sample(c("a","b","c","d") , nfake , T)
    )

  }

  NVEL_voleq( dfTL = df_fake )

}
