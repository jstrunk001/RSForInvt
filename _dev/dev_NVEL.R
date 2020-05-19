

volume_NVEL=function(

  dfTL = data.frame(dbh=5,ht=5)

  #force a specific volume equation
  ,voleq = NA

  #optional, but these supercede values in dfTL columns regionNm,forestNm, districtNm
  ,region = 1
  ,forest = NA
  ,district = NA

  #optional, provide region, forest, district for every tree
  ,voleqNm = c("voleq")
  ,regionNm = c("region")
  ,forestNm = c("forest")
  ,districtNm = c("district")

  ,spcdNm = "spcd"
  ,dbhNm = "dbh"
  ,htNm = "ht"

  ,pulpDbNm = c("pulpDb")
  ,sawDbNm = c("sawDb")

  ,htPrd1Nm = c("htPrd1")
  ,htPrd2Nm = c("htPrd2")

  ,upHt1Nm = c("upHt1")
  ,upDb1Nm = c("upDb1")
  ,stumpHtNm = "stumpHt"

  ,fclassNm = c("fclass")
  ,dbtbhNm = c("dbtbh")
  ,btrNm = c("btr")

  ,dll_64 = 'code/VolLibDll20190514/vollib-64bits/vollib.dll'
  ,dll_32 = 'code/VolLibDll20190514/vollib-32bits/vollib.dll'
  ,load_dll = T

  ,dll_func_vol = "vollib_r"
  ,dll_func_voleq = "getvoleq_r"

  ,nclus = 4

  ){
  options(stringsAsFactors = F)
  #load dll if needed
  if(load_dll) .load_dll(dll_64,dll_32,dll_func )

  # #test for and load dll
  # arch_in = R.Version()$arch
  # loaded_dlls_in = names(getLoadedDLLs())
  # dll_loaded = "vollib" %in% loaded_dlls_in
  # if(arch_in == "x86_64" & !dll_loaded) dyn.load(dll_64)
  # if(arch_in == "x86_32" & !dll_loaded) dyn.load(dll_32)

  #test for existence of voleq
  get_voleq = T
  if(!is.na(voleq)) get_voleq = F
  if(!is.na(voleqNm)[1]) if((voleqNm)[1] %in% names(dfTL)) if( sum(sapply(dfTL[,voleqNm],nchar)==0 ) == 0 ) get_voleq = F

  #deal with column names, add all columns / rename
  dfTL0_in = .formatTL2NVEL(
                     dfTL0 = dfTL
                     ,voleq  = voleq[1]
                     ,region  = region[1]
                     ,forest  = forest[1]
                     ,district  = district[1]
                     ,voleqNm = voleqNm[1]
                     ,regionNm = regionNm[1]
                     ,forestNm  =  forestNm[1]
                     ,districtNm  =  districtNm[1]
                     ,spcdNm = spcdNm[1]
                     ,dbhNm  = dbhNm[1]
                     ,htNm  = htNm[1]

                     ,pulpDbNm = pulpDbNm
                     ,sawDbNm = sawDbNm

                     ,htPrd1Nm = htPrd1Nm
                     ,htPrd2Nm = htPrd2Nm

                     ,upHt1Nm = upHt1Nm
                     ,upDb1Nm = upDb1Nm
                     ,stumpHtNm = stumpHtNm

                     ,fclassNm = fclassNm
                     ,dbtbhNm = dbtbhNm
                     ,btrNm = btrNm

                     )



  #get volume equations
  if( is.na(voleq[1]) & (!voleqNm %in% names(dfTL)) ){
    vol_eqns_in = getvoleq_NVEL(
                      dfTL = dfTL
                      #optional, provide region, forest, district for every tree
                      ,regionNm = "region"
                      ,forestNm = "forest"
                      ,districtNm = "district"
                      ,spcdNm = "spcd"
                      ,dll_func_voleq = dll_func_voleq
                      ,load_dll = F
                      )
    dfTL0_in[,"voleq"] = vol_eqns_in[,"voleq"]
    voleqNm = "voleq"
  }

  #compute volumes
    #turn of warnings temporarily, this generates scads of them
  defaultW <- getOption("warn")
  options(warn = -1)
  vol_pd0 = mapply(.fn_fortran_vol

                          ,voleq = dfTL0_in[,voleqNm[1]]
                          ,region = dfTL0_in[,regionNm[1]]
                          ,forest = dfTL0_in[, forestNm[1]]
                          ,district  = dfTL0_in[, districtNm[1]]
                          ,spcd = dfTL0_in[,spcdNm[1]]
                          ,dbh  = dfTL0_in[,dbhNm[1]]
                          ,ht  = dfTL0_in[,htNm[1]]

                          ,pulpDb = dfTL0_in[,pulpDbNm[1]]
                          ,sawDb = dfTL0_in[,sawDbNm[1]]

                          ,htPrd1 = dfTL0_in[,htPrd1Nm[1]]
                          ,htPrd2 = dfTL0_in[,htPrd2Nm[1]]

                          ,upHt1 = dfTL0_in[,upHt1Nm[1]]
                          ,upDb1 = dfTL0_in[,upDb1Nm[1]]
                          ,stumpHt= dfTL0_in[,stumpHtNm[1]]

                          ,fclass = dfTL0_in[,fclassNm[1]]
                          ,dbtbh = dfTL0_in[,dbtbhNm[1]]
                          ,btr = dfTL0_in[,btrNm[1]]

                          ,MoreArgs=list(dll_func_vol=dll_func_vol)
                          ,SIMPLIFY = F)
  #turn warnings back on
  options(warn = defaultW)

  #merge predictions together and add their names
  vol_pd0_df = plyr::rbind.fill(vol_pd0 )
  names(vol_pd0_df)[1:15] = c("TCFV","BFV_GRS","BFV_NET","CFV_GRS","MCFV_NET","CDVOL","CV2nd_GRS","CV2nd_NET","CDVOL2nd","BFV_GRS_INTL","BFV_NET_INTL","BFV2nd_GRS","BFV2nd_NET","VStump","VTip")

  #return formatted tree list with predicted volumes
  data.frame(dfTL0_in,vol_pd0_df)

}

#load dll if needed
.load_dll = function(dll_64,dll_32,dll_func ){
  arch_in = R.Version()$arch
  loaded_dlls_in = names(getLoadedDLLs())
  dll_loaded = "vollib" %in% loaded_dlls_in
  if(arch_in == "x86_64" & !dll_loaded) dyn.load(dll_64)
  if(arch_in == "x86_32" & !dll_loaded) dyn.load(dll_32)
}

#call fortran
.fn_fortran_vol = function(
                    voleq
                    ,region
                    ,forest
                    ,district
                    ,spcd
                    ,dbh
                    ,merchDb
                    ,ht
                    ,pulpDb
                    ,sawDb
                    ,htPrd1
                    ,htPrd2
                    ,upHt1
                    ,upDb1
                    ,stumpHt
                    ,fclass
                    ,dbtbh
                    ,btr

                    ,dll_func_vol){

    # print(
    #   list(dll_func_vol
    #    ,as.character(voleq)
    #    ,as.integer(region)
    #    ,as.character(forest)
    #    ,as.character(district)
    #    ,as.integer(spcd)
    #    ,as.double(dbh)
    #    ,as.double(ht)
    #    ,as.double(pulpDb)
    #    ,as.double(sawDb)
    #    ,as.double(htPrd1)
    #    ,as.double(htPrd2)
    #    ,as.double(upHt1)
    #    ,as.double(upDb1)
    #    ,as.double(stumpHt)
    #    ,as.integer(fclass)
    #    ,as.double(dbtbh)
    #    ,as.double(btr)
    #    ,as.double(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
    #    ,as.integer(0)
    #   )
    #   )


    res_vol0 =.Fortran(dll_func_vol
             ,as.character(voleq)
             ,as.integer(region)
             ,as.character(forest)
             ,as.character(district)
             ,as.integer(spcd)
             ,as.double(dbh)
             ,as.double(ht)
             ,as.double(pulpDb)
             ,as.double(sawDb)
             ,as.double(htPrd1)
             ,as.double(htPrd2)
             ,as.double(upHt1)
             ,as.double(upDb1)
             ,as.double(stumpHt)
             ,as.integer(fclass)
             ,as.double(dbtbh)
             ,as.double(btr)
             ,as.double(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
             ,as.integer(0)
             )
    #return
    data.frame(t(res_vol0[[18]] ),err=res_vol0[[19]])


}



#data tree list and format it for these functions
#typically not needed by users
.formatTL2NVEL = function(
  dfTL0

  ,voleq
  ,region
  ,forest
  ,district

  ,voleqNm
  ,regionNm
  ,forestNm
  ,districtNm
  ,spcdNm
  ,dbhNm
  ,htNm

  ,pulpDbNm
  ,sawDbNm

  ,htPrd1Nm
  ,htPrd2Nm

  ,upHt1Nm
  ,upDb1Nm
  ,stumpHtNm

  ,fclassNm
  ,dbtbhNm
  ,btrNm



){

  dfTL1 = dfTL0

  nms_in = c(
    voleq= voleqNm[1]
    ,region = regionNm[1]
    ,forest  =  forestNm[1]
    ,district  =  districtNm[1]
    ,spcd = spcdNm[1]
    ,dbh  = dbhNm[1]
    ,ht  = htNm[1]

    ,pulpDb = pulpDbNm
    ,sawDb = sawDbNm

    ,htPrd1 = htPrd1Nm
    ,htPrd2 = htPrd2Nm

    ,upHt1 = upHt1Nm
    ,upDb1 = upDb1Nm
    ,stumpHt = stumpHtNm

    ,fclass = fclassNm
    ,dbtbh = dbtbhNm
    ,btr = btrNm
  )


  #add columns with na in nms
  na_nms = is.na(nms_in)
  nms_fill_NA = names(nms_in)[na_nms]
  dfTL1[,nms_fill_NA] = 0

  #add missing columns
  na_nms1 =! nms_in %in% names(dfTL1)
  nms_fill_NA1 = names(nms_in)[na_nms1]
  dfTL1[,nms_fill_NA1] = 0

  #update existing column names with fixed column names
  nms_in[na_nms] = names(nms_in)[na_nms]
  nms_in[na_nms1] = names(nms_in)[na_nms1]
  names(dfTL1[,nms_in]) = names(nms_in)


  #overwrite names
  if(!is.na(voleq)) dfTL1[,"voleq"] = voleq
  if(!is.na(region)) dfTL1[,"region"] = region
  if(!is.na(forest)) dfTL1[,"forest"] = forest
  if(!is.na(district)) dfTL1[,"district"] = district

  #return correctly named data
  dfTL1[,names(nms_in)]

}


getvoleq_NVEL = function(

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

#examples of using getvoleq
if(F){
  getvoleq_NVEL(region = 2, forest = "01",district = "01", spcd=951)
  getvoleq_NVEL(region = 2, forest = "01",district = "01", spcd=rep(c(951,201),2))
  getvoleq_NVEL(dfTL=data.frame(region = 6, forest = "01",district = "01", spcd=rep(c(951,201),2)))
}

ht2topd_NVEL = function(){


}


calcdob_NVEL = function(){


}

biomass_NVEL = function(){


}



#volume computation
calcTreeVolume <- function(e,r,f,d,s,dbh,h){
  q <- .Fortran(
    "vollib_r",
    as.character(e), #volume equation number
    as.integer(r),   #USFS Region -- 8: Southern Region
    as.character(f), #USFS Forest -- 12: Francis Marion-Sumnter National Forest
    as.character(d), #USFS Forest District -- 03:Long Cane District; 00:Default; 01:Enoree District; 02:Andrew Pickens District; 05:Francis Marion District
    as.integer(s),   #Species Code ie 122
    as.double(dbh),  #dbh outside bark
    as.double(h),  #height top
    as.double(0),  #merch top diameter
    as.double(0),  #
    as.double(0),  #height for saw timber
    as.double(0),  #pulp height (height to 4" top)
    as.double(0),  #
    as.double(0),  #
    as.double(0),  #
    as.integer(0), #
    as.double(0),  #
    as.double(0),  #
    as.double(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),  #vector of volumes; filled in with values that can be calculated
    as.integer(0)  #error flag; >0 an error returned
  )
  return( q[[18]] ) #volume vector of 15 tree volumes; want [1], total cubic volume
  # 1)	Total Cubic Volume from ground to tip
  # 2)	Gross Scribner board foot volume. Note: The VOL(2) is International ¼ board foot volume for Region 8 Forest 8, 9, 10, and 12 (except Andrew Pickens district); and Region 9 Forest 4,5,8,11,12,14,19,20,21,22,24, and 30 when using Clark profile equation.
  # 3)	Net Scribner board foot volume.
  # 4)	Gross merchantable cubic foot volume
  # 5)	Net merchantable cubic foot volume
  # 6)	Merchantable cordwood volume
  # 7)	Gross secondary product volume in cubic feet
  # 8)	Net secondary product volume in cubic feet
  # 9)	Secondary product in cordwood
  # 10)	Gross International ¼ board foot volume
  # 11)	Net International ¼ board foot volume
  # 12)	Gross secondary product in Scribner board feet
  # 13)	Net secondary product in Scribner board feet
  # 14)	Stump volume
  # 15)	Tip volume
}



#testing
if(F){

  volume_NVEL()

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

  volume_NVEL( dfTL = df_fake )

}
