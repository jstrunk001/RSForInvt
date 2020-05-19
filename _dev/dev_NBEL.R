

#biomass computation
#Functions to Calculate bole biomass for a tree
#Biomass equations were provided by Bernie Parasol, and can take one of 10 forms
#most of the forms have multiple values for the coefficients
#
#requires biomassCoefficients.csv to be loaded in main.R
#

bm_NBEL = function(

  tlDF

  ,dbhNm ="dbh"
  ,htNm = "ht"
  ,spcdNm = "spcd"
  #,regionNm = ""


  ,NBEL_db_path = "code/BiomassEqns.db"
  ,NBEL_db_coef = "BM_EQCoefs"
  ,NBEL_db_eqn = "BM_EQForms2"
  ,NBEL_db_eqn_col = "equation_r"
  ,NBEL_db_comp = "BM_Comp"

  ,comp_ids = (1:49)[1:2]

  ,nclus = 5

  ){

  library(RSQLite)
  if(!file.exists(NBEL_db_path)) stop("please check NBEL_db_path, try file.exists('somepath...') to verify that path to BiomassEqns.db is correct")
  db_in = dbConnect(RSQLite::SQLite(), NBEL_db_path)
  spp_all_in = sort(unique(tlDF[,spcdNm]))

  coeffsDF_in0 = dbGetQuery(db_in, paste("select * from", NBEL_db_coef))
  eqnsDF_in = dbGetQuery(db_in, paste("select * from", NBEL_db_eqn))
  #eqnsNewDF_in = dbGetQuery(db_in, paste("select * from tblEqnsNew"))
  compsDF_in = dbGetQuery(db_in, paste("select * from",NBEL_db_comp ))

  dbDisconnect(db_in)

  #subset
  keep_spp = (coeffsDF_in0[,"species_code"] %in%  spp_all_in) & (coeffsDF_in0[,"comp_id"] %in%  comp_ids)
  coeffsDF_in = coeffsDF_in0[ keep_spp,]

  #test that all species codes in tlDF are present in coeffs
  spp_sql = sort(unique(coeffsDF_in[,"species_code"]))
  missing_spp = !spp_all_in %in% spp_sql
  if(sum(missing_spp) > 0) warning("some species not present in NBEL for the selected components:", paste(spp_all_in[missing_spp],collapse=","),". these obsevations will receive NA.")

  #build prediction models or formulae
  form_ids_in = sort(unique(coeffsDF_in[,"eq_form_id"]))
  eqnsDF_in1 = eqnsDF_in[eqnsDF_in[,"eqform_id"] %in% form_ids_in , ]
  eqns_in0 = eqnsDF_in[,NBEL_db_eqn_col]

  #merge coefficients with formulae
  m1_in = merge(x = coeffsDF_in, y = eqnsDF_in1 , by.x = "eq_form_id", by.y = "eqform_id")

  #substitute variable names
  m1_in[,"equation_r_in"] = gsub("tht", htNm, gsub("dia", dbhNm, m1_in[,c(NBEL_db_eqn_col)]))

  #parse coefficients into formulae
  fn_coef=function(fmla,coeffs){
    #print(fmla)
    eq = try(eval(parse(text=paste("substitute(~I(",fmla,"), as.list(coeffs))"))))
    #if(class(eq) == "try-error") browser()
    eq
  }

  spl_coeffs = split(m1_in[,c(letters[1:5])],1:nrow(m1_in))
  eqns_update = mapply( fn_coef , fmla = m1_in[,"equation_r_in"] , coeffs = spl_coeffs )

  #add equations to input df
  m1_in[,"equation_coefs"] = sapply(eqns_update,function(x)rlang::quo_text(x))

  browser()

  #for(i in 1:nrow(tlDF)){
  for(i in 1:nrow(coeffsDF_in)){

    #get trees that match formula 1
    tl_idx_i = tlDF[,spcdNm] == coeffsDF_in[1,"species_code"]

    #get values
    tlDF[,spcdNm] = model.frame(eqns_update[[1]],data = tlDF[tl_idx_i,,drop=F])

  }

}



if(T){


  if(!"dfSpp" %in% ls()){
    library(RSQLite)
    db0 = dbConnect(RSQLite::SQLite(), "code/BiomassEqns.db")
    dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
    dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
    dbDisconnect(db0)
  }
  set.seed=111
  nfake=length(unique(dfCoeff$species_code))
  df_fake = data.frame(
                        trid=1:(nfake)
                       ,dbh=10*abs(rnorm(nfake))
                       ,ht=100*abs(rnorm(nfake))
                       ,spcd = unique(dfCoeff$species_code)# sample(c("a","b","c","d") , nfake , T)
                      )

  #df_fake[,"spcd"] = as.numeric(df_fake$spp)

  bm_NBEL(df_fake,spcdNm="spcd")

}

getBiomassCoefficients <- function(id){
  return(as.list(bc[which(bc$Equation_ID==id), !(names(bc) %in% c("Eqn_form","Equation_ID")) ]))
}

calcBiomass_equationForm1 <- function(id,dbh,...){
  #calculates mass in kilograms using dbh in cm
  #ht not used, but included so all calcBiomass functions can be called the same way
  #input dbh in inches, returns mass in lbs
  c <- getBiomassCoefficients(id)
  D <- 2.54 * dbh #inch to cm
  b <- (exp(c$c0 + c$c1/D)*exp(c$b0 + c$b1*log(D))) + (exp(c$d0 + c$d1/D)*exp(c$b0 + c$b1*log(D)))
  return(b * 2.204623) #kg to lbs
}

calcBiomass_equationForm2 <- function(id,dbh,ht){
  #calculates mass in kilograms using dbh in cm, ht in m
  #input dbh in inches, height in ft, returns mass in lbs
  c <- getBiomassCoefficients(id)
  D <- 2.54 * dbh  #inch to cm
  H <- 0.3048 * ht #ft to m
  b<- c$b0 + c$b1*(D^2)*H
  return(b * 2.204623) #kg to lbs
}

calcBiomass_equationForm3 <- function(id,dbh,ht){
  #calculates mass in lbs using dbh in inches, ht in ft
  c <- getBiomassCoefficients(id)
  return( 10^(c$b0 + c$b1*log10(dbh^2*ht)) )
}

calcBiomass_equationForm4 <- function(id,dbh,ht){
  #calculates mass in lbs using dbh in inches, ht in ft
  c <- getBiomassCoefficients(id)
  if(dbh<5){
    return( 10^(c$b0 + c$b1*log10(dbh^2*ht)) )
  }else{
    return( 10^(c$c0 + c$c1*log10(dbh^2*ht)) )
  }
}

calcBiomass_equationForm5 <- function(id,dbh,ht){
  #calculates mass in lbs using dbh in inches, ht in ft
  c <- getBiomassCoefficients(id)
  return( exp(c$b0 + c$b1*log(dbh) + c$b2*log(ht)) )
}

calcBiomass_equationForm6 <- function(id,dbh,ht){
  #calculates mass in lbs using dbh in inches, ht in ft
  c <- getBiomassCoefficients(id)
  return( c$b0 + c$b1*(dbh^c$b2)*(ht^c$b3) )
}

calcBiomass_equationForm7 <- function(id,dbh,ht){
  #calculates mass in lbs using dbh in inches, ht in ft
  c <- getBiomassCoefficients(id)
  if(dbh<11){
    return( c$b0*(((dbh^2)*ht)^c$b1) )
  }else{
    return( c$c0*dbh^(2*c$c1)*(ht^c$c2) )
  }
}

bc <- read.csv("//biomassCoefficients.csv", header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE)


calcBiomass_equationForm8 <- function(id,dbh,ht){
  #calculates mass in lbs using dbh in inches, ht in ft
  c <- getBiomassCoefficients(id)
  return( exp(c$b0 + c$b1*log((dbh^2)*ht)) )
}

calcBiomass_equationForm9 <- function(id,dbh,ht=NA){
  #calculates mass in lbs using dbh in cm
  #ht not used, but included so all calcBiomass functions can be called the same way
  D <- 2.54 * dbh #inches to cm
  c <- getBiomassCoefficients(id)
  b <- (exp(c$b0 + c$b1*log(D)))/1000 #grams to kg
  return(b * 2.204623) #kg to lbs
}

calcBiomass_equationForm10 <- function(id,dbh,ht=NA){
  #calculates mass in lbs using dbh in cm
  #ht not used, but included so all calcBiomass functions can be called the same way
  D <- 2.54 * dbh #inches to cm
  c <- getBiomassCoefficients(id)
  b <- (10^(c$b0 + c$b1*log10(D)))+(10^(c$c0 + c$c1*log10(D)))
  return(b * 2.204623) #kg to lbs
}
