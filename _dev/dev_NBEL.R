

#biomass computation
#Functions to Calculate bole biomass for a tree
#Biomass equations were provided by Bernie Parasol, and can take one of 10 forms
#most of the forms have multiple values for the coefficients
#
#requires biomassCoefficients.csv to be loaded in main.R
#

bm_NBEL = function(

  tlDF
  ,spcd = ""
  ,NBEL_db_path = "../code/BiomassEqns.db"

  ){

  library(RSQLite)
  db_in = dbConnect(RSQLite::SQLite(), NBEL_db_path)
  all_spp = unique(tlDF[,spcd])

  coeffsDF = dbGetQuery(db_in, paste("select * from BM_EQCoefs where species_code in (",paste(all_spp,sep=","),")"))
  eqnsDF = dbGetQuery(db_in, paste("select * from BM_EQForms where species_code in (",paste(all_spp,sep=","),")"))

  #test that all species codes in tlDF are present in coeffs


  #build prediction models or formulae


  dbDisconnect(db_in)

}



if(F){

  set.seed=111
  nfake=50
  df_fake = data.frame(tlDFid=1:50
                       ,db=10*abs(rnorm(nfake))
                       ,ht=100*abs(rnorm(nfake))
                       ,spp = sample(c("a","b","c","d") , nfake , T)
  )

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
