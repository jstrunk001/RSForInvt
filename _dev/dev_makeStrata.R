#'@title
#'  Make and assign strata based on two input columns. At least one column should be numeric, otherwise use expand.gri
#'
#'
#'@description
#'  Make and assign strata based on two input attributes and some parameters
#'  functions are makeStrata and assignStrata
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2019 01 24 created \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param x1 name of input x field
#'@param x2 name of input y field
#'@param split_x1 ?
#'@param split_x2 ?
#'@param  nest_x2  ?
#'@param  n1  ?
#'@param  n2  ?
#'@param  type_x1  ?
#'@param  type_x2 ?
#'@param  minRecs  ?
#'@param  precision  ?
#'
#'@param  strata
#'@param data
#'
#'@return
#'  makeStrata returns a data.frame of strata boundaries
#'  assignStrata takes a data.frame and appends a column wiht the stratum name for each record
#'
#'@examples
#'  <Delete and Replace>
#'
#'
#'@export
#
#@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))



#function takes a factor at the first level and then cuts based upon
# a numeric vector as the second level
makeStrata = function(
  data
  , x1="PlotStrata1"
  , x2="Elev.P95"
  , split_x1=c("qt","eq")
  , split_x2=c("qt","eq")
  , nest_x2 = T
  , n1 = 10
  , n2 = 10
  , type_x1 = c("factor","numeric")[1]
  , type_x2 = c("numeric","factor")[1]
  , minRecs = 7
  , precision = 0
){

	x1_in = x1

	#prepare optional holder for height strata
	dfStr0 = read.csv(text=paste("stratum_x1,x1.from,x1.to,n_x1"))

	if( type_x1 == "numeric"){

		r_x1 = range(data[,x1])
		dr_x1 = diff(r_x1)

		if(split_x1[1] == "qt" ){

			str_levels_x1 = c(
				r_x1[1]-dr_x1
				,quantile(data[,x1] ,seq(1/n1,1-1/n1,1/n1))
				,r_x1[1]+dr_x1
			)
		}
		if(split_x1[1] == "eq" ){
			str_levels_x1 = c(
				r_x1[1] - dr_x1
				,seq(r_x1[1],r_x1[2],dr_x1/n1)[-c(1,n1)]
				,r_x1[1]+ dr_x1
			)
		}

		#prep description of x1 stratum
		str_levels_x1 = unique(round(str_levels_x1,precision) )

		#prepare new factor version of attribute
		x1_in = paste(x1,"_ctg",sep="")
		data[,x1_in] = cut(data[,x1] , str_levels_x1 , labels = 1:(length(str_levels_x1)-1) )
		type_x1 = "factor"

		#assign bins
		dfStr0[1:(length(str_levels_x1)-1),"stratum_x1"] = 1:(length(str_levels_x1)-1)
		dfStr0[, "x1.from"] = str_levels_x1[-length(str_levels_x1)]
		dfStr0[, "x1.to"] = str_levels_x1[-1]
		dfStr0[, "n_x1"] = table(data[,c(x1_in)])
	}

  if( nest_x2 ){

    spl1 = split(data,data[,x1_in])

    if( type_x2 == "numeric"){

      for(i in 1:length(spl1)){

        dfStri = read.csv(text=paste("stratum,stratum_x1,stratum_x2,x2.from,x2.to"))

        dati = spl1[[i]]

        if(nrow(dati) / n2 < minRecs ) n2i = max(floor(nrow(dati) / minRecs),1)
        else n2i = n2

        ri = range(dati[,x2])
        dri = diff(ri)

        if(split_x2[1] == "qt" ){

          str_levels = c(
            ri[1]-dri
            ,quantile(dati[,x2] ,seq(1/n2i,1-1/n2i,1/n2i))
            ,ri[2]+dri
          )
        }
        if(split_x2[1] == "eq" ){
          str_levels = c(
            ri[1]-dri
            ,seq(ri[1],ri[2],dri/n2i)[-c(1,n2i)]
            ,ri[2]+dri
          )
        }

        str_levels = unique(round(str_levels,precision) )

        dfStri[1:(length(str_levels)-1),"stratum_x2"] = 1:(length(str_levels)-1)
        dfStri[, "stratum_x1" ] =  dati[1,x1_in]
        dfStri[, "x2.from"] = str_levels[-length(str_levels)]
        dfStri[, "x2.to"] = str_levels[-1]
        dfStri[,"stratum"] = apply(dfStri[,c("stratum_x1","stratum_x2")],1,paste,collapse=".")

        df_ct = as.data.frame(table(as.factor(cut(dati[,x2], round(str_levels,precision) , labels = F ))))
        if(i == 1) dfStr1 = merge(x=dfStri,df_ct,by.x="stratum_x2",by.y="Var1",all=T)
        if(i > 1) dfStr1 = plyr::rbind.fill(dfStr1, merge(x=dfStri,df_ct,by.x="stratum_x2",by.y="Var1",all=T))
      }


    }

    if( type_x2 == "factor"){



    }

  }

  else stop(" first variable y1 must be factor / string")


  #dfStr2 = dfStr1[,c('stratum','stratum_x1','stratum_x2','x2.from','x2.to','Freq')]
  dfStr2 = merge(x=dfStr0, dfStr1 , by = 'stratum_x1',all.y=T)

  dfStr2[, "nm_x1"] = x1
  dfStr2[, "nm_x2"] = x2

  return( dfStr2)

}

#'@export

assignStrata = function(strata,data){

	browser()
  x1numeric = is.na(strata[1,"x1.from"])
  x2numeric = is.na(strata[1,"x2.from"])

  for(i in 1:nrow(strata)){

    id_i =
    	( data[,strata[i,"nm_x1"]] == strata[i,"stratum_x1"] ) &
      ( data[,strata[i,"nm_x2"]] > strata[i,"x2.from"] ) &
      ( data[,strata[i,"nm_x2"]] <= strata[i,"x2.to"] )
    id_i[is.na(id_i)] = F
    data[ id_i , "stratum" ] = strata[ i , "stratum" ]

  }

  data

}


if(T){
#stratify on vegetation type and height

		n=500
		hts = sample(1:150,n,T)
		dbhs = abs(hts/5 + rnorm(n)*2)
		dat_test = data.frame(height = hts , dbh = dbhs, height_cat = cut(hts,10) , dbh_cat = cut(dbhs,10))

		plot(dbhs,hts)

    str_test = makeStrata(
    	dat_test
      , x1="height_cat"
      , x2="dbh"
    	, split_x1 =c("qt","eq")[1]
      , split_x2 =c("qt","eq")[1]
      , nest_x2 =  T
      , n1 = 10
      , n2 = 10
      , type_x1 = "factor"
      , type_x2 = "numeric"
      , minRecs = 7
      , precision = 0
    )


    res = assignStrata(
      str_test
      ,dat_test
    )

    res$stratum

    summary(lm(height ~ dbh , data = res))
    summary(lm(height ~ dbh_cat , data = res))
    summary(lm(height ~ stratum , data = res))

    str_test1 = makeStrata(
    	dat_test
    	, x1="height"
    	, x2="dbh"
    	, split_x1 =c("qt","eq")[1]
    	, split_x2 =c("qt","eq")[1]
    	, nest_x2 =  T
    	, n1 = 10
    	, n2 = 10
    	, type_x1 = "numeric"
    	, type_x2 = "numeric"
    	, minRecs = 7
    	, precision = 0
    )

    res = assignStrata(
    	str_test1
    	,dat_test
    )

}
