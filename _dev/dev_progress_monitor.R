#'@title
#'  <Delete and Replace>
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab date and revisions.. \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
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
# bs<- function(){path <- shQuote(gsub("\\", "\\\\", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# fs<- function(){path <- shQuote(gsub("\\", "/", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep="");writeClipboard(x);return(x)}



progress_monitor=function(
                        dir
                        ,pattern = NULL
                        , nTotal = 100000
                        , update_seconds = 300
                        , prompt = F
                        #, interval_mins=c(5,10,30,60,180,360)
                        , ylim_days = NULL
                        #, saveInterm = T
                        #, return = T
                        #, nSample = 500
                        ){

  library(zoo)
  library(chron)
  library(xts)

  #browser()

  files0 = list.files(dir, pattern = pattern , full.names=T)
  info_all = file.info(files0)

  while(1 < 2){

    #increment file info
    files1 = list.files(dir, pattern = pattern, full.names=T,recursive=T)
    files2 = files1[ ! basename(files1) %in% basename(files0)]

    if(length(files2) > 0 | (!"info_new" %in% ls()) ){

      files0 = c(files2,files0)
      info_new = file.info(files2)
      info_all = rbind(info_all,info_new)
      zoo_all = zoo((1:length(info_all$mtime)),sort(info_all$mtime))
      min_idx = min(as.numeric(.index(zoo_all)))
      idx_all = as.numeric(.index(zoo_all))
      min_idx_all = min(idx_all)
      idx_hours_all = (idx_all - min_idx_all) / 60 / 60
      n_complete = length(files0)
      n_remaining = nTotal - n_complete
      zoo_all_5 = diff(to.period(zoo_all,period="minutes",k=5)$zoo_all.High) / 5
      idx_all_5 = as.numeric(.index(zoo_all_5))
      idx_hours_all_5 = (idx_all_5 - min(idx_all_5)) / 60 / 60

      try(dev.off())

      par(mfrow=c(3,1))

      interval_mins = c(5,15)

      #expected time until completion
      for(i in 1:length(interval_mins)){

        zoo_i = to.period(zoo_all,period="minutes",k=interval_mins[i])
        diff_i_0 = diff(zoo_i$zoo_all.High)

        if(length(diff_i_0)>2)
          diff_i_1 = diff_i_0[-c(1,length(diff_i_0))]
        else
          diff_i_1 = diff_i_0

        proc_time_mins_i = nTotal / ((diff_i_1) / interval_mins[i])
        proc_time_hrs_i = proc_time_mins_i / 60
        proc_time_dys_i = proc_time_hrs_i / 24

        remaining_time_mins_i = (nTotal - cumsum(diff_i_0)) / ((diff_i_1) / interval_mins[i])
        remaining_time_hrs_i = remaining_time_mins_i / 60
        remaining_time_dys_i = remaining_time_hrs_i / 24

        idx_hours_i = (as.numeric(.index(remaining_time_hrs_i)) - min_idx) / 60 / 60
        idx_days_i = idx_hours_i / 24

        if(i==1){
          par(mar=c(0,4,5,2))
          #Remaining.Hours = remaining_time_hrs_i
          try(plot(idx_hours_i, as.numeric(remaining_time_dys_i)
                   ,ylab="Days left"
                   ,xlab ="" # c(paste("Hour of Processing at",as.character(Sys.time())),paste(n_complete,"completed out of",nTotal))
                   ,main=c("Time Remaining To Complete Processing",
                           "(Press [Enter] to Update, [Esc] to stop)")[c(F,prompt)]
                   ,type="l"
                   ,ylim = ylim_days
                   ,xaxt="n"
                   ,lwd=2
                   ,col="blue"
                   )
              )

          abline(h=seq(0,100,2))

        }else{
          try(lines(idx_hours_i, as.numeric(remaining_time_dys_i),col=i,lwd=2))
        }
        abline(v=seq(0,100000,24))


        # legend("top",legend=paste(interval_mins,"min"),col=1:length(interval_mins)
        #        ,lwd=1,ncol=length(interval_mins),text.width =.5 ,pch=15 )

      }

      par(mar=c(2,4,2,2))
      #number completed over time
      try(plot(idx_hours_all, 1:length(idx_hours_all)
               ,ylab="Total # Outputs"
               , xlab = "" #c(paste("Hour of Processing at",as.character(Sys.time())),paste(n_complete,"completed out of",nTotal))
               ,main=c("")
               ,type="l"
               ,xaxt="n"
               ,lwd=2
               ,col="blue"

      )
      )
      abline(v=seq(0,100000,24))

      par(mar=c(5,4,0,2))
      #number completed per minute
      try(plot(idx_hours_all_5, as.numeric(zoo_all_5)
               ,ylab="Outputs Per Minute"
               ,xlab = c(paste("Hour of Processing at",as.character(Sys.time())),paste(n_complete,"completed out of",nTotal))
               ,main=c("")
               ,type="l"
               ,lwd=2
               ,col="blue"

      )
      )
      abline(h=seq(0,100,2))
      abline(v=seq(0,100000,24))



    }

    if(!prompt) Sys.sleep(update_seconds)
    else text_in = readline(prompt="Press [Enter] to update progress, [Esc] to stop")
    if(text_in == "b") browser()
  }

}

progress_monitor(
  #dir = "C:\\Temp\\test_progress"
  dir = "E:/projects/DAP_NAIP_OR_2018/gridmetrics/gridmetrics_csv"
  ,pattern = "[.]csv"
  ,nTotal = 98060
  #,nTotal = 700
  ,ylim_days = c(0,12)
  ,prompt=T
  #, interval_mins=c(10,30,60)
)








