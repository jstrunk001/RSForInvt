#'@name clean_path
#'@title
#'standardize path to file
#'
#'@description
#'
#'paths often have extra slashes, or in opposing directions. This function standardizes slashes
#'
#'@details
#'
#'standardizes slashes - forward or back
#'
#'\cr
#'
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2015 Jan 29 Roxygen header prepared \cr
#'}
#'
#'
#'@author
#'Jacob Strunk <jstrunk@fs.fed.us>
#'
#'@param path supply a path
#'@param backslash T/F backslashes or forwardslashes?
#'@param force_endslash T/F cap path with slash?
#'
#'@return
#'updated path
#'
#'
#'@examples
#'
#'        clean_path("c:\\temp",F,F)
#'        clean_path("c:\\temp",T,F)
#'        clean_path("c:\\temp",T,T)
#'
#'        clean_path("c:/temp/",F,F)
#'        clean_path("c:\\temp\\\\",T,F)
#'        clean_path("c:\\temp\\\\",T,T)
#'
#'
#'

#'
#'@seealso \code{\link{grep}}\cr \code{\link{gsub}}\cr
#'
#'
#Desired upgrades to this function:
#
#
##main function body
#'@rdname clean_path
#'@export
clean_path=function(
              path
              ,backslash=T
              ,force_endslash=F

             ){

             path_in=unlist(path)

             #fix paths
             #path_ok=gsub("XXXLEADINGXXX","\\\\\\\\",gsub("\\\\\\\\","\\\\",gsub("^\\\\\\\\","XXXLEADINGXXX",gsub("/","\\\\",gsub("\\\\$","",gsub("/$","",path_in))))))
             path_ok=gsub("XXXLEADINGXXX","\\\\\\\\",gsub("\\\\\\\\","\\\\",gsub("^\\\\\\\\","XXXLEADINGXXX",gsub("/","\\\\",gsub("\\\\\\\\$","",gsub("//$","",path_in))))))

             #if slashcap
             if(force_endslash) path_ok=paste(gsub("\\\\$","",path_ok),"\\",sep="")

             #use forward slash
             if(!backslash) path_ok= gsub("\\\\","/",path_ok)

             #return data
             return (path_ok)

             }
#'@rdname clean_path
#'@export
        backslash=function(path) clean_path(path,backslash=T)
#'@rdname clean_path
#'@export
        forwardslash=function(path) clean_path(path,backslash=F)

