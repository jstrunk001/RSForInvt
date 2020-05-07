#'@title
#'  gsub using a vector or pattern and replacement values
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/1/2020 created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param pattern
#'@param replacement
#'@param ... all other values as described in gsub
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@export
#
#'@seealso \code{\link{gsub}}\cr
#'



gsubVec = function(pattern,replacement, x ,  ...){ 
	
	for(i in 1:length(pattern)){
		x = gsub( pattern[i] , replacement[i], x ,  ...)
	}
	x
}