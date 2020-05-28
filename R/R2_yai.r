#'@title
#'  Compute R2 values from yaImpute::yai() object or data frame returned by yaImpute::impute(...)
#'
#'@description
#'  Compute R2 values from yaImpute::yai() object or data frame returned by yaImpute::impute(...)
#'
#'@details
#'
#' This function is a repurposing of the yaImpute::rmsd() function to compute R.squared instead.
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
#'1.0 \tab 5/27/2020 Created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param x yaImpute::yai() object or data frame returned by yaImpute::impute(...)
#'@param vars subset of variables
#'@param ... additional arguments to impute() if yaImpute::yai() object is provided
#'
#'@return
#'  Data.frame with 1 column of R2 values
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

yai_r2 = function (x, vars = NULL, ...){
  if (missing(object))
    stop("object required.")
  if (class(object)[1] == "yai")
    object = impute.yai(object, vars = vars, observed = TRUE,...)
  if (is.null(object))
    stop("no imputations found using this object")
  nuke = unlist(lapply(object, function(x) all(is.na(x))))
  nuke = nuke[nuke]
  if (length(nuke) > 0)
    object = object[, -match(names(nuke), names(object)),
                    drop = FALSE]
  object = na.omit(object)
  if (is.null(vars))
    vars = names(object)
  vi = paste(unique(strsplit(vars, ".o", fixed = TRUE)))
  vi = intersect(vi, names(object))
  notFound = setdiff(vars, names(object))
  if (length(notFound) > 0)
    warning("variables not found or had missing values: ",
            paste(notFound, collapse = ", "))
  if (length(vi) == 0)
    stop("nothing to compute")
  vo = paste(vi, "o", sep = ".")
  notFound = setdiff(vo, names(object))
  if (length(notFound) > 0)
    warning("variables not found or had missing values: ",
            paste(notFound, collapse = ", "))
  vo = intersect(vo, names(object))
  both = intersect(paste(unique(strsplit(vo, ".o", fixed = TRUE))),
                   vi)
  if (length(both) == 0)
    stop("nothing to compute")
  vo = paste(both, "o", sep = ".")
  R2_in = data.frame(rep(NA, length(vo)), row.names = both)
  names(R2_in) = "R2_in"
  for (i in 1:length(both)) {
    if (!is.factor(object[, both[i]])) {

      R2_in[i, 1] = 1- var(object[, both[i]])  / var(object[,vo[i]])

    }
  }
  R2_in
}
