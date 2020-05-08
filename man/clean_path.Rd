% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_path.R
\name{clean_path}
\alias{clean_path}
\alias{backslash}
\alias{forwardslash}
\title{standardize path to file}
\usage{
clean_path(path, backslash = T, force_endslash = F)

backslash(path)

forwardslash(path)
}
\arguments{
\item{path}{supply a path}

\item{backslash}{T/F backslashes or forwardslashes?}

\item{force_endslash}{T/F cap path with slash?}
}
\value{
updated path
}
\description{
paths often have extra slashes, or in opposing directions. This function standardizes slashes
}
\details{
standardizes slashes - forward or back

\cr

Revision History
\tabular{ll}{
1.0 \tab 2015 Jan 29 Roxygen header prepared \cr
}
}
\examples{

       clean_path("c:\\\\temp",F,F)
       clean_path("c:\\\\temp",T,F)
       clean_path("c:\\\\temp",T,T)

       clean_path("c:/temp/",F,F)
       clean_path("c:\\\\temp\\\\\\\\",T,F)
       clean_path("c:\\\\temp\\\\\\\\",T,T)




}
\seealso{
\code{\link{grep}}\cr \code{\link{gsub}}\cr
}
\author{
Jacob Strunk <jstrunk@fs.fed.us>
}