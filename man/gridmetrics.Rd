% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gridmetrics.r
\name{gridmetrics}
\alias{gridmetrics}
\title{summarize lidar for raster pixels}
\usage{
gridmetrics(
  las_files,
  dtm_files = NA,
  dtm_folder = NA,
  dtm_ext = ".dtm",
  no_dtm = F,
  fun = compute_metrics2,
  xmin = NA,
  xmax = NA,
  ymin = NA,
  ymax = NA,
  res = 66,
  grid = NA,
  n_read = NA,
  out_name = NA,
  return = F,
  con = NA,
  out_table = NA,
  ...
)
}
\arguments{
\item{las_files}{list of paths to las files}

\item{dtm_files}{optional list of paths to dtms}

\item{dtm_folder}{optional folder of dtms}

\item{dtm_ext}{what is the file extension of dtms}

\item{xmin}{(optional) force extent of analysis}

\item{xmax}{(optional) force extent of analysis}

\item{ymin}{(optional) force extent of analysis}

\item{ymax}{(optional) force extent of analysis}

\item{res}{resolution of processing analysis in units of provided lidar / dtm}

\item{grid}{pre-defined raster for analysis - lidar metrics are computed for each cell}

\item{intersect_only}{only provide outputs over areas in which lidar and dtms intersect?}

\item{fns}{list of functions that will be used to generate columns of summary statistics from the las data}
}
\value{
optional dataframe of xy coordinates for cell centers and additional columns of metrics
}
\description{
summarize lidar for raster pixels
}
\details{
summarize lidar for raster pixels


 This program is free software but it is provided WITHOUT WARRANTY
 and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
 you can redistribute it and/or modify it under the terms of the GNU
 General Public License as published by the Free Software Foundation;
 either version 2 of the License, or (at your option) any later version.



\cr
Revision History
\tabular{ll}{
1.0 \tab 2017 March 08 Created\cr
}
}
\examples{
dtms1="C:\\\\temp\\\\dtm_test\\\\"
las1="C:\\\\temp\\\\lidar_test\\\\"
las_files=list.files(las1,pattern="[.]las",full.names=T)
dtm_files=list.files(dtms1,pattern="[.]dtm",full.names=T)
metrics=gridmetrics(las_files=las_files,dtm_files=dtm_files)
head(metrics)

}
\seealso{
\code{\link{read_las}}\cr \code{\link{read_dtm}}\cr
}
\author{
Jacob Strunk <Jstrunk@fs.fed.us>
}
