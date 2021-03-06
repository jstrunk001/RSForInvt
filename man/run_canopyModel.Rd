% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_canopyModel.R
\name{run_canopyModel}
\alias{run_canopyModel}
\title{run gridmetrics across a project}
\usage{
run_canopyModel(
  proj_polys = NA,
  proj_gpkg_path = NA,
  layer_proj_polys = "RSForInvt_prj",
  layer_proj_config = "RSForInvt_config",
  dir_out = "c:/temp/test_project/canopymodel",
  n_core = 4,
  fusion_path = "c:\\\\fusion\\\\canopymodel.exe",
  cellsize = 5,
  xyunits = c("F", "M"),
  zunits = c("F", "M"),
  cdsys = c(0, 1, 2),
  zone = c(0, 1, 2),
  hzd = c(0, 1, 2),
  vtd = c(0, 1, 2, 3),
  outlier = c(-5, 400),
  fusion_switches = NA,
  xmn = 561066,
  xmx = 2805066,
  ymn = 33066,
  ymx = 1551066,
  fun = compute_metrics2,
  temp = "c:\\\\temp\\\\run_canopymodel\\\\",
  dir_dtm = NA,
  dir_las = NA,
  new_dtm_path = c(from = NA, to = NA),
  new_las_path = c(from = NA, to = NA),
  skip_existing = T,
  existing_coms = c(NA,
    "C:\\\\Temp\\\\run_gridmetrics\\\\2017Aug17_100740\\\\all_commands_canopyModel.txt"),
  debug = F,
  ...
)
}
\arguments{
\item{dir_out}{where should csv files be sent}

\item{n_core}{number of corest to run process on}

\item{fusion_path}{where is gridmetrics.exe (FUSION)}

\item{cellsize}{output raster resolution}

\item{outlier}{c(-5,500) range of inclusion}

\item{fusion_switches}{other fusion switches as a string e.g. "/noground"}

\item{xmn, xmx, ymn, ymx}{set extent for analysis}

\item{fun}{a custom function if gridmetrics is not used}

\item{temp}{temp folder to hold batch files for use with gridmetrics.exe}

\item{dir_dtm}{in case path to dtms has changed from lasR_project}

\item{dir_las}{in case path to las has changed from lasR_project}

\item{skip_existing}{skip tiles if they already have output csvs}

\item{existing_coms}{path to existing batch comamnds, incase processing was interrupted the first time}

\item{lasR_project_csv}{csv file of intersections created by lasR_project() function}

\item{lasR_project__gpkg}{polygon file of intersections created by lasR_project() function}

\item{heightbreak}{Height break for cover calculation}

\item{minht}{set minht for gridmetrics.ex}

\item{first}{T/F use only first returns}

\item{intensity}{T/F include intensity metrics}

\item{fast_cache}{experimental - a high-speed temporary folder to copy las files to}

\item{n_cache}{experimental - number of las files to copy to cache}

\item{con}{a parallel connection, optional, function can also create parallel threads}

\item{table}{output folder name}
}
\value{
<Delete and Replace>
}
\description{
<Delete and Replace>
}
\details{
<Delete and Replace>

\cr
Revision History
\tabular{ll}{
1.0 \tab 2018-01-28 Header added \cr
1.1 \tab 2020-03-17 force csv or geopackage \cr
}
}
\examples{

 gmi=run_gridmetrics(
lasR_project_poly="D:\\\\projects\\\\2017_WA_DSM_Pilot_usgs\\\\2017Aug_NAIP_usgs\\\\lasR_project003.shp"
,dir_out="I:\\\\projects\\\\2017_WA_DSM_Pilot\\\\2017Aug_NAIP_usgs\\\\gridmetrics_07\\\\"
,dir_dtm="c:\\\\usgs_dtms\\\\dtms\\\\"
,dir_las="D:\\\\naip_2015_laz\\\\"
,n_core=10
,existing_coms="C:\\\\Temp\\\\run_gridmetrics\\\\2018Jan21_152618\\\\all_commands.txt"
,fast_cache=c(rep("r:\\\\temp",10),rep("c:\\\\temp",3),rep("i:\\\\temp",3),rep(NA,3))
,n_cache=400
)



}
\seealso{
\code{\link{lasR_project}}\cr \code{\link{gridmetrics}}\cr
}
\author{
Jacob Strunk <Jstrunk@fs.fed.us>
}
