
#cors KMZ: https://www.ngs.noaa.gov/CORS_Map/corsdownload?id=kmz

get_base=function(from,to,xy,n=1,ftp="",download = F,cors_kmz ="" ,get=c(".gz",".Z")){


  library(maptools)
  cds = getKMLcoordinates(textConnection(system("unzip -p /Users/foo/test.kmz", intern = TRUE)))


}
.download_base=function(){



}

.choose_nearest=function(xy,ftp){





}


