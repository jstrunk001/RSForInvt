#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#cors KMZ: https://www.ngs.noaa.gov/CORS_Map/corsdownload?id=kmz

get_base=function(from,to,xy,n=1,ftp="",download = F,cors_kmz ="" ,get=c(".gz",".Z")){


  library(maptools)
  cds = getKMLcoordinates(textConnection(system("unzip -p /Users/foo/test.kmz", intern = TRUE)))


}
.download_base=function(){



}

.choose_nearest=function(xy,ftp){





}


