
library(lasR)
dir_csv = "D:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics\\gridmetrics_csv\\"
dir_sqlite = "D:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics\\gridmetrics_sqlite\\"
if(!dir.exists(dir_sqlite)) dir.create(dir_sqlite)

require(RSQLite)
db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2017_NAIPDAP_Metrics.db"),sychronous = "off")

csv_to_sqlite(db=db
              ,csv_folder=dir_csv
              #,n_load=150
              ,skip_loaded=T
)





###Debug

if(F){

  dir_sqlite="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\sqlite"
  require(RSQLite)
  db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics_usgs007test.db"),sychronous = "off")
  summ=dbReadTable(db,"gm_summary")
  gm=dbReadTable(db,"gm")

  summ[grep(93421,summ$file),]


}
