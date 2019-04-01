pkglist<-c("dplyr", "lubridate", "stringr", "tidyr", "rgeos", 
           "maptools", "ggplot2", "ggrepel", "ggmap", "gridExtra", 
           "evaluate", "formatR", "highr", "markdown", "yaml", 
           "htmltools", "caTools", "knitr", "rmarkdown", "lubridate", 
           "stringr")
pkgcheck <- pkglist %in% row.names(installed.packages())
pkglist[!pkgcheck]
#COMMMENT the line below if you installed packages earlier e.g on root
#for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}
#this command is for root instalation of missing packages:
if(length(pkglist[!pkgcheck])) cat("install.packages(c(");j=0; for(i in pkglist[!pkgcheck]) { j<-j+1 ;  if(j == length(pkglist[!pkgcheck])) cat(paste('"',i,'"',sep="")) else cat(paste('"',i,'",',sep=""));} ; cat("),depend=TRUE)")

#loading all libraries - necessary to avoid errors of execution
#for(i in pkglist) library(i, character.only = TRUE);


rxPrivacyControl(FALSE)
rxOptions(xdfCompressionLevel = 9)
options(encoding = "UTF-8"); par(ask=F)

# http://www.andresmh.com/nyctaxitrips/
# https://github.com/Azure/LearnAnalytics-AnalyzingBigDataWithMRS/tree/master/instructor_resources
# Load the MicrosoftML library
library(MicrosoftML)
#rxCompressXdf(mysource, xdfCompressionLevel = 9, outFile=myfile)

# in transform func you set labels: as.factor(dataList$x, levels = c(1, 2,3), 
#                            labels = c("One", "Two", "Three")) 


data_dir <- "data"
library(lubridate)
most_recent_date <- ymd("2016-07-01") # the day of the months is irrelevant

col_classes <- c(
  'pickup_datetime'       = "character",
  'dropoff_datetime'      = "character",
  'passenger_count'       = "integer",
  'trip_distance'         = "numeric",
  'pickup_longitude'      = "numeric",
  'pickup_latitude'       = "numeric",
  'rate_code_id'          = "factor",
  'dropoff_longitude'     = "numeric",
  'dropoff_latitude'      = "numeric",
  'payment_type'          = "factor",
  'fare_amount'           = "numeric",
  'extra'                 = "numeric",
  'mta_tax'               = "numeric",
  'tip_amount'            = "numeric",
  'tolls_amount'          = "numeric",
  'improvement_surcharge' = "numeric",
  'total_amount'          = "numeric")

# because we keep appending to the same file, we can't run this in parallel
st <- Sys.time()
ii=1
for(ii in 1:6) { # get each month's data and append it to the first month's data
  file_date <- most_recent_date - months(ii)
  input_csv <- sprintf('yellow_tripdata_%s.csv', substr(file_date, 1, 7))
  input_csv <- file.path(data_dir, input_csv)
  input_xdf <- sprintf('yellow_tripdata_%s.xdf', substr(file_date, 1, 7))
  input_xdf <- file.path(data_dir, input_xdf)
  append <- if (ii == 1) "none" else "rows"
  dataout <- rxImport(input_csv, outFile=input_xdf, colClasses = col_classes, overwrite = TRUE)
  rxGetInfo(dataout, getVarInfo=TRUE)
  print(input_csv)
}
Sys.time() - st # stores the time it took to import

output_xdf <- file.path(data_dir, 'yellow_tripdata_2016.xdf')

st <- Sys.time()
ii=1
for(ii in 1:6) { # get each month's data and append it to the first month's data
  file_date <- most_recent_date - months(ii)
  input_xdf <- sprintf('yellow_tripdata_%s.xdf', substr(file_date, 1, 7))
  input_xdf <- file.path(data_dir, input_xdf)
  nyc_xdf <- RxXdfData(input_xdf)
  append <- if (ii == 1) "none" else "rows"
  dataout <- rxImport(nyc_xdf, outFile=output_xdf, 
                      colClasses = col_classes, overwrite = TRUE, append = append)
  rxGetInfo(dataout, getVarInfo=TRUE)
  print(input_xdf)
}
Sys.time() - st # stores the time it took to import


#http://www.mediafire.com/file/u9coo2s9sxuu94f/yellow_tripdata_2016.xdf/file
nyc_xdf <- RxXdfData(output_xdf)
rxGetInfo(nyc_xdf, getVarInfo=TRUE)

system.time(rxsum_xdf <- rxSummary( ~ fare_amount, nyc_xdf))
rxsum_xdf


