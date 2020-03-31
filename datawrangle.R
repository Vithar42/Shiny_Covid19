
# John Hopkens data broke on 3/23/20, this is someone who is taking their broken data stream and fixing it.
# https://github.com/cipriancraciun/covid19-datasets
# 
#baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/jhu/v1/"
#baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/nytimes/v1/us-counties/"
#
#
# New Data Sourec https://github.com/cipriancraciun/covid19-datasets NY Times data that is not broken like the John Hopkins Data.
#
#
# New Data source https://github.com/garykac/covid19 contains positive and negative testing results and Hospitalizations
#
#




minutesSinceLastUpdate = function(fileName) {
 (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

loadDatatsv = function(fileName, baseURL) {
  if (!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {

    data = read_tsv(file.path(baseURL, fileName)) %>%
      select( -location_type, -location_lat, -location_long) %>% 
      filter(country_code == "US")
    
    save(data, file = fileName)  
  } else {
    load(file = fileName)
  }
  return(data)
}

loadDatacsv = function(fileName, baseURL) {
  if (!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
    
    data = read_csv(file.path(baseURL, fileName))
    
    save(data, file = fileName)  
  } else {
    load(file = fileName)
  }
  return(data)
}

# https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/nytimes/v1/us-counties/values.tsv"
baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/nytimes/v1/us-counties/"
loadDatatsv("values.tsv", baseURL)



# https://raw.githubusercontent.com/garykac/covid19/master/data/states-daily.csv
baseURL <- "https://raw.githubusercontent.com/garykac/covid19/master/data/"
fileName <- "states-daily.csv"
loadDatacsv(fileName, baseURL)
