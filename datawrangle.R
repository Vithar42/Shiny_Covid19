
# John Hopkens data broke on 3/23/20, this is someone who is taking their broken data stream and fixing it.
# https://github.com/cipriancraciun/covid19-datasets

#baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/jhu/v1/"
baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/nytimes/v1/us-counties/"

minutesSinceLastUpdate = function(fileName) {
 (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

loadData = function(fileName) {
  if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {

    data = read_tsv(file.path(baseURL, fileName)) %>%
      select( -location_type, -location_lat, -location_long) %>% 
      filter(country_code == "US")
    
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  return(data)
}


loadData("values.tsv")
