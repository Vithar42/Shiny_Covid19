
# John Hopkens data broke on 3/23/20, this is someone who is taking their broken data stream and fixing it.
# https://github.com/cipriancraciun/covid19-datasets
# 
# New Data Sourec https://github.com/cipriancraciun/covid19-datasets NY Times data that is not broken like the John Hopkins Data.
#
# New Data source https://github.com/garykac/covid19 contains positive and negative testing results and Hospitalizations
# his data is a repakaging for usabaility the data from the https://covidtracking.com/ which is also possibly the NY Times source

#### Data loading functoins ----
#### Function to check if it data is older than an hour ----
minutesSinceLastUpdate = function(fileName) {
 (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

#### Function to access the NY Times TSV dataset ####
NYtimes_tsv = function(fileName, baseURL) {
  
#### Code to pull dataset live from source ####
  data = read_tsv(file.path(baseURL, fileName)) %>%                                         # inport data from url
    select( -location_type, -location_lat, -location_long) %>%                              # dropping location data
    filter(country_code == "US",                                                            # isolate only the USA
           is.na(administrative)) %>%                                                       # removes al county data
    select(state = province, date, Confirmed = delta_confirmed, Deaths = delta_deaths) %>%  # Rename some columns for easy use
    filter(state %in% state.name,                                                           # removes all none states
           date > "2020-03-01") %>%                                                         # sets the data's starting point
    mutate(Confirmed = replace_na(Confirmed, 0),                                            # replace Confirmed NA's with 0s
           Deaths = replace_na(Deaths, 0)) %>%                                              # replace Death NA's with 0s
    arrange(state, date)                                                                    # Lineup data by state and date
  
  return(data)

  #### Code to check age of downloaded dataset, and update it older than an hour ####  
  # if (!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
  # 
  #   data = read_tsv(file.path(baseURL, fileName)) %>%
  #     select( -location_type, -location_lat, -location_long) %>% 
  #     filter(country_code == "US")
  #   
  #   #save(data, file = fileName)  
  # } else {
  #   load(file = fileName)
  # }
  # return(data)
}

#### Function to check if it data is older than an hour ####
garykac_csv = function(fileName, baseURL) {
  
  statename <- data.frame(state = state.name, abb = state.abb)

  #### Code to pull dataset live from source ####
  data = read_csv(file.path(baseURL, fileName)) %>%                                      # inport data from url
    replace(is.na(.), 0) %>%                                                              # Make all NA's in table zeros
    mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%                              # Fixes the date to a standard format
    select(-pending, -total, -hash, -dateChecked, -fips, abb = state) %>%                 # Remove unused columns
    left_join(statename, by = "abb") %>%
    filter(state %in% state.name) %>%
    arrange(state, date) %>%
    select(state, everything())
    
  return(data)
  
  #### Code to check age of downloaded dataset, and update it older than an hour ####    
  # if (!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
  #  
  #  data2 = read_csv(file.path(baseURL, fileName))
  #  
  #  #save(data2, file = fileName)  
  # } else {
  #  load(file = fileName)
  # }
  # return(data2)
}


# Reused Plot Functions ----
# used for 1st plot on each tab
DateStateCompPlot <- function(MyData, var, titlelab, Ylab) {
  
  p <- ggplot(MyData, aes_string(x = "date", y = var, colour = "state")) +
    geom_point() +
    geom_line() +
    scale_x_date(date_labels = "%b-%d", date_breaks  = "1 day") +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") +
    labs(title = titlelab,
         x = "Date", 
         y = Ylab)
  
  return(p)
}

# used for n plots on Hospiliation tab
CountPlotFunction <- function(MyData, ii) {
  
  MyData <- MyData %>%
    ungroup()
  
  MyData$state2 <- MyData$state
  
  MyPlot <- ggplot(MyData %>% filter(state == input$states[[ii]]), aes(x = date, y = hospitalized, color = state)) +
    geom_line(data = MyData[,2:14], aes(x = date, y = hospitalized, group = state2), colour = "grey") +
    geom_point() +
    geom_line() + 
    scale_x_date(date_labels="%b-%d",date_breaks  ="1 day") + 
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") + 
    labs(title=paste("Hospitalizations in",input$states[[ii]]),
         x ="Absoulte Date", 
         y = "Hospitalizations")
  
  return(MyPlot)
}

# used for 2nd plots on all tabs
slidestartdatePlotFunction <- function(MyData, var, titlelab, Ylab, slidedate) {
  
  MyPlot <- ggplot(MyData, aes_string(x = "dayNo", y = var, colour = "state")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(0,max(MyData$dayNo),1)) +
    theme(legend.position = "none") +
    labs(title = titlelab,
         x = paste("Day from",slidedate,"infections"), 
         y = Ylab)
  
  return(MyPlot)
}

# used for facet plots on all tabs
facetPlotFunction <- function(MyData, var) {
  
  MyPlot <- ggplot(MyData, aes_string(x = "dayNo", y = var , color = "state")) +
    geom_line(data = MyData[,2:length(names(MyData))], aes_string(x = "dayNo", y = var , group = "state2"), colour = "grey") +
    geom_line() +
    facet_wrap(~ state, scales = "free_y", ncol = 3) +
    theme(legend.position = "none") 
  
  return(MyPlot)
}

# used for 4th plots on all tabs
slidestartdatePopPlotFunction <- function(MyData, var, titlelab, Ylab, slidedate) {

  MyPlot <- ggplot(MyData, aes_string(x = "dayNo", y = var, colour = "state")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(0,max(MyData$dayNo),1)) +
    theme(legend.position = "none") +
    labs(title = titlelab,
         x = paste("Day from",slidedate,"infections/100000"), 
         y = Ylab)
  
  return(MyPlot)
  
}
