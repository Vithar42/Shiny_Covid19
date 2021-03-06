library(tidyverse)
library(scales)
library(plotly)
library(leaflet)
library(geojsonio)
library(sp)

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


#baseURL <- "https://raw.githubusercontent.com/garykac/covid19/master/data/"
#fileName <- "states-daily.csv"

#### Function to check if it data is older than an hour ####
garykac_csv = function(fileName, baseURL) {
  
  statename <- data.frame(state = state.name, abb = state.abb)

  #### Code to pull dataset live from source ####
  data = read_csv(file.path(baseURL, fileName)) %>%                                      # inport data from url
    mutate_if(is.numeric , replace_na, replace = 0) %>%                                  # Make all NA's in table zeros
    mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%                             # Fixes the date to a standard format
    select(-pending, -total, -hash, -dateChecked, -fips, abb = state) %>%                # Remove unused columns
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
DateStateCompPlot <- function(MyData, var, titlelab, Ylab, logscaletoggle) {
  
  p <- ggplot(MyData, aes_string(x = "date", y = var, colour = "state")) +
    #geom_point() +
    geom_line() +
    scale_x_date(date_labels = "%b-%d", date_breaks  = "1 day") +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") +
    labs(title = titlelab,
         x = "Date", 
         y = Ylab)

  if (logscaletoggle == "Log") {
    
    p <- p + scale_y_log10(labels = comma)
    return(p)
    
  } else {
    p <- p + scale_y_continuous(labels = comma)
    return(p)
    
  }
  
}

# used for n plots on Hospiliation tab
CountPlotFunction <- function(MyData, ii) {
  
  MyData <- MyData %>%
    ungroup()
  
  MyData$state2 <- MyData$state
  
  MyPlot <- ggplot(MyData %>% filter(state == input$states[[ii]]), aes(x = date, y = hospitalized, color = state)) +
    geom_line(data = MyData[,2:14], aes(x = date, y = hospitalized, group = state2), colour = "grey") +
    #geom_point() +
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
slidestartdatePlotFunction <- function(MyData, var, titlelab, Xlab, Ylab, slidedate, logscaletoggle) {
  
  p <- ggplot(MyData, aes_string(x = "dayNo", y = var, colour = "state")) +
    #geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(0,max(MyData$dayNo),1)) +
    theme(legend.position = "none") +
    labs(title = titlelab,
         x = paste("Day from", slidedate, Xlab), 
         y = Ylab)
  
  
  if (logscaletoggle == "Log") {
    
    p <- p + scale_y_log10(labels = comma)
    return(p)
    
  } else {
    p <- p + scale_y_continuous(labels = comma)
    return(p)
    
  }
  
}

# used for facet plots on all tabs
facetPlotFunction <- function(MyData, var, logscaletoggle) {
  
  p <- ggplot(MyData, aes_string(x = "dayNo", y = var , color = "state")) +
    geom_line(data = MyData[,2:length(names(MyData))], aes_string(x = "dayNo", y = var , group = "state2"), colour = "grey") +
    geom_line() +
    facet_wrap(~ state, scales = "free_y", ncol = 3) +
    theme(legend.position = "none") 
  
  if (logscaletoggle == "Log") {
    
    p <- p + scale_y_log10(labels = comma)
    return(p)
    
  } else {
    p <- p + scale_y_continuous(labels = comma)
    return(p)
    
  }
}

# used for 4th plots on all tabs
slidestartdatePopPlotFunction <- function(MyData, var, titlelab, Xlab, Ylab, slidedate, logscaletoggle) {

  p <- ggplot(MyData, aes_string(x = "dayNo", y = var, colour = "state")) +
    #geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(0,max(MyData$dayNo),1)) +
    theme(legend.position = "none") +
    labs(title = titlelab,
         x = paste("Day Since", slidedate, Xlab), 
         y = Ylab)
  
  if (logscaletoggle == "Log") {
    
    p <- p + scale_y_log10(labels = comma)
    return(p)
    
  } else {
    p <- p + scale_y_continuous(labels = comma)
    return(p)
    
  }
  
}

# Animation Plot



## Rmarkdown Inclusion code ----

encoding <- getOption("shiny.site.encoding", default = "UTF-8")

## options for knitting/rendering rmarkdown chunks
knitr::opts_chunk$set(
  echo = FALSE,
  comment = NA,
  cache = FALSE,
  message = FALSE,
  warning = FALSE
)

## function to render .md files to html
inclMD <- function(path) {
  markdown::markdownToHTML(
    path,
    fragment.only = TRUE,
    options = "",
    stylesheet = "",
    encoding = encoding
  )
}

## function to render .Rmd files to html - does not embed image or add css
inclRmd <- function(path, r_env = parent.frame()) {
  paste(
    readLines(path, warn = FALSE, encoding = encoding),
    collapse = '\n'
  ) %>%
    knitr::knit2html(
      text = .,
      fragment.only = TRUE,
      envir = r_env,
      options = "",
      stylesheet = "",
      encoding = encoding
    ) %>%
    gsub("&lt;!--/html_preserve--&gt;","",.) %>%  ## knitr adds this
    gsub("&lt;!--html_preserve--&gt;","",.) %>%   ## knitr adds this
    HTML
}

## make html table
make_table <- function(dat, width = "50%") {
  knitr::kable(
    dat,
    align = "c",
    format = "html",
    table.attr = paste0("class='table table-condensed table-hover' style='width:", width, ";'")
  )
}

# Pull in Data using datawrangler ----  
# Pull in State population data from the sensus ----  
filename <-  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
statePop <-  read.csv(file.path(filename), check.names=FALSE, stringsAsFactors=FALSE) %>%
  select(state = NAME, pop = CENSUS2010POP )

# # Pull in NewYorkTimes Data from https://github.com/cipriancraciun/covid19-datasets ----  
# baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/nytimes/v1/us-counties/"
# fileName <- "values.tsv"
# allData <- NYtimes_tsv(fileName, baseURL)

# Pull in covidtracking.com Data from https://github.com/garykac/covid19/tree/master/data ----  
baseURL <- "https://raw.githubusercontent.com/garykac/covid19/master/data/"
fileName <- "states-daily.csv"
alldata <- garykac_csv("states-daily.csv", baseURL) %>%
  filter(date > as.Date("2020-03-01"))



#### Function to import NewYork Times Counry Level Data ####
NYT_Countu_csv = function(fileName, baseURL) {
  
  statename <- data.frame(state = state.name, abb = state.abb)
  
  #### Code to pull dataset live from source ####
  data = read_csv(file.path(baseURL, fileName)) %>%                                      # inport data from url
    mutate_if(is.numeric , replace_na, replace = 0) %>%                                  # Make all NA's in table zeros
    filter(state %in% state.name) %>%
    arrange(state, date) %>%
    select(state, everything())
  
  return(data)

}

# Pull in covidtracking.com Data from https://github.com/garykac/covid19/tree/master/data ----  
baseURL <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/"
fileName <- "us-counties.csv"
alldata_county <- NYT_Countu_csv(fileName, baseURL) %>%
  filter(date > as.Date("2020-03-01"))

# For state UI page
state_ui_fun <- function(state){
  
  #variable that the .Rmd files are exposed to that works as a filter
  tabstate <- state
  
  fluidRow(
    column(6,
           renderUI({
             inclRmd("./ColumnLeft.Rmd")
           })
    ),
    column(6,
           renderUI({
             inclRmd("./ColumnRight.Rmd")
           })
    )
  )
  
}

# For state UI page
state_ui_Pred_fun <- function(state, logscaletoggle){
  
  #variable that the .Rmd files are exposed to that works as a filter
  tabstate <- state
  logscaletoggle <- logscaletoggle
  
  fluidRow(
     renderUI({
       inclRmd("./predictions.Rmd")
       })
     )
}

# For stateTesting UI page
state_ui_Testing_fun <- function(state, logscaletoggle){
  
  #variable that the .Rmd files are exposed to that works as a filter
  tabstate <- state
  logscaletoggle <- logscaletoggle
  
  fluidRow(
    renderUI({
      inclRmd("./StateTesting.Rmd")
    })
  )
}

#### function to build leaflet map ----
map_builder_states <- function(statedata, statedataagg, label){
  states <- geojson_read("DATA/gz_2010_us_040_00_500k.json", what = "sp")
  #class(states)
  #names(states)
  #"Infections", "Deaths", "Tests"


  states <- merge(states, statedataagg, duplicateGeoms = T)
  
  m <- leaflet(states) %>%
    addTiles(
      urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
      attribution = 'Google'
    ) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

  

    bins <- seq(from = round(min(na.omit(states$data)),0), to = round(max(na.omit(states$data)),0), length.out = 10)
    
    pal <- colorBin("YlOrRd", domain = round(states$data,0), bins = bins)
    
    labels <- paste("<strong>", states$NAME, "</strong><br/>", comma(round(states$data,0)), "total", label) %>% 
      lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(data),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(weight = 5,
                                   color = "#666",         
                                   dashArray = "",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))

  m %>% addLegend(pal = pal, 
                  values = ~data, 
                  opacity = 0.7, 
                  title = NULL,
                  position = "bottomright")
    
}

# state Shutdowns data ----
# StayHomeOrder data from: https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html
# Schoo_Close data from: https://thejournal.com/Articles/2020/03/17/List-of-States-Shutting-Down-All-Their-Schools-Grows-to-36.aspx?Page=3
# Bar_Close data from: https://www.today.com/food/which-states-have-closed-restaurants-bars-due-coronavirus-t176039
StateOrders <- tribble(
  ~state, ~Bar_Close,~School_Close,~StayHomeOrder, ~CurbsidePickup, ~StaySafeOrder,
  #--|--|--|--|--|--         
  "Alabama",        NA, "03-16-2020","04-04-2020", NA, NA,
  "Alaska",         NA, "03-16-2020","03-28-2020", NA, NA,
  "Arizona",        NA, "03-16-2020","03-31-2020", NA, NA,
  "Arkansas",       NA, "03-17-2020", NA, NA, NA,
  "California",     NA, "03-16-2020","03-19-2020", NA, NA,
  "Colorado",       "03-17-2020", "03-23-2020","03-26-2020", NA, NA,
  "Connecticut",    NA, "03-16-2020","03-23-2020", NA, NA,
  "Delaware",       NA, "03-16-2020","03-24-2020", NA, NA,
  "Florida",        "03-17-2020", "03-16-2020","04-03-2020", NA, NA,
  "Georgia",        NA, "03-18-2020","04-03-2020", NA, NA,
  "Hawaii",         NA, "03-16-2020","03-25-2020", NA, NA,
  "Idaho",          NA, "03-23-2020","03-25-2020", NA, NA,
  "Illinois",       "03-16-2020", "03-16-2020","03-21-2020", NA, NA,
  "Indiana",        "03-16-2020", "03-16-2020","03-25-2020", NA, NA,
  "Iowa",           "03-17-2020", "03-16-2020", NA, NA, NA,
  "Kansas",         NA, "03-16-2020","03-30-2020", NA, NA,
  "Kentucky",       "03-16-2020", "03-16-2020","03-26-2020", NA, NA,
  "Louisiana",      NA, "03-16-2020","03-23-2020", NA, NA,
  "Maine",          NA, "03-31-2020","04-02-2020", NA, NA,
  "Maryland",       "03-12-2020", "03-16-2020","03-30-2020", NA, NA,
  "Massachusetts",  "03-17-2020", "03-16-2020","03-24-2020", NA, NA,
  "Michigan",       NA, "03-16-2020","03-24", NA, NA,
  "Minnesota",      "03-17-2020", "03-18-2020","03-28-2020", "05-04-2020", "05-18-2020",
  "Mississippi",    NA, "03-16-2020","04-03-2020", NA, NA,
  "Missouri",       "03-13-2020", "03-16-2020","04-06-2020", NA, NA,
  "Montana",        NA, "03-16-2020","03-28-2020", NA, NA,
  "Nebraska",       NA, "03-16-2020", NA, NA, NA,
  "Nevada",         "03-24-2020", "03-16-2020","04-01-2020", NA, NA,
  "New Hampshire",  NA, "03-16-2020","03-28-2020", NA, NA,
  "New Jersey",     "03-17-2020", "03-18-2020","03-21-2020", NA, NA,
  "New Mexico",     NA, "03-16-2020","03-2-20204", NA, NA,
  "New York",       "03-17-2020", "03-18-2020","03-22-2020", NA, NA,
  "North Carolina", "03-17-2020", "03-16-2020","03-30-2020", NA, NA,
  "North Dakota",   NA, "03-16-2020","03-24-2020", NA, NA,
  "Ohio",           "03-15-2020", "03-16-2020","03-24-2020", NA, NA,
  "Oklahoma",       NA, "03-17-2020", NA, NA, NA,
  "Oregon",         "03-16-2020", "03-16-2020","03-23-2020", NA, NA,
  "Pennsylvania",   "03-16-2020", "03-16-2020","04-01-2020", NA, NA,
  "Rhode Island",   "03-09-2020", "03-16-2020","03-28-2020", NA, NA,
  "South Carolina", "03-17-2020", "03-16-2020","04-07-2020", NA, NA,
  "South Dakota",   NA, "03-16-2020", NA, NA, NA,
  "Tennessee",      NA, "03-20-2020","04-01-2020", NA, NA,
  "Texas",          NA, "03-20-2020","04-02-2020", NA, NA,
  "Utah",           NA, "03-16-2020", NA, NA, NA,
  "Vermont",        "03-13-2020", "03-18-2020","03-25-2020", NA, NA,
  "Virginia",       NA, "03-16-2020","03-30-2020", NA, NA,
  "Washington",     "03-16-2020", "03-16-2020","03-23-2020", NA, NA,
  "West Virginia",  NA, "03-16-2020","03-24-2020", NA, NA,
  "Wisconsin",      NA, "03-18-2020","03-25-2020", NA, NA,
  "Wyoming",        NA, "03-16-2020","03-28-2020", NA, NA
)


