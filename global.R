library(tidyverse)
library(scales)
library(plotly)

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
alldata <- garykac_csv("states-daily.csv", baseURL)


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
# state Shutdowns data ----
# StayHomeOrder data from: https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html
# Schoo_Close data from: https://thejournal.com/Articles/2020/03/17/List-of-States-Shutting-Down-All-Their-Schools-Grows-to-36.aspx?Page=3
# Bar_Close data from: https://www.today.com/food/which-states-have-closed-restaurants-bars-due-coronavirus-t176039
StateOrders <- tribble(
  ~state, ~Bar_Close,~School_Close,~StayHomeOrder,
  #--|--|--|--        
  "Alabama",        NA, "03-16","04-04",
  "Alaska",         NA, "03-16","03-28",
  "Arizona",        NA, "03-16","03-31",
  "Arkansas",       NA, "03-17", NA,
  "California",     NA, "03-16","03-19",
  "Colorado",       "03-17", "03-23","03-26",
  "Connecticut",    NA, "03-16","03-23",
  "Delaware",       NA, "03-16","03-24",
  "Florida",        "03-17", "03-16","04-03",
  "Georgia",        NA, "03-18","04-03",
  "Hawaii",         NA, "03-16","03-25",
  "Idaho",          NA, "03-23","03-25",
  "Illinois",       "03-16", "03-16","03-21",
  "Indiana",        "03-16", "03-16","03-25",
  "Iowa",           "03-17", "03-16", NA,
  "Kansas",         NA, "03-16","03-30",
  "Kentucky",       "03-16", "03-16","03-26",
  "Louisiana",      NA, "03-16","03-23",
  "Maine",          NA, "03-31","04-02",
  "Maryland",       "03-12", "03-16","03-30",
  "Massachusetts",  "03-17", "03-16","03-24",
  "Michigan",       NA, "03-16","03-24",
  "Minnesota",      "03-17", "03-18","03-28",
  "Mississippi",    NA, "03-16","04-03",
  "Missouri",       "03-13", "03-16","04-06",
  "Montana",        NA, "03-16","03-28",
  "Nebraska",       NA, "03-16", NA,
  "Nevada",         "03-24", "03-16","04-01",
  "New Hampshire",  NA, "03-16","03-28",
  "New Jersey",     "03-17", "03-18","03-21",
  "New Mexico",     NA, "03-16","03-24",
  "New York",       "03-17", "03-18","03-22",
  "North Carolina", "03-17", "03-16","03-30",
  "North Dakota",   NA, "03-16","03-24",
  "Ohio",           "03-15", "03-16","03-24",
  "Oklahoma",       NA, "03-17", NA,
  "Oregon",         "03-16", "03-16","03-23",
  "Pennsylvania",   "03-16", "03-16","04-01",
  "Rhode Island",   "03-09", "03-16","03-28",
  "South Carolina", "03-17", "03-16","04-07",
  "South Dakota",   NA, "03-16", NA,
  "Tennessee",      NA, "03-20","04-01",
  "Texas",          NA, "03-20","04-02",
  "Utah",           NA, "03-16", NA,
  "Vermont",        "03-13", "03-18","03-25",
  "Virginia",       NA, "03-16","03-30",
  "Washington",     "03-16", "03-16","03-23",
  "West Virginia",  NA, "03-16","03-24",
  "Wisconsin",      NA, "03-18","03-25",
  "Wyoming",        NA, "03-16","03-28"
)


