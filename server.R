# Data from:
# Johns Hopkins University Center for System Science and Engineering (JHU CCSE)

library(tidyverse)

# Johns Hopkins repository for COVID-19 data, https://github.com/CSSEGISandData/COVID-19
#baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series"

f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")

minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

loadData = function(fileName, columnName) {
  if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
      select(-Lat, -Long) %>% 
      pivot_longer(-(1:2), names_to="date", values_to=columnName) %>% 
      mutate(
        date=as.Date(date, format="%m/%d/%y"),
        `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
        `Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
      )
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  return(data)
}


  allData = 
    loadData("time_series_19-covid-Confirmed_archived_0325.csv", "CumConfirmed") %>%
    inner_join(loadData("time_series_19-covid-Deaths_archived_0325.csv", "CumDeaths"))

 #allData = 
 #  loadData("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
 #    inner_join(loadData("time_series_covid19_deaths_global.csv", "CumDeaths"))

 filename <-  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
 statePop <-  read.csv(file.path(filename), check.names=FALSE, stringsAsFactors=FALSE) %>%
   select(NAME, CENSUS2010POP )
 
#allData = 
#  loadData("time_series_covid19_confirmed_US.csv", "Confirmed") %>%
#    inner_join(loadData("time_series_covid19_deaths_US.csv", "Deaths")) %>%
#    inner_join(loadData("time_series_covid19_testing_US.csv", "testing"))


# Main FUnction ----
function(input, output, session) {
  
  data = reactive({
    d = allData %>%
      filter(`Country/Region` == "US") %>% 
      group_by(date) %>% 
      summarise_if(is.numeric, sum, na.rm=TRUE)
    
    d %>%
      mutate(
        dateStr = format(date, format="%b %d, %Y"),    # Jan 20, 2020
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        #NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  Cumdata = reactive({
    allData %>%
      rename(state = `Province/State`,
             country = `Country/Region`) %>%
      filter(country == "US",
             #CumConfirmed > 50,
             date > "2020-03-04")
  })
  
  output$CumulatedPlot = renderPlotly({

    df <- Cumdata() %>%
      rename(Confirmed = CumConfirmed, Deaths = CumDeaths) %>%
      gather(key = "key", value = "value", Confirmed, Deaths) %>%
      filter(key %in% input$metrics) %>% 
      group_by(date, key) %>% 
      summarise(value = sum(value))
    
    ggplot(df, aes(x = date, y = value, fill = key)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none")

    ggplotly()
    
  })
  
  statedata = reactive({
    test <- allData %>%
      rename(state = `Province/State`,
             country = `Country/Region`) %>%
      filter(country == "US",
             state %in% state.name,
             date > "2020-03-04",
             state %in% input$states)
  })
  
  output$statePlot = renderPlotly({
    
    df <- statedata()

    ggplot(df, aes(x = date, y = CumConfirmed, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none")
    
    ggplotly()
    
  })
  
  linedupstatedata = reactive({
    allData %>%
      rename(state = `Province/State`,
             country = `Country/Region`) %>%
      filter(country == "US",
             state %in% state.name,
             date > "2020-03-04",
             state %in% input$states) %>%
      select(-country) %>%
      group_by(state) %>%
      mutate(dayNo = if_else(CumConfirmed > 25, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
  })
  
  output$linedupstatePlot = renderPlotly({
    
    df <- linedupstatedata()
    
    ggplot(df, aes(x = dayNo, y = CumConfirmed, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none")
    
    ggplotly()
    
  })
  
  statecapita <- reactive({
    
    statePop <- statePop %>%
      select(state = NAME, pop = CENSUS2010POP)
    
    
    statedata <- linedupstatedata() %>%
    #statedata <- statedata() %>%
      select(state, dayNo, CumConfirmed)
    
   statedata %>%
      left_join(statePop, by = "state") %>%
      mutate(pop = pop/1000,
             conpercapita =  CumConfirmed/pop)
    

    
  })
  
  output$lineduppercapitastatePlot = renderPlotly({
    
    df <- statecapita()
    
    ggplot(df, aes(x = dayNo, y = conpercapita, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none")
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })

  
  
}


