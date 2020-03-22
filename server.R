# Data from:
# Johns Hopkins University Center for System Science and Engineering (JHU CCSE)

library(reshape2)
library(dplyr)
library(tidyr)  
library(ggplot2)
library(plotly)
library(stringr)

# Johns Hopkins repository for COVID-19 data, https://github.com/CSSEGISandData/COVID-19
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

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
  loadData("time_series_19-covid-Confirmed.csv", "CumConfirmed") %>%
    inner_join(loadData("time_series_19-covid-Deaths.csv", "CumDeaths")) %>%
    inner_join(loadData("time_series_19-covid-Recovered.csv", "CumRecovered"))

function(input, output, session) {
  
  data = reactive({
    d = allData %>%
      filter(`Country/Region` == input$country)
    if(input$state != "<all>") {
      d = d %>% 
        filter(`Province/State` == input$state) 
    } else {
      d = d %>% 
        group_by(date) %>% 
        summarise_if(is.numeric, sum, na.rm=TRUE)
    }
    
    d %>%
      mutate(
        dateStr = format(date, format="%b %d, %Y"),    # Jan 20, 2020
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  observeEvent(input$country, {
    states = allData %>%
      filter(`Country/Region` == input$country) %>% 
      pull(`Province/State`)
    states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state", choices=states, selected=states[1])
  })
  
  countries = sort(unique(allData$`Country/Region`))
  
  updateSelectInput(session, "country", choices=countries, selected="US")
  
  renderBarPlot = function(varPrefix, legendPrefix, yaxisTitle) {
    renderPlotly({
      data = data()
      
      plt = data %>% 
        plot_ly() %>%
        filter(CumConfirmed > 0) %>%
        config(displayModeBar=FALSE) %>%
        layout(
          barmode='group', 
          xaxis=list(
            title="", tickangle=-90, type='category', ticktext=as.list(data$dateStr), 
            tickvals=as.list(data$date), gridwidth=1), 
          yaxis=list(
            title=yaxisTitle
          ),
          legend=list(x=0.05, y=0.95, font=list(size=15), bgcolor='rgba(240,240,240,0.5)')#,
          #font=f1
        )
      
      for(metric in input$metrics) 
        plt = plt %>%
          add_trace(
            x= ~date, y=data[[paste0(varPrefix, metric)]], type='bar', 
            name=paste(legendPrefix, metric, "Cases"),
            marker=list(
              color=switch(metric, Deaths='rgb(200,30,30)', Recovered='rgb(30,200,30)', Confirmed='rgb(100,140,240)'),
              line=list(color='rgb(8,48,107)', width=1.0)
            )
          )
      
      plt
    })
  }
  
  
  output$CumulatedPlot = renderPlotly({
    
    usData <- allData %>%
      rename(state = `Province/State`,
             country = `Country/Region`) %>%
      filter(country == "US",
             CumConfirmed > 50) 
    
    test <- usData %>%
      rename(Confirmed = CumConfirmed, Deaths = CumDeaths, Recovered = CumRecovered) %>%
      gather(key = "key", value = "value", Confirmed, Deaths, Recovered) %>%
      filter(key %in% input$metrics)
    
    test <- test %>% 
      group_by(date, key) %>% 
      summarise(value = sum(value))
    
    
    # ggplot(usData, aes(x = date, y = CumConfirmed, fill = state)) +
    #   geom_bar(position="dodge", stat = "identity") + 
    #   facet_wrap(~state, ncol = 5) + 
    #   theme(legend.position = "none")
    
    p <- ggplot(test, aes(x = date, y = value, fill = key)) +
      geom_bar(position="dodge", stat = "identity") +
      #facet_wrap(~state, ncol = 5) + 
      theme(legend.position = "none")
    
    ggplotly(p)
    
  })
  
  
  
  output$filletedPlot = renderPlotly({
    
    usData <- allData %>%
      rename(state = `Province/State`,
             country = `Country/Region`) %>%
      filter(country == "US",
             state %in% state.name,
             CumConfirmed > 0,
             state %in% input$states)
    
    test <- usData %>%
      rename(Confirmed = CumConfirmed, Deaths = CumDeaths, Recovered = CumRecovered) %>%
      gather(key = "key", value = "value", Confirmed, Deaths, Recovered) %>%
      filter(key %in% input$metrics)
    
    test2 <- test %>%
      filter(key == "Confirmed") %>%
      spread(state, value)
    
    
    # ggplot(usData, aes(x = date, y = CumConfirmed, fill = state)) +
    #   geom_bar(position="dodge", stat = "identity") + 
    #   facet_wrap(~state, ncol = 5) + 
    #   theme(legend.position = "none")
     
     # ggplot(test, aes(x = date, y = value, fill = key)) +
     #     geom_bar(position="dodge", stat = "identity") +
     #     facet_wrap(~state, ncol = 5) + 
     #     theme(legend.position = "none")
    
     p <- ggplot(usData, aes(x = date, y = CumConfirmed , group = state, color = state)) +
       geom_line() + 
       theme(legend.position = "none")

     ggplotly(p)
    
  })
  
  output$dailyMetrics = renderBarPlot("New", legendPrefix="New", yaxisTitle="New Cases per Day")
  output$cumulatedMetrics = renderBarPlot("Cum", legendPrefix="Cumulated", yaxisTitle="Cumulated Cases")
  

  
    
}


