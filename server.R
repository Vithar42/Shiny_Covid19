# Data from:
# Johns Hopkins University Center for System Science and Engineering (JHU CCSE)

library(tidyverse)
library(scales)
library(plotly)
#source("~/datawrangle.R")

# Johns Hopkins repository for COVID-19 data, https://github.com/CSSEGISandData/COVID-19
#baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
# baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series"

# f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")

# minutesSinceLastUpdate = function(fileName) {
#   (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
# }
# 
# loadData = function(fileName, columnName) {
#   if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
#     data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
#       select(-Lat, -Long) %>% 
#       pivot_longer(-(1:2), names_to="date", values_to=columnName) %>% 
#       mutate(
#         date=as.Date(date, format="%m/%d/%y"),
#         `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
#         `Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
#       )
#     save(data, file=fileName)  
#   } else {
#     load(file=fileName)
#   }
#   return(data)
# }
# 
# 
#   allData = 
#     loadData("time_series_19-covid-Confirmed_archived_0325.csv", "CumConfirmed") %>%
#     inner_join(loadData("time_series_19-covid-Deaths_archived_0325.csv", "CumDeaths"))

  filename <-  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
  statePop <-  read.csv(file.path(filename), check.names=FALSE, stringsAsFactors=FALSE) %>%
    select(NAME, CENSUS2010POP )
  
  load("values.tsv")
  allData <- data %>%
    filter(is.na(administrative)) %>%
    select(state = province, date, Confirmed = delta_confirmed, Deaths = delta_deaths) %>%
    filter(state %in% state.name,
           date > "2020-03-01") %>%
    mutate(Confirmed = replace_na(Confirmed, 0),
           Deaths = replace_na(Deaths, 0)) %>%
    arrange(state, date)

 #allData = 
 #  loadData("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
 #    inner_join(loadData("time_series_covid19_deaths_global.csv", "CumDeaths"))


 
#allData = 
#  loadData("time_series_covid19_confirmed_US.csv", "Confirmed") %>%
#    inner_join(loadData("time_series_covid19_deaths_US.csv", "Deaths")) %>%
#    inner_join(loadData("time_series_covid19_testing_US.csv", "testing"))


# Main FUnction ----
function(input, output, session) {
  
  # Plot 1 for USA Total Tab ----
  output$CumulatedPlot = renderPlotly({

    df <- allData %>%
      arrange(date) %>%
      gather(key = "key", value = "value", Confirmed, Deaths) %>%
      filter(value >= 0) %>%
      filter(key %in% input$metrics) %>% 
      group_by(date, key) %>% 
      summarise(value = sum(value)) %>%
      group_by(key) %>%
      mutate(cumsum = cumsum(value))
    
    ggplot(df, aes(x = date, y = cumsum, fill = key)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title="Reported Cumulative US Infections",
           x ="Date", 
           y = "Infections")

    ggplotly()
    
  })
  
  # Plot 2 for USA Total Tab ----
  output$CumulatedPlotinfected = renderPlotly({
    
    df <- allData %>%
      arrange(date) %>%
      gather(key = "key", value = "value", Confirmed, Deaths) %>%
      filter(value >= 0) %>%
      filter(key %in% input$metrics) %>% 
      group_by(date, key) %>% 
      summarise(value = sum(value)) %>%
      group_by(key) %>%
      mutate(cumsum = cumsum(value)) %>%
      mutate(infected = cumsum / (input$deathrate/100))
    
    ggplot(df, aes(x = date, y = infected, fill = key)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day")  +
      scale_y_continuous(labels = comma) +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title="Predicted Cumulative US Infections from death rate",
           x ="Date", 
           y = "Infections")
    
    ggplotly()
    
  })
  
  # 1st Plot for Infection by State Tab ----
  statedata = reactive({
    df <- allData %>%
      select(-Deaths) %>%
      group_by(state, date) %>%
      #filter(state == "New Jersey") %>%
      filter(state %in% input$states) %>% 
      gather(key = "key", value = "value", Confirmed) %>%
      group_by(state, date) %>% 
      summarise(value = sum(value)) %>%
      group_by(state) %>%
      mutate(cumsum = cumsum(value))
  })
  
  output$statePlot = renderPlotly({
    
    df <- statedata()

    ggplot(df, aes(x = date, y = cumsum, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title="Infections by State",
           x ="Absoulte Date", 
           y = "Infections")
    
    ggplotly()
    
  })

  # 2nd Plot for Infection by State Tab ----  
  linedupstatedata = reactive({
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(cumsum > input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
  })
  
  output$linedupstatePlot = renderPlotly({
    
    df <- linedupstatedata()
    
    ggplot(df, aes(x = dayNo, y = cumsum, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none") +
      labs(title="Infections by State",
           x = paste("Day from",input$dayo,"infections"), 
           y = "Infections")
    
    ggplotly()
    
  })
  
  # 3rd Plot for Infection by State Tab ----   
  output$facetPlot1 = renderPlotly({
    
    df <- linedupstatedata() %>%
      ungroup()
    
    df$state2 <- df$state
    
    ggplot(df, aes(x = dayNo, y = cumsum , color = state)) +
      geom_line(data = df[,2:6], aes(x = dayNo, y = cumsum , group = state2), colour = "grey") +
      geom_line() +
      facet_wrap(~ state, scales = "free_y", ncol = 3) +
      theme(legend.position = "none") 
    
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })

  # 4th Plot for Infection by State Tab ----    
  statecapita <- reactive({
    
    statePop <- statePop %>%
      select(state = NAME, pop = CENSUS2010POP)
    
    
    statedata <- linedupstatedata() %>%
      select(state, dayNo, cumsum)
    
    statecapita <- statedata %>%
      left_join(statePop, by = "state") %>%
      mutate(pop = pop/100000,
             conpercapita =  cumsum/pop) %>%
      mutate(dayNo = if_else(conpercapita >= input$dayocap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
  })
  
  output$lineduppercapitastatePlot = renderPlotly({
    
    df <- statecapita()
    
    ggplot(df, aes(x = dayNo, y = conpercapita, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none") +
      labs(title="Infections by State",
           x ="Day from 0.05 infections/100000", 
           y = "Infections per 100000 people")
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })

  # 5th Plot for Infection by State Tab ----   
  output$facetPlot2 = renderPlotly({
    
    df <- statecapita() %>%
      ungroup()
 
    df$state2 <- df$state
    
     ggplot(df, aes(x = dayNo, y = conpercapita, color = state)) +
       geom_line(data = df[,2:6], aes(x = dayNo, y = conpercapita, group = state2), colour = "grey") +
       geom_line() +
       facet_wrap(~ state, scales = "free_y", ncol = 3) +
       theme(legend.position = "none") 
    
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })
  
  # 1st Plot for Deaths by State Tab ----   
  statedeathdata = reactive({
    df <- allData %>%
      select(-Confirmed) %>%
      group_by(state, date) %>%
      #filter(state == "New Jersey") %>%
     filter(state %in% input$states) %>% 
      gather(key = "key", value = "value", Deaths) %>%
      group_by(state, date) %>% 
      summarise(value = sum(value)) %>%
      group_by(state) %>%
      mutate(cumsum = cumsum(value))
  })
  
  output$stateDeathPlot = renderPlotly({
    
    df <- statedeathdata()
    
    ggplot(df, aes(x = date, y = cumsum, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title="Infections by State",
           x ="Date", 
           y = "Deaths")
    
    ggplotly()
    
  })
  
  # 2nd Plot for Deaths by State Tab ----  
  linedupstatedeathdata = reactive({
    df <- statedeathdata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(cumsum > input$dayodeath, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
  })
  
  output$linedupstatedeathPlot = renderPlotly({
    
    df <- linedupstatedeathdata()
    
    ggplot(df, aes(x = dayNo, y = cumsum, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none") +
      labs(title="Deaths by State",
           x = paste("Day from",input$dayodeath,"Deaths"),
           y = "Deaths")
    
    ggplotly()
    
  })
  
  # 3rd Plot for Deaths by State Tab ----   
  output$facetPlot3 = renderPlotly({
    
    df <- linedupstatedeathdata() %>%
      ungroup()
    
    df$state2 <- df$state
    
    ggplot(df, aes(x = dayNo, y = cumsum , color = state)) +
      geom_line(data = df[,2:6], aes(x = dayNo, y = cumsum , group = state2), colour = "grey") +
      geom_line() +
      facet_wrap(~ state, scales = "free_y", ncol = 3) +
      theme(legend.position = "none") 
    
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })
  
  # 4th Plot for Deaths by State Tab ----    
  statecapitadeath <- reactive({
    
    statePop <- statePop %>%
      select(state = NAME, pop = CENSUS2010POP)
    
    
    statedata <- linedupstatedeathdata() %>%
      select(state, dayNo, cumsum)
    
    statecapita <- statedata %>%
      left_join(statePop, by = "state") %>%
      mutate(pop = pop/100000,
             conpercapita =  cumsum/pop) %>%
      mutate(dayNo = if_else(conpercapita >= input$dayodeathcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
  })
  
  output$lineduppercapitadeahtstatePlot = renderPlotly({
    
    df <- statecapitadeath()
    
    ggplot(df, aes(x = dayNo, y = conpercapita, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none") +
      labs(title="Deaths by State",
           x = paste("Day from",input$dayodeathcap,"Deaths/100000"),
           y = "Deaths per 100000 people")
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })
  
  # 5th Plot for Deaths by State Tab ----   
  output$facetPlot4 = renderPlotly({
    
    df <- statecapitadeath() %>%
      ungroup()
    
    df$state2 <- df$state
    
    ggplot(df, aes(x = dayNo, y = conpercapita, color = state)) +
      geom_line(data = df[,2:6], aes(x = dayNo, y = conpercapita, group = state2), colour = "grey") +
      geom_line() +
      facet_wrap(~ state, scales = "free_y", ncol = 3) +
      theme(legend.position = "none") 
    
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })
  
  # 1st Plot for infection prediction ----     
  statedeathratedata = reactive({

    df <- statedeathdata() %>%
      mutate(infected = cumsum / (input$deathrate/100))
    
  })
  
  output$stateDeathratePlot = renderPlotly({
    
    df <- statedeathratedata()
    
    ggplot(df, aes(x = date, y = infected, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title="Predicted Infections by State",
           x ="Date", 
           y = "Deaths")
    
    ggplotly()
    
  })
  
  # 2nd Plot for infection prediction ----  
  linedupstatedeathratedata = reactive({
    df <- statedeathratedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(infected > input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
  })
  
  output$linedupstatedeathratePlot = renderPlotly({
    
    df <- linedupstatedeathratedata()
    
    ggplot(df, aes(x = dayNo, y = infected, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none") +
      labs(title="Deaths by State",
           x = paste("Day from",input$dayo,"Deaths"),
           y = "Deaths")
    
    ggplotly()
    
  })
  
  # 3rd Plot for infection prediction ----   
  output$facetPlot5 = renderPlotly({
    
    df <- linedupstatedeathratedata() %>%
      ungroup()
    
    df$state2 <- df$state
    
    ggplot(df, aes(x = dayNo, y = infected , color = state)) +
      geom_line(data = df[,2:7], aes(x = dayNo, y = infected , group = state2), colour = "grey") +
      geom_line() +
      facet_wrap(~ state, scales = "free_y", ncol = 3) +
      theme(legend.position = "none") 
    
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })
  
  # 4th Plot for infection prediction ----    
  statecapita <- reactive({
    
    statePop <- statePop %>%
      select(state = NAME, pop = CENSUS2010POP)
    
    
    statedata <- linedupstatedeathratedata() %>%
      select(state, dayNo, infected)
    
    statecapita <- statedata %>%
      left_join(statePop, by = "state") %>%
      mutate(pop = pop/100000,
             conpercapita =  infected/pop) %>%
      mutate(dayNo = if_else(conpercapita >= input$dayocap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
  })
  
  output$lineduppercapitastateratePlot = renderPlotly({
    
    df <- statecapita()
    
    ggplot(df, aes(x = dayNo, y = conpercapita, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none") +
      labs(title="Predicted Infections by State",
           x ="Day from 0.05 infections/100000", 
           y = "Infections per 100000 people")
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })
  
  # 5th Plot for infection prediction ----   
  output$facetPlot6 = renderPlotly({
    
    df <- statecapita() %>%
      ungroup()
    
    df$state2 <- df$state
    
    ggplot(df, aes(x = dayNo, y = conpercapita, color = state)) +
      geom_line(data = df[,2:6], aes(x = dayNo, y = conpercapita, group = state2), colour = "grey") +
      geom_line() +
      facet_wrap(~ state, scales = "free_y", ncol = 3) +
      theme(legend.position = "none") 
    
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })
}
