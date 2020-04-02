# Data from:
# Johns Hopkins University Center for System Science and Engineering (JHU CCSE)

library(tidyverse)
library(scales)
library(plotly)
source("datawrangle.R")

# Pull in Data using datawrangler ----  
# Pull in State population data from the sensus ----  
filename <-  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
statePop <-  read.csv(file.path(filename), check.names=FALSE, stringsAsFactors=FALSE) %>%
  select(NAME, CENSUS2010POP )

# Pull in NewYorkTimes Data from https://github.com/cipriancraciun/covid19-datasets ----  
baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/nytimes/v1/us-counties/"
fileName <- "values.tsv"
allData <- NYtimes_tsv(fileName, baseURL)

# Pull in covidtracking.com Data from https://github.com/garykac/covid19/tree/master/data ----  
baseURL <- "https://raw.githubusercontent.com/garykac/covid19/master/data/"
fileName <- "states-daily.csv"
hosdata <- garykac_csv("states-daily.csv", baseURL)

# Main Shiny Function ----  
function(input, output, session) {
  
  # Reused Plot Functions ----
  # used for 1st plot on each tab
  DateStateCompPlot <- function(MyData, titlelab, Ylab) {
    # data must contain date, value, and state columns
    p <- ggplot(MyData, aes(x = date, y = value, colour = state)) +
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
  slidestartdatePlotFunction <- function(MyData, titlelab, Ylab, slidedate) {
    
    MyPlot <- ggplot(MyData, aes(x = dayNo, y = cumsum, colour = state)) +
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
  facetPlotFunction <- function(MyData) {
    
    MyPlot <- ggplot(MyData, aes(x = dayNo, y = plotvalue , color = state)) +
      geom_line(data = MyData[,2:length(names(MyData))], aes(x = dayNo, y = plotvalue , group = state2), colour = "grey") +
      geom_line() +
      facet_wrap(~ state, scales = "free_y", ncol = 3) +
      theme(legend.position = "none") 
    
    return(MyPlot)
  }
  
  # used for 4th plots on all tabs
  slidestartdatePopPlotFunction <- function(MyData, titlelab, Ylab, slidedate) {
    
    MyPlot <- ggplot(MyData, aes(x = dayNo, y = conpercapita, colour = state)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0,max(df$dayNo),1)) +
      theme(legend.position = "none") +
      labs(title="Infections by State",
          x ="Day from 0.05 infections/100000", 
          y = "Infections per 100000 people")
    
    return(MyPlot)
  }
  
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
      mutate(infected = if_else((key == "Confirmed"),cumsum / (input$deathrate/100), cumsum))
    
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
    
    df <- statedata() %>%
      rename(delta_value = value, value = cumsum)
    
    DateStateCompPlot(df, "Infections by State", "Reported Infections")

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
    
    slidestartdatePlotFunction(df, "Infections by State", "Infections", input$dayo)

    ggplotly()
    
  })
  
  # 3rd Plot for Infection by State Tab ----   
  output$facetPlot1 = renderPlotly({
    
    df <- linedupstatedata() %>%
      ungroup() %>%
      rename(plotvalue = cumsum)
    
    df$state2 <- df$state
    
    facetPlotFunction(df)

    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })

  # 4th Plot for Infection by State Tab -----    
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
    
    slidestartdatePopPlotFunction(df, "Infections by State", "Infections per 100000 people", input$dayocap)

    ggplotly()

  })

  # 5th Plot for Infection by State Tab ----   
  output$facetPlot2 = renderPlotly({
    
    df <- statecapita() %>%
      ungroup() %>%
      rename(plotvalue = conpercapita)
 
    df$state2 <- df$state
    
    facetPlotFunction(df)

    ggplotly()

  })
  
  # 1st Plot for Deaths by State Tab ----   
  statedeathdata = reactive({
    df <- allData %>%
      select(-Confirmed) %>%
      group_by(state, date) %>%
      # filter(state == "New Jersey") %>%
      filter(state %in% input$states) %>% 
      gather(key = "key", value = "value", Deaths) %>%
      group_by(state, date) %>% 
      summarise(value = sum(value)) %>%
      group_by(state) %>%
      mutate(cumsum = cumsum(value))
  })
  
  output$stateDeathPlot = renderPlotly({
    
    df <- statedeathdata() %>%
      rename(delta_value = value, value = cumsum)
    
    DateStateCompPlot(df, "Dearhs by State", "Reported Deaths")

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
    
    slidestartdatePlotFunction(df, "Deaths by State", "Deaths", input$dayodeath)

    ggplotly()
    
  })
  
  # 3rd Plot for Deaths by State Tab ----   
  output$facetPlot3 = renderPlotly({
    
    df <- linedupstatedeathdata() %>%
      ungroup() %>%
      rename(plotvalue = cumsum)
    
    df$state2 <- df$state
    
    facetPlotFunction(df)

    ggplotly()

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
    
    slidestartdatePopPlotFunction(df, "Deaths by State", "Deaths per 100000 people", input$dayodeathcap)

    ggplotly()

  })
  
  # 5th Plot for Deaths by State Tab ----   
  output$facetPlot4 = renderPlotly({
    
    df <- statecapitadeath() %>%
      ungroup() %>%
      rename(plotvalue = conpercapita)
    
    df$state2 <- df$state
    
    facetPlotFunction(df)
    
    ggplotly()

  })
  
  # 1st Plot for infection prediction ----     
  statedeathratedata = reactive({

    df <- statedeathdata() %>%
      mutate(infected = cumsum / (input$deathrate/100))
    
  })
  
  output$stateDeathratePlot = renderPlotly({
    
    df <- statedeathratedata() %>%
      rename(delta_value = value, value = infected)
    
    DateStateCompPlot(df, "Predicted Infections by State", "Predicted Infections")

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
    
    slidestartdatePlotFunction(df, "Infections by State", "Deaths", input$dayo)

    ggplotly()
    
  })
  
  # 3rd Plot for infection prediction ----   
  output$facetPlot5 = renderPlotly({
    
    df <- linedupstatedeathratedata() %>%
      ungroup() %>%
      rename(plotvalue = infected)
    
    df$state2 <- df$state
    
    facetPlotFunction(df)

    ggplotly()

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
    
    slidestartdatePopPlotFunction(df, "Predicted Infections by State", "Infections per 100000 people", input$dayocap)
    
    ggplotly()
    
    #assuming 3% mortality, 25% daily spread, 19 days between infection and death)
    
  })
  
  # 5th Plot for infection prediction ----   
  output$facetPlot6 = renderPlotly({
    
    df <- statecapita() %>%
      ungroup() %>%
      rename(plotvalue = conpercapita)
    
    df$state2 <- df$state
    
    facetPlotFunction(df)
    
    ggplotly()

  })
  
  # 1st plot for hospilizatoins tab ----
  hospedata = reactive({
    df <- hosdata %>%
      filter(state %in% input$states)
  })
  
  output$statehospilizations = renderPlotly({
    
    df <- hospedata() %>%
      rename(value = hospitalized)
    
    DateStateCompPlot(df, "Predicted Infections by State", "Predicted Infections")
    
    ggplotly()
    
  })
  
  # 2nd Plot for hospilizatoins tab ----  

  
  # 3rd Plot for hospilizatoins tab ----   

  
  # 4 to n Plot for hospilizatoins tab ---- 
  # gen plot containers
  output$ui_plot <- renderUI({ 
    out <- list()
    if (length(input$states) == 0) {
      return(NULL)
    }
    
    for (i in 1:length(input$states)){
      out[[i]] <-  plotlyOutput(paste0("plot",i), width = "100%")
    }  
    return(out) 
  })

  # observer to fill the plot containers
  observe({  
    for (i in 1:length(input$states)){  
      local({  #because expressions are evaluated at app init
        ii <- i 
        output[[paste0('plot',ii)]] <- renderPlotly({ 
          if ( length(input$states) > ii-1 ){  
            return(CountPlotFunction(hospedata(), ii))
          } 
          NULL
        })
      })
    }                                  
  })
  
}
