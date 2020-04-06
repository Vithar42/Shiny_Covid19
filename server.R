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
  select(state = NAME, pop = CENSUS2010POP )

# # Pull in NewYorkTimes Data from https://github.com/cipriancraciun/covid19-datasets ----  
# baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/nytimes/v1/us-counties/"
# fileName <- "values.tsv"
# allData <- NYtimes_tsv(fileName, baseURL)

# Pull in covidtracking.com Data from https://github.com/garykac/covid19/tree/master/data ----  
baseURL <- "https://raw.githubusercontent.com/garykac/covid19/master/data/"
fileName <- "states-daily.csv"
alldata <- garykac_csv("states-daily.csv", baseURL)

# Main Shiny Function ----  
function(input, output, session) {

  # to use to simulate input when testing
  # input <- list(states = c("Minnesota", "Michigan", "Wisconsin","North Dakota", "Ohio", "South Dakota", "Iowa"),
  #               metrics = c("positive", "death"))

  # Reactive data prep
  statedata = reactive({
    
    alldata %>%
      filter(state %in% input$states) %>%
      left_join(statePop, by = "state") %>%
      mutate(pop = pop/100000) %>%
      mutate(positivepop =  positive/pop,
             deathpop =  death/pop,
             hospitalizedpop = hospitalized/pop)
    
  })
  
  
  # Plot 1 for USA Total Tab ----
  output$CumulatedPlot = renderPlotly({

    df <- alldata %>%
      arrange(date) %>%
      gather(key = "key", value = "value", positive, death) %>%
      select(state, date, key, value) %>%
      filter(value >= 0) %>%
      filter(key %in% input$metrics) %>% 
      group_by(date, key) %>% 
      summarise(value = sum(value))
    
    ggplot(df, aes(x = date, y = value, fill = key)) +
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
    
    df <- alldata %>%
      arrange(date) %>%
      gather(key = "key", value = "value", positive, death) %>%
      filter(value >= 0) %>%
      filter(key %in% input$metrics) %>% 
      group_by(date, key) %>% 
      summarise(value = sum(value)) %>%
      group_by(key) %>%
      mutate(cumsum = cumsum(value)) %>%
      mutate(infected = if_else((key == "positive"),cumsum / (input$deathrate/100), cumsum))
    
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
  output$statePlot = renderPlotly({
    
    df <- statedata() %>%
      filter(state %in% input$states) 
    
    DateStateCompPlot(df, "positive", "Infections by State", "Reported Infections")

    ggplotly()
    
  })

  # 2nd Plot for Infection by State Tab ----  
  output$linedupstatePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive > input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    slidestartdatePlotFunction(df, "positive", "Infections by State", "Infections", input$dayo)
    
    ggplotly()
    
  })
  
  # 3rd Plot for Infection by State Tab ----   
  output$facetPlot1 = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive > input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"positive")

    ggplotly()

  })

  # 4th Plot for Infection by State Tab -----  
  output$lineduppercapitastatePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positivepop >= input$dayocap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
      
    slidestartdatePopPlotFunction(df, "positivepop", "Infections by State", "Infections per 100000 people", input$dayocap)

    ggplotly()

  })

  # 5th Plot for Infection by State Tab ----   
  output$facetPlot2 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positivepop > input$dayocap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"positivepop")
    
    ggplotly()
    
  })
  
  # 1st Plot for Deaths by State Tab ----   
  output$stateDeathPlot = renderPlotly({
    
    df <- statedata() %>%
      filter(state %in% input$states) 
    
    DateStateCompPlot(df, "death", "Deaths by State", "Reported Deaths")
    
    ggplotly()
    
  })

  # 2nd Plot for Deaths by State Tab ----  
  output$linedupstatedeathPlot = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(death > input$dayodeath, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)

    slidestartdatePlotFunction(df, "death", "Deaths by State", "Deaths", input$dayodeath)
    
    ggplotly()
    
  })
  
  # 3rd Plot for Deaths by State Tab ----   
  output$facetPlot3 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive > input$dayodeath, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"death")
    
    ggplotly()

  })
  
  # 4th Plot for Deaths by State Tab ----    
  output$lineduppercapitadeahtstatePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(deathpop >= input$dayodeathcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    slidestartdatePopPlotFunction(df, "deathpop", "Deaths by State", "Deaths per 100000 people", input$dayodeathcap)
    
    ggplotly()
    
  })
  
  # 5th Plot for Deaths by State Tab ----   
  output$facetPlot4 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(deathpop > input$dayodeathcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"deathpop")
    
    ggplotly()

  })
  
  # 1st Plot for infection prediction ----     
  output$stateDeathratePlot = renderPlotly({
    
    df <- statedata() %>%
      mutate(infected = positive / (input$deathrate/100))
    
    DateStateCompPlot(df, "infected", "Predicted Infections by State", "Predicted Infections")
    
    ggplotly()
    
  })
  
  # 2nd Plot for infection prediction ----  
  output$linedupstatedeathratePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive > input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1) %>%
      mutate(infected = positive / (input$deathrate/100)) 
    
    slidestartdatePlotFunction(df, "infected", "Infections by State", "Infections", input$dayo)
    
    ggplotly()
    
  })
  
  # 3rd Plot for infection prediction ----   
  output$facetPlot5 = renderPlotly({
 
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive > input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1) %>%
      mutate(infected = positive / (input$deathrate/100)) 
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"infected")
    
    ggplotly()

  })
  
  # 4th Plot for infection prediction ----    
  output$lineduppercapitastateratePlot = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positivepop >= input$dayocap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1) %>%
      mutate(infected = positivepop / (input$deathrate/100)) 
    
    slidestartdatePopPlotFunction(df, "infected", "Infections by State", "Infections per 100000 people", input$dayocap)
    
    ggplotly()
    
  })
  
  # 5th Plot for infection prediction ----   
  output$facetPlot6 = renderPlotly({
    
    # df <- statecapita() %>%
    #   ungroup() %>%
    #   rename(plotvalue = conpercapita)
    # 
    # df$state2 <- df$state
    # 
    # facetPlotFunction(df, "infected")
    # 
    # ggplotly()
    # 
    # 
    
    # df <- statedata() %>%
    #   group_by(state) %>%
    #   mutate(dayNo = if_else(positivepop > input$dayocap, 1, 0, missing = NULL)) %>%
    #   mutate(dayNo = cumsum(dayNo)) %>%
    #   filter(dayNo >= 1)
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positivepop >= input$dayocap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1) %>%
      mutate(infected = positivepop / (input$deathrate/100)) 
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"infected")
    
    ggplotly()

  })
  
  # 1st plot for hospilizatoins tab ----
  output$StateHospPlot = renderPlotly({
    
    df <- statedata()
    
    DateStateCompPlot(df, "hospitalized", "Hospitalizations by State", "Reported Hospitalizations")
    
    ggplotly()
    
  })
  
  # 2nd Plot for hospilizatoins tab ----  
  output$SlideStateHospPlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(hospitalized > input$dayohosp, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    slidestartdatePlotFunction(df, "hospitalized", "Hospitalizations by State", "Reported Hospitalizations", input$dayohosp)
    
    ggplotly()
    
  })

  # 3rd Plot for hospilizatoins tab ----   
  output$facetPlot7 = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(hospitalized > input$dayohosp, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"hospitalized")
    
    ggplotly()
    
  })
  
  # 4th Plot for hospilizatoins tab ---- 
  output$SlideStateHospPopPlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(hospitalizedpop >= input$dayohospcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    slidestartdatePopPlotFunction(df, "deathpop", "Deaths by State", "Deaths per 100000 people", input$dayohospcap)
    
    ggplotly()
    
  })
  
  
  # 5th Plot for hospilizatoins tab ---- 
  output$facetPlot8 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(hospitalizedpop > input$dayohospcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"hospitalizedpop")
    
    ggplotly()
    
  })
  
}
