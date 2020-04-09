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
  # Prep Stuf ----
  # to use to simulate input when testing
   # input <- list(states = c("Minnesota", "Michigan", "Wisconsin","North Dakota", "Ohio", "South Dakota", "Iowa"),
   #               metrics = c("positive", "death"),
   #               deathrate = c(0.5))

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
      group_by(date) %>%
      select(date, death, positive) %>%
      pivot_longer(cols = c(death, positive)) %>% 
      #filter(key %in% input$metrics) %>% 
      group_by(date, name) %>% 
      summarise(value = sum(value))
    
    p <- ggplot(df, aes(x = date, y = value, fill = name)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title="Reported Cumulative US Infections",
           x ="Date", 
           y = "Predicted Infections")

    
    if (input$logscaletoggle == "Log") {
      p <- p + scale_y_log10(labels = comma)
      gp <- ggplotly(p)
      df2 <- df %>% filter(name == "death")
      gp$x$data[[1]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      df3 <- df %>% filter(name == "positive")
      gp$x$data[[2]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp
      
    } else {
      p <- p + scale_y_continuous(labels = comma)
      gp <- ggplotly(p)
      df2 <- df %>% filter(name == "death")
      gp$x$data[[1]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      df3 <- df %>% filter(name == "positive")
      gp$x$data[[2]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp
    }
    
  })
  
  # Plot 2 for USA Total Tab ----
  output$plot2message <- renderText({ 
    df <- alldata %>%
      group_by(date) %>%
      select(date, death, positive) %>%
      #pivot_longer(cols = c(death, positive)) %>% 
      #filter(key %in% input$metrics) %>% 
      group_by(date) %>% 
      summarise(death = sum(death),
                positive = sum(positive))
    
    deathrate <- round(max(df$death) / (max(df$positive) /  (1 - 0.80)) * 100,2)
    
    
    paste("This graph is a calculated theoretical level of infection based on the reported daily death count.  
     The graph stacks the thoretical missing tests with the confirmed positives.  The <Death Rate [%]> slider 
     changes the assumption this graph is based on.  The starting point ", deathrate, "% is loosly based on the idea that 80% 
     of casses are asymtomatic and so the slider default = (death count / (reported cases / (1 - 0.80)))", sep = "")
  })
  
  output$CumulatedPlotinfected = renderPlotly({

    df <- alldata %>%
      group_by(date) %>%
      select(date, death, positive) %>%
      pivot_longer(cols = c(death, positive)) %>% 
      group_by(date, name) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(id_cols = date,
                  names_from = name,
                  values_from = value) %>%
      mutate(infected = death / (input$deathrate/100)) %>%
      mutate(missingtests = if_else((infected - positive)<=0,0, infected - positive)) %>%
      pivot_longer(cols = c(positive, missingtests))

      
    p <- ggplot(df, aes(x = date, y = value, fill = name),) +
      geom_bar(stat = "identity") +
      scale_x_date(date_labels="%b-%d",date_breaks  ="1 day")  +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title="Predicted Cumulative US Infections from death rate",
           x ="Date", 
           y = "Infections")
    
    
    if (input$logscaletoggle == "Log") {
      p <- p + scale_y_log10(labels = comma)
      gp <- ggplotly(p)
      df2 <- df %>% filter(name == "missingtests")
      gp$x$data[[1]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      df3 <- df %>% filter(name == "positive")
      gp$x$data[[2]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp
      
    } else {
      p <- p + scale_y_continuous(labels = comma)
      gp <- ggplotly(p)
      df2 <- df %>% filter(name == "missingtests")
      gp$x$data[[1]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      df3 <- df %>% filter(name == "positive")
      gp$x$data[[2]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp
    }
      

    
  })
  
  # 1st Plot for Infection by State Tab ----
  output$infectStateplot1message <- renderText({ 
    
    "Infections by state based on the states selected on the side pannel.  
    With very infections states like New York, selecting the Log scale option 
    on the side pannel will make things easier to read."
    
  })
  
  output$statePlot = renderPlotly({
    
    df <- statedata() %>%
      filter(state %in% input$states) 
    
    DateStateCompPlot(df, "positive", "Infections by State", "Reported Infections", input$logscaletoggle)

    ggplotly()
    
  })

  # 2nd Plot for Infection by State Tab ----  
  output$infectStateplot2message <- renderText({ 
    
    paste("Lines up the data so the day 0 has ",
          input$dayo,
          " cases.  The Slider <Number of Infections for Day 0> lets you change the starting point.", 
          sep = "")
    
  })
  
  output$linedupstatePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive > input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    slidestartdatePlotFunction(df, "positive", "Day 0 Adjusted Infections by State", "Reported Infections", input$dayo, input$logscaletoggle)
    
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
    
    facetPlotFunction(df,"positive", input$logscaletoggle)

    ggplotly()

  })

  # 4th Plot for Infection by State Tab -----  
  output$infectStateplot4message <- renderText({ 
    
    paste("Lines up the data so the day 0 has ",
          input$dayocap,
          " cases per 100,000.  The Slider <Number of Infections/100000 for Day 0> lets you change the starting point.", 
          sep = "")
    
  })
  
  output$lineduppercapitastatePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positivepop >= input$dayocap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
      
    slidestartdatePopPlotFunction(df, "positivepop", "Day 0 Adjusted Infections per 100,000 by State", "Infections per 100000 people", input$dayocap, input$logscaletoggle)

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
    
    facetPlotFunction(df,"positivepop", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 1st Plot for Deaths by State Tab ----   
  output$deathStateplot1message <- renderText({ 
    
    "Infections by state based on the states selected on the side pannel.  
    With very infections states like New York, selecting the Log scale option 
    on the side pannel will make things easier to read."
    
  })
  
  output$stateDeathPlot = renderPlotly({
    
    df <- statedata() %>%
      filter(state %in% input$states) 
    
    DateStateCompPlot(df, "death", "Deaths by State", "Reported Deaths", input$logscaletoggle)
    
    ggplotly()
    
  })

  # 2nd Plot for Deaths by State Tab ----  
  output$linedupstatedeathPlot = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(death > input$dayodeath, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)

    slidestartdatePlotFunction(df, "death", "Deaths by State", "Deaths", input$dayodeath, input$logscaletoggle)
    
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
    
    facetPlotFunction(df,"death", input$logscaletoggle)
    
    ggplotly()

  })
  
  # 4th Plot for Deaths by State Tab ----    
  output$lineduppercapitadeahtstatePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(deathpop >= input$dayodeathcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    slidestartdatePopPlotFunction(df, "deathpop", "Deaths by State", "Deaths per 100000 people", input$dayodeathcap, input$logscaletoggle)
    
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
    
    facetPlotFunction(df,"deathpop", input$logscaletoggle)
    
    ggplotly()

  })
  
  # 1st Plot for infection prediction ----    
  output$PredStateplot1message <- renderText({ 
    
    "Infections by state based on the states selected on the side pannel.  
    With very infections states like New York, selecting the Log scale option 
    on the side pannel will make things easier to read."
    
  })
  
  output$stateDeathratePlot = renderPlotly({
    
    df <- statedata() %>%
      mutate(infected = death / (input$deathrate/100))
    
    DateStateCompPlot(df, "infected", "Predicted Infections by State", "Predicted Infections", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 2nd Plot for infection prediction ----  
  output$linedupstatedeathratePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(death > input$dayodeathcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1) %>%
      mutate(infected = death / (input$deathrate/100)) 
    
    slidestartdatePlotFunction(df, "infected", "Infections by State", "Infections", input$dayodeathcap, input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 3rd Plot for infection prediction ----   
  output$facetPlot5 = renderPlotly({
 
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(death > input$dayodeathcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1) %>%
      mutate(infected = death / (input$deathrate/100)) 
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"infected", input$logscaletoggle)
    
    ggplotly()

  })
  
  # 4th Plot for infection prediction ----    
  output$lineduppercapitastateratePlot = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(deathpop >= input$dayodeathcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1) %>%
      mutate(infected = deathpop / (input$deathrate/100)) 
    
    slidestartdatePopPlotFunction(df, "infected", "Infections by State", "Infections per 100000 people", input$dayodeathcap, input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 5th Plot for infection prediction ----   
  output$facetPlot6 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(deathpop >= input$dayodeathcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1) %>%
      mutate(infected = deathpop / (input$deathrate/100)) 
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"infected", input$logscaletoggle)
    
    ggplotly()

  })
  
  # 1st plot for hospilizatoins tab ----
  output$HospStateplot1message <- renderText({ 
    
    "Hospilizatoins by state based on the states selected on the side pannel.  
    With very infections states like New York, selecting the Log scale option 
    on the side pannel will make things easier to read."
    
  })
  
  output$StateHospPlot = renderPlotly({
    
    df <- statedata()
    
    DateStateCompPlot(df, "hospitalized", "Hospitalizations by State", "Reported Hospitalizations", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 2nd Plot for hospilizatoins tab ----  
  output$SlideStateHospPlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(hospitalized > input$dayohosp, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    slidestartdatePlotFunction(df, "hospitalized", "Hospitalizations by State", "Reported Hospitalizations", input$dayohosp, input$logscaletoggle)
    
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
    
    facetPlotFunction(df,"hospitalized", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 4th Plot for hospilizatoins tab ---- 
  output$SlideStateHospPopPlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(hospitalizedpop >= input$dayohospcap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo)) %>%
      filter(dayNo >= 1)
    
    slidestartdatePopPlotFunction(df, "deathpop", "Deaths by State", "Deaths per 100000 people", input$dayohospcap, input$logscaletoggle)
    
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
    
    facetPlotFunction(df,"hospitalizedpop", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  
  
  # Plot and Data Play for S Curve Predictive Tab ----
  # N(t) = N(0)exp(-c(exp(at)-1))      Gompertz function
  # N(t) = N0 exp(ln(N1/N0)(1-exp(-bt)))

  
  
}
