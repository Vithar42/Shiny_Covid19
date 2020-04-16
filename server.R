library(tidyverse)
library(scales)
library(plotly)
#source("global.R")

# Pull in Data using datawrangler ----  
# Pull in State population data from the sensus ----  
# filename <-  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
# statePop <-  read.csv(file.path(filename), check.names=FALSE, stringsAsFactors=FALSE) %>%
#   select(state = NAME, pop = CENSUS2010POP )
#
# # Pull in NewYorkTimes Data from https://github.com/cipriancraciun/covid19-datasets ----  
# baseURL <- "https://raw.githubusercontent.com/cipriancraciun/covid19-datasets/master/exports/nytimes/v1/us-counties/"
# fileName <- "values.tsv"
# allData <- NYtimes_tsv(fileName, baseURL)

# Pull in covidtracking.com Data from https://github.com/garykac/covid19/tree/master/data ----  
# baseURL <- "https://raw.githubusercontent.com/garykac/covid19/master/data/"
# fileName <- "states-daily.csv"
# alldata <- garykac_csv("states-daily.csv", baseURL)
# 
# Main Shiny Function ----  
function(input, output, session) {
  # Prep Stuf ----
  # to use to simulate input when testing
  #  input <- list(states = c("Minnesota", "Michigan", "Wisconsin","North Dakota", "Ohio", "South Dakota", "Iowa"),
  #                deathrate = 0.8,
  #                dayo = 100,
  #                dayodeath = 5,
  #                logscaletoggle = "Log")

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
      group_by(date, name) %>% 
      summarise(value = sum(value))
    
    p <- ggplot() +
      geom_area(data = df %>% filter(name == "positive"), aes(x = date, y = value, fill = name), position = "identity") +
      geom_line(data = df %>% filter(name == "positive"), aes(x = date, y = value), color = "black") +
      geom_area(data = df %>% filter(name == "death"), aes(x = date, y = value, fill = name), position = "identity") +
      geom_line(data = df %>% filter(name == "death"), aes(x = date, y = value), color = "black") +
      scale_x_date(date_labels = "%b-%d",date_breaks  = "1 day") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title = "Reported Cumulative US Infections",
           x = "Date", 
           y = "Predicted Infections")

    
    if (input$logscaletoggle == "Log") {
      p <- p + scale_y_log10(labels = comma)
      gp <- ggplotly(p)
      df2 <- df %>% filter(name == "positive")
      gp$x$data[[1]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      gp$x$data[[2]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      df3 <- df %>% filter(name == "death")
      gp$x$data[[3]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp$x$data[[4]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp
      
    } else {
      p <- p + scale_y_continuous(labels = comma)
      gp <- ggplotly(p)
      df2 <- df %>% filter(name == "positive")
      gp$x$data[[1]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      gp$x$data[[2]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      df3 <- df %>% filter(name == "death")
      gp$x$data[[3]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp$x$data[[4]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
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
     The graph stacks the thoretical missing tests with the confirmed positives, the line on the graph is the cumulative total tests taken.  The <Death Rate [%]> slider 
     changes the assumption this graph is based on.  The starting point ", deathrate, "% is loosly based on the idea that 80% 
     of casses are asymtomatic and so the slider default = (death count / (reported cases / (1 - 0.80)))", sep = "")
  })
  
  output$CumulatedPlotinfected = renderPlotly({

    df <- alldata %>%
      group_by(date) %>%
      select(date, death, positive, totalTestResults) %>%
      pivot_longer(cols = c(death, positive, totalTestResults)) %>% 
      group_by(date, name) %>%
      summarise(value = sum(value)) %>%
      pivot_wider(id_cols = date,
                  names_from = name,
                  values_from = value) %>%
      mutate(infected = death / (input$deathrate/100)) %>%
      mutate(missingtests = if_else((infected - positive) <= 0,0, infected - positive)) 
    
    df1 <- df %>%
      pivot_longer(cols = c(positive, missingtests))

      
    p <- ggplot() +
      geom_area(data = df1 %>% filter(name == "missingtests"), aes(x = date, y = value, fill = name), position = "identity") +
      geom_line(data = df1 %>% filter(name == "missingtests"), aes(x = date, y = value), color = "black") +
      geom_area(data = df1 %>% filter(name == "positive"), aes(x = date, y = value, fill = name), position = "identity") +
      geom_line(data = df1 %>% filter(name == "positive"), aes(x = date, y = value), color = "black") +
      geom_line(data = df, aes(x = date,y = totalTestResults), color = "blue") +
      scale_x_date(date_labels = "%b-%d",date_breaks  = "1 day")  +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none") +
      labs(title = "Predicted Cumulative US Infections from death rate",
           x = "Date", 
           y = "Infections")


    
    if (input$logscaletoggle == "Log") {
      p <- p + scale_y_log10(labels = comma)
      gp <- ggplotly(p)
      df2 <- df1 %>% filter(name == "missingtests")
      gp$x$data[[1]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      gp$x$data[[2]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      df3 <- df1 %>% filter(name == "positive")
      gp$x$data[[3]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp$x$data[[4]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      df4 <- df %>% pivot_longer(cols = c(totalTestResults))
      gp$x$data[[5]]$text = paste("date: ",df4$date, "<br />value:", format(df4$value, big.mark = ","), "<br />name:", df4$name)
      gp
      
    } else {
      p <- p + scale_y_continuous(labels = comma)
      gp <- ggplotly(p)
      df2 <- df1 %>% filter(name == "missingtests")
      gp$x$data[[1]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      gp$x$data[[2]]$text = paste("date: ",df2$date, "<br />value:", format(df2$value, big.mark = ","), "<br />name:", df2$name)
      df3 <- df1 %>% filter(name == "positive")
      gp$x$data[[3]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      gp$x$data[[4]]$text = paste("date: ",df3$date, "<br />value:", format(df3$value, big.mark = ","), "<br />name:", df3$name)
      df4 <- df %>% pivot_longer(cols = c(totalTestResults))
      gp$x$data[[5]]$text = paste("date: ",df4$date, "<br />value:", format(df4$value, big.mark = ","), "<br />name:", df4$name)
      gp
    }
      

    
  })
  
  # plot 3 for USA total Tab ----
  
  # build from https://stackoverflow.com/questions/37186172/bubble-chart-without-axis-in-r
  
  output$bubbleplot = renderPlot({
    library(packcircles)
    
    df <- alldata %>%
      group_by(state) %>%
      select(state, positiveIncrease) %>%
      summarise(sum = sum(positiveIncrease)) %>%
      arrange(sum)
    
    p <- circleRepelLayout(df$sum)
    d <- circleLayoutVertices(p)
    
    p <- ggplot(d, aes(x, y)) + 
      geom_polygon(aes(group = id, fill = id), 
                   colour = "black", show.legend = FALSE) +
      geom_text(data = p$layout, aes(x, y), label = df$state) +
      scale_fill_distiller(palette = "Pastel1") +
      theme_void()

    p
    
  },
  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)))
  
  # 1st Plot for Infection by State Tab ----
  output$infectStateplot1message <- renderText({ 
    
    "Infections by state based on the states selected on the side pannel.  
    With very infections states like New York, selecting the Log scale option 
    on the side pannel will make things easier to read."
    
  })
  
  output$statePlot = renderPlotly({
    
    df <- statedata() %>%
      filter(state %in% input$states) 
    
    DateStateCompPlot(df, "positive", "Infections by State", "Cumulitave Cases", input$logscaletoggle)

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
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    slidestartdatePlotFunction(df, 
                               "positive", 
                               "Day 0 Adjusted Infections by State", 
                               "Confirmed Cases", 
                               "Cumulitave Cases", 
                               input$dayo, 
                               input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 3rd Plot for Infection by State Tab ----   
  output$facetPlot1 = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive > input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"positive", input$logscaletoggle)

    ggplotly()

  })

  # 4th Plot for Infection by State Tab -----  
  output$infectStateplot4message <- renderText({ 
    
    "Day 0 is the same as the cumulitive cases plot above, use the <Number of Infections for Day 0> slider to influacne this chart."
    
  })
  
  output$lineduppercapitastatePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      #mutate(dayNo = if_else(positivepop >= input$dayocap, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
      
    slidestartdatePopPlotFunction(df, 
                                  "positivepop", 
                                  "Day 0 Adjusted Infections per 100,000 by State", 
                                  "Confirmed Cases", 
                                  "Cumulitave Cases Per 100000 people", 
                                  input$dayo, 
                                  input$logscaletoggle)

    ggplotly()

  })

  # 5th Plot for Infection by State Tab ----   
  output$facetPlot2 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"positivepop", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 1st Plot for Deaths by State Tab ----   
  output$deathStateplot1message <- renderText({ 
    
    "Deaths by state based on the states selected on the side pannel."
    
  })
  
  #output$stateDeathPlot = renderPlotly({
  output$stateDeathPlot = renderPlotly({ 
    df <- statedata() %>%
      filter(state %in% input$states) 
    
    DateStateCompPlot(df, 
                      "death", 
                      "Deaths by State", 
                      "Cumulitave Deaths", 
                      input$logscaletoggle)
    
    ggplotly()
    
  })

  # 2nd Plot for Deaths by State Tab ----  
  output$deathStateplot2message <- renderText({ 
    
    paste("Lines up the data so the day 0 has ",
          input$dayodeath,
          " deaths.  The Slider <Number of Deaths for Day 0> lets you change the starting point.", 
          sep = "")
    
  })
  
  output$linedupstatedeathPlot = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(death > input$dayodeath, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)

    slidestartdatePlotFunction(df, 
                               "death", 
                               "Deaths by State",
                               "Deaths", 
                               "Cumulitave Deaths", 
                               input$dayodeath, 
                               input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 3rd Plot for Deaths by State Tab ----   
  output$facetPlot3 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(death > input$dayodeath, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"death", input$logscaletoggle)
    
    ggplotly()

  })
  
  # 4th Plot for Deaths by State Tab ----    
  output$deathStateplot4message <- renderText({ 
    
    "Day 0 is the same as the cumulitive cases plot above, use the <Number of Deaths for Day 0> slider to influacne this chart."
    
  })
  
  output$lineduppercapitadeahtstatePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(death > input$dayodeath, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    slidestartdatePopPlotFunction(df, 
                                  "deathpop", 
                                  "Deaths by State", 
                                  "Deaths", 
                                  "Cumulitave Deaths Per 100000 people", 
                                  input$dayodeath, 
                                  input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 5th Plot for Deaths by State Tab ----   
  output$facetPlot4 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(death > input$dayodeath, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"deathpop", input$logscaletoggle)
    
    ggplotly()

  })
  
  # 1st Plot for infection prediction ----    
  output$PredStateplot1message <- renderText({ 
    
    df <- alldata %>%
      group_by(date) %>%
      select(date, death, positive) %>%
      #pivot_longer(cols = c(death, positive)) %>% 
      #filter(key %in% input$metrics) %>% 
      group_by(date) %>% 
      summarise(death = sum(death),
                positive = sum(positive))
    
    deathrate <- round(max(df$death) / (max(df$positive) /  (1 - 0.80)) * 100,2)
    
    
    paste("Deaths by state based on the states selected on the side pannel.  The starting point ", deathrate, "% is loosly based on the idea that 80% 
     of casses are asymtomatic and so the slider default = (death count / (reported cases / (1 - 0.80)))", sep = "")
    
  })

  
  output$stateDeathratePlot = renderPlotly({
    
    df <- statedata() %>%
      mutate(infected = death / (input$deathrate/100))
    
    DateStateCompPlot(df, 
                      "infected", 
                      "Predicted Infections by State", 
                      "Predicted Infections", 
                      input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 2nd Plot for infection prediction ----  
  output$PredStateplot2message <- renderText({ 
    
    paste("Lines up the data so the day 0 has ",
          input$dayo,
          " deaths.  The Slider <Number of Infections for Day 0> lets you change the starting point.", 
          sep = "")
    
  })
  
  output$linedupstatedeathratePlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0) %>%
      mutate(infected = death / (input$deathrate/100)) 
    
    slidestartdatePlotFunction(df, 
                               "infected", 
                               "Infections by State", 
                               "Confirmed Cases", 
                               "Predicted Infections", 
                               input$dayo, 
                               input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 3rd Plot for infection prediction ----   
  output$facetPlot5 = renderPlotly({
 
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0) %>%
      mutate(infected = death / (input$deathrate/100)) 
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"infected", input$logscaletoggle)
    
    ggplotly()

  })
  
  # 4th Plot for infection prediction ----
  output$PredStateplot4message <- renderText({ 
    
    "Day 0 is the same as the cumulitive cases plot above, use the <Number of Infections for Day 0> slider to influacne this chart."
    
  })
  
  output$lineduppercapitastateratePlot = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0) %>%
      mutate(infected = deathpop / (input$deathrate/100)) 
    
    slidestartdatePopPlotFunction(df, 
                                  "infected", 
                                  "Infections by State", 
                                  "Confirmed Cases", 
                                  "Infections per 100000 people", 
                                  input$dayo, 
                                  input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 5th Plot for infection prediction ----   
  output$facetPlot6 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0) %>%
      mutate(infected = deathpop / (input$deathrate/100)) 
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"infected", input$logscaletoggle)
    
    ggplotly()

  })
  
  # 1st plot for hospilizatoins tab ----
  output$HospStateplot1message <- renderText({ 
      
      "Hospitalizations by state based on the states selected on the side pannel."
      
    })
  
  output$StateHospPlot = renderPlotly({
    
    df <- statedata()
    
    DateStateCompPlot(df, "hospitalized", "Hospitalizations by State", "Reported Hospitalizations", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 2nd Plot for hospilizatoins tab ----  
  output$HospStateplot2message <- renderText({ 
    
    paste("Lines up the data so the day 0 has ",
          input$dayo,
          " deaths.  The Slider <Number of Infections for Day 0> lets you change the starting point.", 
          sep = "")
    
  })
  
  output$SlideStateHospPlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    slidestartdatePlotFunction(df, 
                               "hospitalized", 
                               "Hospitalizations by State", 
                               "Confirmed Cases", 
                               "Reported Hospitalizations", 
                               input$dayo, 
                               input$logscaletoggle)
    
    ggplotly()
    
  })

  # 3rd Plot for hospilizatoins tab ----   
  output$facetPlot7 = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"hospitalized", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  # 4th Plot for hospilizatoins tab ---- 
  output$HospStateplot4message <- renderText({ 
    
    "Day 0 is the same as the cumulitive cases plot above, use the <Number of Infections for Day 0> slider to influacne this chart."
    
  })
  
  output$SlideStateHospPopPlot = renderPlotly({
    
    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    slidestartdatePopPlotFunction(df, 
                                  "deathpop", 
                                  "Deaths by State", 
                                  "Confirmed Cases", 
                                  "Deaths per 100000 people", 
                                  input$dayo, 
                                  input$logscaletoggle)
    
    ggplotly()
    
  })
  
  
  # 5th Plot for hospilizatoins tab ---- 
  output$facetPlot8 = renderPlotly({

    df <- statedata() %>%
      group_by(state) %>%
      mutate(dayNo = if_else(positive >= input$dayo, 1, 0, missing = NULL)) %>%
      mutate(dayNo = cumsum(dayNo) - 1) %>%
      filter(dayNo >= 0)
    
    df$state2 <- df$state
    
    facetPlotFunction(df,"hospitalizedpop", input$logscaletoggle)
    
    ggplotly()
    
  })
  
  
  

  
  # 1st Plot for  Testing Tab----
  output$TestStateplot1message <- renderText({ 
    
    "Current work in progress (wip) will eventually compare states on their Positive vs Negative testing."
    
  })
  
  output$NewStatePlot = renderPlotly({
    
    df2 <- statedata() %>% # statedata() %>%
      select(state, date, positive, negative) %>%
      mutate(dayNo = 1) %>%
      mutate(dayNo = cumsum(dayNo)-1) %>%
      pivot_longer(cols = c(positive, negative))
      
    # DateStateCompPlot(df, "positive", "Infections by State", "Reported Infections", input$logscaletoggle)
    p <- ggplot(df2, aes(x = dayNo, y = value, fill = name)) + 
      geom_area(alpha = 0.6 , size = 1, colour = "black") +
      facet_wrap(~ state, scales = "free_y", ncol = 3)
    
    if (input$logscaletoggle == "Log") {
      p + scale_y_log10(labels = comma)
    } else {
      p + scale_y_continuous(labels = comma)
    }
    
    
    ggplotly()
    
  })
  
  
  # 2nd to n Plots for Testing Tab ----
  # max plots set to 50 so all states can be accounted for
  max_plots <- 50

  # Insert the right number of plot output objects into the web page
  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(input$states), function(i) {
      plotname <- paste("plot", i, sep="")
      plotlyOutput(plotname, height = 280) #, width = 250)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlotly({
        
        df2 <- statedata() %>%
          select(state, date, positive, negative) %>%
          mutate(dayNo = 1) %>%
          mutate(dayNo = cumsum(dayNo)-1) %>%
          pivot_longer(cols = c(positive, negative)) %>%
          filter(state == input$states[my_i])
        
        p <- ggplot(df2, aes(x = dayNo, y = value, fill = name)) + 
          geom_area(alpha = 0.6 , size = 1, colour = "black") +
          theme(legend.position = "none") +
          labs(title = input$states[my_i],
               x = "Day Number", 
               y = "Cases")
        
        if (input$logscaletoggle == "Log") {
          p + scale_y_log10(labels = comma)
        } else {
          p + scale_y_continuous(labels = comma)
        }
        
        ggplotly()
        
      })
    })
  }
  
  # States Output Tab ----

  output$ui_statepanel <- renderUI({
    
    state_choice <- input$states
    
    do.call(tabsetPanel,
            lapply(state_choice,  function(state){
              
              tabPanel(title = state, state_ui_fun(state))
              
            })
    )
    
  })
  

}
