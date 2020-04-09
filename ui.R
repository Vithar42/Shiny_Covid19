library(shiny)
library(plotly)

# calcuation for death rate slider
df <- alldata %>%
  group_by(date) %>%
  select(date, death, positive) %>%
  #pivot_longer(cols = c(death, positive)) %>% 
  #filter(key %in% input$metrics) %>% 
  group_by(date) %>% 
  summarise(death = sum(death),
            positive = sum(positive))

deathrate <- round(max(df$death) / (max(df$positive) /  (1 - 0.80)) * 100,2)



# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("COVID-19"),
  
  p("Data pulled from https://github.com/garykac/covid19/tree/master/data which is updated daily."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: checkbox for USA Total graph to show confirmations and or deaths ----
      #checkboxGroupInput("logscaletoggle", 
      #                   label = h5("Toggle Scales Log or Not"), 
      #                   choices = c("Log", "cartision"),
      #                   selected = c("positive", "death"), width = "100%"),
      
      radioButtons("logscaletoggle", 
                   label = h5("Toggle Scales Log or Cartesian"), 
                   choices = c("Cartesian", "Log"), 
                   selected = "Cartesian"),
      
      # Input: checkbox for USA Total graph to show confirmations and or deaths ----
      #checkboxGroupInput("metrics", 
      #                   label = h5("Selected Metrics"), 
      #                   choices = c("positive", "death"),
      #                   selected = c("positive", "death"), width = "100%"),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Select the random distribution type ----
      checkboxGroupInput("states", 
                         label = h5("Selected States"), 
                         choices = state.name,
                         selected = c("Minnesota", "Wisconsin","North Dakota", "Ohio", "South Dakota", "Iowa"),
                         inline = TRUE),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider to change the Day 1 starting point ----
      # Plot 2 sliders
      sliderInput("dayo", 
                  label = h5("Number of Infections for Day 0"),
                  min = 0, max = 500,
                  value = 100),
      sliderInput("dayodeath", 
                  label = h5("Number of Deaths for Day 0"),
                  min = 0, max = 500,
                  value = 5),
      sliderInput("dayohosp", 
                  label = h5("Number of Hospitilizatoins for Day 0"),
                  min = 0, max = 500,
                  value = 25),
      # Plot 4 sliders
      sliderInput("dayocap", 
                  label = h5("Number of Infections/100000 for Day 0"),
                  min = 0, max = 25, step = 0.5,
                  value = 1),
      sliderInput("dayodeathcap", 
                  label = h5("Number of Death/100000s for Day 0"),
                  min = 0, max = 5, step = 0.1,
                  value = .5),
      sliderInput("dayohospcap", 
                  label = h5("Number of Hospitilizatoins/100000 for Day 0"),
                  min = 0, max = 5, step = 0.1,
                  value = .5),
      # Prediction Slider
      sliderInput("deathrate", 
                  label = h5("Death Rate [%]"),
                  min = 0, max = 10, step = 0.1,
                  value = deathrate)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("USA Total", 
                           h2("USA Totals"),
                           #br(),
                           plotlyOutput("CumulatedPlot", width = "100%"),
                           br(),
                           textOutput("plot2message"),
                           plotlyOutput("CumulatedPlotinfected", width = "100%")
                           ),
                  tabPanel("Infections by State", 
                           h2("Infections by State"),
                           textOutput("infectStateplot1message"),
                           plotlyOutput("statePlot", width = "100%"),
                           h2("Adjusted Day 0 Plot"),
                           textOutput("infectStateplot2message"),
                           plotlyOutput("linedupstatePlot", width = "100%"),
                           plotlyOutput("facetPlot1", width = "100%"),
                           h2("Adjusted Day 0 Plot Per 100,000 people"),
                           textOutput("infectStateplot4message"),
                           plotlyOutput("lineduppercapitastatePlot", width = "100%"),
                           plotlyOutput("facetPlot2", width = "100%")
                          ),
                  tabPanel("Death by State", 
                           h2("Death by State"),
                           textOutput("deathStateplot1message"),
                           plotlyOutput("stateDeathPlot", width = "100%"),
                           h2("Adjusted Day 0 Plot"),
                           textOutput("deathStateplot2message"),
                           plotlyOutput("linedupstatedeathPlot", width = "100%"),
                           plotlyOutput("facetPlot3", width = "100%"),
                           h2("Adjusted Day 0 Plot Per 100,000 people"),
                           textOutput("deathStateplot4message"),
                           plotlyOutput("lineduppercapitadeahtstatePlot", width = "100%"),
                           plotlyOutput("facetPlot4", width = "100%")
                           ),
                  tabPanel("Predicted Infection", 
                           h2("Predicted Infections by State"),
                           textOutput("PredStateplot1message"),
                           plotlyOutput("stateDeathratePlot", width = "100%"),
                           h2("Adjusted Day 0 Plot"),
                           textOutput("PredStateplot2message"),
                           plotlyOutput("linedupstatedeathratePlot", width = "100%"),
                           plotlyOutput("facetPlot5", width = "100%"),
                           h2("Adjusted Day 0 Plot Per 100,000 people"),
                           textOutput("PredStateplot4message"),
                           plotlyOutput("lineduppercapitastateratePlot", width = "100%"),
                           plotlyOutput("facetPlot6", width = "100%")
                           ),
                  tabPanel("Hospitilizatoins", 
                           h2("Hospitilizatoins by State"),
                           textOutput("HospStateplot1message"),
                           plotlyOutput("StateHospPlot", width = "100%"),
                           h2("Adjusted Day 0 Plot"),
                           textOutput("HospStateplot2message"),
                           plotlyOutput("SlideStateHospPlot", width = "100%"),
                           plotlyOutput("facetPlot7", width = "100%"),
                           h2("Adjusted Day 0 Plot Per 100,000 people"),
                           textOutput("HospStateplot4message"),
                           plotlyOutput("SlideStateHospPopPlot", width = "100%"),
                           plotlyOutput("facetPlot8", width = "100%")
                           )
      )
    )
  )
)