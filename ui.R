library(shiny)
library(plotly)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
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
                   label = h5("Toggle Scales Log or Not"), 
                   choices = c("Log", "Cartesian"), 
                   selected = "Cartesian"),
      
      # Input: checkbox for USA Total graph to show confirmations and or deaths ----
      checkboxGroupInput("metrics", 
                         label = h5("Selected Metrics"), 
                         choices = c("positive", "death"),
                         selected = c("positive", "death"), width = "100%"),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Select the random distribution type ----
      checkboxGroupInput("states", 
                         label = h5("Selected States"), 
                         choices = state.name,
                         selected = c("Minnesota", "Michigan", "Wisconsin","North Dakota", "Ohio", "South Dakota", "Iowa"),
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
                  min = 0, max = 5, step = 0.1,
                  value = 0.5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("USA Total", 
                           h2("USA Totals"),
                           #br(),
                           p("The first Graph is the cumulative Toatal Infections in the USA.  
                             The Select Metrics inpute on the left sidebar lets you add adjsut what is visible on this graph."),
                           plotlyOutput("CumulatedPlot", width = "100%"),
                           br(),
                           p("This secong graph is a calculated theoretical level of infection based on the reported daily death count.  
                             The graph stacks the thoretical missing tests with the confirmed positives."),
                           plotlyOutput("CumulatedPlotinfected", width = "100%")
                           ),
                  tabPanel("Infections by State", 
                           plotlyOutput("statePlot", width = "100%"),
                           plotlyOutput("linedupstatePlot", width = "100%"),
                           plotlyOutput("facetPlot1", width = "100%"),
                           plotlyOutput("lineduppercapitastatePlot", width = "100%"),
                           plotlyOutput("facetPlot2", width = "100%")
                          ),
                  tabPanel("Death by State", 
                           plotlyOutput("stateDeathPlot", width = "100%"),
                           plotlyOutput("linedupstatedeathPlot", width = "100%"),
                           plotlyOutput("facetPlot3", width = "100%"),
                           plotlyOutput("lineduppercapitadeahtstatePlot", width = "100%"),
                           plotlyOutput("facetPlot4", width = "100%")
                           ),
                  tabPanel("Predicted Infection", 
                           plotlyOutput("stateDeathratePlot", width = "100%"),
                           plotlyOutput("linedupstatedeathratePlot", width = "100%"),
                           plotlyOutput("facetPlot5", width = "100%"),
                           plotlyOutput("lineduppercapitastateratePlot", width = "100%"),
                           plotlyOutput("facetPlot6", width = "100%")
                           ),
                  tabPanel("Hospitilizatoins", 
                           plotlyOutput("StateHospPlot", width = "100%"),
                           plotlyOutput("SlideStateHospPlot", width = "100%"),
                           plotlyOutput("facetPlot7", width = "100%"),
                           plotlyOutput("SlideStateHospPopPlot", width = "100%"),
                           plotlyOutput("facetPlot8", width = "100%")
                           )
      )
    )
  )
)