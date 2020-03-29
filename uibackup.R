library(shiny)
library(plotly)


shinyUI(fluidPage(
  tags$style(
    type='text/css', 
    ".selectize-input { font-family: Courier New, monospace; } .selectize-dropdown { font-family: Courier New, monospace; }"
  ),
  tags$style(HTML(
    "body { font-family: Courier New, monospace; line-height: 1.1; }"
  )),
  
  titlePanel("Case History of the Coronavirus (COVID-19)"),
  fluidRow(
    column(
      1, 
      checkboxGroupInput(
        "metrics", label=h5("Selected Metrics"), 
        choices=c("Confirmed", "Deaths"), 
        selected=c("Confirmed", "Deaths"), width="100%")
    ),
    column(
      10, 
      checkboxGroupInput(
        "states", label=h5("Selected States"), 
        choices= c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                   "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                   "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
                   "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                   "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                   "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                   "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                   "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
                   "Wisconsin", "Wyoming"), 
        selected=c("Minnesota", "Michigan", "Wisconsin","North Dakota", "Ohio", "South Dakota", "Iowa"), 
        inline = TRUE)
    )
  ),
  fluidRow(
    plotlyOutput("CumulatedPlot", width = "100%")
  ),
  fluidRow(
    plotlyOutput("statePlot", width = "100%")
  ),
  fluidRow(
    plotlyOutput("linedupstatePlot", width = "100%")
  ),
  fluidRow(
    plotlyOutput("lineduppercapitastatePlot", width = "100%")
  ),
  fluidRow(
    plotlyOutput("facetPlot", width = "100%")
  )
))

