library(shiny)

fluidPage(
  
  title = 'COVID-19 Dashboard by Alex Chen',
  
  h1('Time course of COVID-19 cases in US'),
  
  fluidRow(
    column(width=6, 
           DTOutput('US_table1'),
           helpText('Pop.Density unit: people/mile-square'),
           textOutput('US_table1_select'),
           plotlyOutput('US_casevspop'),
           plotlyOutput('US_casevsdensity')),
    column(width=6, 
           plotlyOutput('StateCumuCases'),
           helpText(' '),
           plotlyOutput('StateDailyCases'),
           helpText(' '),
           plotlyOutput('StateCumuDeaths'),
           helpText(''),
           plotlyOutput('StateDailyDeaths'))
  ),
  
  hr(),
  
  h1('Time course of global COVID-19 cases except US')
  
  #fluidRow(
    #column(5, DT::dataTableOutput('x3')),
    #column(7, verbatimTextOutput('x4'))
  #)
  
)