shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("readable"), 
    includeCSS("www/styles.css"),
    titlePanel("DSM Results", windowTitle = "DSM Results"),
    fluidRow(
      column(width = 4, 
             tags$h2("Percent Change from No Actions"),
             DT::dataTableOutput("percent_change_table")), 
      column(width = 8, 
             tags$h2("Action Units"),
             plotlyOutput("actions_plot"), 
             tags$br(),
             tags$br())
    ), 
    fluidRow(
      tags$h2("Action Overview"),
      column(width = 9, DT::dataTableOutput("actions_summary"))
    )
  )
)
