shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("readable"), 
    titlePanel("DSM Results", windowTitle = "DSM Results"),
    column(width = 4, 
           tags$h2("Percent Change from No Actions"),
           DT::dataTableOutput("percent_change_table")), 
    column(width = 8, 
           tags$h2("Action Units"),
           plotlyOutput("actions_plot"))
  )
)
