server <- function(input, output) {
  
  selected_scenario <- reactive({
    idx <- input$percent_change_table_rows_selected
    scenario_table_name <- percent_change_from_no_action[idx, ]$Scenario
    scenario_names_to_scenario[scenario_table_name]
  })
  
  selected_actions <- reactive({
    actions %>%
      filter(scenario == selected_scenario()) %>% 
      group_by(watershed, action_description) %>% 
      summarise(count = n()) %>% 
      filter(!is.na(action_description)) %>% 
      mutate(sr = watershed %in% sr_exists,
             wr = watershed %in% wr_exists,
             quantity = count * action_units[action_description],
             units = units[action_description]) 
  })

  
  output$percent_change_table <- DT::renderDataTable(
    percent_change_from_no_action, 
    selection = "single",
    extensions = "Scroller",
    options = list(dom = "t", scrollY = 300, scroller = TRUE))
  
  output$actions_plot <- renderPlotly({
    
    validate(need(length(selected_scenario()) > 0, 
                  "Select a Scenario to View Action Unit Results"), 
             errorClass = "app-errors")
    
    selected_actions() %>% 
      plot_ly(y = ~watershed, x = ~count, color = ~action_description,
              type = 'bar', orientation = 'h', hoverinfo = 'text',
              text = ~paste('</br>', count, 'units of', action_description, '-', quantity, units,
                            '</br> Spring Run:', str_to_title(sr), 
                            '</br> Winter Run:', str_to_title(wr))) %>% 
      layout(yaxis = list(title = ''), xaxis = list(title = ''), barmode = 'stack',
             legend = list(orientation = 'h'), showlegend = TRUE) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$scenario_definition <- renderUI({
    
    validate(need(length(selected_scenario()) > 0, 
                  ""), 
             errorClass = "app-errors") 
    
    if (
      names(selected_scenario()) %in%
      c('Maximum Adults with Diversity Groups', 'Minimum Adults with Diversity Groups')
    ) {
      tagList(
        tags$p(tags$b(paste0(names(selected_scenario()), ": ")), 
               scenario_definitions[selected_scenario()]),
        tags$p("Diversity groups were developed from the Central Valley Chinook Salmon & Steelhead Recovery Plan ",
               tags$a("(NOAA 2014)", target = "_blank",
                      href = "https://archive.fisheries.noaa.gov/wcr/publications/recovery_planning/salmon_steelhead/domains/california_central_valley/cv_chin_stlhd_r_plan_fs_071614.pdf"))
        
      )
    } else {
      tags$p(tags$b(paste0(names(selected_scenario()), ": ")), 
             scenario_definitions[selected_scenario()])
    }
    
    
    
  })
  
  output$actions_summary <- DT::renderDataTable(actions_summary, 
                                                options = list(dom = "t", 
                                                               pageLength = 100), 
                                                escape = FALSE)
}
