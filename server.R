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
    options = list(dom = "t"))
  
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
             legend = list(orientation = 'h')) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$actions_summary <- DT::renderDataTable(actions_summary, 
                                                options = list(dom = "t", 
                                                               pageLength = 100), 
                                                escape = FALSE)
}
