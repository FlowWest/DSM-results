server <- function(input, output) {
  output$percent_change_table <- DT::renderDataTable(percent_change_from_no_action)
  
  output$actions_plot <- renderPlotly({
    selected_actions %>% 
      plot_ly(y = ~watershed, x = ~count, color = ~action_description,
              type = 'bar', orientation = 'h', hoverinfo = 'text',
              text = ~paste('</br>', count, 'units of', action_description, '-', quantity, units,
                            '</br> Spring Run:', str_to_title(sr), 
                            '</br> Winter Run:', str_to_title(wr))) %>% 
      layout(yaxis = list(title = ''), xaxis = list(title = ''), barmode = 'stack',
             legend = list(orientation = 'h')) %>% 
      config(displayModeBar = FALSE)
    
  })
}
