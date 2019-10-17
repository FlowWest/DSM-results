library(shiny)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(plotly)

actions <- read_rds('data/actions.rds')
diversity_groups <- read_rds('data/diversity_groups.rds')
juv_biomass_chipps <- read_rds('data/juv_biomass_chipps.rds')
nat_spawners <- read_rds('data/nat_spawners.rds')
viability <- read_rds('data/viability.rds')

sr_exists <- cvpiaHabitat::modeling_exist %>% 
  select(Watershed, SR_fry) %>% 
  filter(!is.na(SR_fry)) %>% 
  pull(Watershed)

wr_exists <- c('Upper Sacramento River', 'Upper-mid Sacramento River',
               'Lower-mid Sacramento River', 'Lower Sacramento River',
               'Battle Creek')

action_units <- c(1, 2, 2, .5)
names(action_units) <- c('Spawning Habitat', 'Inchannel Rearing Habitat', 
                         'Floodplain Habitat', 'Survival')

units <- c(rep('acres', 3), '%')
names(units) <- c('Spawning Habitat', 'Inchannel Rearing Habitat', 
                  'Floodplain Habitat', 'Survival')

# example plot of actions -----------
watershed_order <- rev(unique(actions$watershed))

# Action plot
# selected scenario should be a reactive activated by clicking on table
selected_actions <- actions %>%
  filter(scenario == 'MaxAdults') %>% 
  group_by(watershed, action_description) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(action_description)) %>% 
  mutate(sr = watershed %in% sr_exists,
         wr = watershed %in% wr_exists,
         quantity = count * action_units[action_description],
         units = units[action_description]) 

selected_actions %>% 
  plot_ly(y = ~watershed, x = ~count, color = ~action_description,
          type = 'bar', orientation = 'h', hoverinfo = 'text',
  text = ~paste('</br>', count, 'units of', action_description, '-', quantity, units,
                '</br> Spring Run:', sr, 
                '</br> Winter Run:', wr)) %>% 
  layout(yaxis = list(title = ''), xaxis = list(title = ''), barmode = 'stack',
         legend = list(orientation = 'h')) %>% 
  config(displayModeBar = FALSE)


# example stats ----
valley_wide_biomass <- juv_biomass_chipps %>% 
  group_by(scenario, year) %>% 
  summarise(juv_biomass = sum(biomass))

valley_wide_nat_spawners <- nat_spawners %>%
  group_by(scenario, year) %>% 
  summarise(nat_spawners = sum(nat_spawners))

unique(valley_wide_biomass$scenario)

valley_wide_biomass %>% 
  filter(scenario %in% c('NoActions', 'MaxAdults', 'MaxAdults_withDGs')) %>%
  ggplot(aes(year, juv_biomass, color = scenario)) +
  geom_line()

valley_wide_nat_spawners %>% 
  filter(scenario %in% c('NoActions', 'MaxAdults', 'MaxAdults_withDGs')) %>%
  ggplot(aes(year, nat_spawners, color = scenario)) +
  geom_line()

# don't include
viability %>% 
  filter(scenario %in% c('NoActions', 'MaxAdults', 'MaxAdults_NOHatcheryStreams', 
                         'MaxAdults_onlyHatcheryStreams')) %>% 
  ggplot(aes(year, viability_count, fill = scenario)) +
  geom_col() +
  facet_wrap(~diversity_group)
