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


# example plot of actions -----------
watershed_order <- rev(unique(actions$watershed))

actions %>%
  filter(scenario == 'MaxAdults') %>% 
  mutate(watershed = factor(watershed, levels = watershed_order)) %>% 
  ggplot(aes(watershed, year, fill = action_description)) +
  geom_tile() +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal() +
  coord_flip() +
  labs(x = '', fill = '')

plotly::ggplotly()

# example stats ----
valley_wide_biomass <- juv_biomass_chipps %>% 
  group_by(scenario, year) %>% 
  summarise(juv_biomass = sum(biomass))

valley_wide_nat_spawners <- nat_spawners %>%
  group_by(scenario, year) %>% 
  summarise(nat_spawners = sum(nat_spawners))

unique(valley_wide_biomass$scenario)

valley_wide_biomass %>% 
  filter(scenario %in% c('NoActions', 'MaxAdults', 'MaxAdults_NOHatcheryStreams', 
                         'MaxAdults_onlyHatcheryStreams')) %>%
  ggplot(aes(year, juv_biomass, color = scenario)) +
  geom_line()

valley_wide_nat_spawners %>% 
  filter(scenario %in% c('NoActions', 'MaxAdults', 'MaxAdults_NOHatcheryStreams', 
                         'MaxAdults_onlyHatcheryStreams')) %>%
  ggplot(aes(year, nat_spawners, color = scenario)) +
  geom_line()

viability %>% 
  filter(scenario %in% c('NoActions', 'MaxAdults', 'MaxAdults_NOHatcheryStreams', 
                         'MaxAdults_onlyHatcheryStreams')) %>% 
  ggplot(aes(year, viability_count, fill = scenario)) +
  geom_col() +
  facet_wrap(~diversity_group)
