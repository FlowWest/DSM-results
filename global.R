library(shiny)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(plotly)
library(stringr)
library(DT)

actions <- read_rds('data/actions.rds')
diversity_groups <- read_rds('data/diversity_groups.rds')
juv_biomass_chipps <- read_rds('data/juv_biomass_chipps.rds')
nat_spawners <- read_rds('data/nat_spawners.rds')
viability <- read_rds('data/viability.rds')

watershed_order <- unique(actions$watershed)

scenario_definitions <- c(
  "No actions are undertaken to increase habitat or survival.",
  "Annual maximization of inputs that result in maximum natural spawners",
  "Annual minimization of inputs",
  "Annual maximization of inputs that result in maximum natural spawners, with an additional requirement that at least one unit is expended in each of the four diversity groups each year.",
  "Annual minimization of inputs, with an additional requirement that at least one unit is expended in each of the four diversity groups each year.",
  "Annual maximization of inputs that result in maximum natural spawners, with all units concentrated on streams without an active fish hatchery",
  "Annual maximization of inputs that result in maximum natural spawners, with all units concentrated on streams with an active fish hatchery"
)

names(scenario_definitions) <- c('NoActions', 'MaxAdults', 'MinAdults', 'MaxAdults_withDGs', 
                                 'MinAdults_withDGs', 'MaxAdults_NOHatcheryStreams', 
                                 'MaxAdults_onlyHatcheryStreams')

scenario_names <- c('No Actions', 'Maximum Adults', 'Minimum Adults', 'Maximum Adults with Diversity Groups',
               'Minimum Adults with Diversity Groups', 'Maximum Adults with No Hatchery Streams',
               'Maximum Adults with Only Hatchery Streams')
names(scenario_names) <- c('NoActions', 'MaxAdults', 'MinAdults', 'MaxAdults_withDGs', 
                      'MinAdults_withDGs', 'MaxAdults_NOHatcheryStreams', 
                      'MaxAdults_onlyHatcheryStreams')

scenario_names_to_scenario <- names(scenario_names)
names(scenario_names_to_scenario) <- as.character(scenario_names)

actions_summary <- actions %>% 
  mutate(action_occured = ifelse(!is.na(action), TRUE, FALSE), 
         scenario = scenario_names[scenario]) %>% 
  group_by(watershed, scenario) %>% 
  summarise(
    total_actions = sum(action_occured)
  ) %>% ungroup() %>% 
  spread(scenario, total_actions) %>% 
  mutate(watershed = factor(watershed, levels = watershed_order)) %>% 
  arrange(watershed) %>% 
  filter(!(watershed %in% c('Yolo Bypass', 'Sutter Bypass')))

names(actions_summary) <- c(
  "Watershed", 
  "Maximum </br> Adults", 
  "Maximum Adults </br> with Diversity Groups", 
  "Maximum Adults </br> with No Hatchery Streams", 
  "Maximum Adults </br> with Only Hatchery Streams", 
  "Minimum </br> Adults", 
  "Minimum Adults </br> with Diversity Groups")

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

# example stats ----
valley_wide_biomass <- juv_biomass_chipps %>% 
  group_by(scenario, year) %>% 
  summarise(juv_biomass = sum(biomass))

no_action_end_biomass <- valley_wide_biomass %>% 
  filter(scenario == 'NoActions', year == 25) %>% 
  pull(juv_biomass)

biomass <- valley_wide_biomass %>% 
  filter(year == 25) %>% 
  ungroup() %>% 
  mutate(no_action_end = no_action_end_biomass,
         `Juvenile Biomass at Chipps` = 
           paste(round(((juv_biomass - no_action_end) / no_action_end) * 100, 1), '%'),
         Scenario = scenario_names[scenario]) %>% 
  select(Scenario, `Juvenile Biomass at Chipps`)

valley_wide_nat_spawners <- nat_spawners %>%
  group_by(scenario, year) %>% 
  summarise(nat_spawners = sum(nat_spawners))

no_action_end_nat_spawners <- valley_wide_nat_spawners %>% 
  filter(scenario == 'NoActions', year == 25) %>% 
  pull(nat_spawners)

spawners <- valley_wide_nat_spawners %>% 
  filter(year == 25) %>% 
  ungroup() %>% 
  mutate(no_action_end = no_action_end_nat_spawners,
         `Natural Spawners` = 
           paste(round(((nat_spawners - no_action_end) / no_action_end) * 100, 1), '%'),
         Scenario = scenario_names[scenario]) %>% 
  select(Scenario, `Natural Spawners`)

percent_change_from_no_action <- biomass %>% 
  left_join(spawners) %>% 
  filter(Scenario != "No Actions")
  





