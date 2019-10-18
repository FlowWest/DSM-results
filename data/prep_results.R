library(readxl)
library(tidyverse)
library(stringr)

# Scenarios ----
# NoActions = baseline
# MaxAdults = maximize adult returns
# MaxAdults_withDGs = maximize adult returns but make sure to work in each diversity group each year

scenarios <- c('NoActions', 'MaxAdults', 'MinAdults', 'MaxAdults_withDGs', 'MinAdults_withDGs',
               'MaxAdults_NOHatcheryStreams', 'MaxAdults_onlyHatcheryStreams')#, 
               # 'NoActionsWR', 'OneActionWR', 'TwoActionsWR')
file_paths <- paste0('data/', scenarios, '.xlsx')

# Natural Spawners ----
map2_df(file_paths, scenarios, 
        ~ mutate(read_excel(path = .x, sheet = 'nat.spawners'), scenario = .y)) %>% 
  gather(year, nat_spawners, -watershed, -scenario) %>%
  mutate(year = as.numeric(gsub('year', '', year))) %>% 
  write_rds('data/nat_spawners.rds')

# Juvenile Biomass ----
map2_df(file_paths, scenarios, 
        ~ mutate(read_excel(path = .x, sheet = 'juvenileBiomassChipps'), scenario = .y)) %>% 
  gather(year, biomass, -watershed, -scenario) %>%
  mutate(year = as.numeric(gsub('year', '', year))) %>% 
  write_rds('data/juv_biomass_chipps.rds')


# Viability ----
# Measuring viable populations
# • Escapement > 833 annually (all.spawners)
# • Cohort Replacement Rate ≥1 (lambda)
# • Hatchery stray rate ≤10% (1-proportionNatural)

viability <- map2_df(file_paths, scenarios, 
                     ~ mutate(read_excel(path = .x, sheet = 'Viability'), scenario = .y))

crr <- map2_df(file_paths, scenarios,
               ~ mutate(read_excel(path = .x, sheet = 'lambda'), scenario = .y))

select(crr, watershed, diversity_groups = DiversityGroup) %>% 
  write_rds('data/diversity_groups.rds')

all_spawners <- map2_df(file_paths, scenarios,
                        ~ mutate(read_excel(path = .x, sheet = 'all.spawners'), scenario = .y))

prop_nat <- map2_df(file_paths, scenarios,
                    ~ mutate(read_excel(path = .x, sheet = 'proportionNatural'), scenario = .y))

v1 <- all_spawners %>%
  gather(year, escapement, -watershed, -scenario) 

v2 <- crr %>%
  gather(year, crr, -watershed, -DiversityGroup, -scenario) 

v3 <- prop_nat %>%
  gather(year, prop_nat, -watershed, -scenario) 

viability_stats <- bind_cols(v1, v2, v3) %>%
  mutate(year = as.numeric(gsub('year', '', year))) %>% 
  filter(((escapement > 833) + (crr >= 1) + (prop_nat >= .9)) == 3) %>% 
  select(watershed:escapement, crr, prop_nat, diversity_group = DiversityGroup)

viability %>% 
  rename(diversity_group = diversityGroup) %>% 
  gather(year, viability_count, -diversity_group, -scenario) %>%
  mutate(year = as.numeric(gsub('year', '', year))) %>% 
  left_join(viability_stats) %>% 
  write_rds('data/viability.rds')

# Actions ----
# • 2 = 1 unit spawning habitat
# • 3 = 1 unit in channel rearing habitat
# • 4 = 1 unit floodplain habitat
# • 5 = 1 unit survival 

# spawning habitat unit = 1 ac, in channel rearing habitat unit = 2 ac,
# floodplain habitat unit = 2 ac, survival unit = 0.5%

actions <- map2_df(file_paths[-1], scenarios[-1],
        ~ mutate(read_excel(path = .x, sheet = 'Actions'), scenario = .y))

# action_lookup <- c('Spawning Habitat - 1 acres', 'Inchannel Rearing Habitat - 2 acres', 
#                    'Floodplain Habitat - 2 acres', 'Survival - 0.5%')
action_lookup <- c('Spawning Habitat', 'Inchannel Rearing Habitat', 
                   'Floodplain Habitat', 'Survival')
names(action_lookup) <- as.character(2:5)

actions %>% 
  rename(diversity_group = DiversityGroup) %>% 
  gather(year, action, -watershed, -diversity_group, -scenario) %>% 
  mutate(year = as.numeric(gsub('year', '', year)),
         action_description = action_lookup[as.character(action)]) %>% 
  write_rds('data/actions.rds')

