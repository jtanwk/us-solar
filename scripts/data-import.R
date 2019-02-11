# Shining a Light on U.S. Solar Energy Generation
# Data Import Script
# Purpose: compartmentalize data import and cleaning tasks from presentation in
#           main R Notebook and resulting html

##### SETUP ###################################################################

library(tidyverse)
library(magrittr) # extended pipe functionality
library(lubridate) # dealing with date objects
library(here) # for OS-agnostic filepaths
library(readxl) # for reading Excel files

# Geospatial tools and related libraries
library(sf)
library(zipcode) # for geocoding zipcodes
library(noncensus) # for more geocoding data 
library(rnaturalearth)
library(rnaturalearthdata)


##### GEOSPATIAL DATA ########################################################

# Read state- and county-level shapefiles
us_states <- ne_states(country = "united states of america",
                       returnclass = "sf")
county_map <- st_read(here::here('data', 'shapefiles-usa', 'cb_2017_us_county_500k'))

# Get county and zip code fips code data
data(counties)
data(zip_codes)

# zip_codes FIPS data is in numeric formats, drops leading 0s
zip_codes %<>%
  mutate(fips = as.character(fips)) %>%
  mutate(fips = ifelse(nchar(fips) == 4, 
                       paste0("0", fips),
                       fips))

##### SOLAR ENERGY GENERATION DATA ############################################

# Import solar energy generation data
gen <- read_csv(here('data', 'data-power-generation', 'Complete_SEDS.csv'))

# Extract desired data and replace labels with actual name of source
# Solar "SO", Wind "WY", Geothermal "GE", Hydroelectric "HY"
gen %<>%
  filter(!StateCode %in% c("US", "X3", "X5")) %>% # filter out Total and Offshore
  mutate(energy_type = substr(MSN, 1, 2)) %>%
  mutate(source = substr(MSN, 3, 5)) %>%
  filter(energy_type %in% c("SO", "WY", "GE", "HY")) %>%
  filter(source %in% c("CCP", "EGP", "ICP", "R7P")) %>%
  mutate(energy_type = ifelse(energy_type == "SO", "Solar",
                              ifelse(energy_type == "WY", "Wind",
                                     ifelse(energy_type == "GE", "Geothermal", 
                                            "Hydroelectric")))) %>%
  mutate(source = ifelse(source == "CCP", "Commercial",
                         ifelse(source == "EGP", "Electric Utility",
                                ifelse(source == "ICP", "Industrial",
                                       "Residential"))))

##### SOLAR IRRADIANCE DATA ###################################################

# Import solar irradiance data
ghi <- read_excel(here('data', 'data-solar-irradiance', 'solarsummaries.xlsx'),
                  sheet = 'GHI State',
                  range = 'A1:BO52',
                  col_types = c('text', rep('numeric', 66)))

# Reshape from wide to long
ghi %<>%
  gather(key = metric, value =  value, -c("State", "State FIPS")) 

# Remove time period from metric name and reshape back to wide-ish
# Now unit of analysis is state-month-estimate instead of state-estimate
ghi %<>%
  mutate(period = substr(ghi$metric, 1, str_locate(ghi$metric, " ") - 1)) %>%
  mutate(attr = substr(ghi$metric, str_locate(ghi$metric, " ") + 1, 
                       nchar(ghi$metric))) %>%
  mutate(period = substr(period, 1, 3)) %>%
  mutate(period = factor(period,
                         levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
                                    'Ann'))) %>%
  select(-metric) %>%
  spread(key = attr, value = value) %>%
  rename(avg = 'Average (kWh/m2/day)',
         max = 'Average Maximum (kWh/m2/day)',
         min = 'Average Minimum (kWh/m2/day)',
         median = 'Average Median (kWh/m2/day)',
         sd = 'Average Standard Deviation (kWh/m2/day)')

##### SOLAR PANEL DATA #######################################################

# Import solar panel data
panels <- read_csv(here('data', 'data-solar-panels', 'openpv_all.csv'),
                   col_types = cols_only(state = "c",
                                         date_installed = "c",
                                         zipcode = "c",
                                         size_kw = "n",
                                         cost = "n",
                                         cost_per_watt = "n",
                                         install_type = "c"),
                   quoted_na = TRUE)

# Clean up date field and extract year 
panels %<>%
  mutate(date_installed = as.Date(date_installed, format = '%m/%d/%Y')) %>%
  mutate(year_installed = year(date_installed)) %>%
  mutate(cost = as.numeric(cost))

# Clean up install_type labels for residential panels 
panels %<>%
  mutate(install_type = tolower(install_type)) %>%
  mutate(install_type = ifelse(install_type == "residential/sf", 
                               "residential", 
                               install_type))


##### STATE POPULATION DATA ###################################################

# Import population data
pop <- read_csv(here('data', 'data-population-estimates',
                     'us.1969_2016.19ages.adjusted.txt'),
                col_types = 'c',
                col_names = FALSE)

# Extract individual variables from non-separated text data
# Then discard demographic characteristics and summarize by year-state
pop %<>%
  mutate(year = as.numeric(substr(X1, 1, 4)),
         state = substr(X1, 5, 6),
         state_fips = substr(X1, 7, 8),
         county_fips = substr(X1, 9, 11),
         seer_registry = substr(X1, 12, 13),
         race = substr(X1, 14, 14),
         origin = substr(X1, 15, 15),
         sex = substr(X1, 16, 16),
         age = as.numeric(substr(X1, 17, 18)),
         population = as.numeric(substr(X1, 19, 27))) %>%
  group_by(year, state) %>%
  summarize(total_pop = sum(population))

# 1. Group by year and fill in empty years with 0 installs
panels_by_pop <- panels %>%
  filter(install_type == "residential") %>%
  group_by(state, year_installed) %>%
  count() %>%
  ungroup() %>%
  complete(state, year_installed, fill = list(n = 0)) %>%
  rename(year = year_installed)

# 2. Inner-join population data
# 3a. Get cumulative sum of installations by state
# 3b. Calculate number of panels per 10,000 people
panels_by_pop %<>%
  inner_join(pop, by = c('state', 'year')) %>%
  # filter(year != 1969) %>%
  mutate(sum_n = ave(n, state, FUN = cumsum)) %>%
  mutate(panels_per_10k = (sum_n / total_pop) * 10000)

##### STATE RENEWABLE ENERGY FINANCIAL INCENTIVE DATA #########################

# Import master list of programs
policy <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                        'program.csv'))

# Import relevant data to merge on
policy_states <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                               'state.csv')) %>%
  select(id, name) %>%
  rename(state = name)

policy_impl <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                             'implementing_sector.csv')) %>%
  select(id, name) %>%
  rename(implementing_sector = name)

policy_cats <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                             'program_category.csv')) %>%
  rename(program_category = name)

policy_types <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                              'program_type.csv')) %>%
  select(id, name) %>%
  rename(program_type = name)

policy_tech_link <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                                  'program_technology.csv'))

policy_tech <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                             'technology.csv')) %>%
  select(id, name, technology_category_id) %>%
  rename(technology = name)

policy_tech_cat <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                                 'technology_category.csv')) %>%
  rename(technology_category = name)

policy_energy_cat <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                                   'energy_category.csv')) %>%
  rename(energy_category = name)

policy_sector_link <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                                    'program_sector.csv'))

policy_sector <- read_csv(here('data', 'data-state-policies', 'dsire-2019-01',
                               'sector.csv')) %>%
  select(id, name, parent_id) %>%
  rename(sector = name)

# Merge on relevant data and replace labels with real values
policy %<>%
  left_join(policy_states, by = c("state_id" = "id")) %>%
  left_join(policy_impl, by = c("implementing_sector_id" = "id")) %>%
  left_join(policy_cats, by = c("program_category_id" = "id")) %>%
  left_join(policy_types, by = c("program_type_id" = "id")) %>%
  left_join(policy_tech_link, by = c("id" = "program_id")) %>%
  left_join(policy_tech, by = c("technology_id" = "id")) %>%
  left_join(policy_tech_cat, by = c("technology_category_id" = "id")) %>%
  left_join(policy_energy_cat, by = c("energy_category_id" = "id")) %>%
  left_join(policy_sector_link, by = c("id" = "program_id")) %>%
  left_join(policy_sector, by = c("sector_id" = "id")) %>%
  left_join(policy_sector, by = c("parent_id" = "id")) %>%
  rename(sector = sector.x, 
         parent_sector = sector.y)

# Drop auxiliary dataframes from memory
rm(policy_states, policy_impl, policy_cats, policy_types, policy_tech_link,
   policy_tech, policy_tech_cat, policy_energy_cat, policy_sector_link,
   policy_sector)

# Drop extraneous information and retain only useful columns
policy %<>%
  filter(technology_category == "Solar Technologies") %>%
  filter(technology == "Solar Photovoltaics") %>%
  filter(program_category == "Financial Incentive") %>%
  filter(is_entire_state == 1) %>%
  filter(!is.na(start_date) | !is.na(start_date_text)) %>% 
  select(id, name, state, start_date, start_date_text, end_date, implementing_sector, 
         program_category, program_type, technology, technology_category,
         energy_category, sector, parent_sector)

# Fix start_date entries that are blank but have data in start_date_text column
policy %<>%
  mutate(start_date = replace(start_date, id == 75, as.POSIXct("01/01/1999", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 80, as.POSIXct("01/01/1998", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 83, as.POSIXct("01/01/2001", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 192, as.POSIXct("01/01/1988", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 248, as.POSIXct("01/01/2001", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 297, as.POSIXct("01/01/2003", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 351, as.POSIXct("01/01/2010", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 635, as.POSIXct("01/01/2001", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 636, as.POSIXct("01/01/2001", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 917, as.POSIXct("01/01/2003", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 1220, as.POSIXct("01/01/2002", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 1234, as.POSIXct("01/01/2005", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 1837, as.POSIXct("01/01/2006", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 2363, as.POSIXct("01/01/2004", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 2511, as.POSIXct("01/01/2003", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3223, as.POSIXct("01/01/2011", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3352, as.POSIXct("01/01/2011", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3383, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3603, as.POSIXct("01/01/2011", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3604, as.POSIXct("01/01/2011", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3701, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3706, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3707, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3708, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3709, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3710, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3760, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3761, as.POSIXct("01/01/2009", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 3969, as.POSIXct("01/01/2010", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 4067, as.POSIXct("01/01/2010", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 4111, as.POSIXct("01/01/2008", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 4168, as.POSIXct("01/01/2008", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 4419, as.POSIXct("01/01/2010", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 4476, as.POSIXct("01/01/2006", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 4551, as.POSIXct("01/01/2010", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 4857, as.POSIXct("01/01/2005", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 5321, as.POSIXct("01/01/2012", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 5679, as.POSIXct("01/01/2014", format = "%m/%d/%Y"))) %>%
  mutate(start_date = replace(start_date, id == 5850, as.POSIXct("01/01/2015", format = "%m/%d/%Y")))

##### SAVE .RDATA FILE SO I DON'T HAVE TO RELOAD EVERYTHING EVERY TIME #######

save(counties, county_map, gen, ghi, panels, panels_by_pop, policy, pop,
     us_states, zip_codes,
     file = here('data', 'solar_data_master.RData'))

