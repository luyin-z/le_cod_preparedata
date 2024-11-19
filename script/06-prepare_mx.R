# This R script downloads and creates mortality using denominators from WPP/ONS and
# numerators from WHO.


# 0. Init ----------------------------------------------------------------------
library(here); library(httr); library(readxl); library(tidyverse)
library(DemoTools); library(ungroup)
setwd(str_remove(here(),'/script'))

region_meta <- read_csv('cfg/region_metadata.csv', na = '.') %>%
  add_row(region_code_iso3166_2_alpha3 = 'RUS', region_name = 'Russian Federation', who_code = 4272)



# 1. Download Data -------------------------------------------------------------
# WPP2024 death counts and population exposure
wpp_prefix <- 'https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/'

wpp_url <- list()
wpp_url <- within(wpp_url, {
  pop_sa <- paste0(wpp_prefix, 'WPP2024_PopulationExposureBySingleAgeSex_Medium_1950-2023.csv.gz')
  pop_y5 <- paste0(wpp_prefix, 'WPP2024_PopulationExposureByAge5GroupSex_Medium.csv.gz')
  death <- paste0(wpp_prefix, 'WPP2024_DeathsBySingleAgeSex_Medium_1950-2023.csv.gz')
})

wpp_get <- wpp_url %>% map(~{
  GET(.x, progress())
})

names <- c('death_sa','pop_y5','pop_sa')
lapply(
  c(1:3), function(x) writeBin(
    object = content(wpp_get[[x]], 'raw'),
    con = paste0('raw/', names[[x]],'.csv.gz'))
)

raw_death <- read_csv('raw/death_sa.csv.gz')
raw_pop <- list()
raw_pop$y5 <- read_csv('raw/pop_y5.csv.gz')
raw_pop$sa <- read_csv('raw/pop_sa.csv.gz')

# 2022 all-cause death counts for Northern Ireland and Scotland
ni_url <- 'https://www.nisra.gov.uk/system/files/statistics/Section%205%20-%20Death_Tables_2022_Revised_Final.xlsx'
local_file <- 'raw/death2022_ni.xlsx'
download.file(ni_url, destfile = local_file, mode = 'wb')

scotland_url <- 'https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2022/vital-events-22-ref-tabs-5.xlsx'
local_file <- 'raw/death2022_scotland.xlsx'
download.file(scotland_url, destfile = local_file, mode = 'wb')

death_uk <- list(
  ni = read_excel('raw/death2022_ni.xlsx', sheet = '5.4a', skip = 12) %>%
    filter(Area=='Northern Ireland') %>%
    select(Name = Area, starts_with('Aged')) %>%
    mutate(Sex = c('Male','Female')),
  scotland = read_excel('raw/death2022_scotland.xlsx', sheet = '5.02', range = c('B4:W8')) %>%
    mutate(Name = 'Scotland', Sex = str_remove(`...1`,'s')) %>%
    select(-c(`...1`,`...2`)) %>%
    filter(Sex %in% c('Male','Female'))
) %>%
  map(~{
    .x <- .x %>%
      gather('Age','Death',-c(Name,Sex)) %>%
      mutate(Age = parse_number(Age), Year = 2022, Death = as.numeric(Death),
             Age_width = case_when(Age==0 ~ 1, Age==1 ~ 4, Age==90 ~ Inf, T ~ 5))
  }) %>%
  bind_rows() %>%
  left_join(region_meta %>% select(Country = who_code, Name = region_name))

# UK population estimates
uk_pop <- readRDS('cleaned/uk_pop.rds') %>%
  map(~{
    .x <- .x %>%
      left_join(region_meta %>% select(Country = who_code, Name = region_name))
  })



# 2. Load & Prepare Data -------------------------------------------------------
# WHO death counts
death <- list()
death$y5 <- readRDS('cleaned/cleaned_who2.rds') %>%
  rbind(readRDS('cleaned/extracod.rds')) %>%
  filter(Cause=='All') %>%
  select(-Cause) %>%
  rbind(death_uk)
death$sa <- readRDS('cleaned/singleage_cod.rds') %>%
  filter(Cause=='All') %>% 
  select(-Cause)

## identify the open-ended group for each country
open_arg <- death$y5 %>%
  filter(Age_width==Inf) %>%
  select(Name, open_arg = Age) %>%
  unique() %>%
  add_row(Name = 'Russian Federation', open_arg = 100)

# WPP2024 pop exposure
pop <- map2(raw_pop, uk_pop, function(x, y) {
  x %>%
    inner_join(region_meta %>% select(region_code_iso3166_2_alpha3, who_code, region_name),
               by = c('ISO3_code'='region_code_iso3166_2_alpha3')) %>%
    filter(Time>=2015, Time<=2022) %>%
    transmute(Country = who_code, Name = region_name, Year = Time, Age = AgeGrpStart, 
              Age_width = ifelse(Age==100, Inf, AgeGrpSpan), Male = PopMale, Female = PopFemale) %>%
    gather(Sex, Pop, Male:Female) %>%
    mutate(Pop = Pop*1000) %>%
    rbind(y) %>%
    mutate(Source_Pop = ifelse(Name %in% c('England and Wales','Northern Ireland','Scotland'), 'ONS', 'WPP2024'))
})

pop$y5 <- pop$y5 %>%
  filter(Age>=5) %>%
  rbind(pop$sa %>%
          filter(Age>=0 & Age<=4)) %>%
  left_join(open_arg) %>%
  mutate(Age = case_when(Age>=open_arg ~ open_arg, Age %in% 1:4 ~ 1, T ~ Age),
         Age_width = case_when(Age==open_arg ~ Inf, Age==1 ~ 4, T ~ Age_width)) %>%
  group_by(Country, Name, Year, Sex, Age, Age_width, Source_Pop) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup()

# WPP2024 death counts
death2 <- list(y5 = NULL, sa = NULL)
death2$sa <- raw_death %>%
  inner_join(region_meta %>% select(region_code_iso3166_2_alpha3, who_code, region_name),
             by = c('ISO3_code'='region_code_iso3166_2_alpha3')) %>%
  filter(Time>=2015, Time<=2022) %>%
  transmute(Country = who_code, Name = region_name, Year = Time, Age = AgeGrpStart, 
            Age_width = ifelse(Age==100, Inf, AgeGrpSpan), Male = DeathMale, Female = DeathFemale) %>%
  gather(Sex, Death, Male:Female)

death2$y5 <- death2$sa %>%
  left_join(open_arg) %>%
  mutate(Age = case_when(Age>=open_arg ~ open_arg, Age %in% 1:4 ~ 1, T ~ floor(Age/5)*5),
         Age_width = case_when(Age==open_arg ~ Inf, Age==0 ~ 1, Age==1 ~ 4, T ~ 5)) %>%
  group_by(Country, Name, Year, Sex, Age, Age_width) %>%
  summarise(Death = sum(Death)) %>%
  ungroup()

# derive mx from WHO death counts and WPP exposure
death_all <- map2(death, death2, function(x,y) {
  x %>%
    mutate(Source_Death = 'WHO') %>%
    rbind(y %>%
            mutate(Source_Death = 'WPP2024', Death = Death*1000))
})

mx <- map2(death_all, pop, function(x, y) {
  inner_join(x, y) %>%
    mutate(mx = Death/Pop, Name = ifelse(Name=='Russian Federation','Russia',Name)) %>%
    drop_na()
})



# 3. Export Data ---------------------------------------------------------------
saveRDS(mx, 'cleaned/harmonized_mx.rds')

