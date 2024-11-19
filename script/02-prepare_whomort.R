# This R script harmonizes the cause of death data from the WHO Mortality Database.


# 0. Init ----------------------------------------------------------------------
library(here); library(readxl); library(tidyverse)
setwd(str_remove(here(),'/script'))

cause_label <- c('Acute CVD', # I20-I24, I60-I64
                 'Other CVD', # rest of I, except I42.6
                 'Acute respiratory diseases', # J00-J22, U04
                 'COPD', # J40-J47
                 'Certain infectious diseases', # A00-B99
                 'Suicide', # X60–X84, Y87.0
                 'Drug-related', # F11–F19, X40–X44, X85, Y10–Y14
                 'Alcohol-related', # F10, I42.6, K70, K74, X45, Y15
                 'Cancer', # C00-D48
                 'Other external causes', # V, W and the rest of X and Y
                 'COVID', # U07.1, U07.2
                 'Residual') # All else



# 1. Import Data ---------------------------------------------------------------
raw <- readRDS('raw/raw_who.rds')
region_meta <- read_csv('cfg/region_metadata.csv', na = '.') %>%
                      select(region_name, who_code)



# 2. Prepare Data --------------------------------------------------------------
subset <- raw %>%
  filter(Country %in% region_meta$who_code, Year>=2015, Year<=2022) %>%
  # aggregate deaths at ages 1 - 4
  mutate(Deaths6 = ifelse(Frmat==2, Deaths3, Deaths3 + Deaths4 + Deaths5 + Deaths6),
         Deaths5 = Deaths2) %>%
  select(-c(Deaths1:Deaths4)) %>%
  # reshape from wide to long
  gather(Age, Death, Deaths5:Deaths26) %>%
  mutate(
    Age = case_when(
      parse_number(Age)==5 ~ 0,
      parse_number(Age)==6 ~ 1,
      parse_number(Age)>=7 & parse_number(Age)<=24 ~ (parse_number(Age)-6)*5,
      parse_number(Age)==25 ~ 95 # Deaths26: deaths at age unspecified
    ),
    Age_width = case_when(
      Age==0 ~ 1,
      Age==1 ~ 4,
      Age>=5 & Age<=80 ~ 5,
      Age %in% c(85,90) & Frmat==0 ~ 5,
      Age==95 | (Age==85 & Frmat %in% c(1,2)) ~ Inf
    ),
    Sex = ifelse(Sex==9, NA, Sex) %>% factor(labels = c('Male','Female')) # Sex==9: sex unspecified
  ) %>%
  filter(!(Frmat!=0 & Age %in% c(90,95)), !(is.na(Sex) & Death==0), !(is.na(Age) & Death==0)) %>%
  right_join(region_meta, by = c('Country' = 'who_code')) %>%
  select(Country, Name = region_name, Year, Cause, Sex, Age, Age_width, Death)

cleaned <- subset %>%
  mutate(
    Cause = case_when(
      # Acute CVD
      grepl("^I2[0-4]|^I6[0-4]", Cause) ~ 1,
      # Other CVD
      grepl("^I[0-9]", Cause) & !grepl("^I2[0-4]|^I6[0-4]", Cause)  & !grepl("^I426", Cause) ~ 2,
      # Acute respiratory diseases
      grepl("^J[01]|^J2[0-2]|^U04", Cause) ~ 3,
      # COPD
      grepl("^J4[0-7]", Cause) ~ 4,
      # Infectious and Parasitic Diseases
      grepl("^[AB][0-9]", Cause) ~ 5,
      # Suicide
      grepl("^X[67]|^X8[0-4]|^Y870", Cause) ~ 6,
      # Drug-related
      grepl("^F1[1-9]|^X4[0-4]|^X85|^Y1[0-4]", Cause) ~ 7,
      # Alcohol-related
      grepl("^F10|^I426|^K7[04]|X45|Y15", Cause) ~ 8,
      # Cancer
      grepl("^C|^D[0-4]", Cause) ~ 9,
      # Other external causes of death
      grepl("^[VWXY]", Cause) & !grepl("^X4[0-5]|^X[67]|^X8[0-5]|^Y1[0-5]|^Y870", Cause) ~ 10,
      # COVID
      grepl("^U07[1-2]", Cause) ~ 11,
      # All
      Cause=='AAA' ~ 13,
      # Residual
      TRUE ~ 12
    ) %>% factor(labels = c(cause_label, 'All'))
  ) %>%
  # aggregate deaths according to country, year, sex, age, and causes of death
  group_by(Country, Name, Year, Sex, Age, Age_width, Cause) %>%
  summarise(Death = sum(Death)) %>%
  ungroup() %>%
  # complete the data frame
  complete(nesting(Country, Name, Year, Sex, Age, Age_width), Cause, fill = list(Death=0))

# create true open-ended age groups (in effect for Canada and Singapore)
cleaned_oag <- cleaned %>%
  group_by(Country, Name, Year, Sex, Age, Age_width) %>%
  mutate(drop = ifelse(sum(Death)==0 & Age>=85,1,0)) %>% 
  filter(drop==0) %>%
  group_by(Country, Name, Year, Sex, Cause) %>%
  mutate(Age_width = ifelse(Age==max(Age, na.rm=T), Inf, Age_width)) %>%
  select(-drop) %>%
  drop_na() %>%
  ungroup()

# make consistent open-ended groups for each country across years
## England & Wales changed to 85+ because 2022 cause-of-death from ONS used 85+
## Scotland and Northern Ireland changed to 90+ because population exposure from ONS used 90+
## besides the UK countries, in effect only Canada experiences changes from 90+ to 85+
consistent_oag <- cleaned_oag %>%
  group_by(Name) %>%
  group_modify(~{
    oag <- .x %>% filter(Age_width==Inf)
    .x$min_oag <- min(oag$Age)
    return(.x)
  }) %>%
  ungroup() %>%
  mutate(min_oag = case_when(Name=='England and Wales' ~ 85, 
                             Name %in% c('Scotland','Northern Ireland') ~ 90, 
                             T ~ min_oag),
         Age_width = ifelse(Age>=min_oag, Inf, Age_width),
         Age = ifelse(Age>=min_oag, min_oag, Age)) %>%
  group_by(Name, Country, Year, Sex, Age, Age_width, Cause) %>%
  summarise(Death = sum(Death)) %>%
  ungroup() 



# 3. Export Data ---------------------------------------------------------------
saveRDS(cleaned_oag, 'cleaned/cleaned_who.rds')
saveRDS(consistent_oag, 'cleaned/cleaned_who2.rds')

