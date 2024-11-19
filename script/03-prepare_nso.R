# This R script harmonizes the additional cause of death data from the national
# statistical offices of the USA and England & Wales.


# 0. Init ----------------------------------------------------------------------
library(here); library(readr); library(readxl); library(tidyverse); library(ungroup)
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

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position = 'plot', 
          plot.caption.position = 'plot',
          strip.background = element_blank(), 
          strip.text = element_text(face='bold', size=rel(0.8), margin=margin(0,0,2,0)),
          plot.title = element_text(face='bold', size=rel(1.2), hjust=0,
                                    margin=margin(0,0,5.5,0)))
}



# 1. Import Data ---------------------------------------------------------------
# England and Wales
url <- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset/current/21stcmortality.xlsx'
local_file <- 'raw/21stcmortality.xlsx'
download.file(url, destfile = local_file, mode = 'wb')

raw_enw <- read_excel('raw/21stcmortality.xlsx', sheet = '21', skip = 4) %>%
  rbind(read_excel('raw/21stcmortality.xlsx', sheet = '22', skip = 4)) %>%
  filter(Age!='Neonates') %>%
  mutate(Country = 4310, 
         Name = 'England and Wales',
         Sex = factor(Sex, labels = c('Male','Female')),
         Age = ifelse(Age=='<1', 0, str_sub(Age,1,2) %>% as.numeric()),
         Age_width = case_when(Age==0 ~ 1, Age==1 ~ 4, Age==85 ~ Inf, TRUE ~ 5)) %>%
  rename(Cause = `ICD-10`, Death = `Number of deaths`)

# USA cause-specific deaths
raw_usa <- cause_label[-12] %>% 
  str_replace('and ', '') %>%
  map(~{.x <- read_excel('raw/USA-2022.xlsx', sheet = .x) }) %>%
  bind_rows()

# USA all-cause deaths
usa_all <- read_delim('raw/usa_all.txt', delim = '\t', trim_ws = TRUE) %>%
  select(matches('Code'), Deaths) %>%
  drop_na() %>%
  rename(Year = `Year Code`, Sex = `Gender Code`, Age = `Five-Year Age Groups Code`,
         Death = Deaths) %>%
  mutate(Sex = ifelse(Sex=='M',1,2) %>% factor(labels = c('Male','Female')),
         Age = ifelse(Age=='1', 0, parse_number(Age)), Cause = 'All')



# 2. Prepare Data --------------------------------------------------------------
# USA
## calculate the number of deaths from 'Residual'
usa_residual <- raw_usa %>%
  group_by(Year, Sex, Age) %>%
  summarise(Death = sum(Death)) %>%
  ungroup() %>%
  left_join(usa_all %>% rename(all = Death)) %>%
  mutate(Death = all - Death, Cause = 'Residual') %>%
  select(-all)

## append
cleaned_usa <- raw_usa %>%
  rbind(usa_residual) %>%
  rbind(usa_all) %>%
  mutate(Country = 2450, Name = 'USA', Age = ifelse(Age>=95, 95, Age),
         Age_width = case_when(Age==0 ~ 1, Age==1 ~ 4, Age==95 ~ Inf, T ~ 5),
         Cause = factor(Cause, level = c(cause_label, 'All'))) %>%
  group_by(Country, Name, Year, Sex, Age, Age_width, Cause) %>%
  summarise(Death = sum(Death)) %>%
  ungroup()

# England and Wales
cleaned_enw <- raw_enw %>%
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
      TRUE ~ 12 # Residual
    )
  ) %>%
  group_by(Country, Name, Year, Sex, Age, Age_width, Cause) %>%
  summarise(Death = sum(Death)) %>%
  ungroup() %>%
  complete(nesting(Country, Name, Year, Sex, Age, Age_width), Cause, fill = list(Death=0))

## calculate total deaths
enw_all <- cleaned_enw %>%
  group_by(Country, Name, Year, Sex, Age, Age_width) %>%
  summarise(Death = sum(Death)) %>%
  mutate(Cause = 13) %>%
  ungroup()

## append 
cleaned_enw <- cleaned_enw %>%
  rbind(enw_all) %>%
  arrange(Country, Name, Year, Sex, Age, Age_width, Cause) %>%
  mutate(Cause = factor(Cause, labels = c(cause_label, 'All')))

# put data for the two countries into a list
extra <- cleaned_usa %>%
    rbind(cleaned_enw)



# 3. Export --------------------------------------------------------------------
saveRDS(extra, 'cleaned/extracod.rds')

