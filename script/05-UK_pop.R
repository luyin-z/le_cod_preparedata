# This R script downloads and prepares population estimates for the three UK countries.


# 0. Init ----------------------------------------------------------------------
library(here); library(httr); library(readxl); library(tidyverse); library(ungroup)
setwd(str_remove(here(),'/script'))

region_meta <- read_csv('cfg/region_metadata.csv', na = '.')

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position = 'plot', plot.caption.position = 'plot',
          strip.background = element_blank(), 
          strip.text = element_text(face='bold', size=rel(0.8), margin=margin(0,0,2,0)),
          plot.title = element_text(face='bold', size=rel(1.2), hjust=0,
                                    margin=margin(0,0,5.5,0)))
}



# 1. Download Data from ONS ----------------------------------------------------
ons_prefix <- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/'

ons_url <- list(
  y2022 = paste0(ons_prefix, 'mid2022/mye22final.xlsx'),
  y2021 = paste0(ons_prefix, 'mid2021/ukpopestimatesmid2021on2021geographyfinal.xls'),
  y2020 = paste0(ons_prefix, 'mid2001tomid2020detailedtimeseries/ukpopulationestimates18382020.xlsx')
)

ons_get <- ons_url %>% map(~{
  .x <- GET(.x)
})

names <- c('uk_pop2022.xlsx','uk_pop2021.xls','uk_pop2020.xlsx')
lapply(
  c(1:3), function(x) writeBin(
    object = content(ons_get[[x]], 'raw'),
    con = paste0('raw/', names[[x]]))
)



# 2. Load & Prepare Data -------------------------------------------------------
pre_2020 <- lapply(paste('Table',c(9,16,19)), function(t) {
  d <- read_excel('raw/uk_pop2020.xlsx', sheet = t, skip = 4)
  d <- d[c(97:187,191:281),1:7]
  d$Sex <- rep(c('Male','Female'), each = 91)
  return(d)
}) %>%
  set_names(c('England and Wales','Scotland','Northern Ireland')) %>%
  bind_rows(.id = 'Name') %>%
  gather(Year, Pop, starts_with('Mid')) %>%
  mutate(Age = parse_number(Age),
         Sex = factor(Sex, levels = c('Male', 'Female')),
         Year = as.numeric(str_remove(Year,'Mid-')))

post_2020 <- lapply(c(1:2), function(x) {
  read_excel(paste0('raw/', names[[x]]), sheet = 'MYE2 - Females', skip = 7) %>%
    mutate(Sex = 'Female') %>%
    rbind(read_excel(paste0('raw/', names[[x]]), sheet = 'MYE2 - Males', skip = 7) %>%
            mutate(Sex = 'Male')) %>%
    filter(Name %in% c('ENGLAND AND WALES', 'NORTHERN IRELAND', 'SCOTLAND'))
}) %>%
  set_names(c('2022','2021')) %>%
  bind_rows(.id = 'Year') %>%
  gather(Age, Pop, `0`:`90+`) %>%
  transmute(Name = str_to_title(Name),
            Name = str_replace(Name, 'And', 'and'),
            Year = as.numeric(Year),
            Sex = factor(Sex, levels = c('Male', 'Female')),
            Age = parse_number(Age), Pop)

all <- pre_2020 %>%
  rbind(post_2020)

# prepare the 5-year-age-group dataset
y5 <- all %>%
  mutate(
    Age = case_when(
      Age==0 ~ 0,
      Age %in% 1:4 ~ 1,
      # change the open-ended group for England & Wales changed to 85+
      # to be consistent with the 2022 cause-of-death from ONS
      Name=='England and Wales' & Age>=85 ~ 85,
      T ~ floor(Age/5)*5
    ),
    Age_width = case_when(
      Age==0 ~ 1,
      Age==1 ~ 4,
      (Age==85 & Name=='England and Wales') | Age==90 ~ Inf,
      T ~ 5
    )
  ) %>%
  group_by(Name, Year, Sex, Age, Age_width) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup()

# prepare the single-age dataset with open-ended group of 100+
ungrouped <- all %>% 
  group_by(Name, Year, Sex) %>%
  group_modify(~{
    ungrouped <- tryCatch({
      openage <- max(.x$Age, na.rm = T)
      fit_pclm <- pclm(y = .x$Pop, x = .x$Age, nlast = 110-openage+1)
      ungrouped_pop <- fitted.values(fit_pclm)[1:111]
      ungrouped_pop <- tibble(
        Age = 0:110,
        Pop = round(ungrouped_pop, 2),
        Lambda = fit_pclm$smoothPar[1],
        Error = FALSE,
        Message = as.character(NA)
      )
    },
    error = function(e) {
      cat('Error: Exception in ', .y$Name, .y$Year, .y$Sex, geterrmessage(), '\n')
      tibble(
        Age = 0:110,
        Pop = as.numeric(NA),
        Lambda = as.numeric(NA),
        Error = TRUE,
        Message = geterrmessage()
      )
    })
    return(ungrouped)
  }) %>%
  ungroup()

if (all(ungrouped$Error)==F) {
  uk_pop <- list(
    y5 = y5,
    sa = ungrouped %>% 
      select(-c(Lambda, Error, Message)) %>%
      mutate(
        Pop = Pop,
        Age = ifelse(Age>=100, 100, Age),
        Age_width = ifelse(Age==100, Inf, 1)) %>%
      group_by(Name, Year, Sex, Age, Age_width) %>%
      summarise(Pop = sum(Pop)) %>%
      ungroup()
  )
} else {
  print('Error exists for PCLM')
}



# 3. Export Data ---------------------------------------------------------------
saveRDS(uk_pop, 'cleaned/uk_pop.rds')

