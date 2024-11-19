# This R script joins all information together.


# 0. Init ----------------------------------------------------------------------
library(here); library(readxl); library(tidyverse)
setwd(str_remove(here(),'/script'))

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position = 'plot', plot.caption.position = 'plot',
          strip.background = element_blank(), 
          strip.text = element_text(face='bold', size=rel(1), margin=margin(0,0,2,0)),
          plot.title = element_text(face='bold', size=rel(1), hjust=0,
                                    margin=margin(0,0,5.5,0)))
}



# 1. Import Data ---------------------------------------------------------------
cod <- list()
cod$y5 <- readRDS('cleaned/cleaned_who2.rds') %>%
  rbind(readRDS('cleaned/extracod.rds')) %>%
  arrange(Country, Name, Year, Sex, Age, Age_width, Cause, Death)
cod$sa <- readRDS('cleaned/singleage_cod.rds')
mx <- readRDS('cleaned/harmonized_mx.rds')
region_meta <- read_csv('cfg/region_metadata.csv', na = '.')
russia <- c('cleaned/Russia_data_100p_abridged.csv','cleaned/Russia_data_100p.csv') %>% 
  map(~{
    .x <- read_csv(.x) %>%
      drop_na() %>%
      mutate(mx = Death/Pop)
  }) %>%
  set_names(c('y5','sa'))



# 2. Prepare Data --------------------------------------------------------------
# Cause1: Acute CVD (acute IHD and strokes)
# Cause2: Other CVD
# Cause3: Acute respiratory diseases
# Cause4: Chronic obstructive pulmonary disease (COPD)
# Cause5: Certain infectious diseases 
# Cause6: Suicides
# Cause7: Drug-related deaths
# Cause8: Alcohol-related deaths
# Cause9: Cancer (or Neoplasms)
# Cause10: Other external causes of death
# Cause11: COVID
# Cause12: Residual
dbc <- all <- list()
for (d in c('y5','sa')) {
  dbc[[d]] <- cod[[d]] %>% 
    group_by(Country, Name, Year, Sex, Age, Age_width) %>%
    group_modify(~{
      .x$Death[13] <- sum(.x[1:12,'Death'])
      if (.x$Death[13]!=0) {
        .x[1:12,'Death'] <- .x[1:12,'Death']/.x$Death[13]
      } else {
        # if Death = 0, set Cause12 = 1 and Causes1-11 = 0
        .x[1:11,'Death'] <- 0
        .x[12,'Death'] <- 1
      }
      return(.x)
    }) %>% 
    ungroup() %>%
    spread(Cause, Death) %>%
    select(-All)
  names(dbc[[d]])[c(7:18)] <- paste0('Cause',c(1:12))
  dbc[[d]] <- dbc[[d]] %>%
    rbind(russia[[d]] %>% select(names(dbc[[d]])))
  
  all[[d]] <- mx[[d]] %>%
    left_join(dbc[[d]]) %>%
    rbind(russia[[d]]) %>%
    rename_all(tolower) %>%
    relocate(age_width, .after = age) %>%
    rename(age_start = age) %>% 
    mutate(source_mx = ifelse(source_death!='WPP2024','type1','type2'))
}
# type1: WHO/NSO-based death counts, WPP2024/ONS-based population exposure, Russia data
# type2: WPP2024-based death counts, WPP2024-based population exposure

# plot mortality by 5-year age group
d <- all$y5 %>%
  mutate(source_mx = ifelse(source_mx=='type1', 'WHO-MDB or NSO', 'WPP2024')) %>%
  group_by(year) %>%
  group_split() %>%
  set_names(2015:2022)

pdf('output/mortality_y5.pdf', height = 20, width = 20)
lapply(seq_along(d), function(i) {
  ggplot(d[[i]]) +
    geom_line(aes(x = age_start, y = log(mx), color = source_mx, linetype = source_mx)) +
    facet_wrap(~ name + sex) +
    scale_linetype(guide = F) +
    labs(title = paste('Mortality rates at year', names(d)[[i]]), 
         x = 'Age', y = 'Mortality on a log scale',
         color = 'Sources of death counts') +
    theme_custom() +
    theme(legend.position = 'top') +
    guides(color = guide_legend(override.aes = list(linetype = c('solid','dashed'))))
})
dev.off()



# 3. Export Data ---------------------------------------------------------------
saveRDS(all, 'cleaned/harmonized_all.rds')

