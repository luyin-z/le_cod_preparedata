# This R script ungroups cause of death counts into single ages using PCLM (doi:10.1093/aje/kwv020).


# 0. Init ----------------------------------------------------------------------
library(here); library(readr); library(tidyverse); library(ungroup)
setwd(str_remove(here(),'/script'))

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position = 'plot', plot.caption.position = 'plot',
          strip.background = element_blank(), 
          strip.text = element_text(face='bold', size=rel(0.8), margin=margin(0,0,2,0)),
          plot.title = element_text(face='bold', size=rel(1.2), hjust=0,
                                    margin=margin(0,0,5.5,0)))
}

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



# 1. Load & Prepare Data -------------------------------------------------------
who <- readRDS('cleaned/cleaned_who.rds')
extra <- readRDS('cleaned/extracod.rds')

to_ungroup <-
  who %>%
  rbind(extra) %>%
  arrange(Country, Name, Year, Sex, Age, Age_width, Cause, Death) %>%
  mutate(
    # replace 0 with a very small number (0.01) to avoid erroneous results
    # input data contain small values, so multiply by 1000 for the purpose of ungrouping
    Death = ifelse(Death==0, 0.001, Death*1000)
  )



# 2. PCLM ----------------------------------------------------------------------
ungrouped <- to_ungroup %>% 
  # ungroup deaths into single ages (up to 110+) for each country-year-sex-cause stratum
  group_by(Country, Name, Year, Sex, Cause) %>%
  group_modify(~{
    ungrouped_deaths <- tryCatch({
      openage <- max(.x$Age, na.rm = T)
      fit_pclm <- pclm(y = .x$Death, x = .x$Age, nlast = 110-openage+1)
      ungrouped_deaths <- fitted.values(fit_pclm)[1:111]
      ungrouped_deaths <- tibble(
        Age = 0:110,
        Death = round(ungrouped_deaths, 2),
        Lambda = fit_pclm$smoothPar[1],
        Error = FALSE,
        Message = as.character(NA)
      )
    },
    # catch unexpected errors
    error = function(e) {
      cat('Error: Exception in ', .y$Country, .y$Year, .y$Sex, .y$Cause, geterrmessage(), '\n')
      tibble(
        Age = 0:110,
        Death = as.numeric(NA),
        Lambda = as.numeric(NA),
        Error = TRUE,
        Message = geterrmessage()
      )
    })
    return(ungrouped_deaths)
  }) %>%
  ungroup()

cod_sa <- ungrouped %>%
  select(-c(Lambda, Error, Message)) %>%
  mutate(
    Death = Death/1000,
    # set the open-ended age group at 100+
    Age = ifelse(Age>=100, 100, Age),
    Age_width = ifelse(Age==100, Inf, 1)) %>%
  group_by(Country, Name, Year, Sex, Age, Age_width, Cause) %>%
  summarise(Death = sum(Death)) %>%
  ungroup()



# 3. Diagnostic Plot -----------------------------------------------------------
data_plot <- cod_sa %>%
  mutate(color = case_when(Year==2021 ~ 1, Year==2022 ~ 2, TRUE ~ 3) %>% as.factor()) %>%
  group_by(Cause) %>%
  group_split() %>%
  set_names(c(cause_label, 'All'))

diag_plot <- list()
for (i in 1:length(data_plot)) {
  diag_plot[[i]] <- data_plot[[i]] %>%
    ggplot() +
    geom_line(
      aes(
        x = Age, y = Death, color = color,
        group = interaction(Year)
      )
    ) +
    scale_color_manual(values = c(`1` = 'blue', `2` = 'red', `3` = 'grey')) +
    facet_wrap(~Name+Sex, scales = 'free_y') +
    guides(color = 'none', size = 'none') +
    theme_custom() +
    labs(
      title = paste0('Ungrouped Death Counts by Age and Country: ', 
                     c(cause_label, 'All')[[i]]),
      subtitle = 'Year 2021 is blue, Year 2022 is red, prior years grey',
      x = '',
      y = ''
    )
}



# 4. Export --------------------------------------------------------------------
pdf('output/pclm_diagnostics_2022.pdf')
pdf.options(width = 16, height = 12)
print(diag_plot)
dev.off()

saveRDS(cod_sa, file = 'cleaned/singleage_cod.rds')

