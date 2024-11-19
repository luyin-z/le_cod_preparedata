# This R script downloads the cause of death data from the WHO Mortality Database.


# 0. Init ----------------------------------------------------------------------
library(here); library(httr); library(glue); library(tidyverse)
setwd(paste0(str_remove(here(),'/script'),'/raw'))



# 1. Download Data -------------------------------------------------------------
prefix <- 'https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/'

url <- list()
url <- within(url, {
  part1 <- glue('{prefix}morticd10_part1.zip?sfvrsn=e2a4f93a_15&ua=1')
  part2 <- glue('{prefix}morticd10_part2.zip?sfvrsn=6e55000b_3&ua=1')
  part3 <- glue('{prefix}morticd10_part3.zip?sfvrsn=9f1111a2_7&ua=1')
  part4 <- glue('{prefix}morticd10_part4.zip?sfvrsn=259c5c23_20&ua=1')
  part5 <- glue('{prefix}morticd10_part5.zip?sfvrsn=ad970d0b_26&ua=1')
})

get <- url %>% map(~{
  GET(.x, progress())
})

names <- paste0('part',c(5:1))
lapply(
  c(1:5), function(x) writeBin(
    object = content(get[[x]], 'raw'),
    con = paste0(names[[x]],'.zip'))
)



# 2. Append Data ---------------------------------------------------------------
# obtain file names
filenames <- as.list(paste0(names,'.zip')) %>%
  map(~{
    unzip(.x, list = TRUE)[['Name']][grepl('part',unzip(.x, list = TRUE)[['Name']])]
  })

# import and bind all files in zip archives into single dataset
raw <- lapply(c(1:5), function(x) 
  unz(paste0(names[[x]],'.zip'), filename = filenames[[x]]) %>% 
    read_csv(col_types = paste0(c('i',rep('c',2),'i',rep('c',2),rep('i',33)), collapse=''))
  ) %>%
  bind_rows()

# remove the original zip archives
unlink('*.zip')



# 3. Export Data --------------------------------------------------------------
saveRDS(raw, 'raw_who.rds')

