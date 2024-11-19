1. "01-download_whomort.R" downloads the cause of death data from the WHO Mortality Database. The output file is "raw_who.rds".

2. "02-prepare_whomort.R" harmonizes the 2015-2022 (or 2021 for some countries) cause of death data from the WHO Mortality Database for selected countries. The output files are "cleaned_who.rds" and "cleaned_who2.rds".

3. "03-prepare_nso.R" downloads and harmonizes the additional cause of death data from UK ONS and USA CDC. (NB: Data from the CDC were downloaded manually.) The output file is "extracod.rds".

4. "04-PCLM_whomort.R" ungroups the harmonized cause of death data by 5-year age group into single ages using PCLM. The ungrouping is performed for each country-year-sex-cause stratum. The output file is "singleage_cod.rds".

5. “05-UK_pop.R" downloads and prepares population/exposure estimates from NSO for the three UK countries. The output file is "uk_pop.rds".

6. "06-prepare_mx.R" prepares 2 sets of mortality rates by sex and single age (open-ended: 100+) or 5-year age group using denominators from WPP2024/ONS and numerators from the WHO or WPP2024. The output file is "harmonized_mx.rds".

7. "07-join_all.R" merges all data together. The output file is "harmonized_all.rds".