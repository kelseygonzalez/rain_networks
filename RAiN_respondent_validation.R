## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Kelsey Gonzalez
##
## Date Created: 2021-11-29
##
## Email: kelseygonzalez@email.arizona.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## load up the packages we will need: 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(glue, tidyverse, here, naniar)

## ---------------------------


## IMPORTANT OPTION ## 
version <- 'pretest'
# version <- 'fullsurvey'

## ---------------------------

# load data
clean <- read_rds(glue("data/qualtrics_{version}_clean_{lubridate::today()}.rds"))
mturk <- read_csv('data/Batch_4613304_batch_results.csv')



# check data quality ------------------------------------------------------


issues_with_nulls <- clean %>%
  drop_na(alter_data) %>% 
  mutate(total_activation_examples = map_dbl(alter_data, ~ 
                                               count(.x, activation_type, activation_instance) %>% 
                                               nrow()),
         n_unique_activation_examples = map_dbl(alter_data, ~ 
                                                  count(.x, activation_instance_example) %>% 
                                                  nrow()), 
         alter_nmissing = map_dbl(alter_data, ~ naniar::add_prop_miss(.x) %>% 
                                    summarize(flag_perc_missingness_alters = 
                                                mean(prop_miss_all, na.rm = TRUE)) %>% 
                                    pull(flag_perc_missingness_alters)),
         same_repeated_example = n_unique_activation_examples / total_activation_examples,
         
         alters_perc_unique = map_dbl(alter_data, ~ summarize(.x, 
                                                              new = mean(alters_unique_n, na.rm = T) /
                                                                mean(alters_total_n, na.rm = T)) %>% 
                                        pull(new))) %>% 
  select(MID, ResponseId, same_repeated_example, alter_nmissing, alters_perc_unique) 


validation <- clean %>% 
  full_join(mturk, by = c("MID" = "WorkerId")) %>% 
  naniar::add_prop_miss() %>% 
  left_join(issues_with_nulls, by = c('MID', 'ResponseId')) %>% 
  mutate(flag_wrong_code = ifelse(code != Answer.surveycode, 1, 0),
         flag_same_example_3x = ifelse(same_repeated_example < .8, 1, 0),
         flag_less_than_5 = ifelse(duration < 300, 1, 0),
         flag_perc_missingness_overall = ifelse(prop_miss_all > .25, 1, 0),
         flag_low_unique_names = ifelse(alters_perc_unique < .70, 1, 0),
         # flag_max_alters = "",
         # flag_long_lat_missing = "",
         # flag_same_name_no = "",
         # This is misbehaving
         # total_flags =  across(starts_with("flag_"), sum)
  ) %>% 
  rowwise() %>% 
  mutate(total_flags = sum(flag_wrong_code, flag_same_example_3x, 
             flag_less_than_5, flag_perc_missingness_overall,
             flag_low_unique_names, na.rm = T)) %>% 
  select(starts_with("flag_"), total_flags)
  




validation <- data %>% 
  filter(consent_2 == "I agree",
         Q_RecaptchaScore > .85,
         RecordedDate < lubridate::ymd_hms("2021-11-23 22:03:53"),
         !(MID %in% disqualified))



validation %>% 
  filter(!(MID %in% disqualified)) %>% 
  select(MID, ResponseId, code, contains("_act_")) %>% 
  arrange(MID) %>% view()


validation %>% 
  filter(MID %in% disqualified) %>% view()
select(MID, ResponseId, code)







disqualified <-  c('AY66B9W9D0E76', 'A1DSQRD14WE083', 'A2F5DF3UGPZV9G', 'A1RFECS63DTC3U', 'ANA8HLS4U28ZY', 'A3L8SP8N52XSIT', 'A3L59AK9RX2EJT')
