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
# version <- 'pretest'
version <- 'fullsurvey'

## ---------------------------

# load data

# cleaned data after running "RAiN_data_cleaning.R"
clean <- read_rds(glue("data/qualtrics_{version}_clean_{lubridate::today()}.rds"))
# the csv from mturk which you use to mark who gets paid and who doesn't
mturk <- read_csv('data/Batch_4613304_batch_results.csv')



# check data quality ------------------------------------------------------

# this part drills down into the alter data and doesn't work alter_data is
# empty. run by itself first.

validation_alter_data <- clean %>%
  drop_na(alter_data) %>% 
  mutate(total_activation_examples = 
           map_dbl(alter_data, ~ 
                     count(.x, activation_type, activation_instance) %>% 
                     nrow()),
         
         n_unique_activation_examples = 
           map_dbl(alter_data, ~ 
                     count(.x, activation_instance_example) %>% 
                     nrow()), 
         perc_repeated_activation_examples = n_unique_activation_examples / total_activation_examples,
         
         alter_perc_missingness = 
           map_dbl(alter_data, ~ naniar::add_prop_miss(.x) %>% 
                     summarize(flag_perc_missingness_alters = 
                                 mean(prop_miss_all, na.rm = TRUE)) %>% 
                     pull(flag_perc_missingness_alters)),
         
         alters_perc_unique =
           map_dbl(alter_data, ~ summarize(.x, new = mean(alters_unique_n, na.rm = T) /
                                             mean(alters_total_n, na.rm = T)) %>% pull(new)),
         
         alters_total_n = 
           map_dbl(alter_data, ~ summarize(.x, alters_total_n = 
                                             mean(alters_total_n, na.rm = T)) %>% 
                     pull(alters_total_n)),
         
         alters_incorrect_duplicate_catch = 
           map_dbl(alter_data, ~ filter(.x, duplicate_catch == "No") %>% nrow())
         
  ) %>% 
  select(MID, 
         ResponseId,
         perc_repeated_activation_examples, 
         alter_perc_missingness, 
         alters_perc_unique, 
         alters_total_n,
         alters_incorrect_duplicate_catch) 


validation <- clean %>% 
  full_join(mturk, by = c("MID" = "WorkerId")) %>% 
  naniar::add_prop_miss() %>% 
  left_join(validation_alter_data, by = c('MID', 'ResponseId')) %>% 
  # fill the validation_alter_data for those who weren't calculated
  replace_na(list(perc_repeated_activation_examples = 1,
                  alter_perc_missingness = 1,
                  alters_perc_unique = 0,
                  alters_total_n = 0,
                  alters_incorrect_duplicate_catch = 1)) %>% 
  
  # create flags
  add_count(MID, name = 'survey_taken_n') %>% 
  mutate(flag_wrong_survey_code = ifelse(code != Answer.surveycode, 1, 0),
         flag_wrong_survey_code = ifelse(is.na(code) | is.na(Answer.surveycode), 1, 0),
         flag_same_example_3x = ifelse(perc_repeated_activation_examples < .8, 1, 0),
         flag_less_than_5_min = ifelse(duration < 300, 1, 0),
         flag_perc_missingness_overall = ifelse(prop_miss_all > .25, 1, 0),
         flag_perc_missingness_alters = ifelse(alter_perc_missingness > .25, 1, 0),
         flag_low_unique_names = ifelse(alters_perc_unique < .70, 1, 0),
         flag_max_alters = ifelse(alters_total_n == 27, 1, 0),
         flag_no_alters = ifelse(alters_total_n == 0, 1, 0),
         flag_long_lat_missing = ifelse(is.na(LocationLatitude) |
                                          is.na(LocationLongitude),1, 0),
         flag_duplicate_catch_incorrect = ifelse(alters_incorrect_duplicate_catch > 0,
                                                 1, 0),
         flag_survey_taken_twice = ifelse(survey_taken_n > 1, 1, 0)) %>% 
  # add up flags
  rowwise() %>% 
  mutate(total_flags =  sum(c_across(starts_with("flag_")), na.rm = T)) %>% 
  select(MID, ResponseId, starts_with("flag_"), total_flags)



# create file for manual checks

double_check_me <- validation %>%
  filter(total_flags > 1) %>% 
  left_join(select(mturk, WorkerId, Answer.surveycode), by = c("MID" = "WorkerId")) %>% 
  left_join(clean, by = c('MID', 'ResponseId')) 

# download double check file if need be for exploration in excel
double_check_me %>% 
  unnest(cols = c(alter_data)) %>% 
  openxlsx::write.xlsx(file = glue("data/qualtrics_{version}_invest_flags_{lubridate::today()}.xlsx"))


# select which cases to approve and disprove 
# remember, only can upload to github with responseIDs NOT MIDs
approved <- validation %>%
  filter(total_flags <= 1) %>% 
  pull(ResponseId)

rejected <- double_check_me %>% 
  pull(ResponseId)


# fill out mturk csv file
mturk_to_upload <- mturk %>% 
  left_join(select(clean, MID, ResponseId), by = c("WorkerId" = "MID")) %>% 
  mutate(Approve = ifelse(ResponseId %in% approved, 'x', NA_character_),
         Reject = ifelse(ResponseId %in% rejected, 'x', NA_character_)) %>% 
  select(-ResponseId)

# test to make sure each row is selected
mturk_to_upload %>% 
  filter(is.na(Approve) & is.na(Reject)) %>% 
  select(WorkerId, Approve, Reject)

# test to make sure no row is double selected 
mturk_to_upload %>% 
  filter(!is.na(Approve) & !is.na(Reject)) %>% 
  select(WorkerId, Approve, Reject)

# save for upload to Mturk batch manager
write_csv(mturk_to_upload, file = glue("data/mturk_batch_{version}_validated_{lubridate::today()}.csv"))

