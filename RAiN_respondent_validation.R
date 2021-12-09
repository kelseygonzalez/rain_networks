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
pacman::p_load(glue, tidyverse, here, naniar, rIP, excluder)
# install_github("ip2location/ip2proxy-r")


## ---------------------------


## IMPORTANT OPTION ## 
# version <- 'pretest'
version <- 'fullsurvey'
batch <- 3

## ---------------------------

# load data

# Raw data for IP & location checks
IPcheck_data_raw <- read_csv(glue("data/qualtrics_{version}_raw_{lubridate::today()}.csv"))

# cleaned data after running "RAiN_data_cleaning.R"
clean <- read_rds(glue("data/qualtrics_{version}_clean_{lubridate::today()}.rds"))
# the csv from mturk which you use to mark who gets paid and who doesn't
mturk <- read_csv(glue('data/Batch_{batch}_results.csv'))
# %>% 
#   filter(is.na(ApprovalTime),
#          is.na(RejectionTime))

hit_already_completed <- read_csv('data/HID_already_paid.csv') 

already_paid_MID <- hit_already_completed %>% pull(hit_already_completed)

# Batch reject data

mturk_batches <- read_csv('data/Batch_1_results.csv') %>% 
  mutate(batch = 1) %>% 
  bind_rows(mutate(read_csv('data/Batch_2_results.csv'), batch = 2)) %>% 
  bind_rows(mutate(read_csv('data/Batch_3_results.csv'), batch = 3))


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
  right_join(mturk, by = c("MID" = "WorkerId")) %>%
  naniar::add_prop_miss() %>% 
  left_join(validation_alter_data, by = c('MID', 'ResponseId')) %>%
  # fill the validation_alter_data for those who weren't calculated
  replace_na(list(perc_repeated_activation_examples = 1,
                  alter_perc_missingness = 1,
                  alters_perc_unique = 0,
                  alters_total_n = 0,
                  alters_incorrect_duplicate_catch = 1)) %>% 
  
  # create flags
  add_count(MID, ResponseId, name = 'survey_taken_n') %>% 
  mutate(
    flag_double_dipper = ifelse(MID %in% already_paid_MID & is.na(ApprovalTime), 3, 0),
         flag_wrong_survey_code = ifelse(is.na(code) | is.na(Answer.surveycode), 1, 0),
         flag_wrong_survey_code = ifelse(code != Answer.surveycode, 1, 0),
         flag_same_example_3x = ifelse(perc_repeated_activation_examples < .8, 1, 0),
         flag_less_than_5_min = ifelse(duration < 300, 1, 0),
         flag_perc_missingness_overall = ifelse(prop_miss_all > .25, 1, 0),
         flag_perc_missingness_alters = ifelse(alter_perc_missingness > .25, 1, 0),
         flag_low_unique_names = ifelse(alters_perc_unique < .50, 1, 0),
         flag_max_alters = ifelse(alters_total_n == 27, 1, 0),
         flag_no_alters = ifelse(alters_total_n == 0, 1, 0),
         flag_long_lat_missing = ifelse(is.na(LocationLatitude) |
                                          is.na(LocationLongitude),1, 0),
         flag_duplicate_catch_incorrect = ifelse(alters_incorrect_duplicate_catch > 0,
                                                 1, 0),
         flag_survey_taken_twice = ifelse(survey_taken_n > 1, 1, 0),
         flag_duplicateIP = ifelse(IPcheck_data_raw %>% mark_duplicates(dupl_location = FALSE) == "", 0, 1),
         flag_IPnonUS = ifelse(IPcheck_data_raw %>% mark_ip(country = "US") == "", 0, 1),
         flag_locNonUS = ifelse(IPcheck_data_raw %>% mark_location() == "", 0, 1)) %>% 
  # add up flags
  rowwise() %>% 
  mutate(total_flags = sum(c_across(starts_with("flag_")), na.rm = T)) %>% 
  select(MID, HITId, ResponseId, starts_with("flag_"), total_flags)



# select which cases to approve and disprove 
# remember, only can upload to github with responseIDs NOT MIDs
approved <- validation %>%
  filter((total_flags <= 2) |
           (ResponseId %in% c('R_3nIBmsvGWsScy9C','R_2zZLO18c3LMQ9ss', 'R_An9AnLvg0912tgZ', 'R_zVBrNzzC7lUCtPP', 
                              'R_ZC6RxDxU8SPsF57', 'R_3nIBmsvGWsScy9C', 'R_2ANEmFg2ylBP3cL', 
                              'R_2qELJ29qJwyJrb0','R_2zZLO18c3LMQ9ss','R_2U0iioWA0IEx0hW',
                              'R_w4wrdNM0WwZgB8t','R_1zdIOXjNtSxwReF','R_2Yh6JNqVCRqylWp',
                              'R_ZC6RxDxU8SPsF57','R_3k1DfomFVRxumu8','R_33mAxizCzef41IC',
                              'R_2dfDkG90DQtHuKt', 'R_1LpB2HZXiCMaSgj', 'R_1P5FwixTbyLLFIV',
                              'R_11crdDcPZUQyzNM', 'R_2WPaszkg8Fmwy0s', 'R_2XamfBxCbfk6NN0',
                              'R_didKpO3G5BFyqAN', 'R_1rJsN7Ch1zz6GRM'
           ))) %>% 
  select( MID, HITId) %>% 
  mutate(Approve = 'x',
         RequesterFeedback_a = 'Approved')

# decide who to reject

rejected_double_dip <- validation %>% 
  filter(flag_double_dipper > 0) %>% 
  anti_join(approved, by = c("MID", "HITId")) %>% 
  select( MID, HITId) %>% 
  mutate(Reject_b = 'x',
         RequesterFeedback_b = 'Survey Already paid, we pay only once per worker.')


rejected_survey_not_taken <- validation %>%
  filter(is.na(ResponseId)) %>%
  anti_join(approved, by = c("MID", "HITId")) %>% 
  select( MID, HITId) %>% 
  mutate(Reject_c = 'x',
         RequesterFeedback_c = 'No Survey was received from Qualtrics for this HIT.')

rejected_poor_responses <- validation %>% 
  filter(ResponseId %in% c('R_2s5x19uXea58aLf','R_3M9Ztlh4KdTx6rQ','R_3JfuTwQWToCyTuV',
                           'R_1pPEO230mmLWEbK','R_3D5hcr0hItlv5aZ','R_An9AnLvg0912tgZ',
                           'R_zVBrNzzC7lUCtPP','R_1hYkg1rys53HVwF','R_vZztkSMD0XOLZUB',
                           'R_3qQhPzEHA0IVx5T','R_w4wVkkQ8XdfyUmZ','R_4GAcstrBfNWZ3vb',
                           'R_2aCrSMjJ5lgxPMe','R_2xJhRVR8K0VobhO','R_2xJhRVR8K0VobhO',
                           'R_22JkMYnT65EkKKY','R_SUBcWSdfRJ3CmRz','R_dbuZb1se6PeoEY9',
                           'R_3oXDbTGgYODrLe1','R_22WTSM8ttAacqN6','R_2vZVaibJm5x75VB',
                           'R_232iYcjen9VKWbU','R_1rvJWcHeOwHSdda','R_C1wJJzPk07zxq37',
                           'R_2s5x19uXea58aLf', 'R_1JEDE3FF04AuJtc','R_1mpYM7AeRYyrI5M',
                           'R_2t5TXRc2ISdKbNG','R_2VsTJwSoPzZwVnC', 'R_33pWl3aCQeHeYt3',
                           'R_3DkzSCWx3qJCJ1W', 'R_3HYX6Nq9I6L8VGa','R_3njdLmJ0SAtlJ9c',
                           'R_3oFoiNEKOsmbm34','R_psXpq49MovXdt2p', 'R_UsihLHKsArkwuJP',
                           'R_VR1Ji08qzvfgqbf'
                           )) %>% 
  anti_join(approved, by = c("MID", "HITId")) %>% 
  mutate(Reject_d = 'x',
         RequesterFeedback_d = 'Rejected due to low response quality',
         RequesterFeedback_d = ifelse(ResponseId %in% c('R_VR1Ji08qzvfgqbf', 'R_3HYX6Nq9I6L8VGa'),
                                      'Completed the HIT multiple times; low response quality', 
                                      RequesterFeedback_d)) %>% 
  select(MID, HITId,Reject_d, RequesterFeedback_d) 



# create file for manual checks
double_check_me <- validation %>%
    anti_join(approved, by = c("MID", "HITId")) %>% 
    anti_join(rejected_double_dip, by = c("MID", "HITId")) %>% 
    anti_join(rejected_survey_not_taken, by = c("MID", "HITId")) %>% 
    anti_join(rejected_poor_responses, by = c("MID", "HITId")) %>% 
    left_join(select(mturk, WorkerId, Answer.surveycode), by = c("MID" = "WorkerId")) %>% 
    left_join(clean, by = c('MID', 'ResponseId')) 

if (nrow(double_check_me) > 0) {
  # download double check file if need be for exploration in excel
  drop_no_variance_columns <- double_check_me %>%
    ungroup() %>% 
    summarize(across(starts_with('flag_'), sum)) %>% 
    pivot_longer(cols = everything()) %>% 
    filter(value == 0) %>% 
    pull(name)
  
  double_check_me %>% 
    select(-all_of(drop_no_variance_columns)) %>% 
    unnest(cols = c(alter_data)) %>% view()
  openxlsx::write.xlsx(file = glue("data/qualtrics_{version}_{lubridate::today()}_batch{batch}.xlsx"),
                       overwrite = TRUE)
}





# fill out mturk csv file
mturk_to_upload <- mturk %>% 
  # left_join(select(clean, MID, ResponseId), by = c("WorkerId" = "MID")) %>% 
  select(-c(Approve, Reject)) %>% 
  left_join(approved, by = c("WorkerId" = "MID", 'HITId' = 'HITId')) %>% 
  left_join(rejected_double_dip, by = c("WorkerId" = "MID",  'HITId' = 'HITId')) %>% 
  left_join(rejected_survey_not_taken, by = c("WorkerId" = "MID",  'HITId' = 'HITId'))  %>% 
  left_join(rejected_poor_responses, by = c("WorkerId" = "MID",  'HITId' = 'HITId'))  %>% 
  mutate(RequesterFeedback = case_when(!is.na(RequesterFeedback_a) ~ RequesterFeedback_a,
                                       !is.na(RequesterFeedback_b) ~ RequesterFeedback_b,
                                       !is.na(RequesterFeedback_c) ~ RequesterFeedback_c,
                                       !is.na(RequesterFeedback_d) ~ RequesterFeedback_d),
         Reject = case_when(!is.na(Reject_b) ~ Reject_b,
                            !is.na(Reject_c) ~ Reject_c,
                            !is.na(Reject_d) ~ Reject_d)) %>% 
  select(HITId, HITTypeId,Title,                  
         Description, Keywords,Reward,
         CreationTime, MaxAssignments,RequesterAnnotation,
         AssignmentDurationInSeconds, AutoApprovalDelayInSeconds,Expiration,
         NumberOfSimilarHITs, LifetimeInSeconds,AssignmentId,
         WorkerId, AssignmentStatus,AcceptTime,
         SubmitTime, AutoApprovalTime,ApprovalTime,
         RejectionTime, RequesterFeedback,WorkTimeInSeconds,
         LifetimeApprovalRate, Last30DaysApprovalRate,Last7DaysApprovalRate,
         Input.hits, Answer.surveycode, Approve, Reject)




length(approved) + length(rejected_double_dip) + length(rejected_poor_responses) + length(rejected_survey_not_taken) == nrow(validation)

# test to make sure each row is selected
mturk_to_upload %>% 
  filter(is.na(Approve) & is.na(Reject)) %>% 
  select(WorkerId, Approve, Reject)

# test to make sure no row is double selected 
mturk_to_upload %>% 
  filter(!is.na(Approve) & !is.na(Reject)) %>% 
  select(WorkerId, Approve, Reject, RequesterFeedback) %>% 
  arrange(RequesterFeedback) 

# save for upload to Mturk batch manager
write_csv(mturk_to_upload, 
          na = '',
          file = glue("data/mturk_batch_{version}_validated_{str_replace_all(string = lubridate::now(), pattern = ':', '-')}.csv"))


# add onto the already_paid list to prevent double dippers
validation %>% 
  filter(ResponseId %in% approved$MID) %>% 
  select('hit_already_completed' = MID) %>% 
  bind_rows(hit_already_completed) %>% 
  write_csv(file = 'data/HID_already_paid.csv')


# Check IPs -------------------------------------------------------------

### Using rIP package ---Currently not working. No idea why at this point

# Our ip hub key

sicss_iphub_key <- "MTYxMzA6RHg2MEh5NVBJTkRVSDgwQXNqS2FVaGJHSEh6Y0liVzQ="


# Get IP addresses assessment from IPHub
IPcheck_output <- IPcheck_data_raw %>% 
  select(ResponseId, MID, IPAddress) %>% 
  as.data.frame() %>% 
  getIPinfo(i = "IPAddress",
            iphub_key = sicss_iphub_key)

IPcheck_output <- IPcheck_output %>% as_tibble()

write_rds(IPcheck_output, file = glue("data/IPaddress_check_{lubridate::today()}.rds"))

# Analyze data

IPcheck_output %>% summarize(
  percent_nonUS = sum(IP_Hub_nonUSIP) / n(),
  percent_VPS = sum(IP_Hub_VPS) / n(),
  percent_blockRec = sum(IP_Hub_recommend_block) / n()
)


### Using excluder package

# I think the duplicate location function is not as helpful, so for now I didn't include it here.

IPcheck_data_raw %>% summarize(
  perc_duplicateIP = IPcheck_data_raw %>% check_duplicates(dupl_location = FALSE) %>% nrow() / nrow(IPcheck_data_raw),
  perc_IPnonUS = IPcheck_data_raw %>% check_ip(country = "US") %>% nrow() / nrow(IPcheck_data_raw),
  perc_locNonUS = IPcheck_data_raw %>% check_location() %>% nrow() / nrow(IPcheck_data_raw)
)


### Analysis by rejected/accepted
# Get Reject data to use for facet_wrap












