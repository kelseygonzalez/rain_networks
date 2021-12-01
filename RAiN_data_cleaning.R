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
pacman::p_load(qualtRics, glue, tidyverse, here, config)

## ---------------------------



## IMPORTANT OPTION ## 
version <- 'pretest'
# version <- 'fullsurvey'

#choose pretest of full survey
config <- config::get(config = version)

# Set this yourself on your computer, only needs to be run once:
# qualtRics::qualtrics_api_credentials(api_key = config$qualtrics_key,
#                                       base_url = "uarizona.iad1.qualtrics.com",
#                                       install = TRUE,
#                                      overwrite=TRUE)


# download data
data <- fetch_survey(config$qualtrics_survey_id,
                     breakout_sets = FALSE,
                     add_var_labels = FALSE,
                     force_request = TRUE)  

write_csv(data, file = glue("data/qualtrics_{version}_raw_{lubridate::today()}.csv"))



if (version == 'pretest'){
  data <- data %>% 
    filter(ResponseId %in% 
             c("R_21dHgG1zVDAb9ST", "R_1HesZEpvOzoXe7T", "R_3ktVKlCR6LjmU4G", "R_0Dmp1JPuNq80Lhn",
               "R_1eLtKyts2ZAI9pj", "R_DTbHT84ZkToUlrP", "R_2BrFsGwvSswBRUV", "R_3PvkN59IDZFLqVQ",
               "R_2EgxtjnRHWibaCb", "R_2xV4sLEgZus2FRp", "R_2OMSTVwmqJE0jlX", "R_3hu9mbrmIN4qk6i",
               "R_O1ESR9bl4gRstFv", "R_C9Y8888annJQ4GB", "R_emvPxOYfKxu28nf", "R_3ER4SgR77vjITVj",
               "R_2VgW1UPolN7sCCC", "R_10AvDU7LlTDOiPf", "R_3h0i11I5JDTYoKX", "R_cx6mDiOfuYdcld7")
    )
}



# Clean Ego Data ----------------------------------------------------------
ego <- data %>% 
  select(ResponseId, MID, StartDate:EndDate,
         duration = `Duration (in seconds)`,RecordedDate,
         Q_RecaptchaScore,consent_2,Q170:zipcode) %>% 
  # wellness came in coded strangely, clean it up
  mutate(across(wellness_1:wellness_5, ~ as.numeric(str_extract(.x, "\\d$"))))

# Clean Interview ---------------------------------------------------------
if (version == 'pretest') { 
  interview <- data %>% 
    select(ResponseId, MID, hhh_lSES:lll_hSES) %>% 
    mutate(across(ends_with("SES"), as.character)) %>% 
    pivot_longer(cols = ends_with("SES"), 
                 names_pattern = "(\\w{3})_(\\w)SES",
                 names_to = c("interview_cat", "SES_level"),
                 values_to = "interview_consent") %>% 
    filter(!is.na(interview_consent)) %>% 
    pivot_longer(cols = emo_high_2:emo_low_4, 
                 names_to = "interview_question", 
                 values_to = "interview_response" ) %>% 
    drop_na(interview_response) %>% 
    nest(interview = interview_cat:interview_response)
}



# Clean Important People --------------------------------------------------
important_people <- data %>% 
  select(MID, ResponseId, imp_family_1:imp_v_alter_10) %>%
  pivot_longer(cols = matches("imp_\\w{6}_\\d"),
               names_pattern = "imp_(\\w{6})_(\\d)",
               names_to = c("alter_type", "alter_number"),
               values_to = "important_name", 
               values_drop_na = TRUE) %>%
  mutate(alter_number = ifelse(alter_type == "person", 
                               as.numeric(alter_number) + 5,
                               as.numeric(alter_number)),
         across(imp_v_alter_1:imp_v_alter_10, as.character)) %>%  
  pivot_longer(cols = imp_v_alter_1:imp_v_alter_10,
               names_prefix = "imp_v_alter_",
               names_to = "alter_number_verify",
               values_to = 'important_person_is_alter') %>% 
  mutate(important_person_is_alter = replace_na(important_person_is_alter, "No")) %>%
  filter(alter_number == alter_number_verify) %>% 
  select(MID, ResponseId, 
         important_alter_name = important_name, 
         important_alter_type = alter_type, 
         important_person_is_alter) 


# Clean Alters ------------------------------------------------------------
alters <-
  data %>% 
  select(ResponseId, MID,
         emo_act_1:Inst3Name_catch3, 
         why_emo_1_a:why_inst_3_c_11_TEXT,
         A_gender_1:A_communication_mode_27) %>% 
  select(-contains("catch")) %>% 
  mutate(across(A_gender_1:A_communication_mode_27, as.character)) %>% 
  # qualtrics added extra _letter at the end of age and tie type variables, this removes those.
  rename_with(~ str_remove(string = .x, patter = "_\\d+$"), 
              .cols = starts_with("A_age_")) %>% 
  rename_with(~ str_remove(string = .x, patter = "_\\d+$"), 
              .cols = starts_with("A_tietype_other_")) %>% 
  # reshape demographic questions
  pivot_longer(cols =  starts_with("A_"),
               names_to = c("demographic_alter_question", "alter_number_from_demo"),
               names_pattern = "A_(.*)_(.*)$") %>%
  pivot_wider(id_cols = !c(demographic_alter_question,value),
              names_from = "demographic_alter_question", 
              values_from = 'value',
              names_repair = 'unique') %>% 
  rename_with(~ paste0('alter_', .x), gender:communication_mode )  %>% 
  # reshape activation instance examples
  pivot_longer(contains("_act_"),
               names_to = c("activation_type", "activation_instance"),
               names_pattern = "(.*)_act_(\\d*)$",
               values_to = 'activation_instance_example') %>%
  # reshape activation named examples
  pivot_longer(cols = matches("\\w{3,4}\\dName_\\d"),
               names_to = c("activation_type_to_verify_name",
                            "activation_instance_to_verify_name", 
                            "alter_number_within_instance"),
               names_pattern = "(\\w{3,4})(\\d)Name_(\\d)$",
               values_to = "alter_name",
               names_repair = 'unique') %>% 
  # drop duplicates from reshaping
  filter(str_to_lower(activation_type) == str_to_lower(activation_type_to_verify_name),
         activation_instance == activation_instance_to_verify_name) %>% 
  drop_na(activation_instance_example,alter_name) %>% 
  select(-c(activation_type_to_verify_name, activation_instance_to_verify_name)) %>% 
  # create an alter_key which indicates which # the alter is, 1-27
  mutate(alter_key = case_when(activation_type == "emo" &  activation_instance == 1 & alter_number_within_instance == 1 ~ 1,
                               activation_type == "emo" &  activation_instance == 1 & alter_number_within_instance == 2 ~ 2,
                               activation_type == "emo" &  activation_instance == 1 & alter_number_within_instance == 3 ~ 3,
                               activation_type == "emo" &  activation_instance == 2 & alter_number_within_instance == 1 ~ 4,
                               activation_type == "emo" &  activation_instance == 2 & alter_number_within_instance == 2 ~ 5,
                               activation_type == "emo" &  activation_instance == 2 & alter_number_within_instance == 3 ~ 6,
                               activation_type == "emo" &  activation_instance == 3 & alter_number_within_instance == 1 ~ 7,
                               activation_type == "emo" &  activation_instance == 3 & alter_number_within_instance == 2 ~ 8,
                               activation_type == "emo" &  activation_instance == 3 & alter_number_within_instance == 3 ~ 9,
                               activation_type == "Info" & activation_instance == 1 & alter_number_within_instance == 1 ~ 10,
                               activation_type == "Info" & activation_instance == 1 & alter_number_within_instance == 2 ~ 11,
                               activation_type == "Info" & activation_instance == 1 & alter_number_within_instance == 3 ~ 12,
                               activation_type == "Info" & activation_instance == 2 & alter_number_within_instance == 1 ~ 13,
                               activation_type == "Info" & activation_instance == 2 & alter_number_within_instance == 2 ~ 14,
                               activation_type == "Info" & activation_instance == 2 & alter_number_within_instance == 3 ~ 15,
                               activation_type == "Info" & activation_instance == 3 & alter_number_within_instance == 1 ~ 16,
                               activation_type == "Info" & activation_instance == 3 & alter_number_within_instance == 2 ~ 17,
                               activation_type == "Info" & activation_instance == 3 & alter_number_within_instance == 3 ~ 18,
                               activation_type == "Inst" & activation_instance == 1 & alter_number_within_instance == 1 ~ 19,
                               activation_type == "Inst" & activation_instance == 1 & alter_number_within_instance == 2 ~ 20,
                               activation_type == "Inst" & activation_instance == 1 & alter_number_within_instance == 3 ~ 21,
                               activation_type == "Inst" & activation_instance == 2 & alter_number_within_instance == 1 ~ 22,
                               activation_type == "Inst" & activation_instance == 2 & alter_number_within_instance == 2 ~ 23,
                               activation_type == "Inst" & activation_instance == 2 & alter_number_within_instance == 3 ~ 24,
                               activation_type == "Inst" & activation_instance == 3 & alter_number_within_instance == 1 ~ 25,
                               activation_type == "Inst" & activation_instance == 3 & alter_number_within_instance == 2 ~ 26,
                               activation_type == "Inst" & activation_instance == 3 & alter_number_within_instance == 3 ~ 27),
         alter_number_from_demo = as.numeric(alter_number_from_demo)) %>% 
  # drop duplicates 
  filter(alter_key == alter_number_from_demo) %>% 
  select(-alter_number_from_demo) %>% 
  # to get rid of the excess variables with raw text like why_emo_1_a_6_TEXT, concatenate with the non-raw version.  
  mutate(why_emo_1_a =  ifelse(!is.na(why_emo_1_a_6_TEXT),   paste0( as.character(why_emo_1_a), ", ", why_emo_1_a_6_TEXT ), why_emo_1_a),
         why_emo_1_b =  ifelse(!is.na(why_emo_1_b_11_TEXT),  paste0( as.character(why_emo_1_b), ", ", why_emo_1_b_11_TEXT), why_emo_1_b),
         why_emo_1_c =  ifelse(!is.na(why_emo_1_c_11_TEXT),  paste0( as.character(why_emo_1_c), ", ", why_emo_1_c_11_TEXT), why_emo_1_c),
         why_emo_2_a =  ifelse(!is.na(why_emo_2_a_66_TEXT),  paste0( as.character(why_emo_2_a), ", ", why_emo_2_a_66_TEXT), why_emo_2_a),
         why_emo_2_b =  ifelse(!is.na(why_emo_2_b_11_TEXT),  paste0( as.character(why_emo_2_b), ", ", why_emo_2_b_11_TEXT), why_emo_2_b),
         why_emo_2_c =  ifelse(!is.na(why_emo_2_c_11_TEXT),  paste0( as.character(why_emo_2_c), ", ", why_emo_2_c_11_TEXT), why_emo_2_c),
         why_emo_3_a =  ifelse(!is.na(why_emo_3_a_11_TEXT),  paste0( as.character(why_emo_3_a), ", ", why_emo_3_a_11_TEXT), why_emo_3_a),
         why_emo_3_b =  ifelse(!is.na(why_emo_3_b_11_TEXT),  paste0( as.character(why_emo_3_b), ", ", why_emo_3_b_11_TEXT), why_emo_3_b),
         why_emo_3_c =  ifelse(!is.na(why_emo_3_c_11_TEXT),  paste0( as.character(why_emo_3_c), ", ", why_emo_3_c_11_TEXT), why_emo_3_c),
         why_info_1_a = ifelse(!is.na(why_info_1_a_11_TEXT), paste0(as.character(why_info_1_a), ", ", why_info_1_a_11_TEXT), why_info_1_a),
         why_info_1_b = ifelse(!is.na(why_info_1_b_11_TEXT), paste0(as.character(why_info_1_b), ", ", why_info_1_b_11_TEXT), why_info_1_b),
         why_info_1_c = ifelse(!is.na(why_info_1_c_11_TEXT), paste0(as.character(why_info_1_c), ", ", why_info_1_c_11_TEXT), why_info_1_c),
         why_info_2_a = ifelse(!is.na(why_info_2_a_11_TEXT), paste0(as.character(why_info_2_a), ", ", why_info_2_a_11_TEXT), why_info_2_a),
         why_info_2_b = ifelse(!is.na(why_info_2_b_11_TEXT), paste0(as.character(why_info_2_b), ", ", why_info_2_b_11_TEXT), why_info_2_b),
         why_info_2_c = ifelse(!is.na(why_info_2_c_11_TEXT), paste0(as.character(why_info_2_c), ", ", why_info_2_c_11_TEXT), why_info_2_c),
         why_info_3_a = ifelse(!is.na(why_info_3_a_76_TEXT), paste0(as.character(why_info_3_a), ", ", why_info_3_a_76_TEXT), why_info_3_a),
         why_info_3_b = ifelse(!is.na(why_info_3_b_11_TEXT), paste0(as.character(why_info_3_b), ", ", why_info_3_b_11_TEXT), why_info_3_b),
         why_info_3_c = ifelse(!is.na(why_info_3_c_11_TEXT), paste0(as.character(why_info_3_c), ", ", why_info_3_c_11_TEXT), why_info_3_c),
         why_inst_1_a = ifelse(!is.na(why_inst_1_a_66_TEXT), paste0(as.character(why_inst_1_a), ", ", why_inst_1_a_66_TEXT), why_inst_1_a),
         why_inst_1_b = ifelse(!is.na(why_inst_1_b_11_TEXT), paste0(as.character(why_inst_1_b), ", ", why_inst_1_b_11_TEXT), why_inst_1_b),
         why_inst_1_c = ifelse(!is.na(why_inst_1_c_11_TEXT), paste0(as.character(why_inst_1_c), ", ", why_inst_1_c_11_TEXT), why_inst_1_c),
         why_inst_2_a = ifelse(!is.na(why_inst_2_a_11_TEXT), paste0(as.character(why_inst_2_a), ", ", why_inst_2_a_11_TEXT), why_inst_2_a),
         why_inst_2_b = ifelse(!is.na(why_inst_2_b_57_TEXT), paste0(as.character(why_inst_2_b), ", ", why_inst_2_b_57_TEXT), why_inst_2_b),
         why_inst_2_c = ifelse(!is.na(why_inst_2_c_11_TEXT), paste0(as.character(why_inst_2_c), ", ", why_inst_2_c_11_TEXT), why_inst_2_c),
         why_inst_3_a = ifelse(!is.na(why_inst_3_a_11_TEXT), paste0(as.character(why_inst_3_a), ", ", why_inst_3_a_11_TEXT), why_inst_3_a),
         why_inst_3_b = ifelse(!is.na(why_inst_3_b_12_TEXT), paste0(as.character(why_inst_3_b), ", ", why_inst_3_b_12_TEXT), why_inst_3_b),
         why_inst_3_c = ifelse(!is.na(why_inst_3_c_11_TEXT), paste0(as.character(why_inst_3_c), ", ", why_inst_3_c_11_TEXT), why_inst_3_c)) %>% 
  # drop the aforementioned excess raw text variables
  select(-matches("why_\\w{3,4}_\\d_\\w_\\d+_TEXT")) %>% 
  # reshape the why_ questions 
  pivot_longer(cols = starts_with("why_"),
               names_to = c("activation_type_to_verify_why",
                            "activation_instance_to_verify_why",
                            "alter_letter_to_verify_why"),
               names_pattern = "why_(\\w{3,4})_(\\d)_(\\w)$",
               values_to = "why_asked_alter",
               names_repair = 'unique') %>%
  mutate(alter_number_to_verify_why = case_when(alter_letter_to_verify_why == 'a' ~ 1,
                                                alter_letter_to_verify_why == 'b' ~ 2,
                                                alter_letter_to_verify_why == 'c' ~ 3)) %>% 
  # drop the duplicates from the why_questions 
  filter(str_to_lower(activation_type) == str_to_lower(activation_type_to_verify_why) &
           activation_instance == activation_instance_to_verify_why &
           alter_number_within_instance == alter_number_to_verify_why)  %>%  
  select(-c(activation_type_to_verify_why, activation_instance_to_verify_why, 
            alter_number_to_verify_why, alter_letter_to_verify_why)) %>% 
  # reorder questions for usability
  select(ResponseId,MID, alter_key, alter_name, 
         activation_type, activation_instance, alter_number_within_instance, 
         activation_instance_example, why_asked_alter, everything()) %>% 
  # add in important people information
  left_join(filter(important_people, important_person_is_alter == "Yes"),
            by = c('ResponseId', "alter_name" = "important_alter_name", "MID")) %>% 
  replace_na(list(important_alter_type = "none")) %>% 
  select(-important_person_is_alter)



# Join data together ------------------------------------------------------
clean_data <- ego %>% 
  # for now we can skip joining important people; they information we need is
  # already included in alters df
  # full_join(y = nest(important_people, important_people = important_alter_name:important_person_is_alter),
            # by = c("MID", "ResponseId")) %>% 
  full_join(interview,
            by = c("MID", "ResponseId")) %>% 
  full_join(nest(alters, alter_data = alter_key:important_alter_type),
            by = c("MID", "ResponseId")) 

write_rds(clean_data, file = glue("data/qualtrics_{version}_clean_{lubridate::today()}.rds"))
