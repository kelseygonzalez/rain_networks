# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(qualtRics, glue, tidyverse)

# Set this yourself on your computer, only needs to be run once:
# key from qualtrics -> account settings -> qualtrics ids -> API token
#
# qualtRics::qualtrics_api_credentials(api_key = "arsdMwOH24PxSHAVQmQlzvw0t69aYWZn5trdG17P",
#                                       base_url = "uarizona.iad1.qualtrics.com",
#                                       install = TRUE,
#                                      overwrite=TRUE)


# download data
data <- fetch_survey("SV_6XMICAT6LuzSGge", #survey ID
             breakout_sets = FALSE,
             add_var_labels = FALSE,
             force_request = TRUE)  

# check data quality ------------------------------------------------------

disqualified <-  c('AY66B9W9D0E76', 'A1DSQRD14WE083', 'A2F5DF3UGPZV9G', 'A1RFECS63DTC3U', 'ANA8HLS4U28ZY', 'A3L8SP8N52XSIT', 'A3L59AK9RX2EJT')

validation <- data %>% 
  filter(consent_2 == "I agree",
         Q_RecaptchaScore > .85,
         RecordedDate < lubridate::ymd_hms("2021-11-23 22:03:53"),
         !(MID %in% disqualified))

interest <- "I am interested in participating in the follow-up interview"

pay <- validation %>% 
  mutate(bonus = case_when(hhh_lSES == interest ~ "bonus",
                           hhl_lSES == interest ~ "bonus",
                           hlh_lSES == interest ~ "bonus",
                           lhh_lSES == interest ~ "bonus",
                           hll_lSES == interest ~ "bonus",
                           lhl_lSES == interest ~ "bonus",
                           llh_lSES == interest ~ "bonus",
                           lll_lSES == interest ~ "bonus",
                           hhh_hSES == interest ~ "bonus",
                           hhl_hSES == interest ~ "bonus",
                           hlh_hSES == interest ~ "bonus",
                           lhh_hSES == interest ~ "bonus",
                           hll_hSES == interest ~ "bonus",
                           lhl_hSES == interest ~ "bonus",
                           llh_hSES == interest ~ "bonus",
                           lll_hSES == interest ~ "bonus",
                           TRUE ~ "no")) %>% 
  select(MID, ResponseId, bonus) 


validation %>% 
  filter(!(MID %in% disqualified)) %>% 
  select(MID, ResponseId, code, contains("_act_")) %>% 
  arrange(MID) %>% view()


validation %>% 
  filter(MID %in% disqualified) %>% view()
  select(MID, ResponseId, code)



# Clean Ego Data ----------------------------------------------------------
ego <- data %>% 
  select(ResponseId, MID, StartDate:consent_2,Q170:zipcode)

# Clean Interview ---------------------------------------------------------
interview <- data %>% 
  select(ResponseId, MID, hhh_lSES:lll_hSES) %>% 
  mutate(
    across(ends_with("SES"), as.character)) %>% 
  pivot_longer(cols = ends_with("SES"), 
               names_pattern = "(\\w{3})_(\\w)SES",
               names_to = c("interview_cat", "SES_level"),
               values_to = "interview_consent") %>% 
  filter(!is.na(interview_consent))



# Clean Important People --------------------------------------------------
important_people <- data %>% 
  select(ResponseId, imp_fam_1:imp_v_alter_10) %>%
  pivot_longer(cols = matches("imp_\\w{3,6}_\\d"),
               names_pattern = "imp_(\\w{3,6})_(\\d)",
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
               values_to = 'alter_same_match') %>% 
  mutate(alter_same_match = replace_na(alter_same_match, "No")) %>%
  filter(alter_number == alter_number_verify) %>% 
  select(ResponseId, important_alter_name = important_name, 
         important_alter_type = alter_type, 
         important_alter_same_match = alter_same_match) 


# Clean Alters ------------------------------------------------------------
alters <- data %>% 
  select(ResponseId, MID,
         emo_act_1:Inst3Name_catch3, 
         why_emo_1_a:why_inst_3_c_11_TEXT,
         A_gender_1:A_communication_mode_27) %>% 
  select(-contains("catch")) %>% 
  mutate(across(A_gender_1:A_communication_mode_27, as.character)) %>% 
  rename_with(~ str_remove(string = .x, patter = "_\\d+$"), .cols = starts_with("A_age_")) %>% 
  rename_with(~ str_remove(string = .x, patter = "_\\d+$"), .cols = starts_with("A_tietype_other_")) %>% 
  pivot_longer(cols =  starts_with("A_"),
               names_to = c("question", "alter"),
               names_pattern = "A_(.*)_(.*)$") %>%
  pivot_wider(id_cols = !c(question,value),
              names_from = "question", 
              values_from = 'value',
              names_repair = 'unique') %>% 
  rename_with(~ paste0('a_', .x), gender:communication_mode )  %>% 
  pivot_longer(contains("_act_"),
               names_to = c("act_type", "act_instance"),
               names_pattern = "(.*)_act_(\\d*)$",
               values_to = 'act_instance_example') %>% 
  pivot_longer(cols = matches("\\w{3,4}\\dName_\\d"),
               names_to = c("act_type_name","act_instance_to_verify", "alter_number"),
               names_pattern = "(\\w{3,4})(\\d)Name_(\\d)$",
               values_to = "alter_name",
               names_repair = 'unique') %>% 
  filter(str_to_lower(act_type) == str_to_lower(act_type_name),
         act_instance == act_instance_to_verify) %>% 
  drop_na(act_instance_example,alter_name) %>% 
  select(-c(act_type_name, act_instance_to_verify)) %>% 
  mutate(alter_key = case_when(act_type == "emo" & act_instance == 1 & alter_number == 1 ~ 1,
                               act_type == "emo" & act_instance == 1 & alter_number == 2 ~ 2,
                               act_type == "emo" & act_instance == 1 & alter_number == 3 ~ 3,
                               act_type == "emo" & act_instance == 2 & alter_number == 1 ~ 4,
                               act_type == "emo" & act_instance == 2 & alter_number == 2 ~ 5,
                               act_type == "emo" & act_instance == 2 & alter_number == 3 ~ 6,
                               act_type == "emo" & act_instance == 3 & alter_number == 1 ~ 7,
                               act_type == "emo" & act_instance == 3 & alter_number == 2 ~ 8,
                               act_type == "emo" & act_instance == 3 & alter_number == 3 ~ 9,
                               act_type == "Info" & act_instance == 1 & alter_number == 1 ~ 10,
                               act_type == "Info" & act_instance == 1 & alter_number == 2 ~ 11,
                               act_type == "Info" & act_instance == 1 & alter_number == 3 ~ 12,
                               act_type == "Info" & act_instance == 2 & alter_number == 1 ~ 13,
                               act_type == "Info" & act_instance == 2 & alter_number == 2 ~ 14,
                               act_type == "Info" & act_instance == 2 & alter_number == 3 ~ 15,
                               act_type == "Info" & act_instance == 3 & alter_number == 1 ~ 16,
                               act_type == "Info" & act_instance == 3 & alter_number == 2 ~ 17,
                               act_type == "Info" & act_instance == 3 & alter_number == 3 ~ 18,
                               act_type == "Inst" & act_instance == 1 & alter_number == 1 ~ 19,
                               act_type == "Inst" & act_instance == 1 & alter_number == 2 ~ 20,
                               act_type == "Inst" & act_instance == 1 & alter_number == 3 ~ 21,
                               act_type == "Inst" & act_instance == 2 & alter_number == 1 ~ 22,
                               act_type == "Inst" & act_instance == 2 & alter_number == 2 ~ 23,
                               act_type == "Inst" & act_instance == 2 & alter_number == 3 ~ 24,
                               act_type == "Inst" & act_instance == 3 & alter_number == 1 ~ 25,
                               act_type == "Inst" & act_instance == 3 & alter_number == 2 ~ 26,
                               act_type == "Inst" & act_instance == 3 & alter_number == 3 ~ 27)) %>% 
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
  select(-matches("why_\\w{3,4}_\\d_\\w_\\d+_TEXT")) %>% 
  pivot_longer(cols = starts_with("why_"),
               names_to = c("act_type_to_verify","act_instance_to_verify", "alter_letter_to_verify"),
               names_pattern = "why_(\\w{3,4})_(\\d)_(\\w)$",
               values_to = "why_asked",
               names_repair = 'unique') %>% 
  mutate(alter_number_to_verify = case_when(alter_letter_to_verify == 'a' ~ 1,
                                            alter_letter_to_verify == 'b' ~ 2,
                                            alter_letter_to_verify == 'c' ~ 3)) %>% 
  filter(act_type_to_verify == act_type &
         act_instance == act_instance_to_verify &
         alter_number_to_verify == alter_number,
         alter == alter_key)  %>% 
  select(-c(act_type_to_verify, act_instance_to_verify, alter_number_to_verify, alter_letter_to_verify, alter_key, alter_number)) %>% 
  distinct() %>% 
  select(ResponseId, alter, alter_name, act_type, act_instance, act_instance_example, why_asked, everything()) %>% 
  left_join(filter(important_people, important_alter_same_match == "Yes"), by = c('ResponseId', "alter_name" = "important_alter_name" ))



