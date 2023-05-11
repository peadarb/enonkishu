library(tidyverse)
library(lubridate)
library(openxlsx)
library(scales)
library(googlesheets4)
library(stats)
library(stats4)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
library(forcats)



######################################################################################################################
####### run data cleaning code for household survey
######################################################################################################################
#  -99 indicates don't know and these are converted to NA in continuous or "Don't know" in categorical   #######

#rm(list=ls())
# bring in sheet ----------------------------------------------------------
hhs<-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1ptDKp9WYxro3W_JmKIcP2fohHkN9av1Tt6ZkDyl9MRo/edit#gid=1677543214") %>% 
  as.data.frame()

########################################################
# set all data types correctly before importation
########################################################

hhs$start <- lubridate::ymd_hms(hhs$start) # convert to date
hhs$end <- lubridate::ymd_hms(hhs$end) # convert to date time

hhs2 <- hhs %>% 
  mutate(elapsed = end-start) %>% 
  rename(agreed = 8, gender = 17, edu = 20, m_child = 22, f_child = 23, m_adult = 24, f_adult = 25, sch_child = 26, int_phon = 27, norm_phon = 28, radio = 29, torch = 30, tv = 31, elec = 32, gen = 33, sola = 34, piki = 35, car = 36, 
         table = 37, sofa = 38, lat = 39, mpesa = 40, bank = 41, fuel = 42, roof = 43, wall = 44, mobility = 45, all_conserve = 47, conserve_lemek = 48, conserve_olchorro = 49, conserve_enonkishu = 50, conserve_mbokishi = 51, 
         conserve_enarau = 52, conserve_other = 53, land_size = 58, activity_before1 = 63, activity_before2 = 65, activity_before3 = 66, skip_meal_before = 67, wellbeing_before = 69, wellbeing_after = 70, before_payment = 71, activity_current1 = 72, activity_current2 = 73, activity_current3 = 74, 
         skip_meal_after = 75, occupation = 77, access_edu = 79, access_health = 80, access_elec = 81, access_water = 82, cow_before = 85, sheep_before = 86, goat_before = 87, donkey_before = 88, cow_now = 90, sheep_now = 91, goat_now = 92, donkey_now = 93, crop_yn = 104, crop_acre = 105, conserve_authority = 111, 
         agree_before = 112, agree_now = 113, graz_hhcons = 114, graz_rules = 115, graz_rules_help = 116, settle_rules = 117, settle_rules_help = 118, forest_rules = 119, forest_rules_help = 120, water_rules = 121, water_rules_help = 122,
         wildlife_rules =123, wildlife_rules_help = 124, receive_income = 125, cons_payment = 127, hhnum_tourism = 128, hhnum_conserve = 129, income_informed = 130, influence = 132, transparency = 133, accountability = 134, women_power = 135, wild_perception = 137, wild_conf_cow = 140, wild_conf_shoat = 141, sample = 254) %>% # rename by index
  mutate(id = row_number()) %>% 
  filter(agreed == "Yes") %>%
  mutate(
    stype = case_when(
      sample == "Mbokishi" ~ "mbo",
      sample == "Enonkishu" ~ "enon",
      sample == "Lemek" ~ "lem",
      sample == "Ol Chorro" ~ "chor",
      sample == "Outside" ~ "out")) %>% 
  mutate(
    fpc = case_when(
      sample == "Mbokishi" ~ 48,
      sample == "Enonkishu" ~ 27,
      sample == "Lemek" ~ 213,
      sample == "Ol Chorro" ~ 100,
      sample == "Outside" ~ 26)) %>% 
  mutate(fpc1 = 414) %>% 
  mutate(
    pw = case_when(
      sample == "Mbokishi" ~ 2.086957, # 23 sampled out of 48
      sample == "Enonkishu" ~ 3, # 9 sampled out of 27
      sample == "Lemek" ~ 3.380952, # 63 sampled out of 213
      sample == "Ol Chorro" ~ 5.263158, # 19 sampled out of 100
      sample == "Outside" ~ 2.363636)) %>% # 11 sampled out of 26
  mutate(more_conservancies = conserve_lemek + conserve_olchorro + conserve_enonkishu + conserve_mbokishi + conserve_enarau + conserve_other) %>% 
  mutate(cow_before = replace(cow_before, cow_before == -99, NA)) %>% 
  mutate(cow_now = replace(cow_now, cow_now == -99, NA)) %>% 
  mutate(sheep_before = replace(sheep_before, sheep_before == -99, NA)) %>% 
  mutate(sheep_now = replace(sheep_now, sheep_now == -99, NA)) %>% 
  mutate(goat_before = replace(goat_before, goat_before == -99, NA)) %>% 
  mutate(goat_now = replace(goat_now, goat_now == -99, NA)) %>% 
  mutate(donkey_before = replace(donkey_before, donkey_before == -99, NA)) %>% 
  mutate(donkey_now = replace(donkey_now, donkey_now == -99, NA)) %>% 
  mutate(cow_before_tlu = cow_before*0.71) %>% #based on Grandin 1988
  mutate(cow_now_tlu = cow_now*0.71) %>%   #based on Grandin 1988
  mutate(sheep_before_tlu = sheep_before*0.17) %>% #based on Grandin 1988
  mutate(sheep_now_tlu = sheep_now*0.17) %>% #based on Grandin 1988
  mutate(goat_before_tlu = goat_before*0.17) %>% #based on Grandin 1988
  mutate(goat_now_tlu = goat_now*0.17) %>% #based on Grandin 1988
  mutate(total_before_tlu = cow_before_tlu+sheep_before_tlu+goat_before_tlu) %>% 
  mutate(total_now_tlu = cow_now_tlu+sheep_now_tlu+goat_now_tlu) %>% 
  mutate(crop_acre = if_else(is.na(crop_acre), 0, crop_acre)) %>% #there were no -99s here 
  mutate(ppl_in_hh = m_child + f_child + m_adult + f_adult+1) %>% #don't include themselves
  mutate(tlu_per_person = total_now_tlu/ppl_in_hh) %>% 
  mutate(perc_child_in_edu = sch_child/ppl_in_hh) %>%  # N.B. this does not give true indication as it includes adults 
  mutate(graze_cons = fct_recode(graz_hhcons, "1" = "Always", "1" = "Often","1" = "Sometimes", "1" = "Rarely", "0" = "Never")) %>%  # graze in cons area yes or no 
  mutate(land_size_fct = fct_recode(land_size, "1" = "Less than 10 acres", "2" = "Between 10 and 20 acres", "3" = "Between 20 and 30 acres", 
                                    "4" = "Over 30 acres", NULL = "I do not want to answer")) %>%  # graze in cons area yes or no 
  mutate(land_size_fct = fct_inseq(land_size_fct)) %>% 
  mutate(cons_payment_fct = fct_recode(cons_payment, "1" = "0 – KES 50,000", "2" = "KES 50,001 – KES 100,000","3" = "KES100,001 – KES 150,000", 
                                       "4" = "KES 150,001 – KES 200,000", "5" = "KES 200,001 – KES 250,000", "6" = "KES 250,000+", NULL = "I do not want to answer")) %>%  # graze in cons area yes or no 
  mutate(cons_payment_fct = fct_inseq(cons_payment_fct)) %>% 
  mutate(hwc_cow_tlu = wild_conf_cow*0.71) %>% #NO -99 in data - tlu based on Grandin 1988)
  mutate(hwc_shoat_tlu = wild_conf_shoat*0.17) %>% #NO -99 in data - tlu based on Grandin 1988)
  mutate(hwc_total_tlu = hwc_cow_tlu + hwc_shoat_tlu) %>% 
  mutate(activity_current1 = as.factor(activity_current1)) %>%  
  mutate(activity_current1 = fct_collapse(activity_current1,
                                          "Conservancy" = c("Conservancy land access payment"),
                                          "Cultivation" = c("Cultivation"),
                                          "Livestock" = c("Livestock and related products"),
                                          "Tourism" = c("Tourism related employment"),
                                          "Employed" = c("Other skilled or permanent employment", "Government Employment"),
                                          "Own business" = c("Own business"),
                                          "None" = c("None"),
                                          "Refused" = c("I do not want to answer"))) %>% 
  mutate(activity_current2 = as.factor(activity_current2)) %>%  
  mutate(activity_current2 = fct_collapse(activity_current2,
                                          "Conservancy" = c("Conservancy land access payment"),
                                          "Cultivation" = c("Cultivation"),
                                          "Livestock" = c("Livestock and related products"),
                                          "Tourism" = c("Tourism related employment"),
                                          "Employed" = c("Other skilled or permanent employment"),
                                          "Dependent" = c("Cash remittances"),
                                          "Loans or credit" = c("Loans or credit"),
                                          "Own business" = c("Own business"),
                                          "None" = c("None"))) %>% 
  mutate(activity_current3 = as.factor(activity_current3)) %>%  
  mutate(activity_current3 = fct_collapse(activity_current3,
                                          "Conservancy" = c("Conservancy land access payment"),
                                          "Cultivation" = c("Cultivation"),
                                          "Livestock" = c("Livestock and related products"),
                                          "Tourism" = c("Tourism related employment"),
                                          "Employed" = c("Other skilled or permanent employment", "Government Employment"),
                                          "Dependent" = c("Cash remittances", "Food aid"),
                                          "Loans or credit" = c("Loans or credit"),
                                          "Own business" = c("Own business"),
                                          "Other" = c("Kibarua or short-term employment (includes working for someone as bodaboda driver or herder)"),
                                          "None" = c("None"))) %>% 
  mutate(activity_before1 = as.factor(activity_before1)) %>%  
  mutate(activity_before1 = fct_collapse(activity_before1,
                                         "Cultivation" = c("Cultivation"),
                                         "Livestock" = c("Livestock and related products"),
                                         "Tourism" = c("Tourism related employment"),
                                         "Employed" = c("Other skilled or permanent employment"),
                                         "Own business" = c("Own business"),
                                         "None" = c("None"))) %>% 
  mutate(activity_before2 = as.factor(activity_before2)) %>%  
  mutate(activity_before2 = fct_collapse(activity_before2,
                                         "Conservancy" = c("Conservancy land access payment"),
                                         "Cultivation" = c("Cultivation"),
                                         "Livestock" = c("Livestock and related products"),
                                         "Tourism" = c("Tourism related employment"),
                                         "Employed" = c("Other skilled or permanent employment", "Government Employment"),
                                         "Dependent" = c("Cash remittances"),
                                         "Own business" = c("Own business"),
                                         "Other" = c("Kibarua or short-term employment (includes working for someone as bodaboda driver or herder)"),
                                         "None" = c("None"))) %>% 
  mutate(activity_before3 = as.factor(activity_before3)) %>%  
  mutate(activity_before3 = fct_collapse(activity_before3,
                                         "Conservancy" = c("Conservancy land access payment"),
                                         "Cultivation" = c("Cultivation"),
                                         "Livestock" = c("Livestock and related products"),
                                         "Tourism" = c("Tourism related employment"),
                                         "Employed" = c("Other skilled or permanent employment", "Government Employment"),
                                         "Dependent" = c("Cash remittances"),
                                         "Loans or credit" = c("Loans or credit"),
                                         "Own business" = c("Own business"),
                                         "Other" = c("Kibarua or short-term employment (includes working for someone as bodaboda driver or herder)"),
                                         "None" = c("None")))
#change outliers in expenditure to NA
#wealth_index_all$expenditure[wealth_index_all$expenditure > quantile(wealth_index_all$expenditure, 0.99, na.rm = T)] <- NA

saveRDS(hhs2, "hhs_cleaned2.rds")

#Categorise the household into pastoral only, diversified pastoral, agri only, diversified agri, wage earner, poor, Other

#glimpse(hhs)

########################################################################################################################################
#########################################################################################################################################
# can end data cleaning here
######################################################################################################################################### 
########################################################################################################################################


#######################################################################################################################
###### constructing wealth index #####
#######################################################################################################################

# 1. exploratory data analysis of variables to potentially use in PCA ######
hhs_pca_eda <- hhs2 %>% 
  select(id, int_phon, norm_phon, radio, torch, tv, elec, gen, sola, piki, car, table, sofa, lat, mpesa, bank, fuel, 
         roof, wall, land_size, cow_now_tlu, sheep_now_tlu, goat_now_tlu, total_now_tlu, crop_acre, more_conservancies) %>% 
  mutate(int_phon = ifelse(int_phon == "Yes", 1, 0)) %>% 
  mutate(norm_phon = ifelse(norm_phon == "Yes", 1, 0)) %>% 
  mutate(radio = ifelse(radio == "Yes", 1, 0)) %>% 
  mutate(torch = ifelse(torch == "Yes", 1, 0)) %>% 
  mutate(tv = ifelse(tv == "Yes", 1, 0)) %>% 
  mutate(elec = ifelse(elec == "Yes", 1, 0)) %>% 
  mutate(gen = ifelse(gen == "Yes", 1, 0)) %>% 
  mutate(sola = ifelse(sola == "Yes", 1, 0)) %>% 
  mutate(piki = ifelse(piki == "Yes", 1, 0)) %>% 
  mutate(car = ifelse(car == "Yes", 1, 0)) %>% 
  mutate(table = ifelse(table == "Yes", 1, 0)) %>% 
  mutate(sofa = ifelse(sofa == "Yes", 1, 0)) %>% 
  mutate(lat = ifelse(lat == "Yes", 1, 0)) %>% 
  mutate(mpesa = ifelse(mpesa == "Yes", 1, 0)) %>% 
  mutate(bank = ifelse(bank == "Yes", 1, 0)) %>% 
  mutate(gas_fuel = ifelse(fuel == "Gas", 1, 0)) %>% 
  mutate(cement_brick_iron_roof = ifelse(roof == "Corrugated Iron" | roof == "Cement/Bricks", 1, 0)) %>% 
  mutate(brick_cement_wall = ifelse(wall == "Bricks (or mud bricks) with Cement (Durable)", 1, 0)) %>% 
  mutate(large_land_size = ifelse(land_size == "Over 30 acres", 1, 0)) %>% 
  mutate(more_conservancies_binary= ifelse(more_conservancies > 1, 1, 0)) %>% 
  select(!c(fuel, roof, wall, land_size))
#change outliers to NA for total TLU and crop area
#hhs_pca_eda$total_tlu[hhs_pca_eda$total_tlu > quantile(hhs_pca_eda$total_tlu, 0.99, na.rm = T)] <- NA
#hhs_pca_eda$crop_acre[hhs_pca_eda$crop_acre > quantile(hhs_pca_eda$crop_acre, 0.99, na.rm = T)] <- NA

# 2. Select all the subset of variables to be used to construct the PCA and omit all NA
# this one contains all the ones possible including some continuous variables
# could remove large land size as this has a number of NAs
hhs_pca_eda_subset_all <- hhs_pca_eda %>% 
  select(!c(more_conservancies, large_land_size, total_now_tlu, cow_now_tlu, sheep_now_tlu, goat_now_tlu, crop_acre, more_conservancies_binary)) %>% 
  na.omit()

### for all, checked all scaling and centering and both scaling and centering are needed - means it is a correlation matrix
hhs_pca_all <- prcomp(hhs_pca_eda_subset_all, center = TRUE, scale = TRUE)
summary(hhs_pca_all)


#boxplot of all the variables with red cross at mean 
plot <- hhs_pca_eda_subset_all %>% 
  select(-id)

#### for binary variables, checked all scaling and centering and only centering needed - means it is a covariance matrix 
hhs_pca_binary <- prcomp(plot, center = TRUE, scale = FALSE)
summary(hhs_pca_binary)

# construct index of principal components
index_all = hhs_pca_binary$x[,1]
nlab<-c(1,2,3,4,5)

# append the index, and the wealth quintiles from all (with tlu and crop area) onto the full hhs dataframe
hhs_pca_eda_subset_all <- hhs_pca_eda_subset_all %>% 
  mutate(quintiles = as.factor(cut(index_all, breaks=5, labels=nlab))) %>% 
  mutate(wealth_pca = index_all) 

hhs_wealth <- full_join(hhs2, hhs_pca_eda_subset_all, by = "id")

#write_csv(hhs, "C:/Users/peada/Documents/PhD/Research/4_data/2_analysis/hhsurvey/1_raw/hhs_cleaned.csv")
saveRDS(hhs_wealth, "hhs_cleaned_wealth2.rds")



######################################################################################################################
####### import cleaned household survey data with wealth index
######################################################################################################################

#rm(list=ls())

hhs_wealth <- readRDS("hhs_cleaned_wealth2.rds")
head(hhs_wealth)


######################################################################################################################
############# Survey based household material and mobility ########
######################################################################################################################

strat_design_srvyr_house <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, roof, wall, mobility, sample)) 
#add weights=~pw to include weights which are      total in strata/number sampled in strata
strat_design_srvyr_house

# each_conserve_roof <- strat_design_srvyr_house %>% 
#   group_by(sample, roof) %>% 
#   summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
#             #total = survey_total(vartype = "ci", na.rm=TRUE),
#             n= unweighted(n()))
# 
# each_conserve_wall <- strat_design_srvyr_house %>% 
#   group_by(sample, wall) %>% 
#   summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
#             #total = survey_total(vartype = "ci", na.rm=TRUE),
#             n= unweighted(n()))

each_conserve_mobility <- strat_design_srvyr_house %>% 
  filter(sample == "Enonkishu") %>% 
  #mutate(mobility = factor(mobility, levels = c(1,2,3), 
  #                      labels=c("None", "Partial", "Whole"))) %>%  
  group_by(sample, mobility) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(each_conserve_mobility, aes(x=sample, y=proportion, fill = mobility)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=each_conserve_mobility, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  # scale_fill_manual(values=c("#D091BB", "#BBD4A6", "#DFDFDF"), 
  #name="Legend Title",
  #breaks=c("None", "Livestock only move", "I do not want to answer"),
  #labels=c("None", "Livestock only move", "I do not want to answer")) +
  labs(title="What is the level of relocating of the \nland titleholder's household to \naccess livestock grazing, but not \nduring a bad drought?",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "level of mobility_enon.png"))

######################################################################################################################
############# Survey based household education level reached  ############
######################################################################################################################

strat_design_srvyr_edu <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, edu))
#add weights=~pw to include weights which are      total in strata/number sampled in strata
strat_design_srvyr_edu

each_conserve_edu <- strat_design_srvyr_edu %>%
  filter(sample == "Enonkishu") %>% 
  group_by(sample, edu) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(each_conserve_edu, aes(x=sample, y=proportion, group = edu, fill = edu)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=each_conserve_edu, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  # scale_fill_manual(values=c("#F5B7B1", "#D091BB", "#A9CCE3", "#BBD4A6", "#FAD7A0", "#2E86C1", "#DFDFDF"), 
  #name="Legend Title",
  # breaks=c("None", "Adult literacy classes (Gumbaru)", "Primary", "Secondary", "Diploma", "Degree", "I do not want to answer"),
  # labels=c("None", "Adult literacy classes (Gumbaru)", "Primary", "Secondary", "Diploma", "Degree", "I do not want to answer")) +
  labs(title="What is the highest completed level of \neducation of the land title holder?",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "level of education completed_enon.png"))

######################################################################################################################
############# Survey based household assets  ##############
######################################################################################################################

strat_design_srvyr_asset <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, int_phon.y, sample)) 
#add weights=~pw to include weights which are      total in strata/number sampled in strata
strat_design_srvyr_asset 

####for each asset ####
each_conserve_int_phon <- strat_design_srvyr_asset %>% 
  group_by(sample, int_phon.y) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  mutate(int_phon.y=recode(int_phon.y,
                           "0" = "No",
                           "1" = "Yes"))

ggplot(each_conserve_int_phon, aes(x=sample, y=proportion, group = int_phon.y, fill = int_phon.y)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=each_conserve_int_phon, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#D091BB", "#BBD4A6"), 
                    #name="Legend Title",
                    breaks=c("No", "Yes"),
                    labels=c("No", "Yes")) +
  labs(title="Does the land title holder's household have the following? \nInternet mobile phone",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "smartphone ownership.png"))

######################################################################################################################
############# Survey based gender of land title holder  ##############
######################################################################################################################

strat_design_srvyr_gender <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, gender, sample)) 
#add weights=~pw to include weights which are      total in strata/number sampled in strata

####for each asset ####
each_conserve_gender <- strat_design_srvyr_gender %>% 
  filter(sample == "Enonkishu") %>%
  group_by(sample, gender) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(each_conserve_gender, aes(x=sample, y=proportion, group = gender, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=each_conserve_gender, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#BBD4A6","#D091BB"), 
                    #name="Legend Title",
                    breaks=c("Female", "Male"),
                    labels=c("Female", "Male")) +
  labs(title="Gender of land title holder", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "Gender of land title holder_enon.png"))


######################################################################################################################
######## Survey based graph of proportion of HHS who agreed with setting up the cons area at the time and now ########
######################################################################################################################

strat_design_srvyr_hhs <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, agree_before, agree_now))

a_before <- strat_design_srvyr_hhs %>% 
  mutate(agree_before = factor(agree_before, levels = c("No", "Yes", "<i>Don't Know</i>", NA), labels=c("No", "Yes", "Don't Know"))) %>% 
  group_by(sample, agree_before) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  na.omit()

ggplot(a_before, aes(x=sample, y=proportion, group = agree_before, fill = agree_before)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a_before, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#D091BB", "#BBD4A6", "#DFDFDF", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("No", "Yes", "Don't Know", "NA"),
                    labels=c("No", "Yes", "Don't Know", "NA")) +
  labs(title="At the time that THIS conservancy was set up, \ndid the land title holder personally agree with \nthat decision?",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "agreed_with_cons_before_enon.png"))

a_now <- strat_design_srvyr_hhs %>% 
  mutate(agree_now = factor(agree_now, levels = c("No", "Yes", "<i>Don't Know</i>", NA), labels=c("No", "Yes", "Don't Know"))) %>% 
  group_by(sample, agree_now) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  na.omit()

ggplot(a_now, aes(x=sample, y=proportion, group = agree_now, fill = agree_now)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a_now, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#D091BB", "#BBD4A6", "#DFDFDF", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("No", "Yes", "<i>Don't Know</i>", "NA"),
                    labels=c("No", "Yes", "<i>Don't Know</i>", "NA")) +
  labs(title="Does the land title holder agree with the community's \ndecision to have THIS conservancy at the moment?",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "agreed_with_cons_now_enon.png"))


#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (can be adjusted with copy paste)
######################################################################################################################

strat_design_srvyr_hhs <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, skip_meal_before, skip_meal_after, 
                                                                      occupation, access_edu, access_health, access_elec, access_water,
                                                                      crop_yn, conserve_authority, graz_hhcons, graz_rules, graz_rules_help,
                                                                      settle_rules, settle_rules_help, forest_rules, forest_rules_help,
                                                                      water_rules, water_rules_help, wildlife_rules, wildlife_rules_help,
                                                                      receive_income, cons_payment_fct, income_informed, influence,
                                                                      transparency, accountability, women_power, wild_perception, hhnum_tourism, hhnum_conserve))

a <- strat_design_srvyr_hhs %>% 
  group_by(sample, women_power) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))


ggplot(a, aes(x=sample, y=proportion, group = women_power, fill = women_power)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  #  scale_fill_manual(values=c("#008b45","#6FAFCA","yellow3", "tan3", "#cd3700", "#DFDFDF"), 
  #                    #name="Legend Title",
  #                    breaks=c("1","2","3", "4", "5", "6"),
  #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
  #                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer")) +
  labs(title = "Women have the power to influence decisions in this conservancy", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.4,0.9))
ggsave(filename = here::here("images", "Women have the power to influence decisions in this conservancy (enonkishu).png"))

#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (women and power)
######################################################################################################################
strat_design_srvyr_hh <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, skip_meal_before, skip_meal_after, 
                                                                      occupation, access_edu, access_health, access_elec, access_water,
                                                                      crop_yn, conserve_authority, graz_hhcons, graz_rules, graz_rules_help,
                                                                      settle_rules, settle_rules_help, forest_rules, forest_rules_help,
                                                                      water_rules, water_rules_help, wildlife_rules, wildlife_rules_help,
                                                                      receive_income, cons_payment_fct, income_informed, influence,
                                                                      transparency, accountability, women_power, wild_perception)) %>%
  mutate(women_power = fct_relevel(women_power, "Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"))

w <- strat_design_srvyr_hh %>% 
  group_by(sample, women_power) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(women_power = fct_reorder(women_power, proportion)) %>%
  na.omit() 
#write.xlsx(w, here::here("images", "women_power_all.xlsx"))



ggplot(w, aes(x=sample, y=proportion, group = women_power, fill = women_power)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=w, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#f7f7f7", "#e9a3c9", "#c51b7d", "#808080", "#000000"), 
                    #                    #name="Legend Title",
                    breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"), 
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "Don't Know", "I do not want to answer")) +
  labs(title = "Do you agree with this statement: \nWomen have the power to influence decisions \nin this conservancy", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "Women have the power_enon.png"))

#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (authority over conservancy)
######################################################################################################################
strat_design_srvyr_aoc <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, skip_meal_before, skip_meal_after, 
                                                                      occupation, access_edu, access_health, access_elec, access_water,
                                                                      crop_yn, conserve_authority, graz_hhcons, graz_rules, graz_rules_help,
                                                                      settle_rules, settle_rules_help, forest_rules, forest_rules_help,
                                                                      water_rules, water_rules_help, wildlife_rules, wildlife_rules_help,
                                                                      receive_income, cons_payment_fct, income_informed, influence,
                                                                      transparency, accountability, women_power, wild_perception)) %>%
  mutate(conserve_authority = fct_relevel(conserve_authority, "All conservancy members","A small number of conservancy members","Conservancy management company", "<i>Don't Know</i>"))


aoc <- strat_design_srvyr_aoc %>% 
  group_by(sample, conserve_authority) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(conserve_authority = fct_reorder(conserve_authority, proportion)) %>%
  na.omit() 
#write.xlsx(aoc, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(aoc, aes(x=sample, y=proportion, group = conserve_authority, fill = conserve_authority)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=aoc, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#e6f5d0","#808080"), 
                    #                    #name="Legend Title",
                    breaks=c("All conservancy members","A small number of conservancy members","Conservancy management company", "<i>Don't Know</i>"),
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("All conservancy members","A small number of conservancy members","Conservancy management company", "Don't Know")) +
  labs(title = "Who has the authority over THIS conservancy?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "auth over conservancy_enon.png"))

#######################################################################################################################
####### 38. Compare your life before and after land access payments were paid by the conservancy. Is your life?
######################################################################################################################

strat_design_srvyr_bef <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, skip_meal_before, skip_meal_after, 
                                                                      occupation, access_edu, access_health, access_elec, access_water,
                                                                      crop_yn, conserve_authority, graz_hhcons, graz_rules, before_payment, graz_rules_help,
                                                                      settle_rules, settle_rules_help, forest_rules, forest_rules_help,
                                                                      water_rules, water_rules_help, wildlife_rules, wildlife_rules_help,
                                                                      receive_income, cons_payment_fct, income_informed, influence,
                                                                      transparency, accountability, women_power, wild_perception)) 


bef_pay <- strat_design_srvyr_bef %>% 
  group_by(sample, before_payment) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(before_payment = fct_reorder(before_payment, proportion)) %>%
  na.omit() 
#write.xlsx(aoc, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(bef_pay, aes(x=sample, y=proportion, group = before_payment, fill = before_payment)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=bef_pay, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#808080", "#e9a3c9", "#c51b7d", "#f7f7f7", "#000000"), 
                    #                    #name="Legend Title",
                    breaks=c("A lot better","A little better","The same", "A little worse", "A lot worse", "<i>Don't Know</i>", "I do not want to answer"),
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("A lot better","A little better","The same", "A little worse", "A lot worse", "Don't Know", "I do not want to answer")) +
  labs(title = "Compare your life before & after land access \npayments were paid by the conservancy. \nIs your life?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "quality of life_enon.png"))

#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (HH influence in the conservancy)
######################################################################################################################


inf <- strat_design_srvyr_hhs %>% 
  group_by(sample, influence) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(influence = fct_reorder(influence, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(inf, aes(x=sample, y=proportion, group = influence, fill = influence)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=inf, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#c51b7d","#000000","#808080"), 
                    # name="Legend Title",
                    breaks=c("A lot of influence","A little influence","No influence", "I do not want to answer", "<i>Don't Know</i>"),
                    # labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("A lot of influence","A little influence","No influence", "I do not want to answer", "Don't Know")) +
  labs(title = "How much influence do you feel the landtitle \nholder's household has in decision making in \nTHIS conservancy?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "inf in conservancy_enon.png"))


#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Accountability in the conservancy)
######################################################################################################################

acct <- strat_design_srvyr_hhs %>% 
  group_by(sample, accountability) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(accountability = fct_reorder(accountability, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(acct, aes(x=sample, y=proportion, group = accountability, fill = accountability)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=acct, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#e9a3c9","#c51b7d","#808080"), 
                    #                    #name="Legend Title",
                    breaks=c("Very satisfied","Satisfied","Unsatisfied","Very unsatisfied","<i>Don't Know</i>"),
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("Very satisfied","Satisfied","Unsatisfied","Very unsatisfied","Don't Know")) +
  labs(title = "Are you satisfied with the level of accountability \nin THIS conservancy's decision making?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "accountability in conservancy_enon.png"))


#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Transparency in the conservancy)
######################################################################################################################

trans <- strat_design_srvyr_hhs %>% 
  group_by(sample, transparency) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(transparency = fct_reorder(transparency, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(trans, aes(x=sample, y=proportion, group = transparency, fill = transparency)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=trans, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#e9a3c9","#c51b7d","#808080"), 
                    #  name="Legend Title",
                    breaks=c("Very satisfied","Satisfied","Unsatisfied","Very unsatisfied","<i>Don't Know</i>"),
                    #  labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("Very satisfied","Satisfied","Unsatisfied","Very unsatisfied","Don't Know")) +
  labs(title = "Are you satisfied with the transparency of decision \nmaking in THIS conservancy?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "transparency in conservancy_enon.png"))


#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Use of Money in the Conservancy )
######################################################################################################################

money_use <- strat_design_srvyr_hhs %>% 
  group_by(sample, income_informed) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(income_informed = fct_reorder(income_informed, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(money_use, aes(x=sample, y=proportion, group = income_informed, fill = income_informed)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=money_use, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#c51b7d", "#4d9221", "#808080"), 
                    #                    #name="Legend Title",
                    breaks=c("No","Yes","<i>Don't Know</i>"),
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("No","Yes","Don't Know")) +
  labs(title = "Do you feel like the land title holder's \nhousehold is sufficiently informed about the \nuse of the money by the conservancy", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "Use of Money in the Conservancy_enon.png"))

#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Employment Status per Conservancy)
######################################################################################################################

occp <- strat_design_srvyr_hhs %>% 
  group_by(sample, occupation) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(occupation = fct_reorder(occupation, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(occp, aes(x=sample, y=proportion, group = occupation, fill = occupation)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=occp, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  # scale_fill_manual(values=c("#fde0ef","#a1d76a","#f7f7f7", "#e6f5d0", "#c51b7d", "#e9a3c9", "#4d9221")) + 
  #                    #name="Legend Title",
  #                    breaks=c("1","2","3", "4", "5", "6"),
  #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
  #                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "Don't Know", "I do not want to answer")) +
  labs(title = "Does the land title holder have an occupation? \nIf yes, what is land title holder's occupation?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "Leaseholder Occupations_enon.png"))


#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Access to Education)
######################################################################################################################

edu <- strat_design_srvyr_hhs %>% 
  group_by(sample, access_edu) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(access_edu = fct_reorder(access_edu, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(edu, aes(x=sample, y=proportion, group = access_edu, fill = access_edu)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=edu, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#f7f7f7", "#e9a3c9", "#c51b7d", "#808080", "#000000"), 
                    #                    #name="Legend Title",
                    breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"), 
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "Don't Know", "I do not want to answer")) +
  labs(title = "How do you feel about the following statements: \nCurrently, I am happy with my household's \naccess to education facilities & services", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "education access_enon.png"))


#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Access to health facilities)
######################################################################################################################

health <- strat_design_srvyr_hhs %>% 
  group_by(sample, access_health) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(access_health = fct_reorder(access_health, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(health, aes(x=sample, y=proportion, group = access_health, fill = access_health)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=health, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#f7f7f7", "#e9a3c9", "#c51b7d", "#808080", "#000000"), 
                    #                    #name="Legend Title",
                    breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"), 
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "Don't Know", "I do not want to answer")) +
  labs(title = "How do you feel about the following statements \nCurrently, I am happy with my household's \naccess to health facilities & services", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "access_to_health_enon.png"))

#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Access to electricity services)
######################################################################################################################

elec <- strat_design_srvyr_hhs %>% 
  group_by(sample, access_elec) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>%
  mutate(access_elec = fct_reorder(access_elec, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(elec, aes(x=sample, y=proportion, group = access_elec, fill = access_elec)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=elec, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221","#a1d76a","#f7f7f7", "#e9a3c9", "#c51b7d", "#808080", "#000000"), 
                    #                    #name="Legend Title",
                    breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"), 
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "Don't Know", "I do not want to answer")) +
  labs(title = "How do you feel about the following statements \nCurrently, I am happy with my household's \naccess to electricity services", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "access_to_electricity_enon.png"))


#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Growing Crops)
######################################################################################################################

crops <- strat_design_srvyr_hhs %>% 
  group_by(sample, crop_yn) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(crop_yn = fct_reorder(crop_yn, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(crops, aes(x=sample, y=proportion, group = crop_yn, fill = crop_yn)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=crops, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#c51b7d", "#4d9221","#808080"), 
                    #                    #name="Legend Title",
                    breaks=c("No","Yes","I do not want to answer"),
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("No","Yes","I do not want to answer")) +
  labs(title = "Has the land title holder's household cultivated \ncrops in the last year, here or elsewhere", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "crops_enon.png"))

#######################################################################################################################
####### Survey based graph of proportion of HHS and a number of different variables (Wildlife Perception)
######################################################################################################################

wildlife <- strat_design_srvyr_hhs %>% 
  group_by(sample, wild_perception) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  mutate(wild_perception = fct_reorder(wild_perception, proportion)) %>%
  na.omit() 
#write.xlsx(inf, here::here("images", "authority_over_conservancy.xlsx"))


ggplot(wildlife, aes(x=sample, y=proportion, group = wild_perception, fill = wild_perception)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=wildlife, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#4d9221", "#e6f5d0", "#808080", "#c51b7d"), 
                    #                    #name="Legend Title",
                    breaks=c("Strongly like","Like","Neutral", "Strongly dislike"),
                    #                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
                    labels=c("Strongly like","Like","Neutral", "Strongly dislike")) +
  labs(title = "How do you feel about the wildlife living here", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 0.9)) +
  cowplot::theme_half_open() + 
  facet_wrap(~sample, drop = T, ncol=1, scales = "free_y") + coord_flip()
ggsave(filename = here::here("images", "wildlife_enon.png"))


########################################################################################################################################################################################
########  agree with cons before after (enon sankey)   ################################################################################################################################
########################################################################################################################################################################################

sankey <- hhs_wealth %>% 
  filter(sample == "Enonkishu") %>% 
  select(stype, fpc, pw, sample, agree_before, agree_now)%>% 
  mutate(agree_before = fct_explicit_na(agree_before, na_level = "NA")) %>% 
  mutate(agree_now = fct_explicit_na(agree_now, na_level = "NA")) %>% 
  make_long(agree_before, agree_now) %>% 
  mutate(node = fct_relevel(node, "NA", "No", "Yes", "Don't Know"), 
         next_node = fct_relevel(next_node, "NA", "No", "Yes", "Don't Know")) %>%
  mutate(node = factor(node,      levels = c("NA", "No", "Yes", "<i>Don't Know</i>"),
                       labels=c("NA", "No", "Yes", "Don't Know"))) %>%
  mutate(next_node = factor(next_node,      levels = c("NA", "No", "Yes", "<i>Don't Know</i>"),
                            labels=c("NA", "No", "Yes", "Don't Know")))
agree <- sankey 
levels(agree$x) <- c("Before Joining Conservancy", "After Joining Conservancy")

ggplot(agree, aes(x = x, 
                  next_x = next_x, 
                  node = node, 
                  next_node = next_node,
                  fill = factor(node), 
                  label = node)) +
  geom_alluvial(flow.alpha = .6) +
  geom_alluvial_text(size = 3, color = "black") +
  #scale_fill_viridis_d(option = "magma", direction = -1) +
  #scale_fill_manual(values = c("NA" = "#999999", "10" = "#999999"))%>% 
  theme_alluvial(base_size = 18) +
  labs(xlab = NULL, ylab = "Number of Respondents") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        #axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Change in acceptance of conservancies \nbefore their establishment and now") +
  ylim(0,10)

ggsave(filename = here::here("images", "Sankey agree with cons before after_enon.png"))

########################################################################################################################################################################################
########  agree with cons before after (enon boxplot)   ################################################################################################################################
########################################################################################################################################################################################

sankey_cons <- hhs_wealth %>% 
  filter(sample == "Enonkishu") %>% 
  select(stype, fpc, pw, sample, agree_before, agree_now)%>% 
  mutate(agree_before = fct_explicit_na(agree_before, na_level = "NA")) %>% 
  mutate(agree_now = fct_explicit_na(agree_now, na_level = "NA")) %>% 
  make_long(agree_before, agree_now) %>% 
  mutate(node = fct_relevel(node, "NA", "No", "Yes", "<i>Don't Know</i>"), 
         next_node = fct_relevel(next_node, "NA", "No", "Yes", "<i>Don't Know</i>"))

## As boxplot

cons <- sankey_cons %>%
  mutate(node = as.numeric(node)) 

cons<- cons 
levels(cons$x) <- c("Before Joining Conservancy", "After Joining Conservancy")

cons_box <- ggplot()+
  geom_boxplot(data = cons, aes(x = x, y = node, col = "red")) +
  scale_fill_brewer(palette="Dark2") +
  theme_alluvial(base_size = 13) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Change in acceptance of conservancies \nbefore their establishment and now") +
  ylim(0,10)

cons_box

cowplot::save_plot("images/agree cons boxplot_enon.png", cons_box)

########################################################################################################################################################################################
########    skipping meals before after as graph ####################################################################################################################################
########################################################################################################################################################################################

sankey_meals <- hhs_wealth %>% 
  filter(sample == "Enonkishu") %>%
  select(stype, fpc, pw, sample, activity_before1, activity_before2, 
         activity_before3, skip_meal_before, wellbeing_before, wellbeing_after,
         activity_current1, activity_current2, activity_current3, skip_meal_after,
         cow_before, sheep_before, goat_before, donkey_before, 
         cow_now, sheep_now, goat_now, donkey_now,
         agree_before, agree_now, total_before_tlu, total_now_tlu)%>% #
  mutate(skip_meal_before = fct_explicit_na(skip_meal_before, na_level = "NA")) %>% 
  mutate(skip_meal_after = fct_explicit_na(skip_meal_after, na_level = "NA")) %>% 
  make_long(skip_meal_before, skip_meal_after) %>% 
  mutate(node = fct_relevel(node, "NA", "Never", "Only a few days in the worst months", "Some days in every month", "Some days in every week"), 
         next_node = fct_relevel(next_node, "NA", "Never", "Only a few days in the worst months", "Some days in every month", "Some days in every week"))


## Box plot 
meals <- sankey_meals %>%
  mutate(node = as.numeric(node)) 

meals2 <- meals 
levels(meals2$x) <- c("Skip Meal Before", "Skip Meal After")

meals_bar <- ggplot()+
  geom_boxplot(data = meals2, aes(x = x, y = node, col = "red")) +
  scale_fill_brewer(palette="Dark2") +
  theme_alluvial(base_size = 10) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Rate of Leaseholders skipping meals \nbefore and after joining the conservancies") +
  ylim(0,10)

meals_bar

cowplot::save_plot("images/skipping meals boxplot_enon.png", meals_bar)

### Trying another visualization (bar graph)

meals3 <- sankey_meals %>%
  mutate(node = as.factor(node)) 

meals4 <- meals3 
levels(meals4$x) <- c("Skip Meal Before Joining", "Skip Meal After Joining")

meals_bar <- ggplot()+
  geom_bar(data = meals4, aes(x = x, fill = node, group = node), position = "dodge") +
  #scale_fill_brewer(direction = -1) +
  scale_fill_manual(values=c("#f7f7f7","#4d9221","#a1d76a","#e9a3c9", "#c51b7d", "#808080"), 
                    breaks=c("NA", "Never", "Only a few days in the worst months", "Some days in every month", "Some days in every week", "I do not want to answer"),
                    labels=c("NA", "Never", "Only a few days in the worst months", "Some days in every month", "Some days in every week", "I do not want to answer")) +
  theme_alluvial(base_size = 10) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = .1),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Rate of Leaseholders skipping meals before and after \njoining the conservancies") +
  ylim(0,25)

meals_bar

cowplot::save_plot("images/skipping meals bar_enon.png", meals_bar)

########################################################################################################################################################################################
########    wellbeing before after as graph ####################################################################################################################################
########################################################################################################################################################################################

sankey <- hhs_wealth %>% 
  mutate(wellbeing_before = as.numeric(wellbeing_before)) %>% 
  mutate(wellbeing_before = as.factor(wellbeing_before)) %>% 
  mutate(wellbeing_before = fct_explicit_na(wellbeing_before, na_level = "NA")) %>% 
  mutate(wellbeing_after = as.numeric(wellbeing_after)) %>% 
  mutate(wellbeing_after = as.factor(wellbeing_after)) %>% 
  mutate(wellbeing_after = fct_explicit_na(wellbeing_after, na_level = "NA")) %>% 
  select(stype, fpc, pw, sample, activity_before1, activity_before2, 
         activity_before3, skip_meal_before, wellbeing_before, wellbeing_after,
         activity_current1, activity_current2, activity_current3, skip_meal_after,
         cow_before, sheep_before, goat_before, donkey_before, 
         cow_now, sheep_now, goat_now, donkey_now,
         agree_before, agree_now, total_before_tlu, total_now_tlu)%>%
  filter(sample == "Enonkishu") %>% 
  na.omit() %>%
  make_long(wellbeing_before, wellbeing_after) %>%
  mutate(node = as.numeric(node))  

wellbeing <- sankey 
levels(wellbeing$x) <- c("Before Joining Conservancy", "After Joining Conservancy")

wellbeing_bar <- ggplot()+
  geom_boxplot(data = wellbeing, aes(x =x, y = node, col = "red")) +
  theme_alluvial(base_size = 15) +
  labs(xlab = NULL, y = "Count") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  #scale_color_grey () +
  ggtitle("Self-assessed Wellbeing in Enonkishu") +
  ylim(0,10)

wellbeing_bar

#ggsave(filename = here::here("images", "wellbeing box all.png"))


cowplot::save_plot("images/wellbeing boxplot enon.png", wellbeing_bar)

########################################################################################################################################################################################
########   Leaseholder Occupation as totals ####################################################################################################################################
########################################################################################################################################################################################

occp <- strat_design_srvyr_hhs %>% 
  group_by(sample, occupation) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  na.omit() 

occp_bar <- ggplot()+
  geom_bar(data = occp, aes(x = occupation, fill = total, group = occupation), position = position_dodge()) +
  # scale_fill_brewer(direction = -1) +
  # theme_alluvial(base_size = 13) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .1),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Number of people employed in various fields in Enonkishu Conservancy") 
  #ylim(0,100)

occp_bar

cowplot::save_plot("images/occupation_totals.png", occp_bar)

########################################################################################################################################################################################
########   Tourism Occupation as totals ####################################################################################################################################
########################################################################################################################################################################################

occpt <- strat_design_srvyr_hhs %>% 
  group_by(sample, hhnum_tourism) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  na.omit() 

occpt_bar <- ggplot()+
  geom_bar(data = occpt, aes(x = hhnum_tourism, fill = total, group = hhnum_tourism), position = position_dodge()) +
  # scale_fill_brewer(direction = -1) +
  # theme_alluvial(base_size = 13) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .1),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Number of people employed in tourism in Enonkishu Conservancy") 
#ylim(0,100)

occpt_bar

cowplot::save_plot("images/tourism_occupation_totals.png", occpt_bar)

########################################################################################################################################################################################
########   Conservation Occupation as totals ####################################################################################################################################
########################################################################################################################################################################################

occpc <- strat_design_srvyr_hhs %>% 
  group_by(sample, hhnum_conserve) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  filter(sample == "Enonkishu") %>% 
  na.omit() 

occpc_bar <- ggplot()+
  geom_bar(data = occpc, aes(x = hhnum_conserve, fill = total, group = hhnum_conserve), position = position_dodge()) +
  # scale_fill_brewer(direction = -1) +
  # theme_alluvial(base_size = 13) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .1),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Number of people employed in conservation in Enonkishu Conservancy") 
#ylim(0,100)

occpc_bar

cowplot::save_plot("images/conservation_occupation_totals.png", occpc_bar)