library(tidyverse)
library(lubridate)
library(scales)
library(googlesheets4)
library(stats)
library(stats4)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

######################################################################################################################
####### run data cleaning code for household survey
######################################################################################################################
#  -99 indicates don't know and these are converted to NA in continuous or "Don't know" in categorical   #######

rm(list=ls())
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
         conserve_enarau = 52, conserve_other = 53, land_size = 58, activity_before1 = 63, activity_before2 = 65, activity_before3 = 66, skip_meal_before = 67, wellbeing_before = 69, wellbeing_after = 70, activity_current1 = 72, activity_current2 = 73, activity_current3 = 74, 
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

saveRDS(hhs2, "hhs_cleaned.rds")

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
saveRDS(hhs_wealth, "hhs_cleaned_wealth.rds")
