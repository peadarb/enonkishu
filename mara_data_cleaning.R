########################      Done in post data collection:   #################################
##########################################################################################
# 


#rm(list=ls())
#rm(list=setdiff(ls(), "x")) to remove all by x
library(tidyverse)
library(hablar)


#using read.csv instead of read_csv to keep the commas in the columns with multiple values
surveyraw <- read.csv("C:/Users/peada/Documents/PhD/Research/4_data/2_analysis/hhsurvey/1_raw/27032019_raw.csv") 

#read from google sheets


###########################################
#######      BE AWARE OF THIS    ##########
############################################

######  -99 indicates don't know and these are    ######
####### converted to NA in continuous or     ########
####### "Don't know" in categorical   #######

#surveyraw[surveyraw==-99]<-NA # or <-"Don't Know"


########################################################
# set all data types correctly before importation
########################################################


hhs <- hhs %>%
  convert(fct(enum_name)) %>%  # convert to factor (the last few could be logical)
  convert(num(ObjectID, fpc)) %>%  # convert to numeric
  #convert(int()) %>%  # convert to integer
  #convert(lgl(on_original_list)) %>%  # convert to logical
  convert(chr(other_issue)) #N.B. remove other_1 when not using read_csv

hhs$surv_date <- lubridate::dmy(hhs$surv_date) # convert to date
hhs$start_time <- lubridate::dmy_hm(hhs$start_time) # convert to date time


hhs <- hhs %>% 
  mutate(w_time18 = w_time18 * 60, w_time17 = w_time17 * 60) %>% # converting time from hours to mins with mutate 
  mutate(expenditure = rowSums(.[c("sch_cost", "live_cost", "vet_cost", "crop_cost", "health_cost", 
                                   "home_cost", "transp_cost", "herder_cost", "social_support", "boma_dam_fencing", "other")], na.rm = TRUE)) %>% 
  mutate(expend_edu_per_child = sch_cost/sch_child) %>% 
  mutate(cow_bef17 = replace(cow_bef17, cow_bef17 == -99, NA)) %>% 
  mutate(cow_tlu = cow_bef17*0.71) %>% #based on Grandin 1988
  mutate(sheep_bef17 = replace(sheep_bef17, sheep_bef17 == -99, NA)) %>% 
  mutate(sheep_tlu = sheep_bef17*0.17) %>% #based on Grandin 1988
  mutate(goat_bef17 = replace(goat_bef17, goat_bef17 == -99, NA)) %>% 
  mutate(goat_tlu = goat_bef17*0.17) %>% #based on Grandin 1988
  mutate(total_tlu = cow_tlu+sheep_tlu+goat_tlu) %>% 
  mutate(crop_acre = if_else(is.na(crop_acre), 0, crop_acre)) %>% #there were no -99s here 
  mutate(m_child = replace(m_child, m_child == -99, NA)) %>% # remove the one m_child who said they had 41 male children but only 4 female
  mutate(ppl_in_hh = m_child + f_child + m_adult + f_adult+1) %>% 
  mutate(tlu_per_person = total_tlu/ppl_in_hh) %>% 
  mutate(perc_child_in_edu = sch_child/ppl_in_hh) %>% # N.B. this does not give true indication as it includes adults 
  mutate(graze_cons = fct_recode(graz_hhcons, "1" = "2", "1" = "3","1" = "4", "0" = "1")) %>% # graze in cons area yes or no 
  mutate(hwc_cow_tlu = wild_conf_cow*0.71) %>% #NO -99 in data - tlu based on Grandin 1988)
  mutate(hwc_shoat_tlu = wild_conf_shoat*0.17) %>% #NO -99 in data - tlu based on Grandin 1988)
  mutate(hwc_total_tlu = hwc_cow_tlu + hwc_shoat_tlu) %>% 
  mutate(major_human = na_if(major_human, major_human == "none")) %>% 
  mutate(major_human = na_if(major_human, major_human == "non")) %>% 
  mutate(major_human = na_if(major_human, major_human == "NO ONE")) %>% 
  mutate(major_human = na_if(major_human, major_human == "there weren't")) %>% 
  mutate(major_human = na_if(major_human, major_human == "Ghjf")) %>% 
  mutate(hwc_human = str_detect(major_human, "")) %>% 
  mutate(act_aft17_1 = factor(act_aft17_1, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
                              labels=c("Cultivation", "Livestock and related enterprises", "Tourism related", "Own business", "Skilled or permanent employment",
                                       "Permanent Employment Magadi", "Short term employment (kibarua)", "Food aid", "Government Employment", "Cash remittances",
                                       "Eating wild fruit and meat", "Pension", "Wood harvesting", "Loans", "None", "Pastor"))) %>% 
  mutate(current_activity_1 = fct_collapse(act_aft17_1,
                                           "Cultivation" = c("Cultivation"),
                                           "Livestock" = c("Livestock and related enterprises"),
                                           "Tourism" = c("Tourism related"),
                                           "Employed" = c("Skilled or permanent employment", "Permanent Employment Magadi", "Government Employment", "Pastor"),
                                           "Dependent" = c("Food aid", "Cash remittances", "Eating wild fruit and meat"),
                                           "Other" = c("Own business", "Short term employment (kibarua)", "Pension", "Wood harvesting", "Loans"),
                                           "None" = c("None"))) %>% 
  mutate(act_aft17_2 = factor(act_aft17_2, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
                              labels=c("Cultivation", "Livestock and related enterprises", "Tourism related", "Own business", "Skilled or permanent employment",
                                       "Permanent Employment Magadi", "Short term employment (kibarua)", "Food aid", "Government Employment", "Cash remittances",
                                       "Eating wild fruit and meat", "Pension", "Wood harvesting", "Loans", "None", "Pastor"))) %>% 
  mutate(current_activity_2 = fct_collapse(act_aft17_2,
                                           "Cultivation" = c("Cultivation"),
                                           "Livestock" = c("Livestock and related enterprises"),
                                           "Tourism" = c("Tourism related"),
                                           "Employed" = c("Skilled or permanent employment", "Permanent Employment Magadi", "Government Employment", "Pastor"),
                                           "Dependent" = c("Food aid", "Cash remittances", "Eating wild fruit and meat"),
                                           "Other" = c("Own business", "Short term employment (kibarua)", "Pension", "Wood harvesting", "Loans"),
                                           "None" = c("None"))) %>% 
  mutate(act_aft17_3 = factor(act_aft17_3, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
                              labels=c("Cultivation", "Livestock and related enterprises", "Tourism related", "Own business", "Skilled or permanent employment",
                                       "Permanent Employment Magadi", "Short term employment (kibarua)", "Food aid", "Government Employment", "Cash remittances",
                                       "Eating wild fruit and meat", "Pension", "Wood harvesting", "Loans", "None", "Pastor"))) %>% 
  mutate(current_activity_3 = fct_collapse(act_aft17_3,
                                           "Cultivation" = c("Cultivation"),
                                           "Livestock" = c("Livestock and related enterprises"),
                                           "Tourism" = c("Tourism related"),
                                           "Employed" = c("Skilled or permanent employment", "Permanent Employment Magadi", "Government Employment", "Pastor"),
                                           "Dependent" = c("Food aid", "Cash remittances", "Eating wild fruit and meat"),
                                           "Other" = c("Own business", "Short term employment (kibarua)", "Pension", "Wood harvesting", "Loans"),
                                           "None" = c("None"))) %>% 
  mutate(lead_senior = str_detect(leadpos, "")) # logical for senior leadership position

hhs <- hhs %>% 
  mutate(tourism_all = (apply(hhs, 1, function(r) any(r == "Tourism")))) %>%  # function to check if are currently employed in Tourism
  convert(num(tourism_all))
hhs$tourism_all[is.na(hhs$tourism_all)]<-0


#######################################################################################################################
###### constructing wealth index #####
#######################################################################################################################

# 1. exploratory data analysis of variables to potentially use in PCA ######
hhs_pca_eda <- hhs %>% 
  select(ObjectID, int_phon, norm_phon, radio, torch, tv, sola, piki, car, lat, 
         roof, wall, cow_bef17, sheep_bef17, goat_bef17, crop_acre, w_now) %>% 
  mutate(int_phon = replace(int_phon, int_phon == -99, 0)) %>% 
  mutate(norm_phon = replace(norm_phon, norm_phon == -99, 0)) %>% 
  mutate(radio = replace(radio, radio == -99, 0)) %>% 
  mutate(torch = replace(torch, torch == -99, 0)) %>% 
  mutate(tv = replace(tv, tv == -99, 0)) %>% 
  mutate(sola = replace(sola, sola == -99, 0)) %>% 
  mutate(piki = replace(piki, piki == -99, 0)) %>% 
  mutate(car = replace(car, car == -99, 0)) %>% 
  mutate(lat = replace(lat, lat == -99, 0)) %>% 
  mutate(iron_roof = ifelse(roof == 1, 1, 0)) %>% 
  mutate(tile_roof = ifelse(roof == 2, 1, 0)) %>% 
  mutate(cement_brick_roof = ifelse(roof == 3, 1, 0)) %>% 
  mutate(grass_makuti_roof = ifelse(roof == 4, 1, 0)) %>% 
  mutate(mud_dung_plastic_roof = ifelse(roof == 5, 1, 0)) %>% 
  mutate(brick_cement_wall = ifelse(wall == 1, 1, 0)) %>%
  mutate(iron_wall = ifelse(wall == 2, 1, 0)) %>%
  #combined mud_brick and mud_stone as they were in low numbers and similar
  mutate(mud_stoneorbrick_wall = ifelse(wall %in% 3:4, 1, 0)) %>% 
  #combined wood_mud and Grass_makuti as grass was low numbers and similar
  mutate(wood_mud_grass_wall = ifelse(wall %in% 5:6, 1, 0)) %>% 
  mutate(cow_bef17 = if_else(is.na(cow_bef17), 0, cow_bef17)) %>% 
  mutate(cow_tlu = cow_bef17*0.71) %>% #based on Grandin 1988
  mutate(sheep_bef17 = if_else(is.na(sheep_bef17), 0, sheep_bef17)) %>% 
  mutate(sheep_tlu = sheep_bef17*0.17) %>% #based on Grandin 1988
  mutate(goat_bef17 = if_else(is.na(goat_bef17), 0, goat_bef17)) %>% 
  mutate(goat_tlu = goat_bef17*0.17) %>% #based on Grandin 1988
  mutate(total_tlu = cow_tlu+sheep_tlu+goat_tlu) %>% 
  mutate(crop_acre = if_else(is.na(crop_acre), 0, crop_acre)) %>% 
  mutate(public_tap_water = ifelse(w_now == 1, 1, 0)) %>% 
  mutate(private_tap_water = ifelse(w_now == 2, 1, 0)) %>%
  mutate(riverbed_water = ifelse(w_now == 3, 1, 0)) %>% 
  mutate(spring_water = ifelse(w_now == 4, 1, 0)) %>% 
  mutate(dam_water = ifelse(w_now == 5, 1, 0)) %>% 
  mutate(flowing_river_stream_water = ifelse(w_now == 6, 1, 0)) %>% 
  #combined spring with flowing river or stream as it was in low numbers and similar
  mutate(river_spring_water = spring_water + flowing_river_stream_water) %>% 
  mutate(rain_water = ifelse(w_now == 7, 1, 0)) %>%
  #combined rainwater with private tap
  mutate(private_tap_rain_water = private_tap_water + rain_water) %>% 
  mutate(tanker_water = ifelse(w_now == 8, 1, 0)) %>% 
  mutate(public_well_water = ifelse(w_now == 9, 1, 0)) %>%
  #combined tanker, public tap and public well 
  mutate(public_tap_tanker_well_water = tanker_water + public_tap_water + public_well_water) %>% 
  mutate(private_well_water = ifelse(w_now == 10, 1, 0)) %>% 
  mutate(bottled_water = ifelse(w_now == 11, 1, 0))

#change outliers to NA for total TLU and crop area
#hhs_pca_eda$total_tlu[hhs_pca_eda$total_tlu > quantile(hhs_pca_eda$total_tlu, 0.99, na.rm = T)] <- NA
#hhs_pca_eda$crop_acre[hhs_pca_eda$crop_acre > quantile(hhs_pca_eda$crop_acre, 0.99, na.rm = T)] <- NA

# 2. Select all the subset of variables to be used to construct the PCA
# this one contains all the ones possible including some continuous variables
hhs_pca_eda_subset_all <- hhs_pca_eda %>% #removed cement brick roof, private well and bottled water as they has 0 observations
  select(int_phon, norm_phon, radio, torch, tv, sola, piki, car, lat, crop_acre, iron_roof, 
         tile_roof, grass_makuti_roof, mud_dung_plastic_roof, brick_cement_wall, 
         iron_wall, mud_stoneorbrick_wall, wood_mud_grass_wall, total_tlu, public_tap_tanker_well_water, private_tap_rain_water, riverbed_water, dam_water, 
         river_spring_water)


### for all, checked all scaling and centering and both scaling and centering are needed - means it is a correlation matrix
hhs_pca_all <- prcomp(hhs_pca_eda_subset_all, center = TRUE, scale = TRUE)

# construct index of principal components
index_all = hhs_pca_all$x[,1]
nlab<-c(1,2,3,4,5)

# 5. append the index, and the wealth quintiles from all (with tlu and crop area) onto the full hhs dataframe
hhs <- hhs %>% 
  mutate(quintiles = as.factor(cut(index_all, breaks=5, labels=nlab))) %>% 
  mutate(wealth_pca = index_all)
#change outliers in expenditure to NA
#wealth_index_all$expenditure[wealth_index_all$expenditure > quantile(wealth_index_all$expenditure, 0.99, na.rm = T)] <- NA

#write_csv(hhs, "C:/Users/peada/Documents/PhD/Research/4_data/2_analysis/hhsurvey/1_raw/hhs_cleaned.csv")
saveRDS(hhs, file = "C:/Users/peada/Documents/PhD/Research/4_data/2_analysis/hhsurvey/1_raw/hhs_cleaned_test.rds")

#Categorise the household into pastoral only, diversified pastoral, agri only, diversified agri, wage earner, poor, Other

#glimpse(hhs)

########################################################################################################################################
#########################################################################################################################################
# can end data cleaning here
######################################################################################################################################### 
########################################################################################################################################
