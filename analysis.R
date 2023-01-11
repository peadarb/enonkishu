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
####### import cleaned household survey data with wealth index
######################################################################################################################

rm(list=ls())

hhs_wealth <- readRDS("hhs_cleaned_wealth.rds")
head(hhs_wealth)


######################################################################################################################
############# Survey based household material and mobility ########
######################################################################################################################

strat_design_srvyr_house <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, roof, wall, mobility, sample)) 
#add weights=~pw to include weights which are      total in strata/number sampled in strata
strat_design_srvyr_house

each_conserve_roof <- strat_design_srvyr_house %>% 
  group_by(sample, roof) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

each_conserve_wall <- strat_design_srvyr_house %>% 
  group_by(sample, wall) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

each_conserve_mobility <- strat_design_srvyr_house %>% 
  #mutate(mobility = factor(mobility, levels = c(1,2,3), 
  #                      labels=c("None", "Partial", "Whole"))) %>%  
  group_by(sample, mobility) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(each_conserve_mobility, aes(x=sample, y=proportion, group = mobility, fill = mobility)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=each_conserve_mobility, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#D091BB", "#BBD4A6", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("None", "Livestock only move", "I do not want to answer"),
                    labels=c("None", "Livestock only move", "I do not want to answer")) +
  labs(title="Level of household mobility",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.62,0.9))
ggsave(filename = here::here("images", "level of mobility.png"))

######################################################################################################################
############# Survey based household education level reached  ############
######################################################################################################################

strat_design_srvyr_edu <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, edu))
#add weights=~pw to include weights which are      total in strata/number sampled in strata
strat_design_srvyr_edu

each_conserve_edu <- strat_design_srvyr_edu %>% 
  group_by(sample, edu) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(each_conserve_edu, aes(x=sample, y=proportion, group = edu, fill = edu)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=each_conserve_edu, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#F5B7B1", "#D091BB", "#A9CCE3", "#BBD4A6", "#FAD7A0", "#2E86C1", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("None", "Adult literacy classes (Gumbaru)", "Primary", "Secondary", "Diploma", "Degree", "I do not want to answer"),
                    labels=c("None", "Adult literacy classes (Gumbaru)", "Primary", "Secondary", "Diploma", "Degree", "I do not want to answer")) +
  labs(title="Level of education completed",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.67,0.8))
ggsave(filename = here::here("images", "level of education completed.png"))

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
  labs(title="Own a smart phone",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.5,0.8))
ggsave(filename = here::here("images", "smartphone ownership.png"))

######################################################################################################################
############# Survey based gender of land title holder  ##############
######################################################################################################################

strat_design_srvyr_gender <- hhs_wealth %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, gender, sample)) 
#add weights=~pw to include weights which are      total in strata/number sampled in strata

####for each asset ####
each_conserve_gender <- strat_design_srvyr_gender %>% 
  group_by(sample, gender) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(each_conserve_gender, aes(x=sample, y=proportion, group = gender, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=each_conserve_gender, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#D091BB", "#BBD4A6"), 
                    #name="Legend Title",
                    breaks=c("Female", "Male"),
                    labels=c("Female", "Male")) +
  labs(title="Gender of land title holder", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.75,0.96))
ggsave(filename = here::here("images", "Gender of land title holder.png"))


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
  na.omit()

ggplot(a_before, aes(x=sample, y=proportion, group = agree_before, fill = agree_before)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a_before, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#D091BB", "#BBD4A6", "#DFDFDF", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("No", "Yes", "<i>Don't Know</i>", "NA"),
                    labels=c("No", "Yes", "<i>Don't Know</i>", "NA")) +
  labs(title="cons agree before (NA omitted)",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.6))
ggsave(filename = here::here("images", "agreed_with_cons_before.png"))

a_now <- strat_design_srvyr_hhs %>% 
  mutate(agree_now = factor(agree_now, levels = c("No", "Yes", "<i>Don't Know</i>", NA), labels=c("No", "Yes", "Don't Know"))) %>% 
  group_by(sample, agree_now) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
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
  labs(title="cons agree now (NA omitted)",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.6))
ggsave(filename = here::here("images", "agreed_with_cons_now.png"))

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
                                                                      transparency, accountability, women_power, wild_perception))

a <- strat_design_srvyr_hhs %>% 
  group_by(sample, wild_perception) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  na.omit() 

ggplot(a, aes(x=sample, y=proportion, group = wild_perception, fill = wild_perception)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
#  scale_fill_manual(values=c("#008b45","#6FAFCA","yellow3", "tan3", "#cd3700", "#DFDFDF"), 
#                    #name="Legend Title",
#                    breaks=c("1","2","3", "4", "5", "6"),
#                   labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
#                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer")) +
  labs(title = "feel about the wildlife living here", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.4,0.9))
ggsave(filename = here::here("images", "feel about the wildlife living here.png"))

############################################################################################################################################
############################################################################################################################################
# model of cons support vs wealth
############################################################################################################################################
############################################################################################################################################

############################
# packages 
############################
library(tidyverse)
library(survey)
library(srvyr)
library(foreign)
library(hexbin)
library(KernSmooth)
library(quantreg)
#library(modelr)
############################################################################################################################################
############################################################################################################################################

########       examples of how to use modelr
# sim1_mod <- lm(y~x, data = sim1)
# grid <- sim1 %>% 
#   data_grid(x)
# grid <- grid %>% 
#   add_predictions(sim1_mod) #if it's for more than one prediction then need to use gather_predictions or spread_predictions which adds new predictions to a new column
# sim1 <- sim1 %>% 
#   add_residuals(sim1_mod)
# ggplot(sim1, aes(x)) +
#   geom_point(aes(y=y)) +
#   geom_line(aes(y=pred), data = grid)

#use model_matrix to see exactly what the model is doing

####################################################################################################################
#############################      set survey design and subset   #######################################
####################################################################################################################

############################
# survey design
############################
cons_ms <- hhs %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw) %>% 
  select(id, total_tlu, wealth_pca, crop_acre, ppl_in_hh, cons_yn1, cons_yn2, loc_name, subloc_name, age, gen, gr_mem, m_child, f_child, m_adult, f_adult, edu, subhh, mobil, 
         graz_hhcons, crop, crop_acre, cropuse, act_aft17_1, wild_perce, wild_damage, hhold_lead, leadpos, tlu_per_person, cons_money, quintiles, pw, dist_cons_area, 
         dist_markets, dist_magadi, tourism_all, lead_senior, current_activity_1, current_activity_2, current_activity_3, hwc_human, hwc_total_tlu, n_water:p_conserve,
         graze_cons, cow_tlu, wild_conf_croparea, cons_help09, cons_help17) %>% 
  mutate(wild_damage = fct_relevel(wild_damage, "0")) %>% 
  mutate(location = fct_recode(loc_name, olkeast = "1", olkwest = "2", shmpeast = "3", shmpwest = "4")) %>% 
  mutate(location = fct_collapse(location,
                                 "Olkiramatian East" = c("olkeast"),
                                 "Olkiramatian West" = c("olkwest"),
                                 "Shompole East" = c("shmpeast"),
                                 "Shompole West" = c("shmpwest"))) %>% 
  mutate(subloc_name = fct_recode(subloc_name, olk = "1", oldorko = "2", nguruman = "3", entasopia = "4", olchorrolepo = "5",
                                  oloika = "7", lenkobei = "8", endoinyolasho = "9", pakaase = "10", shompole ="11")) %>% 
  mutate(gen = fct_recode(gen, male = "1", female = "2")) %>% 
  mutate(hwc_human = as.numeric(hwc_human)) %>% 
  mutate(hwc_human = as.factor(hwc_human)) %>% 
  mutate(hwc_human = fct_relevel(hwc_human, "0")) %>% 
  mutate(tourism_all = as.numeric(tourism_all)) %>% 
  mutate(tourism_all = as.factor(tourism_all)) %>% 
  mutate(tourism_all = fct_relevel(tourism_all, "0")) %>% 
  mutate(lead_senior = as.numeric(lead_senior)) %>% 
  mutate(lead_senior = as.factor(lead_senior)) %>% 
  mutate(lead_senior = fct_relevel(lead_senior, "0")) %>% 
  mutate(gr_mem = as.factor(gr_mem)) %>% 
  mutate(gr_mem = fct_relevel(gr_mem, "0")) %>% 
  mutate(mobil = as.factor(mobil)) %>% 
  mutate(mobil = fct_relevel(mobil, "1")) %>% 
  #mutate(wild_perce = fct_recode(wild_perce, "Strongly Dislike" = "1", "Dislike" = "2", "Neutral" = "3", "Like" = "4", "Strongly Like" = "5", "-99" = "-99")) %>% 
  mutate(wild_perce = fct_recode(wild_perce, Dislike = "1", Dislike = "2", Neutral = "3", Like = "4", Like = "5", "-99" = "-99")) %>% 
  #mutate(wild_perce = fct_relevel(wild_perce, "Like")) %>%   
  mutate(edu = fct_recode(edu, basic_or_none = "1", basic_or_none = "2", basic_or_none = "3", secondary = "4", advanced = "5", advanced = "6")) %>% 
  mutate(edu = fct_relevel(edu, "secondary")) %>% 
  mutate(activity_1 = fct_collapse(current_activity_1,
                                   "Cultivation" = c("Cultivation"),
                                   "Livestock" = c("Livestock"),
                                   "Other" = c("Tourism", "Employed", "Dependent", "None"))) %>% 
  mutate(activity_1 = fct_relevel(activity_1, "Other")) %>% 
  mutate(cons_money = fct_relevel(cons_money, "1")) %>% 
  mutate(cons_yn1 = as.numeric(cons_yn1)) %>% 
  mutate(cons_yn1 = as.factor(cons_yn1)) %>% 
  mutate(cons_yn1 = fct_relevel(cons_yn1, "1")) %>% 
  mutate(hwc_tlu_by_total_tlu = hwc_total_tlu/total_tlu) %>% 
  mutate(hwc_crop_by_crop_area = wild_conf_croparea/crop_acre)
cons_ms$variables$cons_yn1[cons_ms$variables$cons_yn1==-99]<-NA
cons_ms$variables$cons_yn2[cons_ms$variables$cons_yn2==-99]<-NA
cons_ms$variables$wild_perce[cons_ms$variables$wild_perce==-99]<-NA
cons_ms$variables$gr_mem[cons_ms$variables$gr_mem==-99]<-NA
cons_ms$variables$cons_money[cons_ms$variables$cons_money==-99]<-NA
cons_ms$variables$graze_cons[cons_ms$variables$graze_cons==-99]<-NA
cons_ms$variables$wild_conf_croparea[cons_ms$variables$wild_conf_croparea==-99]<-NA
cons_ms$variables$hwc_total_tlu[cons_ms$variables$hwc_total_tlu>30]<-NA
cons_ms$variables$total_tlu[cons_ms$variables$total_tlu>700]<-NA
cons_ms$variables$hwc_tlu_by_total_tlu[cons_ms$variables$hwc_tlu_by_total_tlu>2]<-NA
cons_ms$variables$cons_help09[cons_ms$variables$cons_help09==-99]<-NA
cons_ms$variables$cons_help17[cons_ms$variables$cons_help17==-99]<-NA

############################
# update
############################

cons_ms<-update(cons_ms, group_ranch = ifelse(loc_name %in% 3:4, "Shompole", "Olkiramatian"))

############################
# subset
############################
# to subset e'g male or female headed, or poorest richest, this needs to be done in the survey design
# also removing NAs from the analysis needs to be done in the subsetting 
cons_ms_poor <- subset(cons_ms, quintiles=="1")
a <- subset(survey design name , femalehead=="1" & !is.na(NA you want to remove))
svymean(~x, design = a)

##############################################################################################################################
#####     Check for outliers
##############################################################################################################################

############################
# multipanel cleveland dot plot
############################

############################
# look at exploratory data
############################
summary(cons_ms$variables)

cons_yn2.m <- cons_ms$variables %>% 
  select(wealth_pca, age, total_tlu, tlu_per_person, crop_acre, ppl_in_hh)#, dist_markets, dist_cons_area,dist_magadi) 
ggplot(stack(cons_yn2.m), aes(x = reorder(ind, values, FUN = mean), y = values)) + 
  geom_boxplot(notch = FALSE) #+
#theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#stat_summary(fun.y = "mean", geom = "point", size = 5, color = "red", shape = 3) +
#stat_summary(fun.y = "median", geom = "point", size = 5, color = "blue", shape = 3)

############################
# histograms for normality and zeros
############################
ggplot(cons_ms$variables) +
  geom_histogram(aes(x = hwc_total_tlu, weight = pw), binwidth = 1) + # add or remove weight = pw
  facet_wrap(~subloc_name) #, labeller = "label_both"

############################
#frequency plots for normality and zeros
############################
ggplot(cons_ms$variables, aes(x = hwc_total_tlu, weight = pw)) +
  geom_freqpoly(binwidth = 1) +
  facet_wrap(~group_ranch)
# if non normal or lots of zeros use a poisson or negative binomial shift, or a zero inflated GLM

############################
# 2d bins
############################
ggplot(cons_ms$variables) +
  #geom_point(aes(x = total_tlu, y = wealth_pca), alpha = 1 / 10)
  geom_bin2d(aes(x = dist_cons_area, y = hwc_tlu_by_total_tlu))

ggplot(cons_ms$variables) +
  #geom_point(aes(x = total_tlu, y = wealth_pca), alpha = 1 / 10)
  geom_point(aes(x = dist_cons_area, y = hwc_tlu_by_total_tlu))


############################
# density plots
############################
cons_ms$variables$wild_damage[cons_ms$variables$wild_damage==-99]<-NA
ggplot(data=subset(cons_ms$variables, !is.na(wild_damage)), aes(x = dist_cons_area, colour = wild_damage)) + 
  geom_density(adjust = 1.5, alpha = 0.7, size = 1.2) +
  #geom_line(stat = "density", alpha = 0.7, size = 1.5) +
  labs(x = "Distance to any conservation area (m)", y = "Density", title = "Kernel density estimates of the distance to conservation area and whether hh has experienced HWC in the last year") +
  #facet_wrap(~quintiles) +
  geom_rug(aes(x = dist_cons_area, y = 0), position = position_jitter(height = 0)) +
  guides(fill=guide_legend(title=NULL)) +
  scale_colour_manual(values=c("#008b45", "#cd3700"),
                      name="",
                      breaks=c("0", "1"),
                      labels=c("No HWC", "HWC")
  ) +
  theme_bw() +
  theme(legend.position = c(.95, .85),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"))

############################
# remove incorrect outliers
############################

# without a function:
# remove outliers; values that are less than the value at 1st percentile are 
# replaced by the value at 1st percentile, and values that are greater than 
# the value at 99th percentile are replaced by the value at 99th percentile.
#hhs2 <- hhs2 %>% 
#  filter(cow_bef09 < quantile(cow_bef09, 0.99, na.rm = TRUE)) %>% 
#  filter(cow_bef09 > quantile(cow_bef09, 0.01, na.rm = TRUE))

##############################################################################################################################
#####     MultiCollinearity among covariates? - very important in logistic regressions
##############################################################################################################################

# can use pairwise scatterplots comparing covariates, correlation coefficients or a PCA biplot for all covariates
# drop one of the covariates (based on knowledge or VIF)
# if collinearity is an issure, can  use each of the variables with the dependent variables seperately and only include
# the one with the highest explanatory power
# for continuous data:
library(GGally)
library(sjPlot)
ggpairs(cons_ms$variables, 
        upper = list(continuous = wrap("cor", method = "spearman")), #kendall
        #mapping = aes(color = cons_yn2), 
        columns = c("hwc_total_tlu", "total_tlu"))

# check how many in each edu category ... need to recategorise... no effect then after controling for wealth

a <- wild_damage_proportions <- cons_ms %>% #, hwc_human, hwc_total_tlu, hwc_total_cow, hwc_shoat_tlu, wild_conf_croparea, cons_yn2 
  select(group_ranch, wild_damage) %>% 
  #mutate(cons_money = as.numeric(cons_money)) %>% 
  mutate(wild_damage = factor(wild_damage, levels = c(0,1,-99), labels=c("No","Yes","Don't Know"))) %>% 
  group_by(group_ranch, wild_damage) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(data = subset(a, !is.na(wild_damage)), aes(x=group_ranch, y=proportion, group = wild_damage, fill = wild_damage)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width=0.95) +
  #######   to stack ########### geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(data=subset(a, !is.na(wild_damage)), aes(ymax = proportion_upp, ymin = proportion_low), position = position_dodge(preserve = "total", width = 0.95), width = 0.02) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#cd3700", "#008b45"), 
                    #name="Legend Title",
                    breaks=c("Yes", "No"),
                    labels=c("Yes", "No")) +
  labs(x="Group Ranch", y = "Proportion of Households") +
  scale_y_continuous(breaks=seq(0.0, 1, 0.2), limits=c(0, 1)) +
  theme_sjplot()

b <- hwc_human_proportions <- cons_ms %>%
  select(group_ranch, hwc_human) %>% 
  #mutate(cons_money = as.numeric(cons_money)) %>% 
  mutate(hwc_human = factor(hwc_human, levels = c(0,1,"NA"), labels=c("No","Yes","Don't Know"))) %>% 
  group_by(group_ranch, hwc_human) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(data = subset(b, !is.na(hwc_human)), aes(x=group_ranch, y=proportion, group = hwc_human, fill = hwc_human)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width=0.95) +
  #######   to stack ########### geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(data=subset(b, !is.na(hwc_human)), aes(ymax = proportion_upp, ymin = proportion_low), position = position_dodge(preserve = "total", width = 0.95), width = 0.02) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#cd3700", "#008b45"), 
                    #name="Legend Title",
                    breaks=c("Yes", "No"),
                    labels=c("Yes", "No")) +
  labs(title="Proportion of HHs who described an event where a person fron the HH\nwas injured or killed by wildlife in the last year",x="Group Ranch", y = "Proportion of Households") +
  #scale_y_continuous(breaks=seq(0.0, 1, 0.2), limits=c(0, 1)) +
  theme_minimal() 

c <- wildlife_perce_proportions <- cons_ms %>%
  select(group_ranch, wild_perce) %>% 
  #mutate(cons_money = as.numeric(cons_money)) %>% 
  group_by(group_ranch, wild_perce) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(data = subset(c, !is.na(wild_perce)), aes(x=group_ranch, y=proportion, group = wild_perce, fill = wild_perce)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width=0.95) +
  #######   to stack ########### geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(data=subset(c, !is.na(wild_perce)), aes(ymax = proportion_upp, ymin = proportion_low), position = position_dodge(preserve = "total", width = 0.95), width = 0.02) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#FF8F66", "#cd3700", "#DFDFDF", "#008b45","#ABEBC6"), 
                    #name="Legend Title",
                    breaks=c("Strongly Dislike", "Dislike", "Neutral", "Like", "Strongly Like"),
                    labels=c("Strongly Dislike", "Dislike", "Neutral", "Like", "Strongly Like")) +
  labs(x="Group Ranch", y = "Proportion of Households") +
  scale_y_continuous(breaks=seq(0.0, 1, 0.2), limits=c(0, 1)) +
  theme_sjplot() 
#+  theme(legend.position=c(0.98,0.5))
#+ geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = -1)

d <- cons_help_09 <- cons_ms %>%
  select(location, cons_help09) %>% 
  #mutate(cons_money = as.numeric(cons_money)) %>% 
  group_by(location, cons_help09) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(data = subset(d, !is.na(cons_help09)), aes(x=location, y=proportion, group = cons_help09, fill = cons_help09)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width=0.95) +
  #######   to stack ########### geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(data=subset(d, !is.na(cons_help09)), aes(ymax = proportion_upp, ymin = proportion_low), position = position_dodge(preserve = "total", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#cd3700", "#DFDFDF", "#008b45"), 
                    #name="Legend Title",
                    breaks=c("1", "2", "3"),
                    labels=c("Make things worse", "Make no difference", "Make things better")) +
  labs(title="Over the 2009 drought, did the conservation area...",x="Group Ranch", y = "Proportion of Households") +
  scale_y_continuous(breaks=seq(0.0, 1, 0.2), limits=c(0, 1)) +
  theme_minimal() +  theme(legend.position=c(0.9,0.9))
#+  theme(legend.position=c(0.98,0.5))
#+ geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = -1)

e <- cons_help_17 <- cons_ms %>%
  select(location, cons_help17) %>% 
  #mutate(cons_money = as.numeric(cons_money)) %>% 
  group_by(location, cons_help17) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))

ggplot(data = subset(e, !is.na(cons_help17)), aes(x=location, y=proportion, group = cons_help17, fill = cons_help17)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width=0.95) +
  #######   to stack ########### geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(data=subset(e, !is.na(cons_help17)), aes(ymax = proportion_upp, ymin = proportion_low), position = position_dodge(preserve = "total", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#cd3700", "#DFDFDF", "#008b45"), 
                    #name="Legend Title",
                    breaks=c("1", "2", "3"),
                    labels=c("Make things worse", "Make no difference", "Make things better")) +
  labs(title="Over the 2017 drought, did the conservation area...",x="Group Ranch", y = "Proportion of Households") +
  scale_y_continuous(breaks=seq(0.0, 1, 0.2), limits=c(0, 1)) +
  theme_minimal() +  theme(legend.position=c(0.9,0.9))
#+ geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = -1)
##############################################################################################################################
#####     Are there interactions? use conditioning plots to check (showing 3 variables)
##############################################################################################################################

library(lattice)
#remove the one value of >30 tlu

svycoplot(dist_cons_area~hwc_total_tlu|equal.count(wealth_pca), 
          style = "trans", # or "hexbin"
          design = cons_ms, #& crop_acre>0
          #design = subset(cons_ms, age>0 ), #& crop_acre>0
          xbins=20,
          strip=strip.custom(var.name="wealth"), 
          cex = 1.5, 
          type = c("p", "smooth"), 
          col.line = "red", lwd =2,#  this is by default a loess smoother
          xlab="hwc total tlu",ylab="dist_cons_area",
          rows =3,
          #columns >= rows, # keeps a close to square shape
          overlap = 0.5,
          #key = simpleKey(levels(nhanes$RIAGENDR), space = "right")
          #hexscale = "absolute" # absolute makes hex scales comparable between plots, otherwise each plot is scaled seperately
          basecol=function(d) 
            ifelse(
              d$wild_perce=="Like","#21830E", 
              ifelse(
                d$wild_perce=="Dislike","#D0189B",
                "#444444")), # green is like
)


svycoplot(hwc_tlu_by_total_tlu~dist_cons_area|equal.count(wealth_pca), 
          style = "trans", # or "hexbin"
          design = cons_ms, #& crop_acre>0
          #design = subset(cons_ms, age>0 ), #& crop_acre>0
          xbins=20,
          strip=strip.custom(var.name="Wealth Index"), 
          cex = 1.1, 
          type = c("p", "smooth"), 
          col.line = "red", lwd =1,#  this is by default a loess smoother
          xlab="Distance to any conservation area (m)",ylab="TLU killed by wildlife\ndivided by total TLU",
          rows = 1,
          #columns >= rows, # keeps a close to square shape
          overlap = 0.5,
          #key = simpleKey(levels(nhanes$RIAGENDR), space = "right")
          #hexscale = "absolute" # absolute makes hex scales comparable between plots, otherwise each plot is scaled seperately
          #basecol=function(d) 
          #  ifelse(
          #    d$group_ranch=="Olkiramatian","#21830E", #green is Olkiramatian
          #  ifelse(
          #    d$group_ranch=="Shompole","#D0189B", # red is shompole
          #    "#444444")), # green is like
)

ggplot(data = subset(cons_ms$variables, !is.na(wild_perce)), aes(x=dist_cons_area, y=hwc_tlu_by_total_tlu)) +
  geom_point(aes(colour=wild_perce), alpha = .7, shape=16, stroke = 3) +
  #geom_boxplot() +
  #facet_wrap(~group_ranch)+
  guides(fill=guide_legend(title=NULL)) +
  scale_colour_manual(values=c("#D0189B", "#444444", "#21830E"), 
                      #name="Legend Title",
                      #breaks=c("Like", "Dislike", "Neutral"),
                      #labels=c("Like", "Dislike", "Neutral")
  ) +
  geom_smooth(method = "loess") +
  #labs(title="Proportion of HHs who described an event where a person fron the HH\nwas injured or killed by wildlife in the last year",x="Group Ranch", y = "Proportion of Households") +
  #scale_y_continuous(limits=c(0, 1)) + #breaks=seq(0.0, 1, 0.2), 
  #scale_x_continuous(limits=c(0, 350)) + 
  geom_text(aes(label=ObjectID),hjust=0, vjust=0)+
  theme_minimal()

##############################################################################################################################
#####     Are observations of the response variable independent?
##############################################################################################################################

# there is an independence assumption in regression, therefore need to see if that is present
# are there clear spatial or temporal patterns?

##############################################################################################################################
#####     Spatial autocorrelation and heteroskedasticity 
##############################################################################################################################

# if there is spatial auto correlation, can use spatial lag, or spatial error models
# Spatial error: moran's I, Lagrange multiplier, Robust Lagrage multiplier
# Spatial lag: Lagrange multiplier, Robust Lagrage multiplier

# if heteroskedasticity then include heteroskedasticity robust standard errors
# From lumley 2010 p90 "heteroskedasticity-consistent standard errors sometimes used in model-based regression analysis are 
# almost identical to the design-based standard errors we will use [in survey design]"

##############################################################################################################################
#####     Chi squared and t tests
##############################################################################################################################
# x2 
a <- svytable(~cons_yn2 + quintiles, design = cons_ms) #can incluce statistic = "adjWald"
summary(a)


# t test - see if olk are wealthier than shompole
olk_w <- cons_ms$variables[which(cons_ms$variables$group_ranch=="Olkiramatian"),]
shmp_w <- cons_ms$variables[which(cons_ms$variables$group_ranch=="Shompole"),]
a <- svyttest(wealth_pca~group_ranch, design = cons_ms)
a <- t.test(olk_w$wealth_pca, shmp_w$wealth_pca)
summary(a)

##############################################################################################################################
##### explore how data for model interact
##############################################################################################################################

# based on advice of Lumley and Scott 2017 - do a quick plot of the data to look at how the variables interact with a line fitted 
# note that the design is subsetted in the plot command
par(mfrow = c(1, 1))
svyplot(hwc_tlu_by_total_tlu~dist_cons_area, design=cons_ms, style = "grayhex", xlab = "Distance to any conservation area (m)", ylab = "TLU killed by wildlife divided by total TLU",, legend = 0) + #style = grayhex, subsample, transparent, bubble - the bigger the greater the weight
  lines(svysmooth(hwc_tlu_by_total_tlu~dist_cons_area, design=cons_ms, method="locpoly"), lwd=2, lty=1) #svysmooth fits a weighted density estimate using the local linear smoother from the KernSmooth package, as described by Wand and Jones #### method="quantreg" for quantiles and method = "locpoly" is the default
svyboxplot(crop_acre~group_ranch, cons_ms, col="gray80", varwidth=TRUE, ylab="crop_acres", xlab="GR", ylim = c(0, 3))
svyhist(~crop_acre, cons_ms, main = "", col = "grey80", xlab="crop acre")
# NOTE that svysmooth requires KernSmooth packages. It fits a weighted density estimate using local linear smoother

svyplot(wealth_pca~total_tlu, design=cons_ms, style ="transparent", pch =19, alpha = c(0,0.5)) # alpha gives weight so th darker, the greater the weight
svyplot(wealth_pca~total_tlu, design=cons_ms, style ="subsample", sample.size = 1000)
svyplot(cons_yn2~age, design=cons_ms, style ="grayhex", legend = 0)

a25 <- svysmooth(hwc_tlu_by_total_tlu~dist_cons_area, design=cons_ms, method="quantreg", quantile=0.25, df=4) # quantreg are quantile smoothers 
a50 <- svysmooth(hwc_tlu_by_total_tlu~dist_cons_area,method="quantreg",design=cons_ms, quantile=0.5,df=4)
a75 <- svysmooth(hwc_tlu_by_total_tlu~dist_cons_area,method="quantreg",design=cons_ms, quantile=0.75,df=4)
#plot(wealth_pca~total_tlu, data=wealth_index_all, type="n", xlim=c(1,800))
svyplot(hwc_tlu_by_total_tlu~dist_cons_area, design=cons_ms, style ="transparent", pch =19,  ylab="TLU killed by wildlife divided by total TLU",xlab="Distance to any conservation area (m)") # alpha gives weight so th darker, the greater the weight
lines(a50,lwd=3); lines(a25, lwd=1); lines(a75, lwd=1) # need to remove agri households
legend("topright", legend=c("25%, 75%", "Median"), lwd = c(1,3), lty=c(1,1), bty="")

men <- svysmooth(cons_yn2~wealth_pca, design=subset(cons_ms, gen=="male"), bandwidth = 10) # this one subsets it just for men
women <- svysmooth(cons_yn2~wealth_pca, design=subset(cons_ms, gen=="female"), bandwidth = 10) # this one subsets it just for men
plot(men, ylab="cons_yn2",xlab="wealth")
lines(women,lty=2)
legend("topleft",lty=1:2,bty="n",legend=c("Men","Women"))

##############################################################################################################################
#####     Make a base GLM with all available data that makes sense
##############################################################################################################################
# this is the paper to quote: Lumley and Scott (2014) https://onlinelibrary.wiley.com/doi/epdf/10.1111/anzs.12065 

# in regressions, you can use relevel() to change which level is first, and therefore the reference level
library(jtools)
library(huxtable)
library(latexpdf)
library(ggstance)
library(broom.mixed)

ggbivariate(cons_ms$variables, outcome = "cons_yn2", explanatory = c("wealth_pca", "age", "hwc_human", "lead_senior", "wild_perce"))

# With this, there are several things that show up as having a significant effect until youcontrol for herd size, then see below
m1 <- svyglm(hwc_total_tlu~wealth_pca+age+gen+dist_cons_area+wild_perce+lead_senior+graze_cons+dist_markets+dist_magadi+mobil+edu, design = cons_ms, na.action = na.omit)
#nothing really, only "other" employment, 0.01 wealth, and -0.04 graze cons1 and wild percent_rank
# this 
m2 <- svyglm(I(hwc_tlu_by_total_tlu*10)~wealth_pca+age+gen+dist_cons_area+wild_perce+lead_senior+graze_cons+dist_markets+dist_magadi+mobil+edu, design = cons_ms, na.action = na.omit)
m3 <- svyglm(wild_conf_croparea~wealth_pca+age+gen+dist_cons_area+wild_perce+lead_senior+graze_cons+dist_markets+dist_magadi+mobil+edu, design = cons_ms, na.action = na.omit)
m4 <- svyglm(wild_damage~wealth_pca+age+gen+dist_cons_area+wild_perce+lead_senior+graze_cons+dist_markets+dist_magadi+mobil+edu, design = cons_ms, na.action = na.omit, family = quasibinomial())

summ(m1) # from jtools

############################
# adding splines
############################
#if you notice that the regression might need splines, you can use (here new terms for age (RIDAGEYR) with spines from 0-50, 50-65, 65-90 and in units of decades (10):
cons<-update(stratdesign, age1=pmin(RIDAGEYR,50)/10, age2=pmin(pmax(RIDAGEYR,50),65)/10, age3=pmin(pmax(RIDAGEYR,65),90)/10)
# include age1, age2, age3, in the svyglm
ish0s <- svyglm(ish~age1+age2+age3, design=des,family=quasibinomial)	

# to read this, remember that a one unit difference in pmin is a comparison between two ages, that differ by one year, and are both below 50
# likewise, a one unit different in pmax is a comparison between two ages, that differ by one unit (one year), and are both above 65
# when interpreting this, remember the pmin(age, 50) is for the reference sex term, that is (male), so it is the slope for men at ages up to 50

##############################################################################################################################
#####     Deciding on best GLM (binomial)
##############################################################################################################################

############################
# confidence interval
############################
confint(m1)

############################
# Original coefficients
############################
# original coefficients represent "the change in the log of odds of change in dependent for a unit change in independent"

############################
# to change from the log for the odds rotio to the odds ratio
exp(coef(m3)) 
############################
# odds ratio shows that for a unit increase in the dependent variable, the odds of the independent variable change by the amount given 
# the marginal effects are the change in the outcome as a function of the changes in the treatment holding everything else constant
# so this shows that exp(of estimated coefficient) = 1.679 which means for a unit increase in wealth pca, the odds of agreeing with 
# conservation area then increases by 1.679 times. 
# if the odds ratio is very large, you can use the relative risk (Lumely p116)

# to relevel the references normally you can use levels(df$x) <- c("a", "b") etc in the order you want it in 
# if you want to set a different reference level to the first one (r default) then use relevel()
# df$x N- relevel(df$x, ref="reference of choice")
# to relevel in SURVEY based, you need to use: (n.b.) reference levels are often the best case scenarios
# update(survey_design, x= relevel(x, ref = "a"))

############################
# Adjusted Odds Ratio 
############################
# The adjusted odds ration adjusts for everything else in the model

############################
regTermTest()
############################
# A logistic regression is said to provide a better fit to the data if it demonstrates an improvement over a 
# model with fewer predictors. This occurs by comparing the likelihood of the data under the full model against 
# the likelihood of the data under a model with fewer predictors.

# usually you would use anova() for each of the models fitted (full and reduced) to compare models see http://search.r-project.org/library/survey/html/anova.svyglm.html 
# with anova you can now also build several models, and then use anova(model1) to examine if the model statistically different from the original model
# this will use the Rao-Scott LRT. The statistic tells you the statistic (the bigger the statistic, the bigger the effect), the
# DEff is the Design Effect, so the larger the number, the noiser the data

# The regTermTest() performs LRT(likelihood ratio test)-like or Wald based model (Working likelihood ratio Rao-Scott) comparisons tests on objects from the svyglm()
# It tests the significance of groups of coefficents representing a single factor.
# It can also test whether all coefficients for terms of a certain order, or the terms associated with
# a particular variable, are different from 0.
# if for example you add on a new term with lots of factors, then it can be very hard to read. IF we are not interested in estimating these trends separately, it is worth
# testing whether there is any other sign or evidence of a difference. This can be done with regTermTest()
# the regTermTest() will test the hypothesis that all of the added variables are 0. If the test fails to reject the null hypothesis (p>0.05), this suggests that removing the variable from the 
# model will not substantially harm the fit of that model p=0.09 therefore cannot reject. 
# If the test fails to reject the null hypothesis (p>0.05), this suggests that removing the variable from the
# model will not substantially harm the fit of that model.

regTermTest(m3s, 
            ~v1+v2+v3, # this is a combined test for the effects of v1 and v2 from a single factor
            df=NULL) 

############################
# AIC and BIC
############################
library(xtable)
AIC(m1); BIC(m1); #effects(m1s)
## create a table to compare multiple model AIC, BIC
xtable(round(cbind(dAIC=AIC(m1,m2, m3, m4)[,2], dBIC=BIC(m1,m2,m3, m4, maximal=m4)[,2])))

############################
# diagnostic plots
############################

#removes na values from the model (the - term in front of na.action, which lists the observations dropped
nonmissing <- cons_ms[-m1$na.action]
plot(m1, panel = make.panel.svysmooth(nonmissing))
# in term plot,  If the model is misspecified, the mean of the partial residuals will not follow the fitted relationship
# which can be detected from the smooth mean curve
# partial residuals can be computed from resid(model1, "partial")
termplot(m1, data = model.frame(nonmissing),
         partial = TRUE, se = TRUE, smooth = make.panel.svysmooth(nonmissing)) 

############################
# Homogeneity of variance
############################
# plot the residuals against the fitted values, and make conditional box plots of the residuals
# all variation in the residuals should be similar
# if there is heterogeneity, then transform the dependent variable, or use stats that don't require homogeneity (GLS)

# plot standardised or Peason residuals plotted against fitted values, for each covariate in the model, and agains 
# covariates not in the model, and against time and space. If temporal or spatial aspects, then use autocorrelation or variograms
# to assess the independence of the residuals
par(mfrow = c(1, 1)) # 2 row(s), 2 column(s)
plot(m1$residuals ~ m1$fitted.values)
#plot(p.resid ~ fit.val)
lines(ksmooth(y = m1$residuals, x = m1$fitted.values, bandwidth=.1, kernel="normal"), col="blue")
abline(h=0)
# fitted values vs Pearson residuals
# Include "model assumptions were verified by plotted residuals versus fitted values for covariates in the model and not in the 
# model. We also assessed the residuals for temporal and spatial dependency.

############################
# check for overfitting 
############################

# look at the delta value from cross-validation from the boot package
library(boot)
#'this give you the prediction error
cv.glm(cons_ms$variables[], m1, K = 13)$delta # can remove variables by cons_ms$variables[-c(504),] k is the number of chuncks used to split up the data
#this gives you the final fit. You want final fit to be lower than prediction error
# if prediction error for the final model is lower then model is NOT overfitted
cv.glm(cons_ms$variables[], update(m1, data = cons_ms$variables[]), K = 13)$delta # k is the number of chuncks used to split up the data


##############################################################################################################################
#####     Presenting results - seeing what they statistics are telling you
##############################################################################################################################

############################
# plot regression results
############################
library(sjPlot) # https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html 
library(sjlabelled)
library(sjmisc)

#set_label(cons_ms$variables$cons_yn2) <- "Did not Agree"
theme_set(theme_sjplot()) # set the clear theme
plot_models(m1, m2, m3, m4,
            transform = NULL, # transform = NULL means original coefficients and transform = "plogis" means probabilities
            #sort.est = TRUE, # sort them
            show.values = T, # shows values and whether significant
            #value.offset = -.1,
            #axis.labels = c("They Didn't Initially\nAgree to Conservation Area", "They Don't Feel Sufficiently Informed\nAbout Use of Conservation Money","Their Livestock Graze in\nConservation Area","Currently in Tourism\nor Related Employment", "Age", "Wealth Index"),
            axis.title = "",
            #title = "m3",
            #terms = c("Wealth Index" = "wealth_pca","Age" = "age", "Human HWC" = "hwc_human1", "Leader" = "lead_senior1", "Like Wildlife" = "wild_percelike", "Dislike Wildlife" = "wild_percedislike"),
            #colors = "Accent", #"gs" is grey scale
            #vline.color = "blue",
            #se = T,
            #itter = 1,
            legend.title = "",
            m.labels = c("hwc_total_tlu",   "hwc_tlu_by_total_tlu*10",   "wild_conf_croparea",     "wild_damage")) # the terms to keep if you want to remove some
#m.labels = c("Base Model", "Full Adjusted Model")) # the terms to keep if you want to remove some

############################
# table of regression results
############################
library(jtools)
library(huxtable)
library(latexpdf)
library(ggstance)
library(broom.mixed)

coef_names <- c("Wealth Index" = "wealth_pca", "Age" = "age", "They Didn't Initially\nAgree to Conservation Area" = "cons_yn10", "They Don't Feel Sufficiently Informed\nAbout Use of Conservation Money" = "cons_money0",
                "Their Livestock Graze in\nConservation Area" = "graze_cons1", "Currently in Tourism\nor Related Employment" = "tourism_all1", "Constant" = "(Intercept)")
export_summs(m1, m2, m3, m4,
             #scale = TRUE, # note the model is re-fitted with scaled variables (scale = TRUE).
             transform.response = T, # ransform.response = TRUE leaves outcome variables in their original scale
             #coefs = coef_names,
             AIC = "aic",
             #exp = T,
             model.names = c("Base Model", "Adjusted Model 1", "Adjusted Model 2", "Full Adjusted Model"))
#to.file = "xlsx",
#file.name = here::here("cons_yn2_model_coefficients.xlsx"))

############################
# plot regression summaries
############################

# shows the values of the coefficients and their corresponding uncertainties with plot_summs() 
plot_summs(m1, m2, m3, m4, 
           #ci_level = 0.95,
           #plot.distributions = T,
           #rescale.distributions = T,
           #scale = TRUE, 
           #robust = T, 
           coefs = coef_names[1:6]) # to leave out constant
# shows the values of the coefficients and their corresponding uncertainties
plot_coefs(m1, m2, m3,
           #ci_level = 0.95,
           #plot.distributions = T,
           #rescale.distributions = T,
           #scale = TRUE, 
           #robust = T, 
           coefs = coef_names[1:6])

############################
# coefficients with distributions
############################

# shows the uncertainty of your coefficients is via the plot.distributions argument.
plot_summs(m1, m2, m3, m4, 
           #scale = TRUE, 
           #robust = "HC3", 
           ci_level = 0.95,
           coefs = coef_names, 
           plot.distributions = TRUE,
           rescale.distributions = T)

############################
# Plotting model predictions 
############################

# look at the predictions it generates. 
# plot predictions across values of a predictor variable alongside the observed data.
effect_plot(m3, 
            pred = wealth_pca, 
            interval = TRUE, 
            plot.points = TRUE)

############################
# Plotting model partial residuals 
############################

# plot partial residuals instead of the raw observed data. 
# assess model quality after accounting for effects of control variables.
effect_plot(m3, 
            pred = wealth_pca, 
            interval = TRUE, 
            partial.residuals = TRUE)

############################
#show probability:  #1
############################
m1a = svyglm(formula = cons_yn2 ~ wealth_pca, design = cons_ms, 
             family = quasibinomial(), na.action = na.omit) 
ilink <- family(m1a)$linkinv
pds <- with(cons_ms$variables,
            data.frame(wealth_pca = seq(min(wealth_pca), max(wealth_pca),
                                        length = 100)))
pds <- cbind(pds, predict(m1a, pds, type = "link", se.fit = T)) ## need to specify just [1:2]) at the end for non survey
pds <- transform(pds, Fitted = ilink(link), Upper = ilink(link + (2 * SE)), #notation for non survey is se.fit and link would fit
                 Lower = ilink(link - (2 * SE)))
ggplot(cons_ms$variables, aes(x = wealth_pca, y = as.numeric(cons_yn2))) +
  geom_ribbon(data = pds, aes(ymin = Lower, ymax = Upper, x = wealth_pca),
              fill = "steelblue2", alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = pds, aes(y = Fitted, x = wealth_pca)) +
  ylim(0,1)+
  #geom_point(aes(y=cons_yn2), alpha = 0.5, shape=1, stroke = 1) + #can use it for checking
  labs(y = "Probability of agreeing with\nconservation area now", x = "Wealth Index")#+
theme_classic()

############################
#show probability:  #2
############################

# to see the probability of agreeing with cons 
# first remove NAs from the column you will use from the original data- these have been removed in the model and don't exist in fitted values
new_cons_yn2s <- na.omit(cons_ms$variables$cons_yn2)
# create data frame which combines the probabilities (fitted values) with the original values
predicted_data1s <- data.frame(probability_of_being_wealthy1=m1s$fitted.values, cons_yn2=new_cons_yn2s)
# sort probabilities from low to high
predicted_data1s <- predicted_data1s[order(predicted_data1s$probability_of_being_wealthy1, decreasing = FALSE),]
# add a new column which has the rank of each sample from low to high probability
predicted_data1s$rank <- 1:nrow(predicted_data1s)
# now plot this
ggplot(predicted_data1s, aes(x=rank, y=probability_of_being_wealthy1)) +
  geom_point(aes(colour=cons_yn2), alpha = 0.5, shape=1, stroke = 1) +
  xlab("Index") +
  ylab("Predicted probability of being wealthy")

# pdf("C:/Users/peada/Documents/PhD/Research/4_data/2_analysis/ch_6_conservation/images/regression_result.pdf", 9, 5)
# png("C:/Users/peada/Documents/PhD/Research/4_data/2_analysis/ch_6_conservation/images/regression_result.png", 490, 350)
# dev.off()
