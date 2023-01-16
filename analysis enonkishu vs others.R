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
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

######################################################################################################################
####### import cleaned household survey data with wealth index
######################################################################################################################

rm(list=ls())

hhs_wealth <- readRDS("hhs_cleaned_wealth.rds")
head(hhs_wealth)

#######################################################################################################################
####### Survey based graph of proportion of HHS for enonkishu compared to rest together
######################################################################################################################

strat_design_srvyr_hhs <- hhs_wealth %>% 
  mutate(
    stype = case_when(
      sample == "Mbokishi" ~ "other",
      sample == "Enonkishu" ~ "enon",
      sample == "Lemek" ~ "other",
      sample == "Ol Chorro" ~ "other",
      sample == "Outside" ~ "out")) %>% 
  mutate(
    fpc = case_when(
      sample == "Mbokishi" ~ 361,
      sample == "Enonkishu" ~ 27,
      sample == "Lemek" ~ 361,
      sample == "Ol Chorro" ~ 361,
      sample == "Outside" ~ 26)) %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, skip_meal_before, skip_meal_after, 
                                                                      occupation, access_edu, access_health, access_elec, access_water,
                                                                      crop_yn, conserve_authority, graz_hhcons, graz_rules, graz_rules_help,
                                                                      settle_rules, settle_rules_help, forest_rules, forest_rules_help,
                                                                      water_rules, water_rules_help, wildlife_rules, wildlife_rules_help,
                                                                      receive_income, cons_payment_fct, income_informed, influence,
                                                                      transparency, accountability, women_power, wild_perception)) %>% 
  #mutate(women_power = fct_relevel(women_power, "Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"))
  mutate(skip_meal_after = fct_relevel(skip_meal_after, "NA", "Never", "Only a few days in the worst months", "Some days in every month", "Some days in every week"))

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, skip_meal_after) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = skip_meal_after, fill = skip_meal_after)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  #  scale_fill_manual(values=c("#008b45","#6FAFCA","yellow3", "tan3", "#cd3700", "#DFDFDF" , "#999999"), 
  #                      #name="Legend Title",
  #                      breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"),
  #                      labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
  #                      labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer")) +
  labs(title = "At the moment, how often does hh skip meals due to food shortage", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.8,0.8))
#ggsave(filename = here::here("images", "Skip meals now (enonkishu vs other conservancies vs outside).png"))
