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
      sample == "Mbokishi" ~ "Other northern conservancies",
      sample == "Enonkishu" ~ "Enonkishu",
      sample == "Lemek" ~ "Other northern conservancies",
      sample == "Ol Chorro" ~ "Other northern conservancies",
      sample == "Outside" ~ "No conservancy")) %>% 
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
                                                                      transparency, accountability, women_power, wild_perception, 
                                                                      activity_before1, activity_current1, wellbeing_before, wellbeing_after,
                                                                      agree_before, agree_now, total_before_tlu, total_now_tlu,
                                                                      culture, culture_future, livestock, livestock_future)) %>% 
  #mutate(women_power = fct_relevel(women_power, "Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"))
  mutate(skip_meal_after = fct_relevel(skip_meal_after, "NA", "Never", "Only a few days in the worst months", "Some days in every month", "Some days in every week"))


#######################################################################################################################
####### transparency
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, transparency) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(transparency = factor(transparency, 
                               levels = c("Very satisfied","Satisfied","Unsatisfied", "Very unsatisfied", "<i>Don't Know</i>"), 
                               labels=c("Very satisfied","Satisfied","Unsatisfied", "Very unsatisfied", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = transparency, fill = transparency)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700", "#DFDFDF"), 
                        #name="Legend Title",
                        breaks=c("Very satisfied","Satisfied","Unsatisfied", "Very unsatisfied", "<i>Don't Know</i>"),
                        labels=c("Very satisfied","Satisfied","Unsatisfied", "Very unsatisfied", "Don't Know")) +
  #  scale_fill_manual(values=c("#008b45","#6FAFCA","yellow3", "tan3", "#cd3700", "#DFDFDF" , "#999999"), 
  #                      #name="Legend Title",
  #                      breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"),
  #                      labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
  #                      labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer")) +
  labs(title = "Are you satisfied with the transparency of decision making?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.9))
ggsave(filename = here::here("images/paper", "transparency (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### accountability
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, accountability) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(accountability = factor(accountability, 
                                 levels = c("Very satisfied","Satisfied","Unsatisfied", "Very unsatisfied", "<i>Don't Know</i>"), 
                                 labels=c("Very satisfied","Satisfied","Unsatisfied", "Very unsatisfied", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = accountability, fill = accountability)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("Very satisfied","Satisfied","Unsatisfied", "Very unsatisfied", "<i>Don't Know</i>"),
                    labels=c("Very satisfied","Satisfied","Unsatisfied", "Very unsatisfied", "Don't Know")) +
  #  scale_fill_manual(values=c("#008b45","#6FAFCA","yellow3", "tan3", "#cd3700", "#DFDFDF" , "#999999"), 
  #                      #name="Legend Title",
  #                      breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"),
  #                      labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
  #                      labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer")) +
  labs(title = "Are you satisfied with the  level of accountability in decision making?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.9))
ggsave(filename = here::here("images/paper", "accountability (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### influence
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, influence) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  mutate(influence = factor(influence, 
                                 levels = c("A lot of influence","A little influence","No influence", "I do not want to answer", "<i>Don't Know</i>"), 
                                 labels=c("A lot of influence","A little influence","No influence", "Don't want to answer", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = influence, fill = influence)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "darkgrey", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("A lot of influence","A little influence","No influence", "Don't want to answer", "Don't Know"),
                    labels=c("A lot of influence","A little influence","No influence", "Don't want to answer", "Don't Know")) +
  #  scale_fill_manual(values=c("#008b45","#6FAFCA","yellow3", "tan3", "#cd3700", "#DFDFDF" , "#999999"), 
  #                      #name="Legend Title",
  #                      breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"),
  #                      labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
  #                      labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer")) +
  labs(title = "How much influence do you feel this household has in decision making in conservancy?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.85,0.8))
ggsave(filename = here::here("images/paper", "influence (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### authority
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, conserve_authority) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  mutate(conserve_authority = factor(conserve_authority, 
                            levels = c("All conservancy members","A small number of conservancy members", "Conservancy management company", "<i>Don't Know</i>"), 
                            labels=c("All conservancy members","A small number of conservancy members", "Conservancy management company", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = conserve_authority, fill = conserve_authority)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("All conservancy members","A small number of conservancy members", "Conservancy management company", "Don't Know"),
                    labels=c("All conservancy members","A small number of conservancy members", "Conservancy management company", "Don't Know")) +
 labs(title = "Who has the authority over this conservancy?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.75,0.8))
ggsave(filename = here::here("images/paper", "authority (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### informed about income
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, income_informed) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #mutate(conserve_authority = factor(conserve_authority, 
  #                                   levels = c("All conservancy members","A small number of conservancy members", "Conservancy management company", "<i>Don't Know</i>"), 
  #                                   labels=c("All conservancy members","A small number of conservancy members", "Conservancy management company", "Don't Know"))) %>% 
  na.omit() 
write.xlsx(a, here::here("images/paper", "sufficiently informed about income.xlsx"))


#######################################################################################################################
####### agreed with cons before/after
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, agree_before) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  mutate(agree_before = factor(agree_before, 
                                     levels = c("Yes", "No", "<i>Don't Know</i>"), 
                                    labels=c("Yes", "No", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = agree_before, fill = agree_before)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","tan3", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("Yes", "No", "Don't Know"),
                    labels=c("Yes", "No", "Don't Know")) +
  labs(title = "At the time that this conservancy was set up, did you personally agree with that decision?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.75,0.8))
ggsave(filename = here::here("images/paper", "agree with cons before (enonkishu vs other conservancies vs outside).png"))


b <- strat_design_srvyr_hhs %>% 
  group_by(stype, agree_now) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  mutate(agree_now = factor(agree_now, 
                               levels = c("Yes", "No", "<i>Don't Know</i>"), 
                               labels=c("Yes", "No", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(b, aes(x=stype, y=proportion, group = agree_now, fill = agree_now)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=b, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","tan3", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("Yes", "No", "Don't Know"),
                    labels=c("Yes", "No", "Don't Know")) +
  labs(title = "Do you agree with the decision to have this conservancy at the moment?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.85,0.8))
ggsave(filename = here::here("images/paper", "agree with cons now (enonkishu vs other conservancies vs outside).png"))


#######################################################################################################################
####### rules about settlement
######################################################################################################################

table <- strat_design_srvyr_hhs %>% 
  group_by(stype, settle_rules) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  na.omit() 
write.xlsx(table, here::here("images/paper", "settlement rules yes or no.xlsx"))

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, settle_rules_help) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(settle_rules_help = factor(settle_rules_help, 
                                    levels = c("Brought much help","Brought Help","Brought problems", "Made no difference", "<i>Don't Know</i>"), 
                                    labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = settle_rules_help, fill = settle_rules_help)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"),
                    labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know")) +
  #  scale_fill_manual(values=c("#008b45","#6FAFCA","yellow3", "tan3", "#cd3700", "#DFDFDF" , "#999999"), 
  #                      #name="Legend Title",
  #                      breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer"),
  #                      labels=c("0 – KES 50,000","KES 50,001 – KES 100,000","KES100,001 – KES 150,000", "KES 150,001 – KES 200,000", "KES 200,001 – KES 250,000", "KES 250,000+")) +
  #                      labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree", "<i>Don't Know</i>", "I do not want to answer")) +
  labs(title = "The conservancy rules about settlement are...", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.9))
ggsave(filename = here::here("images/paper", "settlement rules (enonkishu vs other conservancies vs outside).png"))


#######################################################################################################################
####### rules about grazing
######################################################################################################################

table <- strat_design_srvyr_hhs %>% 
  group_by(stype, graz_rules) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  na.omit() 
write.xlsx(table, here::here("images/paper", "grazing rules yes or no.xlsx"))

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, graz_rules_help) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(graz_rules_help = factor(graz_rules_help, 
                                    levels = c("Brought much help","Brought Help","Brought problems", "Made no difference", "<i>Don't Know</i>"), 
                                    labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = graz_rules_help, fill = graz_rules_help)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"),
                    labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know")) +
  labs(title = "The conservancy rules about livestock access to grazing are...", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.9))
ggsave(filename = here::here("images/paper", "grazing rules (enonkishu vs other conservancies vs outside).png"))


#######################################################################################################################
####### rules about water
######################################################################################################################

table <- strat_design_srvyr_hhs %>% 
  group_by(stype, water_rules) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  mutate(water_rules = factor(water_rules, 
                                   levels = c("Yes","No", "<i>Don't Know</i>"), 
                                   labels=c("Yes", "No", "Don't Know"))) %>% 
  
  na.omit() 
write.xlsx(table, here::here("images/paper", "water rules yes or no.xlsx"))

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, water_rules_help) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(water_rules_help = factor(water_rules_help, 
                                  levels = c("Brought much help","Brought Help","Brought problems", "Made no difference", "<i>Don't Know</i>"), 
                                  labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(table, aes(x=stype, y=proportion, group = water_rules, fill = water_rules)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=table, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB", "#cd3700", "#DFDFDF"), 
                   #name="Legend Title",
                  breaks=c("Yes", "No", "Don't Know"),
               labels=c("Yes", "No", "Don't Know")) +
  labs(title = "Does this conservancy mean there are restrictions about the use of water?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.9))
ggsave(filename = here::here("images/paper", "water rules (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### wildlife perceptions
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, wild_perception) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  mutate(wild_perception = factor(wild_perception, 
                                     levels = c("Strongly like", "Like", "Neutral", "Strongly dislike", "<i>Don't Know</i>"), 
                                     labels=c("Strongly like", "Like", "Neutral", "Strongly dislike", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = wild_perception, fill = wild_perception)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA", "darkgrey", "tan3", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("Strongly like", "Like", "Neutral", "Strongly dislike", "Don't Know"),
                    labels=c("Strongly like", "Like", "Neutral", "Strongly dislike", "Don't Know")) +
  labs(title = "How do you feel about the wildlife living here?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.85,0.85))
ggsave(filename = here::here("images/paper", "wildlife perceptions (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### women and power
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, women_power) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  mutate(women_power = factor(women_power, 
                                  levels = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly disagree","I do not want to answer", "<i>Don't Know</i>"), 
                                  labels=c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly disagree","Don't want to answer", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = women_power, fill = women_power)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA", "tan3", "#cd3700",  "darkgrey", "#DFDFDF"), 
                    #name="Legend Title",
                    breaks=c("Agree", "Neutral", "Disagree", "Strongly disagree","Don't want to answer", "Don't Know"),
                    labels=c("Agree", "Neutral", "Disagree", "Strongly disagree","Don't want to answer", "Don't Know")) +
  labs(title = "Women have the power to influence decisions in this conservancy", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.85,0.85))
ggsave(filename = here::here("images/paper", "women and power (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### activity before/after
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  group_by(stype, activity_before1) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  #mutate(activity_before1 = factor(activity_before1, 
  #                                levels = c("Brought much help","Brought Help","Brought problems", "Made no difference", "<i>Don't Know</i>"), 
  #                                labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = activity_before1, fill = activity_before1)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  #scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700", "#DFDFDF"), 
  #                  #name="Legend Title",
  #                  breaks=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"),
  #                  labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know")) +
  labs(title = "Principal household activity before conservancy", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.9))
ggsave(filename = here::here("images/paper", "activity before (enonkishu vs other conservancies vs outside).png"))

b <- strat_design_srvyr_hhs %>% 
  group_by(stype, activity_current1) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  #mutate(activity_before1 = factor(activity_before1, 
  #                                levels = c("Brought much help","Brought Help","Brought problems", "Made no difference", "<i>Don't Know</i>"), 
  #                                labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(b, aes(x=stype, y=proportion, group = activity_current1, fill = activity_current1)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=b, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  #scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700", "#DFDFDF"), 
  #                  #name="Legend Title",
  #                  breaks=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"),
  #                  labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know")) +
  labs(title = "Principal household activity now", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.9))
ggsave(filename = here::here("images/paper", "activity now (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### TLU before/after
######################################################################################################################

a <- strat_design_srvyr_hhs %>% 
  select(stype, total_before_tlu) %>% 
  group_by(stype) %>% 
  summarise(proportion = survey_mean(total_before_tlu, vartype = "ci", na.rm=TRUE)) %>% 
  mutate(before_after = "Before") %>%
  na.omit() 
b <- strat_design_srvyr_hhs %>% 
  select(stype, total_now_tlu) %>% 
  group_by(stype) %>% 
  summarise(proportion = survey_mean(total_now_tlu, vartype = "ci", na.rm=TRUE)) %>% 
  mutate(before_after = "After") %>%
  na.omit()
c <- full_join(a, b) %>% 
  mutate(before_after = factor(before_after, 
                                levels = c("Before", "After"), 
                                  labels=c("Before", "After"))) 

ggplot(c, aes(x= stype, y = proportion, fill = before_after)) +
  geom_bar(aes(x= stype, y = proportion, fill = before_after), stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(x= stype, ymax = proportion_upp, ymin = proportion_low), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  geom_bar(aes(x= stype, y = proportion, fill = before_after), stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(aes(x= stype, ymax = proportion_upp, ymin = proportion_low), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  #scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700", "#DFDFDF"), 
  #                  #name="Legend Title",
  #                  breaks=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know"),
  #                  labels=c("Very helpful","Helpful","Problematic", "Very problematic", "Don't Know")) +
  labs(title = "Total tropical livestock units before and after conservancy", x="Conservancy", y = "Tropical Livestock Units") +
  #scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.9))
ggsave(filename = here::here("images/paper", "TLU before and after (enonkishu vs other conservancies vs outside).png"))


#######################################################################################################################
####### culture
######################################################################################################################
a <- strat_design_srvyr_hhs %>% 
  group_by(stype, culture) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(culture = factor(culture, 
                               levels = c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree"), 
                               labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = culture, fill = culture)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","#DFDFDF", "tan3", "#cd3700"), 
                    #name="Legend Title",
                    breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree"),
                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree")) +
 labs(title = "Do you believe that your culture and traditions are important?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.85))
ggsave(filename = here::here("images/paper", "culture (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### livestock pastoralism
######################################################################################################################
a <- strat_design_srvyr_hhs %>% 
  group_by(stype, livestock) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(livestock = factor(livestock, 
                          levels = c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree"), 
                          labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = livestock, fill = livestock)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","#DFDFDF", "tan3", "#cd3700"), 
                    #name="Legend Title",
                    breaks=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree"),
                    labels=c("Strongly agree","Agree","Neutral", "Disagree", "Strongly disagree")) +
  labs(title = "Do you believe that keeping livestock is important?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.88,0.85))
ggsave(filename = here::here("images/paper", "livestock importance (enonkishu vs other conservancies vs outside).png"))


#######################################################################################################################
####### culture future
######################################################################################################################
a <- strat_design_srvyr_hhs %>% 
  group_by(stype, culture_future) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(culture_future = factor(culture_future, 
                          levels = c("Very good", "Good", "Bad", "Very bad"), 
                          labels=c("Very good", "Good", "Bad", "Very bad"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = culture_future, fill = culture_future)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700"), 
                    #name="Legend Title",
                    breaks=c("Very good", "Good", "Bad", "Very bad"),
                    labels=c("Very good", "Good", "Bad", "Very bad")) +
  labs(title = "Do you feel good or bad about the future of your culture and traditions?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.85))
ggsave(filename = here::here("images/paper", "culture future (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### livestock patoralism future
######################################################################################################################
a <- strat_design_srvyr_hhs %>% 
  group_by(stype, livestock_future) %>% 
  summarise(proportion = survey_mean(vartype = "ci", na.rm=TRUE),
            total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n())) %>% 
  #filter(sample == "Enonkishu") %>% 
  mutate(livestock_future = factor(livestock_future, 
                                 levels = c("Very good", "Good", "Bad", "Very bad"), 
                                 labels=c("Very good", "Good", "Bad", "Very bad"))) %>% 
  na.omit() 
#write.xlsx(a, here::here("images", "skip_meal_after_all.xlsx"))

ggplot(a, aes(x=stype, y=proportion, group = livestock_future, fill = livestock_future)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=a, aes(ymax = ifelse(proportion_upp > 1, 1, proportion_upp), ymin = ifelse(proportion_low < 0, 0, proportion_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("#0047AB","#6FAFCA","tan3", "#cd3700"), 
                    #name="Legend Title",
                    breaks=c("Very good", "Good", "Bad", "Very bad"),
                    labels=c("Very good", "Good", "Bad", "Very bad")) +
  labs(title = "Do you feel good or bad about the future of keeping livestock?", x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.85))
ggsave(filename = here::here("images/paper", "livestock future (enonkishu vs other conservancies vs outside).png"))

#######################################################################################################################
####### Locations of surveys
######################################################################################################################

library(sf)
library(RColorBrewer)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
map <- hhs_wealth %>% 
  select(id, sample, `_129. Location (GPS)_latitude`, `_129. Location (GPS)_longitude`, `_129. Location (GPS)_precision`) %>% 
  rename(lat = `_129. Location (GPS)_latitude`, lon = `_129. Location (GPS)_longitude`, precision = `_129. Location (GPS)_precision`) %>% 
  filter(precision < 3000) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = projcrs)

mapview::mapview(map, 
                 zcol = "sample", 
                 #col.regions = "cornsilk2", 
                 alpha.regions = 1,
                 cex = 5,
                 lwd = 0.2,
                 col.regions=brewer.pal(5, "Set1"),
                 layer.name = "Conservancy title holder")

ggplot(data = map) +
  geom_sf(data = map, fill=alpha("red",0.2), show.legend = TRUE) +
  coord_sf(label_graticule = "SW", expand = FALSE) +
  ggtitle("Locations of sampled households")
