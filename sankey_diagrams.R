#devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(lubridate)
library(scales)
library(tidyverse)
library(forcats)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
#ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)


######################################################################################################################
####### import cleaned household survey data with wealth index
######################################################################################################################

#rm(list=ls())

hhs_wealth <- readRDS("hhs_cleaned_wealth.rds")
head(hhs_wealth)


######################################################################################################################
############# Survey based changes over time ########
######################################################################################################################

strat_design_srvyr_changes <- hhs_wealth %>% 
  mutate(wellbeing_before = as.numeric(wellbeing_before)) %>% 
  mutate(wellbeing_before = as.factor(wellbeing_before)) %>% 
  mutate(wellbeing_after = as.numeric(wellbeing_after)) %>% 
  mutate(wellbeing_after = as.factor(wellbeing_after)) %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, activity_before1, activity_before2, 
                                                                      activity_before3, skip_meal_before, wellbeing_before, wellbeing_after, 
                                                                      activity_current1, activity_current2, activity_current3, skip_meal_after,
                                                                      cow_before, sheep_before, goat_before, donkey_before, 
                                                                      cow_now, sheep_now, goat_now, donkey_now,
                                                                      agree_before, agree_now, total_before_tlu, total_now_tlu))
 

wellbeing_before <- strat_design_srvyr_changes %>% 
  #mutate(mobility = factor(mobility, levels = c(1,2,3), 
  #                      labels=c("None", "Partial", "Whole"))) %>%  
  group_by(sample, wellbeing_before) %>% 
  summarise(proportion_before = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n_before= unweighted(n())) %>% 
  rename(wellbeing = wellbeing_before) %>% 
  #filter(sample == "Enonkishu") %>% 
  na.omit()

ggplot(wellbeing_before, aes(x=sample, y=proportion_before, group = wellbeing, fill = wellbeing)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=wellbeing_before, aes(ymax = ifelse(proportion_before_upp > 1, 1, proportion_before_upp), ymin = ifelse(proportion_before_low < 0, 0, proportion_before_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
#  scale_fill_manual(values=c("#D091BB", "#BBD4A6", "#DFDFDF"), 
#                    #name="Legend Title",
#                    breaks=c("None", "Livestock only move", "I do not want to answer"),
#                    labels=c("None", "Livestock only move", "I do not want to answer")) +
  labs(title="Wellbeing before",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.5))
ggsave(filename = here::here("images", "wellbeing before (all).png"))

wellbeing_after <- strat_design_srvyr_changes %>% 
  #mutate(mobility = factor(mobility, levels = c(1,2,3), 
  #                      labels=c("None", "Partial", "Whole"))) %>%  
  group_by(sample, wellbeing_after) %>% 
  summarise(proportion_after = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n= unweighted(n()))%>% 
  rename(wellbeing = wellbeing_after) %>% 
  filter(sample == "Enonkishu") %>% 
  na.omit()


ggplot(wellbeing_after, aes(x=sample, y=proportion_after, group = wellbeing, fill = wellbeing)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=wellbeing_after, aes(ymax = ifelse(proportion_after_upp > 1, 1, proportion_after_upp), ymin = ifelse(proportion_after_low < 0, 0, proportion_after_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  #  scale_fill_manual(values=c("#D091BB", "#BBD4A6", "#DFDFDF"), 
  #                    #name="Legend Title",
  #                    breaks=c("None", "Livestock only move", "I do not want to answer"),
  #                    labels=c("None", "Livestock only move", "I do not want to answer")) +
  labs(title="wellbeing_after",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.5))
ggsave(filename = here::here("images", "wellbeing after enonkishu.png"))

########################################################################################################################################################################################
  ########    wellbeing before after ####################################################################################################################################
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
  make_long(wellbeing_before, wellbeing_after) %>% 
  mutate(node = fct_relevel(node, "NA", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), 
         next_node = fct_relevel(next_node, "NA", "1", "2","3", "4", "5", "6", "7", "8", "9", "10"))

ggplot(sankey, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node), 
               label = node)) +
  geom_alluvial(flow.alpha = .6) +
  geom_alluvial_text(size = 3, color = "black") +
  scale_color_brewer(palette = "Blues", direction = -1) +
  #scale_fill_viridis_d(option = "magma", direction = -1) +
  #scale_fill_manual(values = c("NA" = "#999999", "10" = "#999999"))%>% 
  theme_alluvial(base_size = 18) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  #scale_color_brewer(palette = "Blues", direction = -1) +
  ggtitle("Comparison of the leaseholders wellbeing \nbefore and after joining the conservancy")
ggsave(filename = here::here("images", "wellbeing sankey.png"))

########################################################################################################################################################################################
########  skip meals before after   ################################################################################################################################
########################################################################################################################################################################################

sankey <- hhs_wealth %>% 
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

ggplot(sankey, aes(x = x, 
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
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Rate of Leaseholders skipping meals \nbefore and after joining the conservancies")
ggsave(filename = here::here("images", "Skip meals before after.png"))

strat_design_srvyr_changes <- hhs_wealth %>% 
  mutate(wellbeing_before = as.numeric(wellbeing_before)) %>% 
  mutate(wellbeing_before = as.factor(wellbeing_before)) %>% 
  mutate(wellbeing_after = as.numeric(wellbeing_after)) %>% 
  mutate(wellbeing_after = as.factor(wellbeing_after)) %>% 
  as_survey_design(1, strata=stype, fpc=fpc, weight=pw, variables = c(stype, fpc, pw, sample, activity_before1, activity_current1))


activity_current1 <- strat_design_srvyr_changes %>% 
  #mutate(mobility = factor(mobility, levels = c(1,2,3), 
  #                      labels=c("None", "Partial", "Whole"))) %>%  
  group_by(sample, activity_current1) %>% 
  summarise(proportion_before = survey_mean(vartype = "ci", na.rm=TRUE),
            #total = survey_total(vartype = "ci", na.rm=TRUE),
            n_before= unweighted(n())) %>% 
  rename(wellbeing = activity_current1) %>% 
  #filter(sample == "Enonkishu") %>% 
  na.omit()

ggplot(activity_current1, aes(x=sample, y=proportion_before, group = wellbeing, fill = wellbeing)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.95) +
  geom_errorbar(data=activity_current1, aes(ymax = ifelse(proportion_before_upp > 1, 1, proportion_before_upp), ymin = ifelse(proportion_before_low < 0, 0, proportion_before_low)), 
                position = position_dodge(preserve = "single", width = 0.95), width = 0.1) +
  guides(fill=guide_legend(title=NULL)) +
  #  scale_fill_manual(values=c("#D091BB", "#BBD4A6", "#DFDFDF"), 
  #                    #name="Legend Title",
  #                    breaks=c("None", "Livestock only move", "I do not want to answer"),
  #                    labels=c("None", "Livestock only move", "I do not want to answer")) +
  labs(title="activity_current1",x="Conservancy", y = "Proportion of Households") +
  scale_y_continuous(limits=c(0, 1)) +
  theme_sjplot() + 
  theme(legend.position=c(0.9,0.5))
ggsave(filename = here::here("images", "activity_current1 (all).png"))

########################################################################################################################################################################################
########  agree with cons before after   ################################################################################################################################
########################################################################################################################################################################################

sankey <- hhs_wealth %>% 
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
  ggtitle("Change in acceptance of conservancies \nbefore their establishment and now")

ggsave(filename = here::here("images", "Sankey agree with cons before after.png"))

#cowplot::save_plot("images/sankey agree before after.png", agree)
########################################################################################################################################################################################
########  total TLU before after   ################################################################################################################################
########################################################################################################################################################################################

sankey <- hhs_wealth %>% 
  select(stype, fpc, pw, sample, total_before_tlu, total_now_tlu)%>% 
  mutate(total_before_tlu_cat = cut(total_before_tlu, breaks=c(0, 25, 50, 75, 100, 125, 150, 175, 200, Inf), labels=c("0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200", "More than 200"))) %>% 
  mutate(total_before_tlu_cat = fct_explicit_na(total_before_tlu_cat, na_level = "NA")) %>%
  mutate(total_now_tlu_cat = cut(total_now_tlu, breaks=c(0, 25, 50, 75, 100, 125, 150, 175, 200, Inf), labels=c("0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200", "More than 200"))) %>% 
  mutate(total_now_tlu_cat = fct_explicit_na(total_now_tlu_cat, na_level = "NA")) %>%
  make_long(total_before_tlu_cat, total_now_tlu_cat) %>% 
  mutate(node = fct_relevel(node, "NA", "0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200", "More than 200"), 
         next_node = fct_relevel(next_node, "NA","0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200", "More than 200"))

ggplot(sankey, aes(x = x, 
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
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Livestock TLY before after")
ggsave(filename = here::here("images", "Sankey Livestock TLY before after.png"))


########################################################################################################################################################################################
########    improved wellbeing before after ####################################################################################################################################
########################################################################################################################################################################################

sankey_enon <- hhs_wealth %>% 
  filter(sample == "Enonkishu") %>% 
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
  make_long(wellbeing_before, wellbeing_after) %>% 
  mutate(node = fct_relevel(node, "NA", "4", "5", "6", "7", "8", "9", "10"), 
         next_node = fct_relevel(next_node, "8", "9", "10"))

sankey_enon <- sankey_enon
levels(sankey_enon$x) <- c("Before Joining Conservancy", "After Joining Conservancy")

ggplot(sankey_enon, aes(x = x, 
                   next_x = next_x, 
                   node = node, 
                   next_node = next_node,
                   fill = factor(node), 
                   label = node)) +
  geom_alluvial(flow.alpha = .6) +
  geom_alluvial_text(size = 3, color = "white") +
  #scale_fill_viridis_d(option = "magma", direction = -1) +
  #scale_fill_manual(values = c("NA" = "#999999", "10" = "#999999"))%>% 
  theme_alluvial(base_size = 18) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_viridis_d(option = "D", alpha = 0.95) + 
  theme(plot.title = element_text(size=10)) +
  ggtitle("1-10 score of life before leases were paid by the conservancy and now")
ggsave(filename = here::here("images", "wellbeing sankey enon.png"))
