#devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(lubridate)
library(scales)
library(tidyverse)
library(forcats)
library(survey)
library(srvyr, warn.conflicts = FALSE)
library(sjPlot)
library(stringr)
#ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)


######################################################################################################################
####### import cleaned household survey data with wealth index
######################################################################################################################

#rm(list=ls())

hhs_wealth <- readRDS("hhs_cleaned_wealth.rds")
head(hhs_wealth)


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
  #filter(sample == "Enonkishu") %>% 
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
  ggtitle("Self-assessed Wellbeing Across all Conservancies") +
  ylim(0,10)

wellbeing_bar

#ggsave(filename = here::here("images", "wellbeing box all.png"))


cowplot::save_plot("images/wellbeing boxplot all.png", wellbeing_bar)

########################################################################################################################################################################################
########    skipping meals before after as graph ####################################################################################################################################
########################################################################################################################################################################################

sankey_meals <- hhs_wealth %>% 
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

meals <- sankey_meals %>%
  mutate(node = as.numeric(node)) 

meals_bar <- ggplot()+
  geom_boxplot(data = meals, aes(x = x, y = node, col = "red")) +
  scale_fill_brewer(palette="Dark2") +
  theme_alluvial(base_size = 18) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Rate of Leaseholders skipping meals \nbefore and after joining the conservancies") +
  ylim(0,10)

meals_bar

cowplot::save_plot("images/skipping meals boxplot.png", meals_bar)

### Trying another visualization

meals2 <- sankey_meals %>%
  mutate(node = as.factor(node)) 

meals_bar <- ggplot()+
  geom_bar(data = meals2, aes(x = x, fill = node, group = node), position = "dodge") +
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
ylim(0,100)

meals_bar

cowplot::save_plot("images/skipping meals bar.png", meals_bar)


########################################################################################################################################################################################
########    agree with cons before after as graph ####################################################################################################################################
########################################################################################################################################################################################

sankey_cons <- hhs_wealth %>% 
  select(stype, fpc, pw, sample, agree_before, agree_now)%>% 
  mutate(agree_before = fct_explicit_na(agree_before, na_level = "NA")) %>% 
  mutate(agree_now = fct_explicit_na(agree_now, na_level = "NA")) %>% 
  make_long(agree_before, agree_now) %>% 
  mutate(node = fct_relevel(node, "NA", "No", "Yes", "<i>Don't Know</i>"), 
         next_node = fct_relevel(next_node, "NA", "No", "Yes", "<i>Don't Know</i>"))

## As boxplot

cons <- sankey_cons %>%
  mutate(node = as.numeric(node)) 

cons_box <- ggplot()+
  geom_boxplot(data = cons, aes(x = x, y = node, col = "red")) +
  scale_fill_brewer(palette="Dark2") +
  theme_alluvial(base_size = 18) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Change in acceptance of conservancies \nbefore their establishment and now") +
  ylim(0,10)

cons_box

cowplot::save_plot("images/agree cons boxplot.png", cons_box)


## As bargraph

cons2 <- sankey_cons %>%
  mutate(node = as.factor(node)) 

cons_bar <- ggplot()+
  geom_bar(data = cons2, aes(x = x, fill = node, group = node), position = position_dodge()) +
  scale_fill_brewer(direction = -1) +
  theme_alluvial(base_size = 13) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .1),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Change in acceptance of conservancies \nbefore their establishment and now") +
  ylim(0,100)

cons_bar

cowplot::save_plot("images/agree cons bar.png", cons_bar)


########################################################################################################################################################################################
########  total TLU before after as graph  ################################################################################################################################
########################################################################################################################################################################################

sankey_TLU <- hhs_wealth %>% 
  select(stype, fpc, pw, sample, total_before_tlu, total_now_tlu)%>% 
  mutate(total_before_tlu_cat = cut(total_before_tlu, breaks=c(0, 25, 50, 75, 100, 125, 150, 175, 200, Inf), labels=c("0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200", "More than 200"))) %>% 
  mutate(total_before_tlu_cat = fct_explicit_na(total_before_tlu_cat, na_level = "NA")) %>%
  mutate(total_now_tlu_cat = cut(total_now_tlu, breaks=c(0, 25, 50, 75, 100, 125, 150, 175, 200, Inf), labels=c("0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200", "More than 200"))) %>% 
  mutate(total_now_tlu_cat = fct_explicit_na(total_now_tlu_cat, na_level = "NA")) %>%
  make_long(total_before_tlu_cat, total_now_tlu_cat) %>% 
  mutate(node = fct_relevel(node, "NA", "0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200", "More than 200"), 
         next_node = fct_relevel(next_node, "NA","0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200", "More than 200"))

## As boxplot

live_tlu <- sankey_TLU %>%
  mutate(node = as.numeric(node)) 

tlu_box <- ggplot()+
  geom_boxplot(data = live_tlu, aes(x = x, y = node, col = "red")) +
  scale_fill_brewer(palette="Dark2") +
  theme_alluvial(base_size = 18) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Livestock TLU before after") +
  ylim(0,10)

tlu_box

cowplot::save_plot("images/ livestock tlu boxplot.png", tlu_box)

## As bargraph

live_tlu2 <- sankey_TLU %>%
  mutate(node = as.factor(node)) 

tlu_bar <- ggplot()+
  geom_bar(data = live_tlu2, aes(x = x, fill = node, group = node), position = position_dodge()) +
  scale_color_discrete("dark2") +
  theme_alluvial(base_size = 13) +
  labs(xlab = NULL, ylab = NULL) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .1),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Livestock TLU before after") +
  ylim(0,100)

tlu_bar

cowplot::save_plot("images/Livestock tlu bar.png", tlu_bar)

#this may not be important to the stakeholders, no need to include