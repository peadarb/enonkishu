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
####### import cleaned household survey data
######################################################################################################################

rm(list=ls())

hhs2 <- readRDS("hhs_cleaned.rds")
glimpse(hhs2)

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
biplot(hhs_pca_all, main = "All variables with centering and scaling")


# check out the loading scores for this data
loading_scores <- hhs_pca_all$rotation[,1]
w_score <- abs(loading_scores)
w_score_ranked <- sort(w_score, decreasing = TRUE) # sort them so we can just take out the most influention ones
top_10_scores <- names(w_score_ranked[1:10]) # show the most influential ones
top_10_scores
hhs_pca_all$rotation[top_10_scores,1] # show the associated scores for the most valuable ones



# 3. check for correlations between wealth indicators and explore data #########
library(corrr)
# using here() create a new folder in the Here() path location, called images
dir.create(here::here("images"))

cor_asset <- hhs_pca_eda_subset_all %>% 
  #replace(is.na(.),0) %>% 
  glimpse
cor_asset %>% 
  corrr::correlate() %>% 
  corrr::shave() %>% 
  corrr::fashion() %>% 
  readr::write_excel_csv(here::here("correlation_matrix.csv"))

cor_asset %>% 
  corrr::correlate() %>% 
  corrr::rearrange(method = "HC", absolute = FALSE) %>% 
  corrr:::shave() %>% 
  corrr::rplot(shape=19, colors = c("red", "green")) %>% 
  ggplot2::ggsave(
    filename = here::here("images", "correlation of assets.png"), 
    width = 20, 
    height = 5)

#boxplot of all the variables with red cross at mean 
plot <- hhs_pca_eda_subset_all %>% 
  select(-id)
ggplot(stack(plot), aes(x = reorder(ind, values, FUN = mean), y = values)) + 
  geom_boxplot(notch = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  stat_summary(fun.y = "mean", geom = "point", size = 5, color = "red", shape = 3) +
  stat_summary(fun.y = "median", geom = "point", size = 5, color = "blue", shape = 3)

#### for binary variables, checked all scaling and centering and only centering needed - means it is a covariance matrix 
hhs_pca_binary <- prcomp(plot, center = TRUE, scale = FALSE)
summary(hhs_pca_binary)
biplot(hhs_pca_binary, main = "Binary variables with centering and NO scaling")

cor_asset_binary <- plot %>% 
  #replace(is.na(.),0) %>% 
  glimpse
cor_asset_binary %>% 
  corrr::correlate() %>% 
  corrr::shave() %>% 
  corrr::fashion() %>% 
  readr::write_excel_csv(here::here("correlation_matrix_binary.csv"))

cor_asset_binary %>% 
  corrr::correlate() %>% 
  corrr::rearrange(method = "HC", absolute = FALSE) %>% 
  corrr:::shave() %>% 
  corrr::rplot(shape=19, colors = c("red", "green")) %>% 
  ggplot2::ggsave(
    filename = here::here("images", "correlation of assets_binary.png"), 
    width = 20, 
    height = 5)

#boxplot of all the variables with red cross at mean 
plot <- hhs_pca_eda_subset_all %>% 
  select(-id)
ggplot(stack(plot), aes(x = reorder(ind, values, FUN = mean), y = values)) + 
  geom_boxplot(notch = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  stat_summary(fun.y = "mean", geom = "point", size = 5, color = "red", shape = 3) +
  stat_summary(fun.y = "median", geom = "point", size = 5, color = "blue", shape = 3)
ggsave(filename = here::here("images", "wealth_index_boxplot.png"))
# from Vyas and Kumaranayake 2006 standard procedure is to select components where the associated eigenvalue is greater than 1
# only the first principal component is then used to measure wealth.

library(factoextra)
dev.off()
fviz_pca_var(hhs_pca_binary,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping
ggsave(filename = here::here("images", "PCA_viz_binary.png"))

# in this case we will drop non binary and non influential variables: crop_acre, more_conservancies, sheep_now_tlu, goat_now_tlu, cow_now_tlu, total_now_tlu

# 4. constructing PCA ######
# if the data have been standardised (e.g. all binary) then use a co-variance matrix for the PCA
# if the data have not been standardised (e.g. some quant data as well as binary), they use the correlation matrix

# construct index of principal components
index_all = hhs_pca_binary$x[,1]
nlab<-c(1,2,3,4,5)

# 5. append the index, and the wealth quintiles from all (with tlu and crop area) onto the full hhs dataframe
hhs_pca_eda_subset_all <- hhs_pca_eda_subset_all %>% 
  mutate(quintiles = as.factor(cut(index_all, breaks=5, labels=nlab))) %>% 
  mutate(wealth_pca = index_all) 

hhs_wealth <- full_join(hhs2, hhs_pca_eda_subset_all, by = "id")

#write_csv(hhs, "C:/Users/peada/Documents/PhD/Research/4_data/2_analysis/hhsurvey/1_raw/hhs_cleaned.csv")
saveRDS(hhs_wealth, "hhs_cleaned_wealth.rds")



########################################################################################################################
######## Test the wealth index by comparing to other variables
########################################################################################################################

# compare to payments from conservancies, land elsewhere, other?

# bar chart of the quintiles against the variable of choice
ggplot(hhs_wealth, aes(x=total_now_tlu)) + 
  geom_point(aes(fill = quintiles), position = "fill",width = 0.4) +
  xlab("TLU") +
  ylab("Percentage") +
  ggtitle("Wealth Breakdown with crop and tlu")

# scatterplot with a linear model best fit line
ggplot(hhs_wealth, aes(x=land_size_fct, y=wealth_pca)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab("crop_acre") +
  ylab("Wealth PCA") +
  ggtitle("perc_child_in_edu vs wealth PCA")