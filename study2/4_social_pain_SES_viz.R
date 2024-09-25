# 3_social_pain_SES_analysis_S2.R

# install ggthemes package to design/style bar graphs
install.packages("plotrix")

# Load packages 
library(dplyr)
library(tidyverse)
library(effectsize)
library(ggplot2)
library(plotrix)

# Read in data
dat = read.csv("socialpain-ses-s2_clean_data.csv")

# 1. Bar graph for two-sample t-test on perceived pain
dat %>%
  group_by(ses_condition) %>%
  summarise(M = mean(perceivedPainQ), SE = std.error(perceivedPainQ), count = n()) -> mean_pain

ggplot(data = mean_pain, aes(x = ses_condition, y = M, ymin = M-SE, ymax= M+SE)) +
  geom_bar(stat = "identity", width = 0.5, fill="grey") +
  geom_errorbar(width = 0.05) +
  coord_cartesian(ylim = c(1,4)) +
  ylab("Mean Social Pain") +
  xlab("SES") +
  theme_minimal()

# 2. Bar graph for two-sample t-test on extension/accommodation
dat %>%
  group_by(ses_condition) %>%
  summarise(M = mean(extension_1, na.rm = TRUE), SE = std.error(extension_1, na.rm = TRUE)) -> mean_ext

ggplot(data = mean_ext, aes(x = ses_condition, y = M, ymin = M-SE, ymax= M+SE)) +
  geom_bar(stat = "identity", width = 0.5, fill = "grey") +
  geom_errorbar(width = 0.05) +
  coord_cartesian(ylim = c(0,7)) +
  ylab("Mean Extension Length (Days)") +
  xlab("Condition") +
  theme_minimal()   

# 3. Bar graphs for two-sample t-test on grade
dat %>%
  group_by(ses_condition) %>%
  summarise(M = mean(grade, na.rm = TRUE), SE = std.error(grade, na.rm = TRUE)) -> mean_grade

ggplot(data = mean_grade, aes(x = ses_condition, y = M, ymin = M-SE, ymax= M+SE)) +
  geom_bar(stat = "identity", width = 0.5, fill = "grey") +
  geom_errorbar(width = 0.05) +
  coord_cartesian(ylim = c(1,5)) +
  ylab("Mean Projected Grade") +
  xlab("Condition") +
  theme_minimal()  

