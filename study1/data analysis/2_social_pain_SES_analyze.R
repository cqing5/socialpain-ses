# 2_social_pain_SES_analyze.R

library(effectsize)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotrix)


dat = read.csv("social_pain_SES_clean_data.csv")

t.test(mean_pain ~ SES, paired = TRUE, data = dat)

dat %>%
  group_by(SES) %>%
  summarise(M = mean(mean_pain), SE = std.error(mean_pain, na.rm = TRUE)) -> mean_dat

ggplot(data = mean_dat, aes(x = SES, y = M, ymin = M-SE, max= M+SE)) +
  geom_bar(stat = "identity", width = 0.5, fill = "grey") +
  geom_col() +
  geom_errorbar(width = 0.2) +
  coord_cartesian(ylim = c(1,4)) +
  ylab("Mean Social Pain")
