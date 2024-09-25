# 1_social_pain_SES_manicheck_S2.R

# Load packages 
library(dplyr)
library(tidyverse)
library(effectsize)

# Read in data
dat = read.csv("Social Pain and SES - Study 2_February 2, 2022_numeric.csv")

# Eliminate first two rows of Qualtrics data
dat = dat[-c(1,2),]

# Remove participants with status = 1 (survey preview data)
dat = dat[dat$Status != 1,]

# Remove participants that did not pass attention check 
dat = dat[dat$attnCheckQ == 1,]

# Aggregate ----
dat$ladderPlacement_1 <- as.numeric(dat$ladderPlacement_1)

dat_sum_mean <- dat %>%
  group_by(FL_20_DO) %>%
  summarise(across(ladderPlacement_1, 
                   mean, na.rm= TRUE))

dat_sum_sd <- dat %>%
  group_by(FL_20_DO) %>%
  summarise(across(ladderPlacement_1, 
                   sd, na.rm= TRUE))

range(dat$ladderPlacement_1[dat$FL_20_DO=="lowThomas"])
range(dat$ladderPlacement_1[dat$FL_20_DO=="highThomas"])
hist(dat$ladderPlacement_1[dat$FL_20_DO=="lowThomas"], main = "low SES condition")
hist(dat$ladderPlacement_1[dat$FL_20_DO=="highThomas"], main = "high SES condition")

# Effect Size 
cohens_d(ladderPlacement_1 ~ FL_20_DO, dat=dat)
hedges_g(ladderPlacement_1 ~ FL_20_DO, dat=dat)

