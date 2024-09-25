# 2_social_pain_SES_clean_S2.R

# Load packages 
library(dplyr)
library(tidyverse)
library(effectsize)

# Read in data
dat = read.csv("Social Pain and SES - Study 2_February 10, 2022_numeric.csv")


# Eliminate first two rows of Qualtrics data
dat = dat[-c(1,2),]

# Remove participants with status = 1 (survey preview data)
dat = dat[dat$Status != 1,]

# Remove participants that did not pass attention check 
dat = dat[dat$attnCheckQ == 1,]

# Remove those who did not complete the full survey (we can be less strict if desired)
# A little arbitrary here as we don't know at what point to deem someone as incomplete 
dat = dat[dat$Finished == 1,]

# Rename conditions 
dat <- rename(dat, ses_condition = FL_20_DO)

# Final sample size 
table(dat$ses_condition)


# Manicheck Aggregate ---- Overall manipulation seemed to work at group level
dat$ladderPlacement_1 <- as.numeric(dat$ladderPlacement_1)

dat_sum_mean <- dat %>%
  group_by(ses_condition ) %>%
  summarise(across(ladderPlacement_1, 
                   mean, na.rm= TRUE))


dat_sum_sd <- dat %>%
  group_by(ses_condition ) %>%
  summarise(across(ladderPlacement_1, 
                   sd, na.rm= TRUE))

hist(dat$ladderPlacement_1[dat$ses_condition =="lowThomas"], main = "low SES condition")
hist(dat$ladderPlacement_1[dat$ses_condition =="highThomas"], main = "high SES condition")


# Effect Size 
cohens_d(ladderPlacement_1 ~ ses_condition, dat=dat)
t.test(ladderPlacement_1 ~ ses_condition, dat=dat)

# Reverse Coding ----

# Recode life hardship items 
table(dat$lifeHardshipMatrix_1)
dat$lifeHardshipMatrix_1 = as.numeric(dplyr::recode(as.character(dat$lifeHardshipMatrix_1), 
                                      "1" = "4",
                                      "2" = "3",
                                      "3" = "2",
                                      "4" = "1", 
                                      ))
table(dat$lifeHardshipMatrix_1)

table(dat$lifeHardshipMatrix_3)
dat$lifeHardshipMatrix_3 = as.numeric(dplyr::recode(as.character(dat$lifeHardshipMatrix_3), 
                                                    "1" = "4",
                                                    "2" = "3",
                                                    "3" = "2",
                                                    "4" = "1", 
))
table(dat$lifeHardshipMatrix_3)

# Composites ----


# Life hardship matrix composite 
dat[, 26:29] = lapply(dat[, 26:29],as.numeric)
str(dat)
dat$lifehardshipcom <- dat %>%
  select(contains(c("lifeHardshipMatrix_1", "lifeHardshipMatrix_2", 
                    "lifeHardshipMatrix_3", "lifeHardshipMatrix_4"))) %>%
  rowMeans(na.rm = TRUE)
dat <- dat %>% relocate(lifehardshipcom, .after = lifeHardshipMatrix_4)


# Warmth composite (items 1 and 2)
dat[, 31:34] = lapply(dat[, 31:34],as.numeric)
str(dat)
dat$warmcom <- dat %>%
  select(contains(c("warmthComp_1", "warmthComp_2" 
                    ))) %>%
  rowMeans(na.rm = TRUE)
dat <- dat %>% relocate(warmcom, .after = warmthComp_2)


# competence composite (items 3 & 4)
dat$compcom <- dat %>%
  select(contains(c("warmthComp_3", "warmthComp_4" 
  ))) %>%
  rowMeans(na.rm = TRUE)
dat <- dat %>% relocate(compcom, .after = warmthComp_4)

# Recode grade values
table(dat$grade)
dat$grade = as.numeric(dplyr::recode(as.character(dat$grade), 
                                                    "1" = "5",
                                                    "2" = "4",
                                                    "3" = "3",
                                                    "4" = "2",
                                                    "5" = "1",
                                     
))
table(dat$grade)


# Write csv ----
filename = "socialpain-ses-s2_clean_data.csv"
write.csv(dat, filename, row.names = FALSE)
