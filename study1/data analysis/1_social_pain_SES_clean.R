# 1_social_pain_SES_clean.R

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

dat = read.csv("SocialPain-SES_9-23-2021_OriginalData_NUMERIC.csv")
mcheck = read.csv("social_pain_SES_mani_check_DONE.csv", colClasses = "character")
mcheck$stim_id = as.numeric(mcheck$stim_id)
mcheck$MC_check[is.na(mcheck$MC_check)] <- 0
mcheck$MC_check[mcheck$MC_check == ""]  <- 0

# Eliminate first two rows of Qualtrics data
dat = dat[-c(1,2),]

# Remove participants with status = 1 (survey preview data)
dat = dat[dat$Status != 1,]

# Wide to long ----
dat %>%
  select(starts_with("w"), ResponseId, Gender, Age, Country, Country_6_TEXT, Race, Race_7_TEXT, Ethnicity, 
         Ethnicity_5_TEXT) %>%
  pivot_longer(cols = starts_with("w"), names_to = "trial_names", values_to = "values") %>%
  separate(trial_names, into = c("trial_names", "item"), sep = "_") -> ldat

ldat$item[is.na(ldat$item)] <- "MC"

# Create SES variable 
ldat %>%
  mutate(SES = case_when(
    grepl("H", trial_names) ~ "high",
    grepl("L", trial_names) ~ "low"
  )) -> ldat

# Create stimulus ID variable
ldat$stim_id = parse_number(ldat$trial_names)

ldat = ldat[ldat$values != "",]

ldat$SES_id = paste(ldat$SES, ldat$stim_id, sep = "_")

# Manipulation checks ----

# Combine hard coded manipulation checks
no_mc_dat = ldat[ldat$item != "MC",]
no_mc_dat$MC_check = "NA"

# Combine two data sets
dat = bind_rows(no_mc_dat, mcheck)
dat$MC_check = as.numeric(dat$MC_check)

dat[dat$item == "MC",] %>%
  group_by(ResponseId) %>%
  summarise(sumMC = sum(MC_check)) -> mc_per_subs

keep_subs = mc_per_subs$ResponseId[mc_per_subs$sumMC >= 14]

dat = dat[(dat$ResponseId %in% keep_subs),]

# Eliminate manipulation check data
dat = dat[dat$item != "MC",]
dat$values = as.numeric(dat$values)

# Aggregate ----
dat %>%
  group_by(ResponseId, SES, Gender, Age, Country, Race, Ethnicity) %>%
  summarise(mean_pain = mean(values, na.rm = TRUE)) -> sum_dat

# Write csv ----
filename = "social_pain_SES_clean_data.csv"
write.csv(sum_dat, filename, row.names = FALSE)

# # Manipulation check ----
# l1_corr = c("cook", "fast food cook", "Fast Food Cook")
# 
# MC_dat = ldat[ldat$item == "MC",]
# MC_dat = MC_dat[MC_dat$values != "",]
# MC_dat = arrange(MC_dat, stim_id)
# MC_dat$values = str_to_lower(str_squish(MC_dat$values))
# MC_dat = arrange(MC_dat, MC_dat$SES_id, MC_dat$values)

# # Write csv ----
# filename = "social_pain_SES_clean_data.csv"
# filename2 = "social_pain_SES_mani_check.csv"
# 
# write.csv(ldat, filename, row.names = FALSE)
# write.csv(MC_dat, filename2, row.names = FALSE)
