# social_pain_SES_clean_data.R

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

dat = read.csv("SocialPain-SES_9-23-2021_OriginalData_NUMERIC.csv")
mcheck = read.csv("manipulation_check.csv")
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

# Manipulation check ----
l1_corr = c("cook", "fast food cook", "Fast Food Cook")

MC_dat = ldat[ldat$item == "MC",]
MC_dat = MC_dat[MC_dat$values != "",]
MC_dat = arrange(MC_dat, stim_id)
MC_dat$values = str_to_lower(str_squish(MC_dat$values))
MC_dat = arrange(MC_dat, MC_dat$SES_id, MC_dat$values)

# Write csv ----
filename = "social_pain_SES_clean_data.csv"
filename2 = "social_pain_SES_mani_check.csv"

write.csv(ldat, filename, row.names = FALSE)
write.csv(MC_dat, filename2, row.names = FALSE)
