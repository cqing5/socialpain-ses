# 3_social_pain_SES_analysis_S2.R

# Load packages 
library(effectsize)
library(Hmisc)
library(tidyverse)


# Read in data
dat = read.csv("socialpain-ses-s2_clean_data.csv")
names(dat)

# Creating a numeric SES variable 
dat$SES_low <-ifelse(dat$ses_condition=="low",1,
                     ifelse(dat$ses_condition=="high",0,NA))

# Bivariate Correlations 
corData_dr <- dat %>% 
  select("Life Hardship"=lifehardshipcom,
         "Warmth" = warmcom,
         "Competence" = compcom,
         "Pity" = emotion_1,
        # "SES (low)" = ses_condition,
         "SES_low" = SES_low,
         "Social Pain" = perceivedPainQ,
         "Accommodation" = extension_1,
         "Projected Grade" = grade, 
         
  )

# Full Clean
corData_dr[is.na(corData_dr)]<-NA
correlationResult <- Hmisc::rcorr(as.matrix(corData_dr),
                                  type = c("pearson"))

# Low SES 
low_dat = subset(corData_dr, corData_dr$SES_low == 1)
low_dat[is.na(low_dat)]<-NA
correlationResult <- Hmisc::rcorr(as.matrix(low_dat),
                                  type = c("pearson"))

# High SES 
high_dat = subset(corData_dr, corData_dr$SES_low == 0)
high_dat[is.na(high_dat)]<-NA
correlationResult <- Hmisc::rcorr(as.matrix(high_dat),
                                  type = c("pearson"))




# Two-sample t-test on perceived pain 
# No difference by condition 
t.test(perceivedPainQ ~ ses_condition, data=dat)
cohens_d(perceivedPainQ ~ ses_condition, data=dat)


# Two-sample t-test on academic extensions 
# Low SES Thomas given a longer extension 
t.test(extension_1 ~ ses_condition, data=dat)
cohens_d(extension_1 ~ ses_condition, data=dat)


# Two-sample t-test on academic extensions 
# A little bit more disruptive for low Thomas 
t.test(disruptive ~ ses_condition, data=dat)
cohens_d(disruptive ~ ses_condition, data=dat)

# Two-sample t-test on grade
# A = 5; F = 1
t.test(grade ~ ses_condition, data=dat)
cohens_d(grade ~ ses_condition, data=dat)


# Two-sample t-test on competence 
t.test(compcom ~ ses_condition, data=dat)
cohens_d(compcom ~ ses_condition, data=dat)

# Two-sample t-test on warmth
t.test(warmcom ~ ses_condition, data=dat)
cohens_d(warmcom ~ ses_condition, data=dat)


# # Two-sample t-test on perceived closeness 
t.test(perceivedClosenessQ ~ ses_condition, data=dat)


# Two-sample t-test on feelings of pity (emotion 1)
t.test(emotion_1 ~ ses_condition, data=dat)
cohens_d(emotion_1 ~ ses_condition, data=dat)

# Two-sample t-test on feelings of envy (emotion 2)
t.test(emotion_2 ~ ses_condition, data=dat)
cohens_d(emotion_2 ~ ses_condition, data=dat)

# Two-sample t-test on feelings of admiration (emotion 3)
t.test(emotion_3 ~ ses_condition, data=dat)
cohens_d(emotion_3 ~ ses_condition, data=dat)

# Two-sample t-test on feelings of contempt (emotion 4)
t.test(emotion_4 ~ ses_condition, data=dat)
cohens_d(emotion_4 ~ ses_condition, data=dat)

# Two-sample t-test on life hardship
t.test(lifehardshipcom ~ ses_condition, data=dat)
cohens_d(lifehardshipcom ~ ses_condition, data=dat)



# ----- Mediation Models (remember to source PROCESS script)

# Make a numeric SES Condition (process won't except non-numeric variables)
dat$SES_low <-ifelse(dat$ses_condition=="low",1,
                  ifelse(dat$ses_condition=="high",0,NA))


# PROCESS Mediation 1: SES condition --> Perceived Pity --> Perceived Social Pain  
process(data=dat,y="perceivedPainQ",x="SES_low",m="emotion_1",model=4,boot=10000,total=1,seed=17623)


# PROCESS Mediation 2: SES condition --> pain perception --> academic accommodation 
process(data=dat,y="extension_1",x="SES_low",m="perceivedPainQ",model=4,boot=10000,total=1,seed=17623)

# PROCESS Mediation 3: SES condition --> Perceived Pity--> academic accommodation 
process(data=dat,y="extension_1",x="SES_low",m="emotion_1",model=4,boot=10000,total=1,seed=17623)


# PROCESS Mediation 4: SES condition --> Competence --> Grade
process(data=dat,y="grade",x="SES_low",m="compcom",model=4,boot=10000,total=1,seed=17623)

