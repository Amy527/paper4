#### Preamble ####
# Purpose: Clean the survey data downloaded from Democracy Fund + UCLA Nationscape
# Author: Yinuo zhang
# Data: 10 April 2022
# Contact: Yinuo.zhang@utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!

#### Workspace setup ####
library(haven)
library(tidyverse)
library(labelled)
# Read in the raw data. 
raw_data2 <- read_dta("inputs/data/usa_00001.dta/usa_00001.dta") %>% 
  to_factor() %>% 
  select(-raced, -educ, -hispand)

#find education levels

educations <- names(table(raw_data2$educd)) 

##clean age into numeric format
# filter income
# only for age >= 18 voters
# do not consider multiple races
# convert education into few combined levels, three groups used here, too many 
# would make model results messy
# convert race into three levels, while, black and other
# convert age into 4 groups
# convert income into 2 groups
# keep hispanic but also convert into 2 levels: not hispanic, hispanic
#at last convert the groups into factor types and drop nas
raw_data2_cleaned <- raw_data2 %>% 
  mutate(age = as.numeric(as.character(age))) %>%
  filter(inctot >= 0) %>%
  filter(inctot < 9999999 ) %>%
  filter(age >= 18)  %>%
  filter(!race %in% c("two major races","three or more major races")) %>%
  mutate(
    education = 
           case_when(
            educd %in% educations[3:26] ~ "High school or Lower", 
            educd %in% educations[27:36] ~ "BA or lower", 
            educd %in% educations[37:43] ~ "Above BA", 
            educd %in% educations[c(1:2,44)] ~ "NA"
           )
           )%>%
  mutate(race2 = 
           case_when(
             race %in% "white" ~ "white", 
             race %in% "black/african american/negro" ~ "black", 
             !race %in% c("white", "black/african american/negro" ) ~ "other"
           )
  ) %>%
  mutate(hispanic = 
           case_when(
             hispan %in% "not hispanic" ~ "not hispanic", 
             ! hispan%in% "not hispanic" ~ "hispanic", 
           )
  ) %>%  mutate(
    incomegroup = case_when(
      inctot <= 36000 ~ "Median or below",
      inctot > 36000 ~ "Above median",
    )
  ) %>% 
  mutate(
    agegroup = case_when(
      age <= 35 ~ "18-35",
      age <= 50 ~ "36-49",
      age <= 65 ~ "50-65",
      age >= 65 ~ "65+"
    )
  ) %>% 
  mutate(
    incomegroup = factor(incomegroup),
    agegroup = factor(agegroup),
    education = factor(education),
    race = factor(race2),
    hispanic = factor(hispanic)
  ) %>% drop_na()



# save cleaned file into outputs 

write_rds(raw_data2_cleaned, "outputs/paper/data/post_cleaned.rds")



