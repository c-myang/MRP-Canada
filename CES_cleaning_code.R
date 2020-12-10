#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from CES
# Author: Cathy Yang
# Date: 9 December 2020
# Contact: cathym.yang@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
# devtools::install_github("hodgettsp/cesR")
# install.packages("labelled")
library(tidyverse)
library(cesR)
library(haven)
library(labelled)
setwd("/Users/cathyyang/OneDrive - University of Toronto/4th Year Drive/STA304/PS4/sta304-final")

# call 2019 CES online survey
get_ces("ces2019_web")

# convert values to factor type
ces2019_web <- to_factor(ces2019_web)
head(ces2019_web)

# Just keep some variables
reduced_CES_data <- 
  ces2019_web %>% 
  select(
    cps19_gender,
    cps19_age,
    cps19_province,
    cps19_education,
    cps19_religion,
    cps19_v_likely,
    cps19_votechoice)

# Make gender, age, and education groups
# Gender
reduced_CES_data$sex = case_when(
  reduced_CES_data$cps19_gender == "A woman" ~ "Female",
  reduced_CES_data$cps19_gender == "A man" ~ "Male",
  reduced_CES_data$cps19_gender == "Other (e.g. Trans, non-binary, two-spirit, gender-queer)" ~ "Other")

# Age
reduced_CES_data$age_group = case_when(
  reduced_CES_data$cps19_age >= 18 & reduced_CES_data$cps19_age <= 29 ~ "18 - 29",
  reduced_CES_data$cps19_age >= 30 & reduced_CES_data$cps19_age <= 44 ~ "30 - 44",
  reduced_CES_data$cps19_age >= 45 & reduced_CES_data$cps19_age <= 59 ~ "45 - 59",
  reduced_CES_data$cps19_age >= 60 ~ "60+")

reduced_CES_data$cps19_age_group <- as.factor(reduced_CES_data$cps19_age_group)

#Education
reduced_CES_data$education_group = case_when(
  reduced_CES_data$cps19_education == "No schooling" |
    reduced_CES_data$cps19_education == "Some elementary school" |
    reduced_CES_data$cps19_education == "Completed elementary school" |
    reduced_CES_data$cps19_education == "Some secondary/ high school" ~ "Less than high school",
  reduced_CES_data$cps19_education == "Completed secondary/ high school" |
    reduced_CES_data$cps19_education == "Other post high school vocational training" | 
    reduced_CES_data$cps19_education == "Some technical, community college, CEGEP, College Classique" ~ "High school graduate",
  reduced_CES_data$cps19_education == "Completed technical, community college, CEGEP, College Classique" |
    reduced_CES_data$cps19_education == "Some university" ~ "Some postsecondary education",
  reduced_CES_data$cps19_education == "Bachelor's degree" |
    reduced_CES_data$cps19_education == "Master's degree" |
    reduced_CES_data$cps19_education == "Professional degree or doctorate" ~ "University graduate")

reduced_CES_data$education_group <- as.factor(reduced_CES_data$education_group)

#Rename province
reduced_CES_data$province = reduced_CES_data$cps19_province

# Make binary vote intention columns for each party

reduced_CES_data<-
  reduced_CES_data %>%
  mutate(vote_Liberal = 
           ifelse(cps19_votechoice=="Liberal Party", 1, 0))

reduced_CES_data<-
  reduced_CES_data %>%
  mutate(vote_Conservative = 
           ifelse(cps19_votechoice=="Conservative Party", 1, 0))

reduced_CES_data<-
  reduced_CES_data %>%
  mutate(vote_NDP = 
           ifelse(cps19_votechoice=="ndp", 1, 0))

reduced_CES_data<-
  reduced_CES_data %>%
  mutate(vote_BQ = 
           ifelse(cps19_votechoice=="Bloc Québécois", 1, 0))

reduced_CES_data<-
  reduced_CES_data %>%
  mutate(vote_Green = 
           ifelse(cps19_votechoice=="Green Party", 1, 0))

reduced_CES_data<-
  reduced_CES_data %>%
  mutate(vote_Peoples = 
           ifelse(cps19_votechoice=="People's Party", 1, 0))

# Saving the survey/sample data as a csv file in my working directory
write_csv(reduced_CES_data, "CES_data.csv")

ggplot() + geom_bar(aes(x = reduced_CES_data$education_group))
ggplot() + geom_bar(aes(x = reduced_CES_data$age_group))
ggplot() + geom_bar(aes(x = reduced_CES_data$cps19_gender))
