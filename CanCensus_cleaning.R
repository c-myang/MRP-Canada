library(janitor)
library(tidyverse)
library(stringr)

setwd("/Users/cathyyang/OneDrive - University of Toronto/4th Year Drive/STA304/PS4/sta304-final")


# Load the data dictionary and the raw data 
raw_censusdata <- read_csv("rawcensus_data.csv") 

can_census <- read_csv("can_census/can_census.csv") 

###CAN CENSUS DATA###
reduced_can_census <- tibble(
  "GEO_ID" = can_census$`GEO_CODE (POR)`,
  "GEO_NAME" = can_census$GEO_NAME,
  "Variable" = can_census$`DIM: Profile of Federal Electoral Districts (2013 Representation Order) (2247)`,
  "Member ID" = can_census$`Member ID: Profile of Federal Electoral Districts (2013 Representation Order) (2247)`,
  "Count - Male" = can_census$`Dim: Sex (3): Member ID: [2]: Male`,
  "Count - Female" = can_census$`Dim: Sex (3): Member ID: [3]: Female`)

reduced_can_census$Variable <- as.factor(reduced_can_census$Variable)  

reduced_can_census$age_group = case_when(
  reduced_can_census$Variable == "20 to 24 years" |
    reduced_can_census$Variable == "25 to 29 years" ~ "18 - 29",
  reduced_can_census$Variable == "30 to 34 years" |
    reduced_can_census$Variable == "35 to 39 years" |
    reduced_can_census$Variable == "40 to 44 years" ~ "30 - 44",
  reduced_can_census$Variable == "45 to 49 years" |
    reduced_can_census$Variable == "50 to 54 years" |
    reduced_can_census$Variable == "55 to 59 years" ~ "45 - 59",
  reduced_can_census$Variable == "60 to 64 years" |
    reduced_can_census$Variable == "65 to 69 years" |
    reduced_can_census$Variable == "70 to 74 years" |
    reduced_can_census$Variable == "75 to 79 years" |
    reduced_can_census$Variable == "80 to 84 years" |
    reduced_can_census$Variable == "85 years and over" ~ "60+"
  )

reduced_can_census$sex = rep("Female", length(reduced_can_census$GEO_ID))
reduced_can_census$Sex = rep("Male", length(reduced_can_census$GEO_ID))

reduced_can_census$education_group = case_when(
  reduced_can_census$Variable == "No certificate, diploma or degree" ~ "Less than high school",
  reduced_can_census$Variable == "Secondary (high) school diploma or equivalency certificate" ~ "High school graduate",
  reduced_can_census$Variable == "Apprenticeship or trades certificate or diploma" |
    reduced_can_census$Variable == "College, CEGEP or other non-university certificate or diploma" |
    reduced_can_census$Variable == "University certificate or diploma below bachelor level" ~ "Some postsecondary education",
  reduced_can_census$Variable == "University certificate, diploma or degree at bachelor level or above" ~ "University graduate")

reduced_can_census <- reduced_can_census %>% filter(age_group != "NA" | education_group != "NA")

#Create separate tables to derive age and sex counts of each FED district
males <- reduced_can_census %>% 
  group_by(age_group, `Member ID`, GEO_NAME, GEO_ID) %>% 
  summarise(`Count - Male`) 

males$sex <- case_when(males$`Member ID` <= 38 ~ "Male")

females <- reduced_can_census %>% 
  group_by(age_group, `Member ID`, GEO_NAME, GEO_ID) %>% 
  summarise(`Count - Female`) 

females$sex <- case_when(females$`Member ID` <= 38 ~ "Female")

#Combine
demographics = bind_rows(males,females)

demographics <- demographics %>% unite("count", `Count - Male`:`Count - Female`, sep = " " ,na.rm = TRUE, remove = FALSE)

demographics$malecount = word(demographics$count, start = 1, sep = " ")
demographics$femalecount = word(demographics$count, start = 2, sep = " ")
demographics$malecount <- as.numeric(demographics$malecount)
demographics$femalecount <- as.numeric(demographics$femalecount)

demographics <- demographics %>% unite("n", malecount:femalecount, sep = " " ,na.rm = TRUE, remove = TRUE)
demographics$n <- as.numeric(demographics$n)
demographics <- demographics %>% filter(sex != "NA")

###EDUCATION###
raw_censusdata$rate_15Nodegree = raw_censusdata$`Males 15 and over: No certificate, diploma or degree`/sum(raw_censusdata$`15 to 19 years; Males`:raw_censusdata$`85 years and over; Males`)

raw_censusdata <- raw_censusdata %>% 
  mutate(rate_M15Nodegree = `Males 15 and over: No certificate, diploma or degree`/(`15 to 19 years; Males` +
           `20 to 24 years; Males` +
           `25 to 29 years; Males` +
           `30 to 34 years; Males` +
           `35 to 39 years; Males` +
           `40 to 44 years; Males` +
           `45 to 49 years; Males` +
           `50 to 54 years; Males` +
           `55 to 59 years; Males` +
           `60 to 64 years; Males` +
           `65 to 69 years; Males` +
           `70 to 74 years; Males` +
           `75 to 79 years; Males` +
           `80 to 84 years; Males` +                                                                    
           `85 years and over; Males`),
         rate_M15Secondary = `Males 15 and over: Secondary (high) school diploma or equivalency certificate`/(`15 to 19 years; Males` +
              `20 to 24 years; Males` +
              `25 to 29 years; Males` +
              `30 to 34 years; Males` +
              `35 to 39 years; Males` +
              `40 to 44 years; Males` +
              `45 to 49 years; Males` +
              `50 to 54 years; Males` +
              `55 to 59 years; Males` +
              `60 to 64 years; Males` +
              `65 to 69 years; Males` +
              `70 to 74 years; Males` +
              `75 to 79 years; Males` +
              `80 to 84 years; Males` +                                                                    
              `85 years and over; Males`),
         rate_M15Apprentice = `Males 15 and over: Apprenticeship or trades certificate or diploma`/(`15 to 19 years; Males` +
                                                                                     `20 to 24 years; Males` +
                                                                                     `25 to 29 years; Males` +
                                                                                     `30 to 34 years; Males` +
                                                                                     `35 to 39 years; Males` +
                                                                                     `40 to 44 years; Males` +
                                                                                     `45 to 49 years; Males` +
                                                                                     `50 to 54 years; Males` +
                                                                                     `55 to 59 years; Males` +
                                                                                     `60 to 64 years; Males` +
                                                                                     `65 to 69 years; Males` +
                                                                                     `70 to 74 years; Males` +
                                                                                     `75 to 79 years; Males` +
                                                                                     `80 to 84 years; Males` +                                                                    
                                                                                     `85 years and over; Males`),
         rate_M15College = `Males 15 and over: College, CEGEP or other non-university certificate or diploma`/(`15 to 19 years; Males` +
                                                                                     `20 to 24 years; Males` +
                                                                                     `25 to 29 years; Males` +
                                                                                     `30 to 34 years; Males` +
                                                                                     `35 to 39 years; Males` +
                                                                                     `40 to 44 years; Males` +
                                                                                     `45 to 49 years; Males` +
                                                                                     `50 to 54 years; Males` +
                                                                                     `55 to 59 years; Males` +
                                                                                     `60 to 64 years; Males` +
                                                                                     `65 to 69 years; Males` +
                                                                                     `70 to 74 years; Males` +
                                                                                     `75 to 79 years; Males` +
                                                                                     `80 to 84 years; Males` +                                                                    
                                                                                     `85 years and over; Males`),
         rate_M15belowBach = `Males 15 and over: University certificate or diploma below bachelor level`/(`15 to 19 years; Males` +
                                                                                     `20 to 24 years; Males` +
                                                                                     `25 to 29 years; Males` +
                                                                                     `30 to 34 years; Males` +
                                                                                     `35 to 39 years; Males` +
                                                                                     `40 to 44 years; Males` +
                                                                                     `45 to 49 years; Males` +
                                                                                     `50 to 54 years; Males` +
                                                                                     `55 to 59 years; Males` +
                                                                                     `60 to 64 years; Males` +
                                                                                     `65 to 69 years; Males` +
                                                                                     `70 to 74 years; Males` +
                                                                                     `75 to 79 years; Males` +
                                                                                     `80 to 84 years; Males` +                                                                    
                                                                                     `85 years and over; Males`),
         rate_M15UniGrad = `Males 15 and over: University certificate, diploma or degree at bachelor level or above`/(`15 to 19 years; Males` +
                                                                                     `20 to 24 years; Males` +
                                                                                     `25 to 29 years; Males` +
                                                                                     `30 to 34 years; Males` +
                                                                                     `35 to 39 years; Males` +
                                                                                     `40 to 44 years; Males` +
                                                                                     `45 to 49 years; Males` +
                                                                                     `50 to 54 years; Males` +
                                                                                     `55 to 59 years; Males` +
                                                                                     `60 to 64 years; Males` +
                                                                                     `65 to 69 years; Males` +
                                                                                     `70 to 74 years; Males` +
                                                                                     `75 to 79 years; Males` +
                                                                                     `80 to 84 years; Males` +                                                                    
                                                                                     `85 years and over; Males`),
         rate_F15Nodegree = `Females 15 and over: No certificate, diploma or degree`/(`15 to 19 years; Females` +
                                                                                     `20 to 24 years; Females` +
                                                                                     `25 to 29 years; Females` +
                                                                                     `30 to 34 years; Female` +
                                                                                     `35 to 39 years; Female` +
                                                                                     `40 to 44 years; Females` +
                                                                                     `45 to 49 years; Females` +
                                                                                     `50 to 54 years; Females` +
                                                                                     `55 to 59 years; Females` +
                                                                                     `60 to 64 years; Females` +
                                                                                     `65 to 69 years; Females` +
                                                                                     `70 to 74 years; Females` +
                                                                                     `75 to 79 years; Females` +
                                                                                     `80 to 84 years; Female` +                                                                    
                                                                                     `85 years and over; Females`),
         rate_F15Secondary = `Females 15 and over: Secondary (high) school diploma or equivalency certificate`/(`15 to 19 years; Females` +
                                                                                        `20 to 24 years; Females` +
                                                                                        `25 to 29 years; Females` +
                                                                                        `30 to 34 years; Female` +
                                                                                        `35 to 39 years; Female` +
                                                                                        `40 to 44 years; Females` +
                                                                                        `45 to 49 years; Females` +
                                                                                        `50 to 54 years; Females` +
                                                                                        `55 to 59 years; Females` +
                                                                                        `60 to 64 years; Females` +
                                                                                        `65 to 69 years; Females` +
                                                                                        `70 to 74 years; Females` +
                                                                                        `75 to 79 years; Females` +
                                                                                        `80 to 84 years; Female` +                                                                    
                                                                                        `85 years and over; Females`),
         rate_F15Apprentice = `Females 15 and over: Apprenticeship or trades certificate or diploma`/(`15 to 19 years; Females` +
                                                                                                      `20 to 24 years; Females` +
                                                                                                      `25 to 29 years; Females` +
                                                                                                      `30 to 34 years; Female` +
                                                                                                      `35 to 39 years; Female` +
                                                                                                      `40 to 44 years; Females` +
                                                                                                      `45 to 49 years; Females` +
                                                                                                      `50 to 54 years; Females` +
                                                                                                      `55 to 59 years; Females` +
                                                                                                      `60 to 64 years; Females` +
                                                                                                      `65 to 69 years; Females` +
                                                                                                      `70 to 74 years; Females` +
                                                                                                      `75 to 79 years; Females` +
                                                                                                      `80 to 84 years; Female` +                                                                    
                                                                                                      `85 years and over; Females`),
         rate_F15College = `Females 15 and over: College, CEGEP or other non-university certificate or diploma`/(`15 to 19 years; Females` +
                                                                                                                 `20 to 24 years; Females` +
                                                                                                                 `25 to 29 years; Females` +
                                                                                                                 `30 to 34 years; Female` +
                                                                                                                 `35 to 39 years; Female` +
                                                                                                                 `40 to 44 years; Females` +
                                                                                                                 `45 to 49 years; Females` +
                                                                                                                 `50 to 54 years; Females` +
                                                                                                                 `55 to 59 years; Females` +
                                                                                                                 `60 to 64 years; Females` +
                                                                                                                 `65 to 69 years; Females` +
                                                                                                                 `70 to 74 years; Females` +
                                                                                                                 `75 to 79 years; Females` +
                                                                                                                 `80 to 84 years; Female` +                                                                    
                                                                                                                 `85 years and over; Females`),
         rate_F15belowBach = `Females 15 and over: University certificate or diploma below bachelor level`/(`15 to 19 years; Females` +
                                                                                                            `20 to 24 years; Females` +
                                                                                                            `25 to 29 years; Females` +
                                                                                                            `30 to 34 years; Female` +
                                                                                                            `35 to 39 years; Female` +
                                                                                                            `40 to 44 years; Females` +
                                                                                                            `45 to 49 years; Females` +
                                                                                                            `50 to 54 years; Females` +
                                                                                                            `55 to 59 years; Females` +
                                                                                                            `60 to 64 years; Females` +
                                                                                                            `65 to 69 years; Females` +
                                                                                                            `70 to 74 years; Females` +
                                                                                                            `75 to 79 years; Females` +
                                                                                                            `80 to 84 years; Female` +                                                                    
                                                                                                            `85 years and over; Females`),
         rate_F15UniGrad = `Females 15 and over: University certificate, diploma or degree at bachelor level or above`/(`15 to 19 years; Females` +
                                                                                                                        `20 to 24 years; Females` +
                                                                                                                        `25 to 29 years; Females` +
                                                                                                                        `30 to 34 years; Female` +
                                                                                                                        `35 to 39 years; Female` +
                                                                                                                        `40 to 44 years; Females` +
                                                                                                                        `45 to 49 years; Females` +
                                                                                                                        `50 to 54 years; Females` +
                                                                                                                        `55 to 59 years; Females` +
                                                                                                                        `60 to 64 years; Females` +
                                                                                                                        `65 to 69 years; Females` +
                                                                                                                        `70 to 74 years; Females` +
                                                                                                                        `75 to 79 years; Females` +
                                                                                                                        `80 to 84 years; Female` +                                                                    
                                                                                                                        `85 years and over; Females`),
  )

### Collate to demographics ###
education_rates <- raw_censusdata %>% 
  select(`GEO UID`, rate_M15Nodegree, rate_M15Secondary, rate_M15Apprentice, 
         rate_M15College, rate_M15belowBach, rate_M15UniGrad, 
         rate_F15Nodegree, rate_F15Secondary, rate_F15Apprentice, rate_F15College, 
         rate_F15belowBach, rate_F15UniGrad)

demographics <- demographics %>% inner_join(by = c("GEO_ID" = "GEO UID") , education_rates)

test <- demographics %>%  
  mutate(count_M15Nodegree = n*rate_M15Nodegree,
         count_M15Secondary = n*rate_M15Secondary,
         count_M15Apprentice = n*rate_M15Apprentice,
         count_M15College = n*rate_M15College,
         count_M15belowBach = n*rate_M15belowBach,
         count_M15UniGrad = n*rate_M15UniGrad,
         count_F15Nodegree = n*rate_F15Nodegree,
         count_F15Secondary = n*rate_F15Secondary,
         count_F15Apprentice = n*rate_F15Apprentice,
         count_F15College = n*rate_F15College,
         count_F15belowBach = n*rate_F15belowBach,
         count_F15UniGrad = n*rate_F15UniGrad
         )

test <- test %>% 
  mutate(count_M_LessHS = count_M15Nodegree,
         count_M_HS = count_M15Secondary,
         count_M_Postsecondary = count_M15Apprentice + count_M15College + count_M15belowBach,
         count_M_UniGrad = count_M15UniGrad,
         count_F_LessHS = count_F15Nodegree,
         count_F_HS = count_F15Secondary,
         count_F_Postsecondary = count_F15Apprentice + count_F15College + count_F15belowBach,
         count_F_UniGrad = count_F15UniGrad)

test <- test %>% 
  mutate(count = NULL, `Count - Male` = NULL, `Count - Female` = NULL, 
         rate_M15Nodegree = NULL, rate_M15Secondary = NULL, rate_M15Apprentice = NULL, 
         rate_M15College = NULL, rate_M15belowBach = NULL, rate_M15UniGrad = NULL, 
         rate_F15Nodegree = NULL, rate_F15Secondary = NULL, rate_F15Apprentice = NULL, rate_F15College = NULL, 
         rate_F15belowBach = NULL, rate_F15UniGrad = NULL,
         count_M15Nodegree = NULL, count_M15Secondary = NULL, count_M15Apprentice = NULL,
         count_M15College = NULL, count_M15belowBach = NULL, count_M15UniGrad = NULL,
         count_F15Nodegree = NULL, count_F15Secondary = NULL, count_F15Apprentice = NULL,
         count_F15College = NULL, count_F15belowBach = NULL, count_F15UniGrad = NULL)

test <- test %>% mutate(count_M_LessHS = ifelse(sex == "Female", NA, count_M_LessHS),
                        count_M_HS = ifelse(sex == "Female", NA, count_M_HS),
                        count_M_Postsecondary = ifelse(sex == "Female", NA, count_M_Postsecondary),
                        count_M_UniGrad = ifelse(sex == "Female", NA, count_M_UniGrad),
                        count_F_LessHS = ifelse(sex == "Male", NA, count_F_LessHS),
                        count_F_HS = ifelse(sex == "Male", NA, count_F_HS),
                        count_F_Postsecondary = ifelse(sex == "Male", NA, count_F_Postsecondary),
                        count_F_UniGrad = ifelse(sex == "Male", NA, count_F_UniGrad))

#Duplicate columns for post-strat data
test$Sex = test$sex
test$Age_group = test$age_group
test$constituencyname = test$GEO_NAME
test$caseID = c(1:length(test$age_group))

poststrat <- test %>% pivot_longer(
  cols = count_M_LessHS:count_F_UniGrad,
  names_to = c(".discard", ".sex", ".education_group"),
  names_sep = "_",
  values_to = "count",
  values_drop_na = TRUE
)

poststrat$education_group = case_when(poststrat$.education_group == "LessHS" ~ "Less than high school",
                                      poststrat$.education_group == "HS" ~ "High school graduate",
                                      poststrat$.education_group == "Postsecondary" ~ "Some postsecondary education",
                                      poststrat$.education_group == "UniGrad" ~ "University graduate")

jointable <- tibble(constituencynumber = raw_censusdata$`GEO UID`,
                    province = raw_censusdata$`Province name`)
jointable <- jointable %>% distinct(constituencynumber, province)

poststrat <- poststrat %>% left_join(by = c("GEO_ID" = "constituencynumber") , jointable)
poststrat$caseID = c(1:length(poststrat$age_group))

poststrat <- poststrat %>% 
  select(caseID, GEO_ID, GEO_NAME, province, age_group, sex, education_group, count) %>% 
  rename(constituencynumber = GEO_ID)

#Edit province levels
poststrat$province = case_when(poststrat$province == "Yukon Territory" ~ "Yukon",
                               poststrat$province != "Yukon Territory" ~ poststrat$province)

write_csv(poststrat, "censusdata.csv")
