#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from CES
# Author: Cathy Yang
# Date: 9 December 2020
# Contact: cathym.yang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(cesR)
setwd("/Users/cathyyang/OneDrive - University of Toronto/4th Year Drive/STA304/PS4/sta304-final")

# Read in the raw data.