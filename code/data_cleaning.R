# data cleaning script
# prepared by Roland 
# dated: 05-26-2023
# Nursing home quality rating

# prep (libraries) ------

library(tidyverse)
library(janitor)
library(DataExplorer)
library(ggthemes)

# importing data -------
df1 <- read_csv("data/Provider_Info.csv")

# select required columns---
df2 <- df1 %>%
  select(`Federal Provider Number`, `Provider Name`, `Provider State`,
         `Ownership Type`, `Number of Certified Beds`,
         `Number of Residents in Certified Beds`, `Provider Type`, 
         `Date First Approved to Provide Medicare and Medicaid services`, 
         `With a Resident and Family Council`,
         `Automatic Sprinkler Systems in All Required Areas`, `Overall Rating`,
         `Health Inspection Rating`, `QM Rating`, `Staffing Rating`, 
         `RN Staffing Rating`,
         `Reported CNA Staffing Hours per Resident per Day`:`Reported Physical Therapist Staffing Hours per Resident Per Day`,
         `Cycle 1 Number of Health Revisits`:`Cycle 3 Total Health Score`,
         `Total Weighted Health Survey Score`:`Total Amount of Fines in Dollars`,
         `Total Number of Penalties`, `Processing Date`) %>%
  clean_names()

# further cleaning ---------
# cleaning date variables 

df2$date_first_approved_to_provide_medicare_and_medicaid_services <- as.Date(df2$date_first_approved_to_provide_medicare_and_medicaid_services, "%m/%d/%Y")
df2$cycle_2_standard_health_survey_date <- as.Date(df2$cycle_2_standard_health_survey_date, "%m/%d/%Y")
df2$cycle_3_standard_health_survey_date <- as.Date(df2$cycle_3_standard_health_survey_date, "%m/%d/%Y")
df2$processing_date <- as.Date(df2$processing_date, "%m/%d/%Y")

# extract ownership type variables
df3 <- df2 |>
  mutate(
    new_ownership = case_when(
      str_detect(ownership_type, "For profit")==TRUE ~ "For profit",
      str_detect(ownership_type, "Government")==TRUE ~ "Government",
      str_detect(ownership_type, "Non profit")==TRUE ~ "Non profit",
      TRUE~NA_character_
    )
  ) |>
  select(-ownership_type) |>
  rename(
    "date_first_approved" = date_first_approved_to_provide_medicare_and_medicaid_services,
    "automatic_sprinkler" = automatic_sprinkler_systems_in_all_required_areas
  )

# remove rows with missing observations and selecting hospital with >=3 overall rating
df4 <- df3 |>
  filter(!is.na(overall_rating) &
           provider_state != "GU" & provider_state != "PR")


