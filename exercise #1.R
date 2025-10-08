library(tidyverse)   # includes readr, dplyr, ggplot2, etc.
library(tidyverse)   # includes readr, dplyr, ggplot2, etc.
install.packages("tidyverse")
# Load Synthea CSV files
patients   <- read_csv("../output/csv/patients.csv")
allergies  <- read_csv("../output/csv/allergies.csv")
encounters <- read_csv("../output/csv/encounters.csv")
# Show first rows
head(patients)
head(allergies)
head(encounters)

# Show structure (column names and types)
glimpse(patients)
glimpse(allergies)
glimpse(encounters)
#link patients with allergies
patient_allergies <- patients %>%
  inner_join(allergies, by = c("Id" = "PATIENT"))

head(patient_allergies)

patient_data <- patient_allergies %>%
  inner_join(encounters, by = c("Id" = "PATIENT"))

head(patient_data)
# How many patients?
nrow(patients)

# How many allergies?
nrow(allergies)

# Quick summary of patients table
summary(patients)

# Count patients by gender
patients %>%
  count(GENDER)

# Age distribution (average, min, max)
patients %>%
  summarize(
    avg_age = mean(AGE, na.rm = TRUE),
    min_age = min(AGE, na.rm = TRUE),
    max_age = max(AGE, na.rm = TRUE)
  )
# Top 10 most common allergies
allergies %>%
  count(DESCRIPTION, sort = TRUE) %>%
  head(10)

patients %>%
  inner_join(allergies, by = c("Id" = "PATIENT")) %>%
  count(GENDER, DESCRIPTION, sort = TRUE) %>%
  group_by(GENDER) %>%
  slice_max(n, n = 5)   # top 5 allergies per gender
# olderpatients have more encountter that younger ones
encounters %>%
  count(PATIENT) %>%
  inner_join(patients, by = c("PATIENT" = "Id")) %>%
  ggplot(aes(x = AGE, y = n)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Encounters vs. Age")
#which allergie group are associated with the highest number of encounters
allergies %>%
  inner_join(encounters, by = c("PATIENT" = "PATIENT")) %>%
  count(DESCRIPTION, PATIENT) %>%
  group_by(DESCRIPTION) %>%
  summarize(avg_encounters = mean(n)) %>%
  arrange(desc(avg_encounters)) %>%
  head(10)
allergies_unique <- allergies %>% distinct(PATIENT, DESCRIPTION)
patients %>%
  inner_join(allergies_unique, by = c("Id" = "PATIENT"))
patients %>%
  inner_join(allergies, by = c("Id" = "PATIENT"), relationship = "many-to-many")
#Do patient with allergies have more encomuters than those without
enc_per_patient <- encounters %>%
  count(PATIENT)

patients_with_allergies <- allergies %>%
  distinct(PATIENT) %>%
  mutate(has_allergy = TRUE)

patients %>%
  left_join(enc_per_patient, by = c("Id" = "PATIENT")) %>%
  left_join(patients_with_allergies, by = c("Id" = "PATIENT")) %>%
  mutate(has_allergy = ifelse(is.na(has_allergy), FALSE, TRUE),
         n = replace_na(n, 0)) %>%
  group_by(has_allergy) %>%
  summarize(avg_encounters = mean(n))
#What is the age distribution by gender?
ggplot(patients, aes(x = AGE, fill = GENDER)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(title = "Age Distribution by Gender")
# fix age column
library(lubridate)   # for date handling
#Now your histogram works
patients <- patients %>%
  mutate(AGE = floor(interval(ymd(BIRTHDATE), Sys.Date()) / years(1)))
ggplot(patients, aes(x = AGE, fill = GENDER)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(title = "Age Distribution by Gender")
library(lubridate)
#“Are certain allergies more common in specific age groups or genders?”
patients <- patients %>%
  mutate(AGE = floor(interval(ymd(BIRTHDATE), Sys.Date()) / years(1)))
#define groups
patients <- patients %>%
  mutate(AGE_GROUP = case_when(
    AGE < 18 ~ "Child",
    AGE >= 18 & AGE < 40 ~ "Young Adult",
    AGE >= 40 & AGE < 65 ~ "Middle Age",
    AGE >= 65 ~ "Senior",
    TRUE ~ "Unknown"
  ))
#joint patients+Allergies
patient_allergies <- patients %>%
  inner_join(allergies, by = c("Id" = "PATIENT"))
#Summarize by age group and gender
allergy_by_demo <- patient_allergies %>%
  count(AGE_GROUP, GENDER, DESCRIPTION, sort = TRUE)
#top allergies by group
allergy_by_demo %>%
  group_by(AGE_GROUP) %>%
  slice_max(n, n = 5) %>%   # top 5 allergies per age group
  ggplot(aes(x = reorder(DESCRIPTION, n), y = n, fill = AGE_GROUP)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~AGE_GROUP, scales = "free_y") +
  labs(title = "Top Allergies by Age Group", x = "Allergy", y = "Count")
#Allergies by gender
  group_by(GENDER) %>%
  slice_max(n, n = 5) %>%
  ggplot(aes(x = reorder(DESCRIPTION, n), y = n, fill = GENDER)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~GENDER, scales = "free_y") +
  labs(title = "Top Allergies by Gender", x = "Allergy", y = "Count")

