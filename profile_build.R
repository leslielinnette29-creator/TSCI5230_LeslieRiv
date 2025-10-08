#### ============================================================
#### Data Profiling Script: sex, age (18–35), condition, social_status
#### Save as: profile_build.R  |  Run in RStudio: Ctrl/Cmd + Shift + S (save), then Ctrl/Cmd + Shift + Enter (source)
#### ============================================================

## ------------------ USER SETTINGS ------------------ ##
# Adjust these paths to your CSV files:
path_patients   <- "../output/csv/patients.csv"
path_encounters <- "../output/csv/encounters.csv"
path_conditions <- "../output/csv/conditions.csv"

# Output paths:
out_profile_csv <- "profile_18to35.csv"     # tidy table
make_report     <- TRUE                      # set FALSE to skip HTML profiling report
report_title    <- "Profiling: Sex, Age 18–35, Condition, Social Status"

# Income thresholds for social_status:
breaks_income   <- c(-Inf, 30000, 70000, Inf)
labels_income   <- c("Low", "Middle", "High")

## ------------------ PACKAGES ------------------ ##
need <- c("dplyr","readr","lubridate","janitor","skimr","DataExplorer")
to_install <- need[!need %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(dplyr)
library(readr)
library(lubridate)
library(janitor)
library(skimr)
library(DataExplorer)

## ------------------ LOAD DATA ------------------ ##
message("Reading CSVs...")
patients   <- read_csv(path_patients,   show_col_types = FALSE)
encounters <- read_csv(path_encounters, show_col_types = FALSE)
conditions <- read_csv(path_conditions, show_col_types = FALSE)

## ------------------ CLEAN & PREP ------------------ ##
# 1) Standardize sex from GENDER/SEX if present
patients <- patients %>%
  mutate(
    sex_raw = dplyr::coalesce(.data$SEX, .data$GENDER),
    sex = case_when(
      is.na(sex_raw) ~ "Unknown",
      tolower(sex_raw) %in% c("female","f") ~ "Female",
      tolower(sex_raw) %in% c("male","m")   ~ "Male",
      tolower(sex_raw) %in% c("unknown","unk","u") ~ "Unknown",
      TRUE ~ "Other"
    ),
    sex = factor(sex, levels = c("Female","Male","Other","Unknown"))
  )

# 2) Parse dates; compute age at encounter; filter 18–35
demo <- encounters %>%
  mutate(START = ymd_hms(START, quiet = TRUE)) %>%
  left_join(
    patients %>%
      mutate(BIRTHDATE = ymd(BIRTHDATE, quiet = TRUE)) %>%
      select(Id, sex, BIRTHDATE, INCOME, EDUCATION, MARITAL, RACE, ETHNICITY),
    by = c("PATIENT" = "Id")
  ) %>%
  mutate(
    age = floor(time_length(interval(BIRTHDATE, START), "years"))
  ) %>%
  filter(!is.na(age), age >= 18, age <= 35)

# 3) Choose a condition label (latest per patient by START in conditions.csv)
cond_latest <- conditions %>%
  mutate(START = ymd_hms(START, quiet = TRUE)) %>%
  arrange(PATIENT, desc(START)) %>%
  group_by(PATIENT) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(PATIENT, condition = DESCRIPTION)

# (Optional alternative) Condition active at encounter time:
# cond_active <- conditions %>%
#   mutate(START = ymd_hms(START, quiet = TRUE),
#          STOP  = ymd_hms(STOP,  quiet = TRUE)) %>%
#   select(PATIENT, START, STOP, DESCRIPTION) %>%
#   rename(cSTART = START, cSTOP = STOP)
# demo <- demo %>%
#   left_join(cond_active, by = c("PATIENT")) %>%
#   filter(!is.na(cSTART), cSTART <= START, is.na(cSTOP) | cSTOP >= START) %>%
#   rename(condition = DESCRIPTION)

# 4) Map income -> social_status
profile_df <- demo %>%
  left_join(cond_latest, by = "PATIENT") %>%
  mutate(
    social_status = if ("INCOME" %in% names(.)) {
      cut(INCOME, breaks = breaks_income, labels = labels_income, right = TRUE)
    } else {
      factor(NA_character_, levels = labels_income)
    }
  ) %>%
  select(sex, age, condition, social_status)

## ------------------ SUMMARIES ------------------ ##
message("\n=== Quick Summaries ===")
message("Rows in profiling set: ", nrow(profile_df))

message("\nSex distribution:")
print(profile_df %>% tabyl(sex) %>% adorn_totals("row") %>% adorn_pct_formatting())

message("\nSocial status distribution:")
print(profile_df %>% tabyl(social_status) %>% adorn_totals("row") %>% adorn_pct_formatting())

message("\nAge summary (18–35 only):")
print(profile_df %>% summarise(n = n(), mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE),
                               min_age = min(age, na.rm = TRUE), max_age = max(age, na.rm = TRUE)))

message("\nTop 15 conditions:")
print(
  profile_df %>%
    count(condition, sort = TRUE) %>%
    mutate(pct = scales::percent(n / sum(n), accuracy = 0.1)) %>%
    slice_head(n = 15)
)

## ------------------ SAVE OUTPUT ------------------ ##
write_csv(profile_df, out_profile_csv)
message("\nSaved tidy profiling table to: ", normalizePath(out_profile_csv))

## ------------------ OPTIONAL: HTML REPORT ------------------ ##
if (isTRUE(make_report)) {
  message("\nCreating DataExplorer report (PCA disabled)...")
  create_report(
    profile_df,
    output_file = "profile_report.html",
    report_title = report_title,
    config = configure_report(add_plot_prcomp = FALSE)
  )
  message("Report written to: ", normalizePath("profile_report.html"))
}

message("\nAll done ✅")
