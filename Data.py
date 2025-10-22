import pandas as pd
import numpy as np
import os
import re
from datetime import datetime, date

# --- init (Setup and Configuration) ---

debug = 0
seed = 22
np.random.seed(seed) # Set seed for reproducibility

# In Python/Pandas, we usually don't need options for print output as
# extensively as in R. max_rows/max_columns settings can be adjusted if needed.
# pd.set_option('display.max_rows', 500)
# pd.set_option('display.max_columns', None)

# --- Data Loading ---

datasource = "../output/csv/"
rxnorm = "./output/Metformin_RxNav_6809_table.csv"

# Load rxnorm lookup table
# Skip the first 2 lines and filter for relevant term types
try:
    rxnorm_lookup = pd.read_csv(rxnorm, skiprows=2)

    
    rxnorm_lookup = rxnorm_lookup[
        rxnorm_lookup['termType'].isin(
            ["BN", "IN", "MIN", "PIN", "SBD", "SBDC", "SBDF", "SBDFP", "SBDG", "SCD", "SCDC", "SCDF", "SCDG"]
        )
    ]
except FileNotFoundError:
    print(f"Error: RxNorm file not found at {rxnorm}")
    # Create an empty DataFrame to prevent script crash
    rxnorm_lookup = pd.DataFrame({'rxcui': [], 'termType': []})
rxnorm2=rxnorm_lookup
rxnorm3=rxnorm_lookup.copy (deep=True)
# Load all CSV files in the datasource directory into a dictionary of DataFrames
data0 = {}
for filename in os.listdir(datasource):
    if filename.endswith(".csv"):
        # Remove prefix "../output/csv/" and suffix ".csv" to get clean names
        key_name = re.sub(r"^\.\./output/csv/{0,1}|\.csv$", "", filename)
        file_path = os.path.join(datasource, filename)
        try:
            data0[key_name] = pd.read_csv(file_path)
            # Standardize column names to uppercase for consistency with R script
            data0[key_name].columns = [col.upper() for col in data0[key_name].columns]
        except Exception as e:
            print(f"Could not load or process {filename}: {e}")

# --- Sub Patterns (Diabetes Identification and Data Subsetting) ---

# Find patients and encounters associated with a diagnosis of 'diab' (diabetes)
# using a case-insensitive regex search for '\bdiab' in the DESCRIPTION column.

if 'CONDITIONS' in data0:
    # Use str.contains with regex for the search
    criteria_df = data0['CONDITIONS'][
        data0['CONDITIONS']['DESCRIPTION'].str.contains(r'\bdiab', case=False, na=False)
    ]

    criteria = {
        'patient_diabetes': criteria_df['PATIENT'].unique().tolist(),
        'encounter_diabetes': criteria_df['ENCOUNTER'].unique().tolist()
    }

    # Filter patients and encounters DataFrames
    if 'PATIENTS' in data0:
        data_diab_patients = data0['PATIENTS'][
            data0['PATIENTS']['ID'].isin(criteria['patient_diabetes'])
        ].copy() # Use .copy() to avoid SettingWithCopyWarning
    else:
        data_diab_patients = pd.DataFrame()

    if 'ENCOUNTERS' in data0:
        data_diab_encounters = data0['ENCOUNTERS'][
            data0['ENCOUNTERS']['ID'].isin(criteria['encounter_diabetes'])
        ].copy()
    else:
        data_diab_encounters = pd.DataFrame()

    # R's setdiff is equivalent to finding elements in one list but not the other
    missing_in_encounters = set(criteria['patient_diabetes']) - set(data_diab_encounters['PATIENT'].unique())
    missing_in_patients = set(data_diab_encounters['PATIENT'].unique()) - set(criteria['patient_diabetes'])

    # print(f"Patients in conditions but not encounters: {missing_in_encounters}")
    # print(f"Patients in encounters but not conditions: {missing_in_patients}")

    # Left join patients and encounters data
    # R: data_diab_patient_encounters <- left_join(data_diab_patients, data_diab_encounters, by=c("Id"="PATIENT"))
    # The 'Id.y' rename and mutate in R is handled implicitly by suffixing in Pandas

    # Handle potential missing columns if data_diab_patients or data_diab_encounters are empty
    if not data_diab_patients.empty and not data_diab_encounters.empty:
        data_diab_patient_encounters = pd.merge(
            data_diab_patients,
            data_diab_encounters,
            left_on='ID',
            right_on='PATIENT',
            how='left',
            suffixes=('_PATIENT', '_ENCOUNTER') # Append suffixes to duplicate columns
        )
        # Recreate the ENCOUNTER column as done in R
        data_diab_patient_encounters['ENCOUNTER'] = data_diab_patient_encounters['ID_ENCOUNTER']
    else:
        # Create an empty DataFrame with expected columns if a dependency is missing
        print("Warning: Missing 'PATIENTS' or 'ENCOUNTERS' data to perform join.")
        data_diab_patient_encounters = pd.DataFrame()

    # Validation check (stopping the script if rows don't match)
    if not data_diab_encounters.empty and not data_diab_patient_encounters.empty:
        if len(data_diab_patient_encounters) != len(data_diab_encounters):
            raise Exception("Join rows do not match the encounter dataset")
        else:
            print("All clear")
    elif data_diab_encounters.empty and not data_diab_patient_encounters.empty:
        # This handles the case where data_diab_encounters is empty but the join somehow produced rows
        raise Exception("Encounter data is empty but join produced rows.")


# --- Metformin Medication and Final Join ---

# Filter medications for metformin based on RxNorm lookup
if 'MEDICATIONS' in data0 and 'rxcui' in rxnorm_lookup.columns:
    med_met = data0['MEDICATIONS'][
        data0['MEDICATIONS']['CODE'].astype(str).isin(rxnorm_lookup['rxcui'].astype(str))
    ]
else:
    med_met = pd.DataFrame()
    print("Warning: Missing 'MEDICATIONS' data or 'rxcui' in lookup to filter metformin.")

# Join diabetes patient encounters with metformin prescriptions
if not data_diab_patient_encounters.empty and not med_met.empty:
    data_diab_encountersmet = pd.merge(
        data_diab_patient_encounters,
        med_met,
        on='ENCOUNTER', # 'ENCOUNTER' column created in the previous step
        how='left',
        suffixes=('_DIAB', '_MED')
    )
else:
    data_diab_encountersmet = pd.DataFrame()
    print("Warning: Skipping final join as data_diab_patient_encounters or med_met is empty.")


# --- Age Distribution Calculation ---

if 'PATIENTS' in data0:
    # Convert date columns to datetime objects
    patients_df = data0['PATIENTS'].copy()
    patients_df['DEATHDATE'] = pd.to_datetime(patients_df['DEATHDATE'])
    patients_df['BIRTHDATE'] = pd.to_datetime(patients_df['BIRTHDATE'])

    # Calculate 'alive' status
    patients_df['ALIVE'] = patients_df['DEATHDATE'].isna()

    # Determine enddate for age calculation
    today = datetime.now().date()
    # Use np.minimum to compare dates, handling NaT/NaN after conversion
    # The R code uses pmin with Sys.Date() and na.rm=TRUE for the DEATHDATE
    def get_end_date(row):
        death_date = row['DEATHDATE']
        if pd.isna(death_date):
            return today
        else:
            return min(today, death_date.date())

    patients_df['ENDDATE'] = patients_df.apply(get_end_date, axis=1)

    # Calculate age in years (age=as.numeric(enddate-BIRTHDATE)/365.25)
    # The 'as.numeric(date)' conversion in R returns the number of days since the epoch (1970-01-01)
    # In Pandas, subtracting datetime objects gives a Timedelta, which we convert to days.
    patients_df['AGE_DAYS'] = (patients_df['ENDDATE'] - patients_df['BIRTHDATE'].dt.date)
    patients_df['AGE'] = patients_df['AGE_DAYS'].dt.days / 365.25

    # Group by 'ALIVE' and summarize age distribution
    age_summary = patients_df.groupby('ALIVE')['AGE'].agg(
        avg_age=('mean', 'mean'), # 'mean' is the pandas aggregation function
        min_age=('min', 'min'),
        max_age=('max', 'max'),
        count=('count', 'size')
    ).reset_index()

    # Print the resulting summary table (similar to pander in R)
    print("\n--- Age Distribution Summary ---")
    print(age_summary)
else:
    print("Warning: Cannot calculate age distribution. 'PATIENTS' data is missing.")
