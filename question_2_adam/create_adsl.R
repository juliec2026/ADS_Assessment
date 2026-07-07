# -------------------------------------------------------------------------
# Question 2: ADaM ADSL Dataset Creation
# -------------------------------------------------------------------------

# Start log file
log_file <- "question_2_adam/adsl.log"
# Open connections to capture both standard print statements and errors/warnings
con <- file(log_file, open = "wt")
sink(con, type = "output")
sink(con, type = "message", append = TRUE)

cat("Execution summary:\n")
cat("Date:", format(Sys.time()), "\n")
sessionInfo()

# Load Required Libraries
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(metatools)

# Read SDTM Data
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)

adsl1 <- dm %>%
  select(-DOMAIN) %>%
  dplyr::mutate(
  # Derive the text grouping (AGEGR9)
  AGEGR9 = case_when(
    AGE < 18                  ~ "<18",
    AGE >= 18 & AGE <= 50     ~ "18 - 50",
    AGE > 50                  ~ ">50",
    TRUE                      ~ NA_character_  # Handles missing/NA ages safely
  ),
  
  # Derive the corresponding numeric grouping (AGEGR9N)
  AGEGR9N = case_when(
    AGE < 18                  ~ 1,
    AGE >= 18 & AGE <= 50     ~ 2,
    AGE > 50                  ~ 3,
    TRUE                      ~ NA_real_       # Handles missing/NA ages safely
  ), 
  
  # Derive ITTFL: "Y" if DM.ARM is not missing, else "N"
  ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N")
)

# EXSTDTC
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "h",
    flag_imputation = "time"
  ) %>%
  derive_vars_dtm(
  dtc = EXENDTC,
  new_vars_prefix = "EXEN",
  time_imputation = "last"
  )

# Merge the first valid treatment observation into ADSL
adsl2 <- adsl1 %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    # Apply specific valid dose filter logic
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) & !is.na(EXSTDTM),
    # Map the newly created EX variables to ADSL names
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    # Order chronologically by date-time, then use sequence to break ties
    order = exprs(EXSTDTM, EXSEQ),
    # Pick the baseline earliest occurrence per subject
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# LSTAVLDT (Last Known Alive Date)
# VS
vs_date <- vs %>%
  # Filter out rows where BOTH result variables are missing
  filter(!(is.na(VSSTRESN) & (is.na(VSSTRESC) | VSSTRESC == ""))) %>%
  # Convert VSDTC to a date, ignoring rows without complete dates
  derive_vars_dt(dtc = VSDTC, new_vars_prefix = "VS", highest_imputation = "n") %>%
  filter(!is.na(VSDT)) %>%
  group_by(USUBJID) %>%
  summarise(LST_VS = max(VSDT), .groups = "drop")

# AE
ae_date <- ae %>%
  derive_vars_dt(dtc = AESTDTC, new_vars_prefix = "AE", highest_imputation = "n") %>%
  filter(!is.na(AEDT)) %>%
  group_by(USUBJID) %>%
  summarise(LST_AE = max(AEDT), .groups = "drop")

# DS
ds_date <- ds %>%
  derive_vars_dt(dtc = DSSTDTC, new_vars_prefix = "DS", highest_imputation = "n") %>%
  filter(!is.na(DSDT)) %>%
  group_by(USUBJID) %>%
  summarise(LST_DS = max(DSDT), .groups = "drop")

# Merge dates info into ADSL
adsl3 <- adsl2 %>%
  # Bring in the summarized dates from the domains
  left_join(vs_date, by = "USUBJID") %>%
  left_join(ae_date, by = "USUBJID") %>%
  left_join(ds_date, by = "USUBJID") %>%
  
  # Calculate the max across all sources row-by-row
  dplyr::mutate(
    # --- 4. Treatment Date (from ADSL.TRTEDTM) ---
    # Extract just the date component from the datetime object
    LST_TRT = as.Date(TRTEDTM),
    
    # Take the max of all non-missing values per patient
    LSTAVLDT = pmax(LST_VS, LST_AE, LST_DS, LST_TRT, na.rm = TRUE)
  ) %>%
  
  # Clean up the intermediate summary date columns
  select(-LST_VS, -LST_AE, -LST_DS, -LST_TRT)

adsl_labelled <- adsl3 %>%
  metatools::add_labels(
    AGEGR9   = "Age Group 9",
    AGEGR9N  = "Age Group 9 (N)",
    TRTSDTM  = "Treatment Start Datetime",
    TRTSTMF  = "Treatment Start Time Imputation Flag",
    TRTEDTM  = "Treatment End Datetime",
    TRTETMF  = "Treatment End Time Imputation Flag",
    ITTFL    = "Intent-to-Treat Population Flag",
    LSTAVLDT = "Last Known Alive Date"
  )

# RFICDTC is always blank and only optional in ADSL, so removed
adsl <- adsl_labelled %>%
  select(-RFICDTC)

# Save output dataset
write.csv(adsl, "question_2_adam/adsl.csv", row.names = FALSE)


# Log info
print(paste("Final ADaM ADSL records generated:", nrow(adsl)))
str(adsl)
cat("\n")

# =========================================================================
# END OF SCRIPT: Turn off Logging and close file
# =========================================================================
sink(type = "message")
sink(type = "output")
close(con)
