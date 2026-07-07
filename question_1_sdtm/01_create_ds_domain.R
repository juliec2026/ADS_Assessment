# -------------------------------------------------------------------------
# Question 1: SDTM DS Domain Creation using {sdtm.oak}
# -------------------------------------------------------------------------

# Start log file
log_file <- "question_1_sdtm/ds.log"
# Open connections to capture both standard print statements and errors/warnings
con <- file(log_file, open = "wt")
sink(con, type = "output")
sink(con, type = "message", append = TRUE)

cat("Execution summary:\n")
cat("Date:", format(Sys.time()), "\n")
sessionInfo()

# Load Required Libraries
library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)
library(haven)

# Load Study Controlled Terminology (CT) Specs
study_ct <- read.csv("question_1_sdtm/sdtm_ct.csv")

# Read Raw Data & Generate OAK ID Variables
ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm
sv <- pharmaversesdtm::sv

print(paste("Raw dataset loaded successfully. Rows:", nrow(ds_raw)))

ds_prepared <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM", 
    raw_src = "ds_raw"
  ) %>%
  # Fix the casing for "Ambul Ecg Removal" in the raw columns before mapping
  mutate(
    INSTANCE = if_else(INSTANCE == "Ambul Ecg Removal", "Ambul ECG Removal", INSTANCE)
  )

# Map the Variables as per Subject_Disposition_aCRF.pdf
ds_mapped <-
  
  # DSTERM
  # OTHERSP is na -> DSTERM=IT.DSTERM
  assign_no_ct(
    raw_dat = ds_prepared,
    raw_var = "IT.DSTERM", 
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  # OTHERSP is not na -> DSTERM=OTHERSP
  assign_no_ct(
    raw_dat = ds_prepared %>% condition_add(!is.na(OTHERSP)),
    raw_var = "OTHERSP", 
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  
  # DSCAT
  # Randomized -> DSCAT='PROTOCOL MILESTONE'
  hardcode_ct(
    raw_dat = ds_prepared %>% condition_add(IT.DSDECOD=='Randomized'),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",  
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  # OTHERSP not NA -> DSCAT='OTHER EVENT'
  hardcode_ct(
    raw_dat = ds_prepared %>% condition_add(!is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",  
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  # other scenarios -> DSCAT='DISPOSITION EVENT'
  hardcode_ct(
    raw_dat = ds_prepared %>% condition_add((IT.DSDECOD != 'Randomized' | is.na(IT.DSDECOD)) & is.na(OTHERSP)),
    raw_var = "IT.DSTERM",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",  
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  
  # DSDECOD
  # OTHERSP is na -> DSDECOD=IT.DSDECOD
  assign_no_ct(
    raw_dat = ds_prepared %>% condition_add(is.na(OTHERSP)),
    raw_var = "IT.DSDECOD", 
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  # OTHERSP is not na -> DSDECOD=OTHERSP
  assign_no_ct(
    raw_dat = ds_prepared %>% condition_add(!is.na(OTHERSP)),
    raw_var = "OTHERSP", 
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%

  # Map date & time variables
  # DSDTC
  assign_datetime(
    raw_dat = ds_prepared,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    raw_fmt = c("m-d-y", "H:M"),
    tgt_var = "DSDTC",
    id_vars = oak_id_vars()
  ) %>%
  # DSDTC
  assign_datetime(
    raw_dat = ds_prepared,
    raw_var = "IT.DSSTDAT",
    raw_fmt = c("m-d-y"),
    tgt_var = "DSSTDTC",
    id_vars = oak_id_vars()
  ) %>%
  
  # VISTNUM & VISIT
  assign_ct(
    raw_dat = ds_prepared,
    raw_var = "INSTANCE", 
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) 

# NOTE: The following terms could not be mapped against Controlled Terminology: 
# "Unscheduled 1.1", "Unscheduled 4.1", "Unscheduled 5.1", 
# "Unscheduled 6.1", "Unscheduled 8.2", "Unscheduled 13.1".
#
# Need to verify chronological placement of unscheduled visits (confirming 
# "Unscheduled 6.1" falls between Visit 6 and 7) with SDTM.SV
# Since SDTM.SV is created independently of SDTM.DS, no circular dependency loop is introduced.
unique_visits <- sv %>%
  select(VISITNUM, VISIT) %>%
  distinct() %>%
  arrange(VISITNUM)

ds_mapped_visitnum <- ds_mapped %>%
  left_join(unique_visits, by = "VISIT")%>%
  # Order by visit and date making protocol completed (without time) are arrange after the Final Lab Visit
  arrange(patient_number, VISITNUM, nchar(DSDTC) == 10)

# Create SDTM derived variables
# et the study name as a single string from your raw data
study_name <- unique(ds_prepared$STUDY)[1]

ds_derived <- ds_mapped_visitnum %>%
  dplyr::mutate(
    STUDYID = study_name,
    DOMAIN = "DS",
    USUBJID = paste0("01-", patient_number),
    DSTERM = toupper(DSTERM),
    DSDECOD = toupper(DSDECOD)
  ) %>%
  # DSSEQ
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "VISITNUM")
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC",
    "DSSTDTC", "DSSTDY"
  )


# All labels
ds_final <- as.data.frame(ds_derived)
  attr(ds_final$STUDYID, "label")  <- "Study Identifier"
  attr(ds_final$DOMAIN, "label")   <- "Domain Abbreviation"
  attr(ds_final$USUBJID, "label")  <- "Unique Subject Identifier"
  attr(ds_final$DSSEQ, "label")    <- "Sequence Number"
  attr(ds_final$DSTERM, "label")   <- "Reported Term for the Disposition Event"
  attr(ds_final$DSDECOD, "label")  <- "Standardized Disposition Term"
  attr(ds_final$DSCAT, "label")    <- "Category for Disposition Event"
  attr(ds_final$VISITNUM, "label") <- "Visit Number"
  attr(ds_final$VISIT, "label")    <- "Visit Name"
  attr(ds_final$DSDTC, "label")    <- "Date/Time of Collection"
  attr(ds_final$DSSTDTC, "label")  <- "Start Date/Time of Disposition Event"
  attr(ds_final$DSSTDY, "label")   <- "Study Day of Start of Disposition Event"

# Now write the file
write_xpt(
  data = ds_final, 
  path = "question_1_sdtm/ds.xpt", 
  name = "DS", 
  version = 5,
  label = "Disposition"
)

# Read the XPT file back to verify label presence
my_data <- haven::read_xpt("question_1_sdtm/ds.xpt")
View(my_data)

# Log info
print(paste("Final SDTM DS records generated:", nrow(ds_final)))
str(ds_final)
cat("\n")

# =========================================================================
# END OF SCRIPT: Turn off Logging and close file
# =========================================================================
sink(type = "message")
sink(type = "output")
close(con)