# -------------------------------------------------------------------------
# Question 3: Summary Table of TEAEs 
# -------------------------------------------------------------------------

# Start log file
log_file <- "question_3_tlg/01_create_ae_summary_table.log"
# Open connections to capture both standard print statements and errors/warnings
con <- file(log_file, open = "wt")
sink(con, type = "output")
sink(con, type = "message", append = TRUE)

cat("Execution summary:\n")
cat("Date:", format(Sys.time()), "\n")
sessionInfo()


library(dplyr)
library(gtsummary)

adsl_safety <- pharmaverseadam::adsl %>% filter(SAFFL == "Y")
adae_teae   <- pharmaverseadam::adae %>% 
  filter(SAFFL == "Y" & TRTEMFL == "Y", !is.na(ACTARM))


ae_table <- adae_teae |>
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl_safety,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
  ) %>%
  add_overall(last=TRUE) |> 
  # 2. Sort the hierarchy by descending frequency
  sort_hierarchical(sort = everything() ~ "descending")

ae_table

# save output
ae_table %>%
  as_gt() %>%
  gt::gtsave("question_3_tlg/t_ae_teae.html")

# =========================================================================
# END OF SCRIPT: Turn off Logging and close file
# =========================================================================
sink(type = "message")
sink(type = "output")
close(con)