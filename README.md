# ADS_Assessment
ADS Programmer Coding Assessment

## 📁 Repository Structure

```text
├── question_1_sdtm/                 # SDTM DS Domain Creation
│   ├── 01_create_ds_domain.R        # Main script
│   ├── ds.log                       # Execution log
│   ├── ds.xpt                       # SDTM DS dataset (850 rows)
│   └── study_ct.csv                 # Study controlled terminology
│
├── question_2_adam/                 # ADaM ADSL Dataset Creation
│   ├── adsl.log                     # Execution log
│   ├── adsl.xpt                     # ADSL dataset (306 rows)
│   └── create_adsl.R                # Main script
│
└── question_3_tlg/                  # TLG - Adverse Events Reporting
    ├── 01_create_ae_summary_table.log # Summary table execution log
    ├── 01_create_ae_summary_table.R   # Summary table script
    ├── 02_create_ae_summary_table.log # Visualizations execution log
    ├── 02_create_ae_summary_table.R   # Visualizations script
    ├── ae_summary_table.html          # Output TEAE Hierarchical Summary Table
    ├── f_ae_severity_distribution.png # Plot 1: Stacked Bar Chart
    └── f_ae_top10_cincidence_ci.png   # Plot 2: Top 10 Preferred Terms with CIs
