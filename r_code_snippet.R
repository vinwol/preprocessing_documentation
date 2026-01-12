# R Code Snippet:

# Initialize patient count tracking
pats_num_list <- list()

#@DOCU-START
#@STEP:001
# Load ADSL dataset from clinical database
#@DOCU-END
adsl <- read_sas("./data/adsl.sas7bdat")
pats_num_list[[1]] <- length(unique(adsl$USUBJID))

#@DOCU-START
#@STEP:002
# Filter for Safety Population (SAFFL == 'Y')
#@DOCU-END
adsl_filtered <- adsl %>% 
  dplyr::filter(SAFFL == "Y")
pats_num_list[[2]] <- length(unique(adsl_filtered$USUBJID))

#@DOCU-START
#@STEP:003
# Exclude patients with missing baseline weight
#@DOCU-END
adsl_filtered <- adsl_filtered %>% 
  dplyr::filter(!is.na(WEIGHTBL))
pats_num_list[[3]] <- length(unique(adsl_filtered$USUBJID))

#@DOCU-START
#@STEP:004
# Create age groups: <65 years, ≥65 years
#@DOCU-END
adsl_final <- adsl_filtered %>%
  dplyr::mutate(AGEGRP = ifelse(AGE < 65, "<65", "≥65"))
pats_num_list[[4]] <- length(unique(adsl_final$USUBJID))

# Save patient counts
cat(unlist(pats_num_list), 
    file = "./output/pats_num.txt", 
    sep = ",")

