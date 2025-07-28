#'------------------------------------------------------------------------------
# @DESCRIPTION-START
# <b>Description</b>
# <br>
# <b>ID: AN001</b>
# <br>
# <b>Analysis Description</b>
# <br>
# Tabular representation of absolute value and change from Baseline in Volume Hard Exudates (unit: nano liters),
# in the 3 mm Center using the study eye.
# Report values are at visits: Baseline, Week 1, Week 4, Week 8.
# Stratification of patients by: Baseline Volume Hard Exudates Tertile in the 3 mm Center.
# Volume of Hard Exudates is calculated as the sum of HEV plus IHRMV.
# <br>
# <b>Used Datasets</b>
# <br>
# OEIMOPM test dataset for Dummy Study 1. 
# <br>
# OEIMOPM test dataset for Dummy Study 2. 
# <br>
# <b>Used Variables</b>
# <br>
# For Filtering:
# <br>
# AFEYE: Treatment Eye
# <br>
# OEIMDPTH: Image Depth
# <br>
# ANL02FL_97: Analysis Flag 02 at 97 BScan
# <br>
# OELOCDTL: Location Detail
# <br>
# AVISIT: Analysis Visit
# <br>
# <b>Generated Variables</b>
# <br>
# Change from Baseline for Volume Hard Exudates.
# <br>
# SUMV: Volume Hard Exudates: SUMV = IHRMV + HEV
# <br>
# HEV: Intraretinal Hard Exudates Volume.
# <br>
# IHRMV: Intraretinal Hyper Ref Material Volume.
# <br>
# <b>Result of Preprocessing</b>
# <br>
# Dataframe with variables UNI_ID, AVISIT, SUMV, CHG_SUMV, BL_TERTILE_PCT
# @DESCRIPTION-END
#'------------------------------------------------------------------------------
library(shinyjs)
library(dplyr)
library(tidyr)
library(webshot2)
library(gt)
#'------------------------------------------------------------------------------
# Function to create a test dataset.
create_test_data <- function(study_id, num_patients) {
    
    common_vars <- c("UNI_ID", 
                     "STUDYID", 
                     "AFEYE", 
                     "ANL02FL_97", 
                     "OELOCDTL", 
                     "OETESTCD", 
                     "AVISIT", 
                     "OEORRES", 
                     "OEORRESU", 
                     "OESTRESC", 
                     "OESTRESN", 
                     "HEV", 
                     "IHRMV", 
                     "OEIMDPTH")
    
    patient_ids <- paste0("PAT-", study_id, "-", sprintf("%03d", 1:num_patients))
    visits <- c("Baseline", "Week 1", "Week 4", "Week 8", "Week 12")
    oetests <- c("HEV", "IHRMV", "OTHER")
    
    # Create a base structure with all combinations
    df <- expand.grid(UNI_ID = patient_ids, 
                      AVISIT = visits, 
                      OETESTCD = oetests, 
                      stringsAsFactors = FALSE)
    
    df <- df %>%
        dplyr::mutate(
            STUDYID = study_id,
            AFEYE = sample(c("Study Eye", "Fellow Eye"), n(), replace = TRUE, prob = c(0.8, 0.2)),
            ANL02FL_97 = sample(c("Y", "N"), n(), replace = TRUE, prob = c(0.9, 0.1)),
            OELOCDTL = sample(c("CENTER 3 MM", "OTHER LOC"), n(), replace = TRUE, prob = c(0.7, 0.3)),
            OEORRES = round(runif(n(), 0.01, 0.5), 4),
            OEORRESU = "mm^3",
            OESTRESC = as.character(OEORRES), 
            OESTRESN = OEORRES,              
            OEIMDPTH = round(runif(n(), 250, 350)),
            HEV = if_else(OETESTCD == "HEV", OEORRES, NA_real_),
            IHRMV = if_else(OETESTCD == "IHRMV", OEORRES, NA_real_)
        )
    
    df <- df %>% dplyr::group_by(UNI_ID) %>%
        dplyr::mutate(
            HEV = if_else(AVISIT == 'Baseline', abs(rnorm(1, 0.2, 0.05)), HEV),
            IHRMV = if_else(AVISIT == 'Baseline', abs(rnorm(1, 0.1, 0.05)), IHRMV)
        ) %>% ungroup()
    
    return(df[, common_vars])
}
#'------------------------------------------------------------------------------
oeimopm_study1 <- create_test_data(88412, 800)
oeimopm_study2 <- create_test_data(88413, 778)
#'------------------------------------------------------------------------------
pats_num_list <- list()
#'------------------------------------------------------------------------------
# Data Preprocessing for test datasets.

#@DOCU-START
#@STEP:001
# Fetching Ophtha test dataset for Study 1.
#@DOCU-END
pats_num_list[[1]] <- length(unique(oeimopm_study1$UNI_ID))

#@DOCU-START
#@STEP:002
# Fetching Ophtha test dataset for Study 2.
#@DOCU-END
pats_num_list[[2]] <- length(unique(oeimopm_study2$UNI_ID))

# Check variables in common.
shared_vars <- intersect(names(oeimopm_study1), names(oeimopm_study2))
print(paste0("Number of columns in oeimopm_study1: ", length(names(oeimopm_study1))))
print(paste0("Number of columns in oeimopm_study2: ", length(names(oeimopm_study2))))
print(paste0("Number of columns in common: ", length(shared_vars)))
unique_cols <- setdiff(names(oeimopm_study1), names(oeimopm_study2))
print("Columns unique to oeimopm_study1:")
print(unique_cols)
unique_cols <- setdiff(names(oeimopm_study2), names(oeimopm_study1))
print("Columns unique to oeimopm_study2:")
print(unique_cols)

#@DOCU-START
#@STEP:003
# Merging datasets.
#@DOCU-END
oeimopm_merged <- rbind(oeimopm_study1[,shared_vars], oeimopm_study2[,shared_vars])
pats_num_list[[3]] <- length(unique(oeimopm_merged$UNI_ID))

# Count empty strings
count_empty <- sum(oeimopm_merged$AVISIT == "")
# Count NA values
count_na <- sum(is.na(oeimopm_merged$AVISIT))
# Print results
cat("Count of empty strings:", count_empty, "\n")
cat("Count of NA values:", count_na, "\n")

# Total number of rows in the dataframe.
total_rows <- nrow(oeimopm_merged)
percentage_empty <- (count_empty / total_rows) * 100
cat("Percentage of empty strings:", round(percentage_empty,3), "%\n")

# Number of patients:
print(length(unique(oeimopm_merged$UNI_ID))) # Should be 1578 with test data

#@DOCU-START
#@STEP:004
# Filtering dataset by AFEYE == "Study Eye".
#@DOCU-END
oeimopm_filtered <- oeimopm_merged %>% dplyr::filter(AFEYE == "Study Eye")
pats_num_list[[4]] <- length(unique(oeimopm_filtered$UNI_ID)) 

#@DOCU-START
#@STEP:005
# Filtering dataset by ANL02FL_97 == "Y".
#@DOCU-END
oeimopm_filtered <- oeimopm_filtered %>% dplyr::filter(ANL02FL_97 == "Y")
pats_num_list[[5]] <- length(unique(oeimopm_filtered$UNI_ID)) 

#@DOCU-START
#@STEP:006
# Filtering dataset by OELOCDTL == "CENTER 3 MM".
#@DOCU-END
oeimopm_filtered <- oeimopm_filtered %>% dplyr::filter(OELOCDTL == "CENTER 3 MM")
pats_num_list[[6]] <- length(unique(oeimopm_filtered$UNI_ID))

# Tertiles are based on SUMV = IHRMV + HEV (OETESTCD == IHRMV or HEV). 
# Volume should be presented with nano Liters (nL) as a unit, i.e. SUMV [nL] = 1000*SUMV [mm^3].
# OETESTCD: Short Name of Ophthalmic Test or Exam
# HEV: Intraretinal Hard Exudates Vol
# IHRMV: Intraretinal Hyper Ref Material Vol
#@DOCU-START
#@STEP:007
# Filtering dataset by OETESTCD %in% c("HEV", "IHRMV").
# HEV: Intraretinal Hard Exudates Volume.
# IHRMV: Intraretinal Hyper Ref Material Volume.
#@DOCU-END
oeimopm_filtered <- oeimopm_filtered %>% dplyr::filter(OETESTCD %in% c("HEV", "IHRMV"))
pats_num_list[[7]] <- length(unique(oeimopm_filtered$UNI_ID))

#@DOCU-START
#@STEP:008
# Filtering dataset by AVISIT != "".
#@DOCU-END
oeimopm_filtered <- oeimopm_filtered %>% dplyr::filter(AVISIT != "")
pats_num_list[[8]] <- length(unique(oeimopm_filtered$UNI_ID))

# Make relevant variables numeric.
oeimopm_filtered <- oeimopm_filtered %>% 
    dplyr::mutate(dplyr::across(c(OEORRES, OESTRESC, OESTRESN, OEIMDPTH), as.numeric))

oeimopm_filtered <- oeimopm_filtered %>% 
    dplyr::mutate(AVISIT = factor(AVISIT, levels = c("Baseline", "Week 1", "Week 4", "Week 8", 
                                                     "Week 12", "Week 16", "Week 20", "Week 24", 
                                                     "Week 28", "Week 32", "Week 36", "Week 40", 
                                                     "Week 44", "Week 48", "Week 52", "Week 56", 
                                                     "Week 60", "Week 64", "Week 68", "Week 72", 
                                                     "Week 76", "Week 80", "Week 84", "Week 88", 
                                                     "Week 92", "Week 96", "Week 100")))
#'------------------------------------------------------------------------------
# OETESTCD: Short Name of Ophthalmic Test or Exam
# OELOCDTL: Location Detail
# OEORRES: Result or Finding in Original Units
#@DOCU-START
#@STEP:009
# Subsetting dataset to the variables: UNI_ID, AVISIT, STUDYID, OETESTCD, 
# OELOCDTL, OEORRES, OEORRESU.
#@DOCU-END
oeimopm_data_subset <- oeimopm_filtered %>%
    dplyr::select(UNI_ID, AVISIT, STUDYID, OETESTCD, OELOCDTL, OEORRES, OEORRESU) 
pats_num_list[[9]] <- length(unique(oeimopm_data_subset$UNI_ID))

#@DOCU-START
#@STEP:010
# Filtering dataset by !is.na(OETESTCD).
#@DOCU-END
oeimopm_data_subset <- oeimopm_data_subset %>%
    dplyr::filter(!is.na(OETESTCD)) 
pats_num_list[[10]] <- length(unique(oeimopm_data_subset$UNI_ID))

#@DOCU-START
#@STEP:011
# Reshape dataset to wide format to have one record per patient.
#@DOCU-END
oeimopm_data_subset_wide_format <- oeimopm_data_subset %>%
    tidyr::pivot_wider(names_from = OETESTCD, values_from = OEORRES)
pats_num_list[[11]] <- length(unique(oeimopm_data_subset_wide_format$UNI_ID))

#@DOCU-START
#@STEP:012
# In dataset create a new variables SUMS = HEV + IHRMV.
#@DOCU-END
oeimopm_data_subset_wide_format <- oeimopm_data_subset_wide_format %>%
    dplyr::mutate(SUMV = HEV + IHRMV) 
pats_num_list[[12]] <- length(unique(oeimopm_data_subset_wide_format$UNI_ID))

#@DOCU-START
#@STEP:013
# In dataset create a new variable CHG_SUMV which shows change from baseline.
#@DOCU-END
oeimopm_data_subset_wide_format <- oeimopm_data_subset_wide_format %>%
    dplyr::group_by(UNI_ID) %>%
    dplyr::mutate(
        BL_SUMV = SUMV[AVISIT == "Baseline"][1],  # same as first() but explicit
        CHG_SUMV = ifelse(!is.na(BL_SUMV), SUMV - BL_SUMV, NA)) %>%
    dplyr::ungroup()
pats_num_list[[13]] <- length(unique(oeimopm_data_subset_wide_format$UNI_ID))

#@DOCU-START
#@STEP:014
# In dataset volume should be presented with nano liters (nL) as a unit, 
# i.e. SUMV [nL] = 1000*SUMV [mm^3].
#@DOCU-END
oeimopm_data_subset_wide_format$SUMV <- oeimopm_data_subset_wide_format$SUMV*1000
oeimopm_data_subset_wide_format$BL_SUMV <- oeimopm_data_subset_wide_format$BL_SUMV*1000
oeimopm_data_subset_wide_format$CHG_SUMV <- oeimopm_data_subset_wide_format$CHG_SUMV*1000
pats_num_list[[14]] <- length(unique(oeimopm_data_subset_wide_format$UNI_ID))
#'------------------------------------------------------------------------------
#@DOCU-START
#@STEP:015
# In dataset add tertile information for Hard Exudates Volume for baseline: 0–33%, 33–67%, 67–100%.
#@DOCU-END
count_na <- sum(is.na(oeimopm_data_subset_wide_format$BL_SUMV))
tertile_cutpoints <- quantile(oeimopm_data_subset_wide_format$BL_SUMV, 
                              probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
tertile_labels_pct <- c("0–33%", "33–67%", "67–100%")
oeimopm_data_subset_wide_format <- oeimopm_data_subset_wide_format %>%
    dplyr::mutate(
        BL_TERTILE_PCT = cut(BL_SUMV, 
                             breaks = tertile_cutpoints,
                             labels = tertile_labels_pct,
                             include.lowest = TRUE))
pats_num_list[[15]] <- length(unique(oeimopm_data_subset_wide_format$UNI_ID))
#'------------------------------------------------------------------------------
opm_vol_data <- oeimopm_data_subset_wide_format

opm_vol_data <- opm_vol_data %>%
    dplyr::mutate(CHG_SUMV = if_else(AVISIT == "Baseline", NA_real_, CHG_SUMV))

#@DOCU-START
#@STEP:016
# Filtering dataset by AVISIT %in% c("Baseline", "Week 1", "Week 4", "Week 8").
#@DOCU-END
opm_vol_data <- opm_vol_data %>% 
    dplyr::filter(AVISIT %in% c("Baseline", "Week 1", "Week 4", "Week 8")) 
pats_num_list[[16]] <- length(unique(opm_vol_data$UNI_ID))

#@DOCU-START
#@STEP:017
# Subsetting dataset to the variables: UNI_ID, AVISIT, SUMV, CHG_SUMV, BL_TERTILE_PCT.
#@DOCU-END
opm_vol_data <- opm_vol_data %>% 
    dplyr::select(UNI_ID, AVISIT, SUMV, CHG_SUMV, BL_TERTILE_PCT) 
pats_num_list[[17]] <- length(unique(opm_vol_data$UNI_ID))

opm_vol_data <- opm_vol_data %>%
    dplyr::mutate(
        AVISIT = factor(AVISIT, levels = c("Baseline", "Week 1", "Week 4", "Week 8")),
        BL_TERTILE_PCT = factor(BL_TERTILE_PCT, levels = c("0–33%", "33–67%", "67–100%")))

#@DOCU-START
#@STEP:018
# Saving dataset to: ./output/oeimopm_volume_data.rds
# Format:
# @HTML_TABLE: "./output/data_table.html"  
#@DOCU-END
path <- "./output/"
saveRDS(opm_vol_data, paste0(path,"volume_data.rds"))
pats_num_list[[18]] <- length(unique(opm_vol_data$UNI_ID))
cat(unlist(pats_num_list), file = paste0(path,"pats_num.txt"), sep = ",") 
data_to_plot <- head(opm_vol_data, 12)
gt_table <- gt::gt(data_to_plot)
gt_table_styled <- gt_table %>%
    gt::tab_options(
        table.font.size = px(11),  
        data_row.padding = px(4)) 
gt::gtsave(
    data = gt_table_styled,   
    filename = paste0(path,"data_table.png"),
    vwidth = 1500,           
    expand = 10,             
    zoom = 1                 
)
gt::as_raw_html(gt_table_styled, inline_css = TRUE) %>%
    writeLines(con = paste0(path,"data_table.html"))
#'------------------------------------------------------------------------------
