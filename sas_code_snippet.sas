* SAS Code Snippet

/* Initialize "list" - conceptually matching R's pats_num_list <- list() */
%let pats_num_list = ;

*@DOCU-START;
*@STEP:001;
* Load ADSL dataset from clinical database;
*@DOCU-END;
libname mydata "./data";
data adsl;
  set mydata.adsl;
run;

/* Capture Count 1 (R equivalent: pats_num_list[[1]]) */
proc sql noprint;
  select count(distinct USUBJID) into :pats_001 trimmed
  from adsl;
quit;


*@DOCU-START;
*@STEP:002;
* Filter for Safety Population (SAFFL = 'Y');
*@DOCU-END;
data adsl_filtered;
  set adsl;
  where SAFFL = "Y";
run;

/* Capture Count 2 (R equivalent: pats_num_list[[2]]) */
proc sql noprint;
  select count(distinct USUBJID) into :pats_002 trimmed
  from adsl_filtered;
quit;


*@DOCU-START;
*@STEP:003;
* Exclude patients with missing baseline weight;
*@DOCU-END;
data adsl_filtered;
  set adsl_filtered;
  where not missing(WEIGHTBL);
run;

/* Capture Count 3 (R equivalent: pats_num_list[[3]]) */
proc sql noprint;
  select count(distinct USUBJID) into :pats_003 trimmed
  from adsl_filtered;
quit;


*@DOCU-START;
*@STEP:004;
* Create age groups: <65 years, ≥65 years;
*@DOCU-END;
data adsl_final;
  set adsl_filtered;
  if AGE < 65 then AGEGRP = "<65";
  else AGEGRP = "≥65";
run;

/* Capture Count 4 (R equivalent: pats_num_list[[4]]) */
proc sql noprint;
  select count(distinct USUBJID) into :pats_004 trimmed
  from adsl_final;
quit;


/* ----------------------------------------------------------- */
/* Construct the final "List" (R equivalent: unlist(pats_num_list)) */
/* ----------------------------------------------------------- */

/* Combine all individual steps into one comma-separated string */
%let pats_num_list = &pats_001, &pats_002, &pats_003, &pats_004;

/* Write it to file */
data _null_;
  file "./output/pats_num.txt";
  /* Use the constructed list variable */
  put "&pats_num_list";
run;