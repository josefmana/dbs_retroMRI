# A neat small script that extracts pre-surgery neuropsychology data of patients for the retrospective post-DBS fMRI project initiated during
# CLIMABI grant proposal (2019-2022) by Ministry of Health, Czech Republic, in iTEMPO center at the First Faculty of Medicine, Charles
# University, Prague

# list packages to be used
pkgs <- c( "rstudioapi", "tidyverse", "dplyr" )

# load or install each of the packages as needed
for (i in pkgs) {
  if (i %in% rownames (installed.packages()) == F) install.packages(i) # install if it ain't installed yet
  if ( i %in% names (sessionInfo() $otherPkgs) == F ) library(i, character.only = T ) # load if it ain't loaded yet
}

# set working directory (works in RStudio only)
setwd( dirname(getSourceEditorContext()$path) )

# read the data sets
d0 <- read.csv( "data/dbs_retroMRI_original_outcome_data.csv", sep = ";" ) # included patients
d1 <- read.csv( "data/dbs_retroMRI_redcap_data.csv", sep = "," ) # data from, REDCap
d2 <- read.csv( "data/dbs_longCOG_data.csv", sep = "," ) # the longitudinal data sets for missing values
k <- read.csv( "data/dbs_retroMRI_subject_key.csv", sep = ";" ) # IDs mapping key

# run through the key and fill-in IPN codes into the included patients data set
for ( i in 1:nrow(d0) ) for ( j in 1:nrow(k) ) if ( d0$SUBJECT[i] == k$SUBJECT[j] ) d0$IPN[i] <- k$IPN[j]

# list names of variables to be included as a pre-surgery cognitive profile measure
# selecting measures based on https://doi.org/10.1080/13854046.2017.1293161
vars <- c( "tmt_a", "ds_b", "tol_anderson", "vf_animals", "sim", "bnt_60", "ravlt_30", "bvmt_30", "wms_family_30", "jol", "clox_i" )

# prepare columns for the battery variables
for ( i in vars ) d0[[i]] <- NA

# run through the REDCap data and fill-in test scores for a pre-surgery battery
for( i in d0$IPN ) for( j in vars ) if( j %in% names(d1) ) d0[ d0$IPN == i , j ] <- d1[ d1$study_id == i, j ]

# rename variables in the longitudinal data set wherever needed
d2 <- d2 %>%
  rename( "tol_anderson" = "tol", "vf_animals" = "cft", "ravlt_30" = "ravlt_dr", "wms_family_30" = "fp_dr" ) %>%
  mutate( sim = sim + 5 ) %>% # "sim" is coded differently in d2 compared to d1, need to add 5 points to make them equivalent
  filter( ass_type == "pre" ) # keep only baseline assessments

# run through d0 and fill-in from d2 what was missing in d1
for( i in d0$IPN ) for( j in vars ) if( (is.na(d0[ d0$IPN == i , j ])) & (i %in% d2$id) & (j %in% names(d2)) ) d0[ d0$IPN == i , j ] <- d2[ d2$id == i, j ]
