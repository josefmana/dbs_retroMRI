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
d1 <- read.csv( "data/dbs_retroMRI_redcap_data.csv", sep = ";" ) # data from, REDCap
d2 <- read.csv( "data/dbs_longCOG_data.csv", sep = "," ) # the longitudinal data sets for missing values
k <- read.csv( "data/dbs_retroMRI_subject_key.csv", sep = ";" ) # IDs mapping key


# ---- dataprep for Karsten ----

# set-up a folder for Karsten's data
if ( !dir.exists("data/karsten") ) dir.create( "data/karsten" )

# run through the key and fill-in IPN codes into the included patients data set
for ( i in 1:nrow(d0) ) for ( j in 1:nrow(k) ) if ( d0$SUBJECT[i] == k$SUBJECT[j] ) d0$IPN[i] <- k$IPN[j]

# list names of variables to be included as a pre-surgery cognitive profile measure
# using only those that have enough values across subjects (i.e., PST, TMT, TOL, DS, RAVLT)
vars <- c( paste0("tmt_", c("a","b") ), paste0("ds_", c("f","b") ), paste0( "corsi_", c("f","b") ), # attention and working memory
           "tol_anderson", paste0( "pst_", c("d","w","c") ), # executive functions
           "sim", "vf_animals", # language
           paste0( "ravlt_", c("irs","b","30","drec50","drec15" ) ) # memory
           )

# prepare columns for the battery variables
for ( i in vars ) d0[[i]] <- NA

# run through the REDCap data and fill-in test scores for a pre-surgery battery
for( i in d0$IPN ) for( j in vars ) if( j %in% names(d1) ) d0[ d0$IPN == i , j ] <- d1[ d1$study_id == i, j ]

# rename variables in the longitudinal data set wherever needed
d2 <- d2 %>%
  rename( "corsi_f" = "ss_f", "corsi_b" = "ss_b", "tol_anderson" = "tol", "vf_animals" = "cft", "ravlt_irs" = "ravlt_ir", "ravlt_30" = "ravlt_dr", "ravlt_drec50" = "ravlt_rec50", "ravlt_drec15" = "ravlt_rec15" ) %>%
  mutate( sim = sim + 5 ) %>% # "sim" is coded differently in d2 compared to d1, need to add 5 points to make them equivalent
  filter( ass_type == "pre" ) # keep only baseline assessments

# run through d0 and fill-in from d2 what was missing in d1
for( i in d0$IPN ) for( j in vars ) if( (is.na(d0[ d0$IPN == i , j ])) & (i %in% d2$id) & (j %in% names(d2)) ) d0[ d0$IPN == i , j ] <- d2[ d2$id == i, j ]

# print number of missing values per variable
sapply( vars, function(i) sum( is.na(d0[[i]]) ) )

# save as csv
write.table( d0, "data/karsten/dbs_retroMRI_psych_data.csv", sep = ",", quote = F, row.names = F )

# remove the data sets from environment
rm( d0, d1, d2 )


# ---- dataprep for Pavel ----

# set-up a folder for Pavel's data
if ( !dir.exists("data/pavel") ) dir.create("data/pavel")

# read IPNs of patients to be included
d.id <- read.table( "data/dbs_retroMRI_connids.txt", header = T )$id
d0 <- read.csv( "data/dbs_retroMRI_redcap_long.csv", sep = "," ) %>% select( study_id, redcap_event_name, drsii_total ) %>% pivot_wider( names_from = redcap_event_name, values_from = drsii_total, id_cols = study_id )

# prepare a data frame for patients in d.id
d1 <- data.frame( id = d.id, miss = NA, drs_pre = NA )

# fill-in the data
for ( i in d.id ) {
  
  # check whether the patient is in REDCap at all
  if ( !(i %in% d0$study_id) ) {
    
    # if the patient is not present in the REDCap data set print a message and continue to the next one
    print( paste0(i, " ain't in REDCap!") )
    d1[ d1$id == i, "miss" ] <- "nonPD" # checked them, they are in REDCap but are not PD patients
    next
    
  } else {
    
    # if the patient is present in the REDCap data set, fill-in their DRS-2
    # start with pre-surgery assessment
    d1[ d1$id == i, "drs_pre" ] <- d0[ d0$study_id == i, "screening_arm_1" ]
    d1[ d1$id == i, "mci_pre" ] <- c( ifelse( d0[ d0$study_id == i, "screening_arm_1" ] < 140, 1, 0 ) )
    
    # next loop through retests
    for ( j in seq(1,5,2) ) {
      
      d1[ d1$id == i, paste0("drs_r",j) ] <- d0[ d0$study_id == i,  paste0("nvtva_r",j,"_arm_1") ]
      d1[ d1$id == i, paste0("mci_r",j) ] <- c( ifelse( d0[ d0$study_id == i, paste0("nvtva_r",j,"_arm_1") ] < 140, 1, 0 ) )
      
    } 
  }
}

# add columns denoting changes in MCI state (approximated via DRS-2 < 140) in post-surgery assessment compared to baseline
for ( i in paste0( "r", seq(1,5,2) ) ) d1[[ paste0("mci_change_pre2",i) ]] <- d1[["mci_pre"]] - 2 * d1[[ paste0("mci_",i) ]]

# re-code such that the change variables identify the type of change compared to pre-surgery
d1 <- d1 %>%
  mutate_at( vars( contains("change") ), ~ case_when( . == -2 ~ "nc2mci", . == -1 ~ "mci2mci", . == 0 ~ "nc2nc", . == 1 ~ "mci2nc") ) %>%
  mutate( last =  ( 2 * sapply( 1:nrow(.), function(i) !is.na( .[ i, grepl("change", names(.) ) ] ) ) %>% t() %>% apply( ., 1, function(x) max( which( x == T ) ) ) - 1 ) %>% ifelse( . == -Inf, NA, . ) ) %>%
  mutate( mci_change_pre2last = sapply( 1:nrow(.), function(i) ifelse( is.na( .[i,"last"] ) , NA, .[ i, paste0( "mci_change_pre2r",last[i] ) ] ) ) )


# count number of patients at each assessment
# note: pre-arranged the data sets such that all patients with at least one post-surgery assessment have pre-surgery assessment as well
sapply( colnames(d1)[seq(3,9,2)], function(i) sum( !(is.na(d1[[i]]) ) ) )

# check distribution of MCI status changes stratified by years of last visit
with( d1, table( last, mci_change_pre2last) )

# list patients that were not in (PD) REDCap (all of them are in fact present in REDCap, only they're non-PD patients)
nonPD <- d1$id[ d1$miss == "nonPD" ] %>% na.omit() %>% c()

# save as csv
write.table( d1, "data/pavel/dbs_retroMRI_drs_data.csv", sep = ",", quote = F, row.names = F )

