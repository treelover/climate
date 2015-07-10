#LAST MODIFIED: 11/17/2014
#AUTHOR: ADAM SIBLEY
#DESCRIPTION: This code reads in the HIPPNET veg resurvey files and assigns sensible classes to the columns. 
read_lau_2013 = function(filename){
#filename = '/home/adam/Dropbox/sapflow/veg/lau/LAU_resurvey_2012_v02_clim_EDITED.csv'

#read the data in
#dat = read.xls(xls = filename, sheet = 1)
dat = read.csv(filename)
#Column names
cn = colnames(dat)

#Test to make sure that the headers in this file are the same as the file that this script was based off of (lau_2011 sheet
#shown in commented-out 'filename'
testheads = c('ID_2','Old_TAG_2','QUAD_2','SUBQUAD_2','POM_2','RE_TAG_2',
'Species_2','DTN_2','NOTES','S_L_P_2','SUBSTRATE_2','MS_1_2','MS_2_2',
'MS_3_2','MS_4_2','MS_5_2','MS_6_2','MS_7_2','MS_10_2','MS_8_2',
'MS_9_2','MS_11_2','MS_12_2','NUM_OF_Stems_2','MS_POM_1_2','MS_POM_2_2',
'MS_POM_3_2','MS_POM_4_2','MS_POM_5_2','MS_POM_6_2','MS_POM_7_2',
'MS_POM_10_2','MS_POM_8_2','MS_POM_9_2','MS_POM_11_2','DBH_2','Status_2',
'X_2','Y_2','DATE_2','OBJECTID','SHAPE','QUAD_1','SUBQUAD_1','TAG_1',
'SPECIES_1','DBH_1','POM_1','DTN_1','S_L_P_1','MAIN_STEM_1','SAMPLE',
'PROBLEM','Prob_Fixed','SUBSTRATE_1','MEAS_NOTES_1','DATE_1',
'Num_of_stems_1','MStem_1_1','MStem_2_1','MStem_3_1','MStem_4_1',
'MStem_5_1','MStem_6_1','MStem_7_1','MStem_8_1','MStem_9_1','MStem_10_1',
'MStem_1_POM_1','MStem_2_POM_1','MStem_3_POM_1','MStem_4_POM_1',
'MStem_5_POM_1','MStem_6_POM_1','MStem_7_POM_1','MStem_8_POM_1',
'MStem_9_POM_1','MStem_10_POM_1','RuleID','X_1','Y_1','Growth?',
'Days_diff','DBH_diff','cm/day','cm_yr','high pig damage')

if(any(match(testheads,cn)) == F){print('Col names DO NOT JIVE')}else{print('Col names OK')}

#Assigning classes to columns - Later year of data
#dat$ID_2 = as.numeric(dat$ID_2)
#dat$Old_TAG_2 = as.numeric(dat$Old_TAG_2)
#dat$QUAD_2 = sprintf(fmt = '%04d',dat$QUAD_2)
#dat$POM_2 = as.numeric(dat$POM_2)
#dat$RE_TAG_2 = as.numeric(dat$RE_TAG_2)
#dat$DTN_2 = as.numeric(dat$DTN_2)
#dat$MS_1_2 = as.numeric(dat$MS_1_2)
#dat$MS_2_2 = as.numeric(dat$MS_2_2)
#dat$MS_3_2 = as.numeric(dat$MS_3_2)
#dat$MS_4_2 = as.numeric(dat$MS_4_2)
#dat$MS_5_2 = as.numeric(dat$MS_5_2)
#dat$MS_6_2 = as.numeric(dat$MS_6_2)
#dat$MS_7_2 = as.numeric(dat$MS_7_2)
#dat$MS_8_2 = as.numeric(dat$MS_8_2)
#dat$MS_9_2 = as.numeric(dat$MS_9_2)
#dat$MS_10_2 = as.numeric(dat$MS_10_2)
#dat$MS_11_2 = as.numeric(dat$MS_11_2)
#dat$NUM_OF_stems_2 = as.numeric(dat$NUM_OF_stems_2)
#dat$MS_POM_1_2 = as.numeric(dat$MS_POM_1_2)
#dat$MS_POM_2_2 = as.numeric(dat$MS_POM_2_2)
#dat$MS_POM_3_2 = as.numeric(dat$MS_POM_3_2)
#dat$MS_POM_4_2 = as.numeric(dat$MS_POM_4_2)
#dat$MS_POM_5_2 = as.numeric(dat$MS_POM_5_2)
#dat$MS_POM_6_2 = as.numeric(dat$MS_POM_6_2)
#dat$MS_POM_7_2 = as.numeric(dat$MS_POM_7_2)
#dat$MS_POM_8_2 = as.numeric(dat$MS_POM_8_2)
#dat$MS_POM_9_2 = as.numeric(dat$MS_POM_9_2)
#dat$MS_POM_10_2 = as.numeric(dat$MS_POM_10_2)
#dat$MS_POM_11_2 = as.numeric(dat$MS_POM_11_2)
dat$DBH_2 = as.numeric(dat$DBH_2)
dat$TAG_2 = as.numeric(dat$TAG_2)
#dat$X_2 = as.numeric(dat$X_2)
#dat$Y_2 = as.numeric(dat$Y_2)
dat$DATE_2 = as.POSIXlt(as.character(dat$DATE_2),format = '%Y-%m-%d')

#Assigning classes to columns - Earlier year of data
#dat$ID_1 = as.numeric(dat$ID_1)
#dat$Old_TAG_1 = as.numeric(dat$Old_TAG_1)
#dat$QUAD_1 = sprintf(fmt = '%04d',dat$QUAD_1)
#dat$POM_1 = as.numeric(dat$POM_1)
#dat$TAG_1 = as.numeric(dat$TAG_1)
#dat$DTN_1 = as.numeric(dat$DTN_1)
#dat$MS_1_1 = as.numeric(dat$MS_1_1)
#dat$MS_2_1 = as.numeric(dat$MS_2_1)
#dat$MS_3_1 = as.numeric(dat$MS_3_1)
#dat$MS_4_1 = as.numeric(dat$MS_4_1)
#dat$MS_5_1 = as.numeric(dat$MS_5_1)
#dat$MS_6_1 = as.numeric(dat$MS_6_1)
#dat$MS_7_1 = as.numeric(dat$MS_7_1)
#dat$MS_8_1 = as.numeric(dat$MS_8_1)
#dat$MS_9_1 = as.numeric(dat$MS_9_1)
#dat$MS_10_1 = as.numeric(dat$MS_10_1)
#dat$MS_11_1 = as.numeric(dat$MS_11_1)
#dat$NUM_OF_Stems_1 = as.numeric(dat$NUM_OF_Stems_1)
#dat$MS_POM_1_1 = as.numeric(dat$MS_POM_1_1)
#dat$MS_POM_2_1 = as.numeric(dat$MS_POM_2_1)
#dat$MS_POM_3_1 = as.numeric(dat$MS_POM_3_1)
#dat$MS_POM_4_1 = as.numeric(dat$MS_POM_4_1)
#dat$MS_POM_5_1 = as.numeric(dat$MS_POM_5_1)
#dat$MS_POM_6_1 = as.numeric(dat$MS_POM_6_1)
#dat$MS_POM_7_1 = as.numeric(dat$MS_POM_7_1)
#dat$MS_POM_8_1 = as.numeric(dat$MS_POM_8_1)
#dat$MS_POM_9_1 = as.numeric(dat$MS_POM_9_1)
#dat$MS_POM_10_1 = as.numeric(dat$MS_POM_10_1)
#dat$MS_POM_11_1 = as.numeric(dat$MS_POM_11_1)
dat$DBH_1 = as.numeric(dat$DBH_1)
dat$X_1 = as.numeric(dat$X_1)
dat$Y_1 = as.numeric(dat$Y_1)
dat$DATE_1 = as.POSIXlt(as.character(dat$DATE_1),format = '%Y-%m-%d')

dat[which(dat == -999, arr.ind = T)] = NA

return(dat)
}



