#LAST MODIFIED: 09/26/2014
#AUTHOR: ADAM SIBLEY
#DESCRIPTION: This code reads in the HIPPNET veg resurvey files and assigns sensible classes to the columns. 
vegread2 = function(filename){
#filename = '/home/adam/Dropbox/sapflow/veg/lau/LAU_resurvey_2011_v03_clim_EDITED.csv'

#read the data in
#dat = read.xls(xls = filename, sheet = 1)
dat = read.csv(filename)
#Column names
cn = colnames(dat)

#Test to make sure that the headers in this file are the same as the file that this script was based off of (lau_2011 sheet
#shown in commented-out 'filename'
testheads = c('ID_2','Old_TAG_2','QUAD_2','SUBQUAD_2','RE_TAG_2','Species_2',
'X_2','Y_2','POM_2','DTN_2','DBH_2','DATE_2','NOTES_2','S_L_P_2',
'NUM_OF_Stems_2','STATUS','SUBSTRATE_2','MS_1_2','MS_2_2','MS_3_2','MS_4_2',
'MS_5_2','MS_6_2','MS_7_2','MS_10_2','MS_8_2','MS_9_2','MS_11_2','MS_POM_1_2',
'MS_POM_2_2','MS_POM_3_2','MS_POM_4_2','MS_POM_5_2','MS_POM_6_2','MS_POM_7_2',
'MS_POM_10_2','MS_POM_8_2','MS_POM_9_2','MS_POM_11_2','ID_1','Old_TAG_1',
'QUAD_1','SUBQUAD_1','POM_1','RE_TAG_1','Species_1','DTN_1','NOTES_1','S_L_P_1',
'SUBSTRATE_1','MS_1_1','MS_2_1','MS_3_1','MS_4_1','MS_5_1','MS_6_1','MS_7_1',
'MS_10_1','MS_8_1','MS_9_1','MS_11_1','MS_12_1','NUM_OF_Stems_1','MS_POM_1',
'MS_POM_2_1','MS_POM_3_1','MS_POM_4_1','MS_POM_5_1','MS_POM_6_1','MS_POM_7_1',
'MS_POM_10_1','MS_POM_8_1','MS_POM_9_1','MS_POM_11_1','DBH_1','Status','X_1',
'Y_1','Date_1','Growth?','Days_diff','DBH_diff','cm/day','cm_yr','high_pig_damage')

if(any(match(testheads,cn)) == F){print('Col names DO NOT JIVE')}else{print('Col names OK')}

#Assigning classes to columns - Later year of data
dat$ID_2 = as.numeric(dat$ID_2)
dat$Old_TAG_2 = as.numeric(dat$Old_TAG_2)
dat$QUAD_2 = sprintf(fmt = '%04d',dat$QUAD_2)
dat$POM_2 = as.numeric(dat$POM_2)
dat$RE_TAG_2 = as.numeric(dat$RE_TAG_2)
dat$DTN_2 = as.numeric(dat$DTN_2)
dat$MS_1_2 = as.numeric(dat$MS_1_2)
dat$MS_2_2 = as.numeric(dat$MS_2_2)
dat$MS_3_2 = as.numeric(dat$MS_3_2)
dat$MS_4_2 = as.numeric(dat$MS_4_2)
dat$MS_5_2 = as.numeric(dat$MS_5_2)
dat$MS_6_2 = as.numeric(dat$MS_6_2)
dat$MS_7_2 = as.numeric(dat$MS_7_2)
dat$MS_8_2 = as.numeric(dat$MS_8_2)
dat$MS_9_2 = as.numeric(dat$MS_9_2)
dat$MS_10_2 = as.numeric(dat$MS_10_2)
dat$MS_11_2 = as.numeric(dat$MS_11_2)
dat$NUM_OF_Stems_2 = as.numeric(dat$NUM_OF_Stems_2)
dat$MS_POM_1_2 = as.numeric(dat$MS_POM_1_2)
dat$MS_POM_2_2 = as.numeric(dat$MS_POM_2_2)
dat$MS_POM_3_2 = as.numeric(dat$MS_POM_3_2)
dat$MS_POM_4_2 = as.numeric(dat$MS_POM_4_2)
dat$MS_POM_5_2 = as.numeric(dat$MS_POM_5_2)
dat$MS_POM_6_2 = as.numeric(dat$MS_POM_6_2)
dat$MS_POM_7_2 = as.numeric(dat$MS_POM_7_2)
dat$MS_POM_8_2 = as.numeric(dat$MS_POM_8_2)
dat$MS_POM_9_2 = as.numeric(dat$MS_POM_9_2)
dat$MS_POM_10_2 = as.numeric(dat$MS_POM_10_2)
dat$MS_POM_11_2 = as.numeric(dat$MS_POM_11_2)
dat$DBH_2 = as.numeric(dat$DBH_2)
dat$X_2 = as.numeric(dat$X_2)
dat$Y_2 = as.numeric(dat$Y_2)
dat$DATE_2 = as.POSIXlt(as.character(dat$DATE_2),format = '%Y-%m-%d')

#Assigning classes to columns - Earlier year of data
dat$ID_1 = as.numeric(dat$ID_1)
dat$Old_TAG_1 = as.numeric(dat$Old_TAG_1)
dat$QUAD_1 = sprintf(fmt = '%04d',dat$QUAD_1)
dat$POM_1 = as.numeric(dat$POM_1)
dat$RE_TAG_1 = as.numeric(dat$RE_TAG_1)
dat$DTN_1 = as.numeric(dat$DTN_1)
dat$MS_1_1 = as.numeric(dat$MS_1_1)
dat$MS_2_1 = as.numeric(dat$MS_2_1)
dat$MS_3_1 = as.numeric(dat$MS_3_1)
dat$MS_4_1 = as.numeric(dat$MS_4_1)
dat$MS_5_1 = as.numeric(dat$MS_5_1)
dat$MS_6_1 = as.numeric(dat$MS_6_1)
dat$MS_7_1 = as.numeric(dat$MS_7_1)
dat$MS_8_1 = as.numeric(dat$MS_8_1)
dat$MS_9_1 = as.numeric(dat$MS_9_1)
dat$MS_10_1 = as.numeric(dat$MS_10_1)
dat$MS_11_1 = as.numeric(dat$MS_11_1)
dat$NUM_OF_Stems_1 = as.numeric(dat$NUM_OF_Stems_1)
dat$MS_POM_1_1 = as.numeric(dat$MS_POM_1_1)
dat$MS_POM_2_1 = as.numeric(dat$MS_POM_2_1)
dat$MS_POM_3_1 = as.numeric(dat$MS_POM_3_1)
dat$MS_POM_4_1 = as.numeric(dat$MS_POM_4_1)
dat$MS_POM_5_1 = as.numeric(dat$MS_POM_5_1)
dat$MS_POM_6_1 = as.numeric(dat$MS_POM_6_1)
dat$MS_POM_7_1 = as.numeric(dat$MS_POM_7_1)
dat$MS_POM_8_1 = as.numeric(dat$MS_POM_8_1)
dat$MS_POM_9_1 = as.numeric(dat$MS_POM_9_1)
dat$MS_POM_10_1 = as.numeric(dat$MS_POM_10_1)
dat$MS_POM_11_1 = as.numeric(dat$MS_POM_11_1)
dat$DBH_1 = as.numeric(dat$DBH_1)
dat$X_1 = as.numeric(dat$X_1)
dat$Y_1 = as.numeric(dat$Y_1)
dat$DATE_1 = as.POSIXlt(as.character(dat$DATE_1),format = '%Y-%m-%d')

return(dat)
}



