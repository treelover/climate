#LAST MODIFIED: 09/26/2014
#AUTHOR: ADAM SIBLEY
#DESCRIPTION: This code reads in the HIPPNET veg resurvey files and assigns sensible classes to the columns. 

#filename = '/home/adam/Dropbox/sapflow/veg/lau/LAU_resurvey_2010_v03_JD.xlsx'

#read the data in
dat = read.xls(xls = filename, sheet = 1)

#Column names
cn = colnames(dat)

#Test to make sure that the headers in this file are the same as the file that this script was based off of (lau_2010 sheet
#shown in commented-out 'filename'

testheads = c("ID","Old_TAG","QUAD","SUBQUAD","POM",         
"Re_TAG","Species","DTN","NOTES","S_L_P",       
"SUBSTRATE","MS_1","MS_2","MS_3","MS_4",        
"MS_5","MS_6","MS_7","MS_8","MS_9",        
"MS_10","MS_11","MS_12","NUM_OF_Stems","MS_POM_1",    
"MS_POM_2","MS_POM_3","MS_POM_4","MS_POM_5","MS_POM_6",    
"MS_POM_7","MS_POM_10","MS_POM_8","MS_POM_9","MS_POM_11",  
"DBH","Status","X","Y","Date_",       
"Shape")    

if(all(testheads == cn)){print('Col names OK')}else{print('Col names DO NOT JIVE')}

#Assigning classes to columns
dat$ID = as.numeric(dat$ID)
dat$Old_TAG = as.numeric(dat$Old_TAG)
dat$QUAD = sprintf(fmt = '%04d',dat$QUAD)
dat$POM = as.numeric(dat$POM)
dat$Re_TAG = as.numeric(dat$Re_TAG)
dat$DTN = as.numeric(dat$DTN)
dat$MS_1 = as.numeric(dat$MS_1)
dat$MS_2 = as.numeric(dat$MS_2)
dat$MS_3 = as.numeric(dat$MS_3)
dat$MS_4 = as.numeric(dat$MS_4)
dat$MS_5 = as.numeric(dat$MS_5)
dat$MS_6 = as.numeric(dat$MS_6)
dat$MS_7 = as.numeric(dat$MS_7)
dat$MS_8 = as.numeric(dat$MS_8)
dat$MS_9 = as.numeric(dat$MS_9)
dat$MS_10 = as.numeric(dat$MS_10)
dat$MS_11 = as.numeric(dat$MS_11)
dat$MS_12 = as.numeric(dat$MS_12)
dat$NUM_OF_Stems = as.numeric(dat$NUM_OF_Stems)
dat$MS_POM_1 = as.numeric(dat$MS_POM_1)
dat$MS_POM_2 = as.numeric(dat$MS_POM_2)
dat$MS_POM_3 = as.numeric(dat$MS_POM_3)
dat$MS_POM_4 = as.numeric(dat$MS_POM_4)
dat$MS_POM_5 = as.numeric(dat$MS_POM_5)
dat$MS_POM_6 = as.numeric(dat$MS_POM_6)
dat$MS_POM_7 = as.numeric(dat$MS_POM_7)
dat$MS_POM_8 = as.numeric(dat$MS_POM_8)
dat$MS_POM_9 = as.numeric(dat$MS_POM_9)
dat$MS_POM_10 = as.numeric(dat$MS_POM_10)
dat$MS_POM_11 = as.numeric(dat$MS_POM_11)
dat$MS_POM_12 = as.numeric(dat$MS_POM_12)
dat$DBH = as.numeric(dat$DBH)
dat$X = as.numeric(dat$X)
dat$Y = as.numeric(dat$Y)
dat$Date_ = as.POSIXlt(dat$Date_)

return(dat)
}



