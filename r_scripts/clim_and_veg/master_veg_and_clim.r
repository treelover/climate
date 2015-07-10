#This code reads in the veg data for each site, the climate data for each site, 
#and computes a 6 month average of various climate metrics for each plant.


#Libraries and functions
#---------------------------------------#
source('/home/adam/Dropbox/sapflow/r_scripts/fun/climread.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read_lau_2010.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read_lau_2011.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read_lau_2012.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read_lau_2013.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read_palam_2010.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read_pww_2013.r')
library(WriteXLS)
#---------------------------------------#

#Directories
#---------------------------------------#
cdir = '/home/adam/Dropbox/sapflow/clim_screened/'
dates = '20140404'
#---------------------------------------#

#Interval over which to aggregate data
#---------------------------------------#
per = (60*60*24*30)*6
#---------------------------------------#

#Percentage of data that can still be NA and give a valid number
#---------------------------------------#
nathresh = 0.3
#---------------------------------------#

#Load up the climate data
#---------------------------------------#
load(paste(cdir,'all_stations_',dates,'.Rdat',sep=''))
#---------------------------------------#

#LAUPAHOEHOE
#---------------------------------------#
#Pulling out the climate data for this location
clim = slist[['lau']]   

#Veg data
vdir = '/home/adam/Dropbox/sapflow/veg/lau/'
veg10 = read_lau_2010(paste(vdir,'LAU_resurvey_2010_v03_clim_rev_EDITED.csv',sep=''))
veg11 = read_lau_2011(paste(vdir,'LAU_resurvey_2011_v03_clim_rev_EDITED.csv',sep=''))
veg12 = read_lau_2012(paste(vdir,'LAU_resurvey_2012_v02_clim_rev_EDITED.csv',sep=''))
veg13 = read_lau_2013(paste(vdir,'LAU_2013-2014_clim_rev_EDITED.csv',sep=''))

#Taking out the few entries in veg13 that are in 2014
veg13 = veg13[-which(substr(veg13$DATE_2,1,4) == '2014'),]

#Getting rid of entries where there's "no growth", or rather growth could not be calculated
veg10 = veg10[-which(veg10$Growth. == 'no'),]
veg11 = veg11[-which(veg11$Growth. == 'no'),]
veg12 = veg12[-which(veg12$Growth. == 'no'),]
veg13 = veg13[-which(veg13$Growth. == 'no'),]

#Identifying trees common to 2011 and 2010
vegmatch = match(veg11$RE_TAG_2,veg10$RE_TAG_2)
vegmatch = vegmatch[-which(is.na(vegmatch) == T)]

#trees common to 2012, too
vegmatch = match(veg10$RE_TAG_2[vegmatch], veg12$RE_TAG_2)
vegmatch = vegmatch[-which(is.na(vegmatch) == T)]

#Oh hell might as well do 2013 too
vegmatch = match(veg12$RE_TAG_2[vegmatch], veg13$TAG_2)
vegmatch = vegmatch[-which(is.na(vegmatch) == T)]

#ID of trees that appear in all 3 datasets:
treeID = sort(veg13$TAG_2[vegmatch])

#Ordering the datasets to match each other
veg10 = veg10[match(treeID,veg10$RE_TAG_2),]
veg11 = veg11[match(treeID,veg11$RE_TAG_2),]
veg12 = veg12[match(treeID,veg12$RE_TAG_2),]
veg13 = veg13[match(treeID,veg13$TAG_2),]


#Filling in a data frame with the laupahoehoe vegetation data
#...........................................................#
veg = as.data.frame(matrix(NA,nrow = length(treeID),ncol = 14))
colnames(veg) = c('tag','site','species','dbh','survey_2010','survey_2011','survey_2012','survey_2013','survey_2014','growth_2010','growth_2011','growth_2012','growth_2013','growth_2014')

veg[,'tag'] = treeID
veg[,'site'] = rep('lau',length(treeID))
veg[,'species'] = veg10$Species_2
veg[,'dbh'] = veg12$DBH_2
veg$survey_2010 = veg10$DATE_2
veg$survey_2011 = veg11$DATE_2
veg$survey_2012 = veg12$DATE_2
veg$survey_2013 = veg13$DATE_2
veg$growth_2010 = veg10$cm.day
veg$growth_2011 = veg11$cm.day
veg$growth_2012 = veg12$cm.day
veg$growth_2013 = veg13$cm.day
#...........................................................#

#Filling in a data frame with the laupahoehoe climate data 
#...........................................................#
vegclim = as.data.frame(matrix(NA,nrow = length(treeID),ncol = 40))
colnames(vegclim) = c('Tave_2010','Tave_2011','Tave_2012','Tave_2013','Tave_2014',
                      'Tmin_2010','Tmin_2011','Tmin_2012','Tmin_2013','Tmin_2014',
                      'Tmax_2010','Tmax_2011','Tmax_2012','Tmax_2013','Tmax_2014',
                      'RF_2010','RF_2011','RF_2012','RF_2013','RF_2014',
                      'Cld_2010','Cld_2011','Cld_2012','Cld_2013','Cld_2014',
                      'SW_2010','SW_2011','SW_2012','SW_2013','SW_2014',
                      'SM_2010','SM_2011','SM_2012','SM_2013','SM_2014',
                      'WS_2010','WS_2011','WS_2012','WS_2013','WS_2014')
                      
#Factor of days and a vector of unique days
dayfac = substr(clim$yyyy.mm.dd.hh.mm,1,10)
dayuni = unique(substr(clim$yyyy.mm.dd.hh.mm,1,10))                             
   
#Pulling out some climate variables               
Tair = rowMeans(clim[,c('Tair_1','Tair_2')],na.rm=T)
RF = clim$RF
SW = clim$SWup
SM = rowMeans(cbind(clim$SM_1,clim$SM_2,clim$SM_3),na.rm=T)
WS = clim$WS	
daymins = tapply(Tair,dayfac,min)
daymaxs = tapply(Tair,dayfac,max)
                     
for(i in 1:nrow(veg)){

	#Coming up with the aggregation window for each year
	win2010 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2010[i],'00:00:00')))
	win2010 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2010[i],'00:00:00'))-per):win2010
	
	win2011 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2011[i],'00:00:00')))
	win2011 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2011[i],'00:00:00'))-per):win2011
	
	win2012 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2012[i],'00:00:00')))
	win2012 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2012[i],'00:00:00'))-per):win2012
	
	win2013 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2013[i],'00:00:00')))
	win2013 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2013[i],'00:00:00'))-per):win2013
	
	#Average Temperature
	if(length(which(is.na(Tair[win2010])))/length(win2010) < nathresh){vegclim$Tave_2010[i] = mean(Tair[win2010],na.rm=T)} #Tave 2010
	if(length(which(is.na(Tair[win2011])))/length(win2011) < nathresh){vegclim$Tave_2011[i] = mean(Tair[win2011],na.rm=T)} #Tave 2011
	if(length(which(is.na(Tair[win2012])))/length(win2012) < nathresh){vegclim$Tave_2012[i] = mean(Tair[win2012],na.rm=T)} #Tave 2012
	if(length(which(is.na(Tair[win2013])))/length(win2013) < nathresh){vegclim$Tave_2013[i] = mean(Tair[win2013],na.rm=T)} #Tave 2013

	#Rainfall
	if(length(which(is.na(RF[win2010])))/length(win2010) < nathresh){vegclim$RF_2010[i] = sum(RF[win2010],na.rm=T)/(length(win2010)/(6*24))} #RF 2010
	if(length(which(is.na(RF[win2011])))/length(win2011) < nathresh){vegclim$RF_2011[i] = sum(RF[win2011],na.rm=T)/(length(win2011)/(6*24))} #RF 2011
	if(length(which(is.na(RF[win2012])))/length(win2012) < nathresh){vegclim$RF_2012[i] = sum(RF[win2012],na.rm=T)/(length(win2012)/(6*24))} #RF 2012
	if(length(which(is.na(RF[win2013])))/length(win2013) < nathresh){vegclim$RF_2013[i] = sum(RF[win2013],na.rm=T)/(length(win2013)/(6*24))} #RF 2013
	
	#Shortwave radiation
	if(length(which(is.na(SW[win2010])))/length(win2010) < nathresh){vegclim$SW_2010[i] = mean(SW[win2010],na.rm=T)} #SW 2010
	if(length(which(is.na(SW[win2011])))/length(win2011) < nathresh){vegclim$SW_2011[i] = mean(SW[win2011],na.rm=T)} #SW 2011
	if(length(which(is.na(SW[win2012])))/length(win2012) < nathresh){vegclim$SW_2012[i] = mean(SW[win2012],na.rm=T)} #SW 2012
	if(length(which(is.na(SW[win2013])))/length(win2013) < nathresh){vegclim$SW_2013[i] = mean(SW[win2013],na.rm=T)} #SW 2013
	
	#Soil Moisture
	if(length(which(is.na(SM[win2010])))/length(win2010) < nathresh){vegclim$SM_2010[i] = mean(SM[win2010],na.rm=T)} #SM 2010
	if(length(which(is.na(SM[win2011])))/length(win2011) < nathresh){vegclim$SM_2011[i] = mean(SM[win2011],na.rm=T)} #SM 2011
	if(length(which(is.na(SM[win2012])))/length(win2012) < nathresh){vegclim$SM_2012[i] = mean(SM[win2012],na.rm=T)} #SM 2012
	if(length(which(is.na(SM[win2013])))/length(win2013) < nathresh){vegclim$SM_2013[i] = mean(SM[win2013],na.rm=T)} #SM 2013
	
	#Windspeed
	if(length(which(is.na(WS[win2010])))/length(win2010) < nathresh){vegclim$WS_2010[i] = mean(WS[win2010],na.rm=T)} #WS 2010
	if(length(which(is.na(WS[win2011])))/length(win2011) < nathresh){vegclim$WS_2011[i] = mean(WS[win2011],na.rm=T)} #WS 2011
	if(length(which(is.na(WS[win2012])))/length(win2012) < nathresh){vegclim$WS_2012[i] = mean(WS[win2012],na.rm=T)} #WS 2012
	if(length(which(is.na(WS[win2013])))/length(win2013) < nathresh){vegclim$WS_2013[i] = mean(WS[win2013],na.rm=T)} #WS 2013
	
	#Now the tricky ones: Minimum and maximum temperature
	#+=+=+=+=+=+=+$
	#2010
	if(length(which(is.na(Tair[win2010])))/length(win2010) < nathresh){
	vegclim$Tmin_2010[i] = mean(daymins[which(dayuni == (veg$survey_2010[i]-per)):which(dayuni == veg$survey_2010[i])],na.rm=T)
	vegclim$Tmax_2010[i] = mean(daymaxs[which(dayuni == (veg$survey_2010[i]-per)):which(dayuni == veg$survey_2010[i])],na.rm=T)}
	
	#2011
	if(length(which(is.na(Tair[win2011])))/length(win2011) < nathresh){
	vegclim$Tmin_2011[i] = mean(daymins[which(dayuni == (veg$survey_2011[i]-per)):which(dayuni == veg$survey_2011[i])],na.rm=T)
	vegclim$Tmax_2011[i] = mean(daymaxs[which(dayuni == (veg$survey_2011[i]-per)):which(dayuni == veg$survey_2011[i])],na.rm=T)}
	
	#2012
	if(length(which(is.na(Tair[win2012])))/length(win2012) < nathresh){
	vegclim$Tmin_2012[i] = mean(daymins[which(dayuni == (veg$survey_2012[i]-per)):which(dayuni == veg$survey_2012[i])],na.rm=T)
	vegclim$Tmax_2012[i] = mean(daymaxs[which(dayuni == (veg$survey_2012[i]-per)):which(dayuni == veg$survey_2012[i])],na.rm=T)}
	
	#2013
	if(length(which(is.na(Tair[win2013])))/length(win2013) < nathresh){
	vegclim$Tmin_2013[i] = mean(daymins[which(dayuni == (veg$survey_2013[i]-per)):which(dayuni == veg$survey_2013[i])],na.rm=T)
	vegclim$Tmax_2013[i] = mean(daymaxs[which(dayuni == (veg$survey_2013[i]-per)):which(dayuni == veg$survey_2013[i])],na.rm=T)}
	#+=+=+=+=+=+=+$
	
	#Cloudiness index
	#+=+=+=+=+=+=+$
	cld = clim$Cloud
	dayID = which(clim$DayFlag == 1)
	
	#Only taking the entries in the window that correspond to day
	cldwin2010 = dayID[dayID %in% win2010]
	cldwin2011 = dayID[dayID %in% win2011]
	cldwin2012 = dayID[dayID %in% win2012]
	cldwin2013 = dayID[dayID %in% win2013]
	
	if(length(which(is.na(cld[cldwin2010])))/length(cldwin2010) < nathresh){vegclim$Cld_2010[i] = mean(cld[cldwin2010],na.rm=T)} #cld 2010
	if(length(which(is.na(cld[cldwin2011])))/length(cldwin2011) < nathresh){vegclim$Cld_2011[i] = mean(cld[cldwin2011],na.rm=T)} #cld 2011
	if(length(which(is.na(cld[cldwin2012])))/length(cldwin2012) < nathresh){vegclim$Cld_2012[i] = mean(cld[cldwin2012],na.rm=T)} #cld 2012
	if(length(which(is.na(cld[cldwin2013])))/length(cldwin2013) < nathresh){vegclim$Cld_2013[i] = mean(cld[cldwin2013],na.rm=T)} #cld 2013
	#+=+=+=+=+=+=+$
	
	if(i/100 == round(i/100,0)){print(paste(i,'trees done'))}
}
#...........................................................#

#LAUPAHOEHOE DONE! STICK THE CLIM MATRIX TO THE VEG MATRIX
laumasta = cbind(veg,vegclim)

#write it out for its own self
WriteXLS(x = 'laumasta',ExcelFileName = paste(vdir,'Lau_master_climveg_6mo.xls',sep=''),AdjWidth = T)
	

#============================================================#
#------------------------------------------------------------#
#............................................................#
#------------------------------------------------------------#
#============================================================#


#PALAMANUI
#---------------------------------------#
#Pulling out the climate data for this location
load('/home/adam/Dropbox/sapflow/veg/palam/palam_climdata.Rdat')
clim = final 

#Veg data
vdir = '/home/adam/Dropbox/sapflow/veg/palam/'
veg10 = read_palam_2010(paste(vdir,'PN_resurvey_2010_v01_clim_rev_EDITED.csv',sep=''))
veg11 = read_palam_2010(paste(vdir,'PN_resurvey_2011_v03_clim_EDITED.csv',sep=''))
veg12 = read_palam_2010(paste(vdir,'PN_resurvey_2012_v02_clim_EDITED2.csv',sep=''))
veg13 = read_palam_2010(paste(vdir,'PN_2013-2014_clim_EDITED.csv',sep=''))

#Getting rid of entries where there's "no growth", or rather growth could not be calculated
veg10 = veg10[-which(veg10$Growth. == 'no'),]
veg11 = veg11[-which(veg11$Growth. == 'no'),]
veg12 = veg12[-which(veg12$Growth. == 'no'),]
veg13 = veg13[-which(veg13$Growth. == 'no'),]

#Identifying trees common to 2011 and 2010
vegmatch = match(veg11$TAG_2,veg10$TAG_2)
vegmatch = vegmatch[-which(is.na(vegmatch) == T)]

#trees common to 2012, too
vegmatch = match(veg10$TAG_2[vegmatch], veg12$TAG_2)
vegmatch = vegmatch[-which(is.na(vegmatch) == T)]

#trees common to 2013, too
vegmatch = match(veg12$TAG_2[vegmatch], veg13$TAG_1)
vegmatch = vegmatch[-which(is.na(vegmatch) == T)]

#ID of trees that appear in all 3 datasets:
treeID = sort(veg13$TAG_1[vegmatch])

#Ordering the datasets to match each other
veg10 = veg10[match(treeID,veg10$TAG_2),]
veg11 = veg11[match(treeID,veg11$TAG_2),]
veg12 = veg12[match(treeID,veg12$TAG_2),]
veg13 = veg13[match(treeID,veg13$TAG_1),]

#Filling in a data frame with the Palamanui vegetation data
#...........................................................#
veg = as.data.frame(matrix(NA,nrow = length(treeID),ncol = 14))
colnames(veg) = c('tag','site','species','dbh','survey_2010','survey_2011','survey_2012','survey_2013','survey_2014','growth_2010','growth_2011','growth_2012','growth_2013','growth_2014')
veg[,'tag'] = treeID
veg[,'site'] = rep('pal',length(treeID))
veg[,'species'] = veg10$SPECIES_2
veg[,'dbh'] = veg12$DBH_2
veg$survey_2010 = veg10$DATE_2
veg$survey_2011 = veg11$DATE_2
veg$survey_2012 = veg12$DATE_2
veg$survey_2013 = veg13$DATE_2
veg$growth_2010 = veg10$cm.day
veg$growth_2011 = veg11$cm.day
veg$growth_2012 = veg12$cm.day
veg$growth_2013 = veg13$cm.day
#...........................................................#

#Filling in a data frame with the Palamanui climate data 
#...........................................................#
vegclim = as.data.frame(matrix(NA,nrow = length(treeID),ncol = 40))
colnames(vegclim) = c('Tave_2010','Tave_2011','Tave_2012','Tave_2013','Tave_2014',
                      'Tmin_2010','Tmin_2011','Tmin_2012','Tmin_2013','Tmin_2014',
                      'Tmax_2010','Tmax_2011','Tmax_2012','Tmax_2013','Tmax_2014',
                      'RF_2010','RF_2011','RF_2012','RF_2013','RF_2014',
                      'Cld_2010','Cld_2011','Cld_2012','Cld_2013','Cld_2014',
                      'SW_2010','SW_2011','SW_2012','SW_2013','SW_2014',
                      'SM_2010','SM_2011','SM_2012','SM_2013','SM_2014',
                      'WS_2010','WS_2011','WS_2012','WS_2013','WS_2014')
                      
#Factor of days and a vector of unique days
dayfac = substr(clim$yyyy.mm.dd.hh.mm,1,10)
dayuni = unique(substr(clim$yyyy.mm.dd.hh.mm,1,10))      

#Pulling out some climate variables
Tair = rowMeans(clim[,c('Tair_1','Tair_2')],na.rm=T)                       
RF = clim$RF     
SW = clim$SWup
SM = rowMeans(cbind(clim$SM_1,clim$SM_2,clim$SM_3),na.rm=T)
WS = clim$WS
daymins = tapply(Tair,dayfac,min)
daymaxs = tapply(Tair,dayfac,max)
cld = clim$Cloud
dayID = which(clim$DayFlag == 1)
                 
for(i in 1:nrow(veg)){

	#Coming up with the aggregation window for each year
	win2010 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2010[i],'00:00:00')))
	win2010 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2010[i],'00:00:00'))-per):win2010
	
	win2011 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2011[i],'00:00:00')))
	win2011 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2011[i],'00:00:00'))-per):win2011
	
	win2012 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2012[i],'00:00:00')))
	win2012 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2012[i],'00:00:00'))-per):win2012
	
	win2013 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2013[i],'00:00:00')))
	win2013 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2013[i],'00:00:00'))-per):win2013
	
	#Average Temperature
	if(length(which(is.na(Tair[win2010])))/length(win2010) < nathresh){vegclim$Tave_2010[i] = mean(Tair[win2010],na.rm=T)} #Tave 2010
	if(length(which(is.na(Tair[win2011])))/length(win2011) < nathresh){vegclim$Tave_2011[i] = mean(Tair[win2011],na.rm=T)} #Tave 2011
	if(length(which(is.na(Tair[win2012])))/length(win2012) < nathresh){vegclim$Tave_2012[i] = mean(Tair[win2012],na.rm=T)} #Tave 2012
	if(length(which(is.na(Tair[win2013])))/length(win2013) < nathresh){vegclim$Tave_2013[i] = mean(Tair[win2013],na.rm=T)} #Tave 2013

	#Rainfall
	if(length(which(is.na(RF[win2010])))/length(win2010) < nathresh){vegclim$RF_2010[i] = sum(RF[win2010],na.rm=T)/(length(win2010)/(6*24))} #RF 2010
	if(length(which(is.na(RF[win2011])))/length(win2011) < nathresh){vegclim$RF_2011[i] = sum(RF[win2011],na.rm=T)/(length(win2011)/(6*24))} #RF 2011
	if(length(which(is.na(RF[win2012])))/length(win2012) < nathresh){vegclim$RF_2012[i] = sum(RF[win2012],na.rm=T)/(length(win2012)/(6*24))} #RF 2012
	if(length(which(is.na(RF[win2013])))/length(win2013) < nathresh){vegclim$RF_2013[i] = sum(RF[win2013],na.rm=T)/(length(win2013)/(6*24))} #RF 2013
	
	#Shortwave radiation
	if(length(which(is.na(SW[win2010])))/length(win2010) < nathresh){vegclim$SW_2010[i] = mean(SW[win2010],na.rm=T)} #SW 2010
	if(length(which(is.na(SW[win2011])))/length(win2011) < nathresh){vegclim$SW_2011[i] = mean(SW[win2011],na.rm=T)} #SW 2011
	if(length(which(is.na(SW[win2012])))/length(win2012) < nathresh){vegclim$SW_2012[i] = mean(SW[win2012],na.rm=T)} #SW 2012
	if(length(which(is.na(SW[win2013])))/length(win2013) < nathresh){vegclim$SW_2013[i] = mean(SW[win2013],na.rm=T)} #SW 2013
	
	#Soil Moisture
	if(length(which(is.na(SM[win2010])))/length(win2010) < nathresh){vegclim$SM_2010[i] = mean(SM[win2010],na.rm=T)} #SM 2010
	if(length(which(is.na(SM[win2011])))/length(win2011) < nathresh){vegclim$SM_2011[i] = mean(SM[win2011],na.rm=T)} #SM 2011
	if(length(which(is.na(SM[win2012])))/length(win2012) < nathresh){vegclim$SM_2012[i] = mean(SM[win2012],na.rm=T)} #SM 2012
	if(length(which(is.na(SM[win2013])))/length(win2013) < nathresh){vegclim$SM_2013[i] = mean(SM[win2013],na.rm=T)} #SM 2013
	
	#Windspeed
	if(length(which(is.na(WS[win2010])))/length(win2010) < nathresh){vegclim$WS_2010[i] = mean(WS[win2010],na.rm=T)} #WS 2010
	if(length(which(is.na(WS[win2011])))/length(win2011) < nathresh){vegclim$WS_2011[i] = mean(WS[win2011],na.rm=T)} #WS 2011
	if(length(which(is.na(WS[win2012])))/length(win2012) < nathresh){vegclim$WS_2012[i] = mean(WS[win2012],na.rm=T)} #WS 2012
	if(length(which(is.na(WS[win2013])))/length(win2013) < nathresh){vegclim$WS_2013[i] = mean(WS[win2013],na.rm=T)} #WS 2013
	
	#Now the tricky ones: Minimum and maximum temperature
	#+=+=+=+=+=+=+$
	#2010
	if(length(which(is.na(Tair[win2010])))/length(win2010) < nathresh){
	vegclim$Tmin_2010[i] = mean(daymins[which(dayuni == (veg$survey_2010[i]-per)):which(dayuni == veg$survey_2010[i])],na.rm=T)
	vegclim$Tmax_2010[i] = mean(daymaxs[which(dayuni == (veg$survey_2010[i]-per)):which(dayuni == veg$survey_2010[i])],na.rm=T)}
	
	#2011
	if(length(which(is.na(Tair[win2011])))/length(win2011) < nathresh){
	vegclim$Tmin_2011[i] = mean(daymins[which(dayuni == (veg$survey_2011[i]-per)):which(dayuni == veg$survey_2011[i])],na.rm=T)
	vegclim$Tmax_2011[i] = mean(daymaxs[which(dayuni == (veg$survey_2011[i]-per)):which(dayuni == veg$survey_2011[i])],na.rm=T)}
	
	#2012
	if(length(which(is.na(Tair[win2012])))/length(win2012) < nathresh){
	vegclim$Tmin_2012[i] = mean(daymins[which(dayuni == (veg$survey_2012[i]-per)):which(dayuni == veg$survey_2012[i])],na.rm=T)
	vegclim$Tmax_2012[i] = mean(daymaxs[which(dayuni == (veg$survey_2012[i]-per)):which(dayuni == veg$survey_2012[i])],na.rm=T)}
	
	#2013
	if(length(which(is.na(Tair[win2013])))/length(win2013) < nathresh){
	vegclim$Tmin_2013[i] = mean(daymins[which(dayuni == (veg$survey_2013[i]-per)):which(dayuni == veg$survey_2013[i])],na.rm=T)
	vegclim$Tmax_2013[i] = mean(daymaxs[which(dayuni == (veg$survey_2013[i]-per)):which(dayuni == veg$survey_2013[i])],na.rm=T)}
	#+=+=+=+=+=+=+$
	
	#Cloudiness index
	#+=+=+=+=+=+=+$
	cld = clim$Cloud
	dayID = which(clim$DayFlag == 1)
	
	#Only taking the entries in the window that correspond to day
	cldwin2010 = dayID[dayID %in% win2010]
	cldwin2011 = dayID[dayID %in% win2011]
	cldwin2012 = dayID[dayID %in% win2012]
	cldwin2013 = dayID[dayID %in% win2013]
	
	if(length(which(is.na(cld[cldwin2010])))/length(cldwin2010) < nathresh){vegclim$Cld_2010[i] = mean(cld[cldwin2010],na.rm=T)} #cld 2010
	if(length(which(is.na(cld[cldwin2011])))/length(cldwin2011) < nathresh){vegclim$Cld_2011[i] = mean(cld[cldwin2011],na.rm=T)} #cld 2011
	if(length(which(is.na(cld[cldwin2012])))/length(cldwin2012) < nathresh){vegclim$Cld_2012[i] = mean(cld[cldwin2012],na.rm=T)} #cld 2012
	if(length(which(is.na(cld[cldwin2013])))/length(cldwin2013) < nathresh){vegclim$Cld_2013[i] = mean(cld[cldwin2013],na.rm=T)} #cld 2013
	#+=+=+=+=+=+=+$
	
	if(i/100 == round(i/100,0)){print(paste(i,'trees done'))}
}
#...........................................................#

#PALAMANUI DONE! STICK THE CLIM MATRIX TO THE VEG MATRIX
palmasta = cbind(veg,vegclim)

#write it out for its own self
WriteXLS(x = 'palmasta',ExcelFileName = paste(vdir,'Pal_master_climveg_6mo.xls',sep=''),AdjWidth = T)
	

masta = rbind(laumasta,palmasta)
#WriteXLS(x = 'temp',ExcelFileName = paste('/home/adam/Dropbox/sapflow/veg/master_climveg_6mo.xls',sep=''),AdjWidth = T)
#============================================================#
#------------------------------------------------------------#
#............................................................#
#------------------------------------------------------------#
#============================================================#



#MAMALAHOA
#---------------------------------------#
#Pulling out the climate data for this location
load('/home/adam/Dropbox/sapflow/veg/mam/mam_climdata.Rdat')
clim = final
#clim = slist[['mama']]   

#Veg data
vdir = '/home/adam/Dropbox/sapflow/veg/mam/'
veg12 = read_pww_2013(paste(vdir,'mam_resurvey_2012_v02_clim_EDITED.csv',sep=''))
veg13 = read_pww_2013(paste(vdir,'mam_resurvey_2013_v02_clim_EDITED.csv',sep=''))
veg14 = read_pww_2013(paste(vdir,'mam_resurvey_2014_v01_clim_EDITED.csv',sep=''))

#Getting rid of entries where there's "no growth", or rather growth could not be calculated
veg12 = veg12[-which(veg12$Growth. == 'no'),]
veg13 = veg13[-which(veg13$Growth. == 'no'),]
veg14 = veg14[-which(veg14$Growth == 'no'),]

#Identifying trees common to 2013 and 2012
vegmatch = match(veg13$TAG_2,veg12$TAG_2)
vegmatch = vegmatch[-which(is.na(vegmatch) == T)]

#trees common to 2014, too
vegmatch = match(veg12$TAG_2[vegmatch], veg14$TAG_1)
#vegmatch = vegmatch[-which(is.na(vegmatch) == T)]

#ID of trees that appear in all 3 datasets:
treeID = sort(veg14$TAG_1[vegmatch])

#Ordering the datasets to match each other
veg12 = veg12[match(treeID,veg12$TAG_2),]
veg13 = veg13[match(treeID,veg13$TAG_2),]
veg14 = veg14[match(treeID,veg14$TAG_1),]


#Filling in a data frame with the Mamalahoa vegetation data
#...........................................................#
veg = as.data.frame(matrix(NA,nrow = length(treeID),ncol = 14))
colnames(veg) = c('tag','site','species','dbh','survey_2010','survey_2011','survey_2012','survey_2013','survey_2014','growth_2010','growth_2011','growth_2012','growth_2013','growth_2014')
veg[,'tag'] = treeID
veg[,'site'] = rep('mam',length(treeID))
veg[,'species'] = veg12$SPECIES_2
veg[,'dbh'] = veg14$DBH_2
veg$survey_2012 = veg12$DATE_2
veg$survey_2013 = veg13$DATE_2
veg$survey_2014 = veg14$DATE_2
veg$growth_2012 = veg12$cm.day
veg$growth_2013 = veg13$cm.day
veg$growth_2014 = veg14$cm.day
#...........................................................#

#Filling in a data frame with the Mamalahoa climate data 
#...........................................................#
vegclim = as.data.frame(matrix(NA,nrow = length(treeID),ncol = 40))
colnames(vegclim) = c('Tave_2010','Tave_2011','Tave_2012','Tave_2013','Tave_2014',
                      'Tmin_2010','Tmin_2011','Tmin_2012','Tmin_2013','Tmin_2014',
                      'Tmax_2010','Tmax_2011','Tmax_2012','Tmax_2013','Tmax_2014',
                      'RF_2010','RF_2011','RF_2012','RF_2013','RF_2014',
                      'Cld_2010','Cld_2011','Cld_2012','Cld_2013','Cld_2014',
                      'SW_2010','SW_2011','SW_2012','SW_2013','SW_2014',
                      'SM_2010','SM_2011','SM_2012','SM_2013','SM_2014',
                      'WS_2010','WS_2011','WS_2012','WS_2013','WS_2014')
                      
#Factor of days and a vector of unique days
dayfac = substr(clim$yyyy.mm.dd.hh.mm,1,10)
dayuni = unique(substr(clim$yyyy.mm.dd.hh.mm,1,10))                             
                      
for(i in 1:nrow(veg)){

	#Coming up with the aggregation window for each year. NOTE THAT WIN2012 HAS BEEN MODIFIED 
	win2012 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2012[i],'00:00:00')))
	win2012 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2012[i],'00:00:00'))-per):win2012
	
	win2013 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2013[i],'00:00:00')))
	win2013 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2013[i],'00:00:00'))-per):win2013
	
	win2014 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2014[i],'00:00:00')))
	win2014 = which(clim$yyyy.mm.dd.hh.mm == as.POSIXct(paste(veg$survey_2014[i],'00:00:00'))-per):win2014
	
	#Average Temperature
	Tair = rowMeans(clim[,c('Tair_1','Tair_2')],na.rm=T)
	
	if(length(which(is.na(Tair[win2012])))/length(win2012) < nathresh){vegclim$Tave_2012[i] = mean(Tair[win2012],na.rm=T)} #Tave 2012
	if(length(which(is.na(Tair[win2013])))/length(win2013) < nathresh){vegclim$Tave_2013[i] = mean(Tair[win2013],na.rm=T)} #Tave 2013
	if(length(which(is.na(Tair[win2014])))/length(win2014) < nathresh){vegclim$Tave_2014[i] = mean(Tair[win2014],na.rm=T)} #Tave 2014
	
	#Rainfall
	RF = clim$RF
	
	if(length(which(is.na(RF[win2012])))/length(win2012) < nathresh){vegclim$RF_2012[i] = sum(RF[win2012],na.rm=T)/(length(win2012)/(6*24))} #RF 2012
	if(length(which(is.na(RF[win2013])))/length(win2013) < nathresh){vegclim$RF_2013[i] = sum(RF[win2013],na.rm=T)/(length(win2013)/(6*24))} #RF 2013
	if(length(which(is.na(RF[win2014])))/length(win2014) < nathresh){vegclim$RF_2014[i] = sum(RF[win2014],na.rm=T)/(length(win2014)/(6*24))} #RF 2014
	
	#Shortwave radiation
	SW = clim$SWup
	
	if(length(which(is.na(SW[win2012])))/length(win2012) < nathresh){vegclim$SW_2012[i] = mean(SW[win2012],na.rm=T)} #SW 2012
	if(length(which(is.na(SW[win2013])))/length(win2013) < nathresh){vegclim$SW_2013[i] = mean(SW[win2013],na.rm=T)} #SW 2013
	if(length(which(is.na(SW[win2014])))/length(win2014) < nathresh){vegclim$SW_2014[i] = mean(SW[win2014],na.rm=T)} #SW 2014
	
	#Soil Moisture
	SM = rowMeans(cbind(clim$SM_1,clim$SM_2,clim$SM_3),na.rm=T)
	
	if(length(which(is.na(SM[win2012])))/length(win2012) < nathresh){vegclim$SM_2012[i] = mean(SM[win2012],na.rm=T)} #SM 2012
	if(length(which(is.na(SM[win2013])))/length(win2013) < nathresh){vegclim$SM_2013[i] = mean(SM[win2013],na.rm=T)} #SM 2013
	if(length(which(is.na(SM[win2014])))/length(win2014) < nathresh){vegclim$SM_2014[i] = mean(SM[win2014],na.rm=T)} #SM 2014
	
	#Windspeed
	WS = clim$WS
	
	if(length(which(is.na(WS[win2012])))/length(win2012) < nathresh){vegclim$WS_2012[i] = mean(WS[win2012],na.rm=T)} #WS 2012
	if(length(which(is.na(WS[win2013])))/length(win2013) < nathresh){vegclim$WS_2013[i] = mean(WS[win2013],na.rm=T)} #WS 2013
	if(length(which(is.na(WS[win2014])))/length(win2014) < nathresh){vegclim$WS_2014[i] = mean(WS[win2014],na.rm=T)} #WS 2014
	
	#Now the tricky ones: Minimum and maximum temperature
	#+=+=+=+=+=+=+$
	daymins = tapply(Tair,dayfac,min)
	daymaxs = tapply(Tair,dayfac,max)
	
	#2012
	if(length(which(is.na(Tair[win2012])))/length(win2012) < nathresh){
	vegclim$Tmin_2012[i] = mean(daymins[which(dayuni == (veg$survey_2012[i]-per)):which(dayuni == veg$survey_2012[i])],na.rm=T)
	vegclim$Tmax_2012[i] = mean(daymaxs[which(dayuni == (veg$survey_2012[i]-per)):which(dayuni == veg$survey_2012[i])],na.rm=T)}
	
	#2013
	if(length(which(is.na(Tair[win2013])))/length(win2013) < nathresh){
	vegclim$Tmin_2013[i] = mean(daymins[which(dayuni == (veg$survey_2013[i]-per)):which(dayuni == veg$survey_2013[i])],na.rm=T)
	vegclim$Tmax_2013[i] = mean(daymaxs[which(dayuni == (veg$survey_2013[i]-per)):which(dayuni == veg$survey_2013[i])],na.rm=T)}
	
	#2014
	if(length(which(is.na(Tair[win2014])))/length(win2014) < nathresh){
	vegclim$Tmin_2014[i] = mean(daymins[which(dayuni == (veg$survey_2014[i]-per)):which(dayuni == veg$survey_2014[i])],na.rm=T)
	vegclim$Tmax_2014[i] = mean(daymaxs[which(dayuni == (veg$survey_2014[i]-per)):which(dayuni == veg$survey_2014[i])],na.rm=T)}
	#+=+=+=+=+=+=+$
	
	#Cloudiness index
	#+=+=+=+=+=+=+$
	cld = clim$Cloud
	dayID = which(clim$DayFlag == 1)
	
	#Only taking the entries in the window that correspond to day
	cldwin2012 = dayID[dayID %in% win2012]
	cldwin2013 = dayID[dayID %in% win2013]
	cldwin2014 = dayID[dayID %in% win2014]
	
	if(length(which(is.na(cld[cldwin2012])))/length(cldwin2012) < nathresh){vegclim$Cld_2012[i] = mean(cld[cldwin2012],na.rm=T)} #cld 2012
	if(length(which(is.na(cld[cldwin2013])))/length(cldwin2013) < nathresh){vegclim$Cld_2013[i] = mean(cld[cldwin2013],na.rm=T)} #cld 2013
	if(length(which(is.na(cld[cldwin2014])))/length(cldwin2014) < nathresh){vegclim$Cld_2014[i] = mean(cld[cldwin2014],na.rm=T)} #cld 2014
	#+=+=+=+=+=+=+$
	
	if(i/100 == round(i/100,0)){print(paste(i,'trees done'))}
}
#...........................................................#

#MAMALAHOHO DONE! STICK THE CLIM MATRIX TO THE VEG MATRIX
mamamasta = cbind(veg,vegclim)

#write it out for its own self
WriteXLS(x = 'mamamasta',ExcelFileName = paste(vdir,'Mam_master_climveg_6mo.xls',sep=''),AdjWidth = T)
	
#Fix the masta survey_2014 column 
masta$survey_2014 = as.POSIXlt(masta$survey_2014)

masta = rbind(masta,mamamasta)
WriteXLS(x = 'masta',ExcelFileName = paste('/home/adam/Dropbox/sapflow/veg/master_climveg_6mo.xls',sep=''),AdjWidth = T)
#============================================================#
#------------------------------------------------------------#
#............................................................#
#------------------------------------------------------------#
#============================================================#


#Making some diagnostic figs for the unscreened climate data
t1 = which(clim[,1] == as.POSIXct('2014-04-04 04:10:00'))
t2 = nrow(clim)
unscreened = clim[t1:t2,]

plot(x = unscreened[,1],y = Tair[t1:t2],type = 'l')
savePlot(filename = '/home/adam/Dropbox/sapflow/veg/mam/unfiltered_T_data.jpeg',type='jpeg')

plot(x = unscreened[,1],y = RF[t1:t2],type = 'l')
savePlot(filename = '/home/adam/Dropbox/sapflow/veg/mam/unfiltered_RF_data.jpeg',type='jpeg')

plot(x = unscreened[,1],y = SW[t1:t2],type = 'l')
savePlot(filename = '/home/adam/Dropbox/sapflow/veg/mam/unfiltered_SW_data.jpeg',type='jpeg')

plot(x = unscreened[,1],y = cld[t1:t2],type = 'l')
savePlot(filename = '/home/adam/Dropbox/sapflow/veg/mam/unfiltered_SW_data.jpeg',type='jpeg')







