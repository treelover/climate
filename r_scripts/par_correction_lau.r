#This code is meant to find how badly the par sensor at XXX site has drifted. Here is the workflow:
#-Read in the screened climate data
#-Prepare inputs for Eben's function
#-Run Eben's function on all data
#-Use the most conservative threshold possible to filter the data down to clear sky conditions.
#-Clear sky conditions: ratio of observed Downwelling Shortwave to modelled SW_down, via Tom's map.



#Libraries and functions
#---------------------------------------#
source('/home/adam/Dropbox/sapflow/r_scripts/fun/climread.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/PAR_correction_eben.r')
library(WriteXLS)
#---------------------------------------#

#Directories
#---------------------------------------#
cdir = '/home/adam/Dropbox/sapflow/clim_screened/'
destdir = '/home/adam/Dropbox/sapflow/clim/summarized_vars/'
dates = '20140404'
figdir = '/home/adam/Dropbox/sapflow/figs/par/'
#---------------------------------------#

#Reading the files in 
#---------------------------------------#
#Weather data
lau   = read.table(paste(cdir,'Laupahoehoe_screened_',dates,'.dat' ,sep=''),sep=',',header = T)

#Fixing the timestamps
lau[,1] = as.POSIXct(lau[,1])

time = lau[,1]

#Modeled clear sky radiation
clear = read.table('/home/adam/Dropbox/sapflow/clim/clear_sky_rad/laupahoehoe.csv',sep=',',header = T)
colnames(clear)[2:16] = 6.5:20.5
#---------------------------------------#


#Laupahoehoe - setting up the inputs to the function
#---------------------------------------#
lau[which(lau[,'SWup'] == -9999),'SWup'] = NA
lau[which(lau[,'PAR'] == -9999),'PAR'] = NA

#Time stuff
time = as.POSIXct(lau[,'yyyy.mm.dd.hh.mm'])
day_of_year = strftime(time,format = '%j')
day_hours = as.numeric(strftime(time,format = '%H'))+(as.numeric(strftime(time,format = '%M'))/60)

#Temperature
lau[which(lau$Tair_1 == -9999),'Tair_1'] = NA
lau[which(lau$Tair_2 == -9999),'Tair_2'] = NA
mid_Ta = rowMeans(cbind(lau$Tair_1,lau$Tair_2),na.rm=T)

#Humidity
lau[which(lau$RH_1 == -9999),'RH_1'] = NA
lau[which(lau$RH_2 == -9999),'RH_2'] = NA
mid_rH = rowMeans(cbind(lau$RH_1,lau$RH_2),na.rm=T)

#Putting the dataframe together
datain = data.frame(day_of_year,day_hours,mid_Ta,mid_rH)

#Some parameters 
D9  = 19.93217                                   # Latitude =	41.7  (from http://itouchmap.com/latlong.html)
D10 = -155.29129                                 # Longitude =	111.8 (from http://itouchmap.com/latlong.html)
D11 = 150                                        # Longitudetz =	105  (150 = Hawaii time zone) (http://clearskycalculator.com/longitudeTZ.htm)
D12 = 1145                                       # Elevation =	1400  (from Google Earth)

#running the function
mdat = PAR_correction_eben(datain,D9,D10,D11,D12)
datain = mdat[[1]]
models_vals = mdat[[2]]


#Making some test data to see why function isn't working
testID = which(substr(time,1,10) == '2009-11-11')
testdat = data.frame(day_of_year[testID],day_hours[testID],mid_Ta[testID],mid_rH[testID])
colnames(testdat) = c('day_of_year','day_hours','mid_Ta','mid_rH')
testrun = PAR_correction_eben(testdat,D9,D10,D11,D12)
#---------------------------------------#

#Computing cloudiness schmowdiness	
#---------------------------------------#
#Turning clear into 10 minute incriments
interp = apply(X = clear[,2:16],1,approx, x = 6.5:20.5,xout = seq(6.5,20.5,by = 1/6))

#Making the resulting list into a matrix
xvals = interp[[1]]$x

yvals = unlist(interp)
yvals = yvals[which(substr(names(yvals),1,1) == 'y')]

yvals = matrix(data = yvals, ncol = length(xvals),nrow = 13,byrow = TRUE)
yvals = yvals[1:12,]
colnames(yvals) = xvals

#Adding the january row on to the bottom to help with the interps
yvals = rbind(yvals,yvals[1,]) 

#Now making a matrix for daily clear sky data: interpolating the monthly data to daily now.
clearday = matrix(NA,ncol = ncol(yvals),nrow = 365)

#List of month midpoints
momid = 0                                          #January
momid[length(momid)+1] = momid[length(momid)]+31   #Feb.
momid[length(momid)+1] = momid[length(momid)]+28   #Mar.
momid[length(momid)+1] = momid[length(momid)]+31   #Apr.
momid[length(momid)+1] = momid[length(momid)]+30   #May.
momid[length(momid)+1] = momid[length(momid)]+31   #Jun.
momid[length(momid)+1] = momid[length(momid)]+30   #Jul.
momid[length(momid)+1] = momid[length(momid)]+31   #Aug.
momid[length(momid)+1] = momid[length(momid)]+31   #Sep.
momid[length(momid)+1] = momid[length(momid)]+30   #Oct.
momid[length(momid)+1] = momid[length(momid)]+31   #Nov.
momid[length(momid)+1] = momid[length(momid)]+30   #Dec.
momid = momid+15
momid[length(momid)+1] = momid[length(momid)]+31   #One more, to loop around with

#Looping through the times of day
for(i in 1:ncol(clearday)){

	tmp = approx(x = momid,y = yvals[,i],xout = momid[1]:tail(momid,1))
	tmp = c(tail(tmp$y,15),head(tmp$y,(365-15)))
	
	clearday[,i] = tmp
	}	
#Done
#---------------------------------------#


#Making a vector of clear sky radiation for each observation
#---------------------------------------#
hrID = substr(seq(as.POSIXlt('06:30:00',format = '%H:%M:%S'),as.POSIXlt('20:30:00',format = '%H:%M:%S'),by=60*10),12,19)

#Day of year of climate observations
doyvec = as.numeric(strftime(time,format='%j'))
hrvec = substr(time,12,19)

#Vector to store loop results in
clearvec = matrix(NA,ncol=1,nrow = nrow(lau))

for(i in 1:length(clearvec)){

	cID = which(hrID == hrvec[i])
	if(length(cID) == 0){next}
	
	clearvec[i] = clearday[doyvec[i],cID]
	
}
#---------------------------------------#	


#Ratio of clear sky radiation to actual radiation
#---------------------------------------#	
clearness = lau[,'SWup']/clearvec
lau = cbind(lau,clearness,datain$Clearsky_PAR_modeled,datain$Clearsky_SW_modeled)
colnames(lau)[(length(colnames(lau))-1):length(colnames(lau))] = c('modeled_PAR','modeled_SW')

#Pulling out the stuff between 10am and 2pm 
noonID = as.numeric(substr(time,12,13))
noonID = which(noonID >= 10 & noonID <= 14)

driftdat = lau[noonID,]

driftdat = driftdat[which(driftdat$clearness >= 0.9),]

caldates = as.POSIXlt(c('2009-05-12','2010-05-27','2011-05-31'))

#Make a "sincecal" vector
sincecal = matrix(NA,ncol = 3, nrow = nrow(driftdat))
sincecal[,1] = difftime(driftdat$yyyy.mm.dd.hh.mm,caldates[1],units = 'days')
sincecal[,2] = difftime(driftdat$yyyy.mm.dd.hh.mm,caldates[2],units = 'days')
sincecal[,3] = difftime(driftdat$yyyy.mm.dd.hh.mm,caldates[3],units = 'days')
sincecal[which(sincecal < 0)] = NA
sincecal = apply(X = sincecal,1,min,na.rm=T)

#Add it to the driftdat
driftdat = cbind(driftdat,sincecal)

plot(x = driftdat$sincecal, y = driftdat$PAR-driftdat$modeled_PAR)

#---------------------------------------#	

