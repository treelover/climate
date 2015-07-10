#Reading in all of the screened data files and making summarized data tables for the Beckster. 
#Started July 14th, 2014
#Author: Adam Sibley

#CHECK UP ON WHY LAUPAHOEHOE SOIL MOISTURE MIN and MAX AREN'T WORKING, EVEN THOUGH SOIL MOISTURE MEAN IS

#Libraries and functions
#---------------------------------------#
source('/home/adam/Dropbox/sapflow/r_scripts/fun/climread.r')
library(WriteXLS)
#---------------------------------------#


#Directories
#---------------------------------------#
cdir = '/home/adam/Dropbox/sapflow/clim_screened/'
destdir = '/home/adam/Dropbox/sapflow/clim/summarized_vars/'
dates = '20140404'
#---------------------------------------#


#Station names
#---------------------------------------#
statnames = c('Hakalau','IPIF','Kiholo','Laupahoehoe','Mamalahoa','Palamanui','PuuWaawaa','Spencer')
#---------------------------------------#

#Reading the files in 
#---------------------------------------#
hak   = read.table(paste(cdir,'Hakalau_screened_20140408.dat' ,sep=''),sep=',',header = T)
ipif  = read.table(paste(cdir,'IPIF_screened_',dates,'.dat' ,sep=''),sep=',',header = T)
kiho  = read.table(paste(cdir,'KiholoBay_screened_',dates,'.dat' ,sep=''),sep=',',header = T)
lau   = read.table(paste(cdir,'Laupahoehoe_screened_',dates,'.dat' ,sep=''),sep=',',header = T)
mama  = read.table(paste(cdir,'Mamalahoa_screened_',dates,'.dat' ,sep=''),sep=',',header = T)
palam = read.table(paste(cdir,'Palamanui_screened_',dates,'.dat' ,sep=''),sep=',',header = T)
pww   = read.table(paste(cdir,'Puuwaawaa_screened_',dates,'.dat' ,sep=''),sep=',',header = T)
spe   = read.table(paste(cdir,'Spencer_screened_',dates,'.dat' ,sep=''),sep=',',header = T)

#Fixing the timestamps
hak[,1] = as.POSIXct(hak[,1])
ipif[,1] = as.POSIXct(ipif[,1])
kiho[,1] = as.POSIXct(kiho[,1])
lau[,1] = as.POSIXct(lau[,1])
mama[,1] = as.POSIXct(mama[,1])
palam[,1] = as.POSIXct(palam[,1])
pww[,1] = as.POSIXct(pww[,1])
spe[,1] = as.POSIXct(spe[,1])

#Making a list of all the stations
slist = list(hak,ipif,kiho,lau,mama,palam,pww,spe)
names(slist) = c('hak','ipif','kiho','lau','mama','palam','pww','spe')
#---------------------------------------#

#Calculating daily averages
#---------------------------------------#
daylist = list()

for(i in 1:length(slist)){

	dat = slist[[i]]
	
	#First, change the -9999 to NA
	wonkers <- dat == -9999
	dat[wonkers] = NA
	
	#Making a vector of day of year
	#doy = factor(strftime(dat$yyyy.mm.dd.hh.mm,'%j'))
	
	#OK day of year doesn't fix leap day. Lets make a vector of unique m-d from the timestamp.
	dayfac = sort(unique(substr(dat$yyyy.mm.dd.hh.mm,6,10)))
	
	#Starting the table to store the data in
	daydat = c()
	
	#Loop through each date
	for(j in 1:length(dayfac)){
	
		#ID of date location in table
		dID = which(substr(dat$yyyy.mm.dd.hh.mm,6,10) == dayfac[j])
		tempdat = dat[dID,]
		
		#Year ID
		yID = sort(unique(substr(tempdat$yyyy.mm.dd.hh.mm,1,4)))
		nyear = length(yID)
		
		#A Table to fill with what I'm gonna calculate
		cnames = c('SWup_avg','SWdn_avg','LWup_avg','LWdn_avg','PAR_avg','Tair_min','Tair_avg','Tair_max','RH_min','RH_avg','RH_max','WS_avg','Tsoil_min','Tsoil_avg','Tsoil_max','SM_avg','RF_avg')
		tab = matrix(data = NA, ncol = length(cnames), nrow = nyear)
		colnames(tab) = cnames
	
		#Looping through the years
		for(k in 1:length(yID)){
			tab[k,'SWup_avg']  = mean(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),'SWup'],na.rm=T)
			tab[k,'SWdn_avg']  = mean(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),'SWdn'],na.rm=T)
			tab[k,'LWup_avg']  = mean(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),'LWup'],na.rm=T)
			tab[k,'LWdn_avg']  = mean(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),'LWdn'],na.rm=T)
			tab[k,'PAR_avg']   = mean(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),'PAR'],na.rm=T)
			tab[k,'Tair_min']  = min(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,4) == 'Tair')]),na.rm=T)
			tab[k,'Tair_avg']  = mean(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,4) == 'Tair')]),na.rm=T)
			tab[k,'Tair_max']  = max(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,4) == 'Tair')]),na.rm=T)
			tab[k,'RH_min']    = min(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,2) == 'RH')]),na.rm=T)
			tab[k,'RH_avg']    = mean(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,2) == 'RH')]),na.rm=T)
			tab[k,'RH_max']    = max(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,2) == 'RH')]),na.rm=T)
			tab[k,'WS_avg']    = mean(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),'WS'],na.rm=T)
			tab[k,'Tsoil_min'] = min(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,5) == 'Tsoil')]),na.rm=T)
			tab[k,'Tsoil_avg'] = mean(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,5) == 'Tsoil')]),na.rm=T)
			tab[k,'Tsoil_max'] = max(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,5) == 'Tsoil')]),na.rm=T)
			tab[k,'SM_avg']    = mean(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,2) == 'SM')]),na.rm=T)
			tab[k,'RF_avg']    = sum(as.matrix(tempdat[which(substr(tempdat$yyyy.mm.dd.hh.mm,1,4) == yID[k]),which(substr(colnames(tempdat),1,2) == 'RF')]),na.rm=T)
		}
		
		#Getting rid of infite values
		tab[which(abs(tab) == Inf)] = NA
		
		#Adding todays values to the table
		daydat = rbind(daydat,as.numeric(c(substr(dayfac[j],1,2),substr(dayfac[j],4,5),colMeans(tab,na.rm=T),nyear)))	
		
		#Printing some information
		print(paste(statnames[i],'-',dayfac[j]))
		
	}
	
	#Adding the column names
	colnames(daydat) = c('Month','Day',colnames(tab),'nyear')
		
	#Adding this to the list
	daylist[[i]] = daydat	
}

#Making a readme file
readme                   = 'README FOR PRECEEDING DATASETS'
readme[length(readme)+1] = 'AUTHOR: Adam Sibley'
readme[length(readme)+1] = 'CONTACT: adam.m.sibley@gmail.com'
readme[length(readme)+1] = paste('LAST MODIFIED:',round(Sys.time(),'days'))
readme[length(readme)+1] = ''
readme[length(readme)+1] = 'VARIABLE NAMES'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = 'SWup  = Downwelling Shortwave Radiation (up-looking sensor)'
readme[length(readme)+1] = 'SWdn  = Upwelling Shortwave Radiation (down-looking sensor)'
readme[length(readme)+1] = 'LWup  = Downwelling Longwave Radiation (up-looking sensor)'
readme[length(readme)+1] = 'LWdn  = Upwelling Longwave Radiation (down-looking sensor)'
readme[length(readme)+1] = 'PAR   = Photosynthetically Active Radiation (I assume downwelling?)'
readme[length(readme)+1] = 'Tair  = Air Temperature'
readme[length(readme)+1] = 'RH    = Relative Humidity'
readme[length(readme)+1] = 'WS    = Wind Speed'
readme[length(readme)+1] = 'Tsoil = Soil Temperature'
readme[length(readme)+1] = 'SM    = Soil Mositure'
readme[length(readme)+1] = 'RF    = Rainfall'
readme[length(readme)+1] = 'nyear = number of years of data used to calculate this row'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = ''
readme[length(readme)+1] = 'NOTES'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = 'nyear represents the number of years present in the station record. For a given variable in a given time'
readme[length(readme)+1] = 'period, data could have been missing, meaning fewer years actually went in to that calculation.'
readme[length(readme)+1] = 'For example, at the Hakalau station, for the first few years there were no instruments to measure radiation.'
readme[length(readme)+1] = 'So radiation averages come from fewer years than, say, air temperature averages.'
readme[length(readme)+1] = '#------------#'

#adding readme to the list
daylist[[length(daylist)+1]] = as.data.frame(readme)
	
names(daylist) = c(statnames,'README')

#Making em all data frames
for(i in 1:length(daylist)){daylist[[i]] = as.data.frame(daylist[[i]])}


#Writing her out!
WriteXLS(x = 'daylist',ExcelFileName = paste(destdir,'Daily_means.xls',sep=''),SheetNames = names(daylist),AdjWidth = T)
	
	


