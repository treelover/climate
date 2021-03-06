#Reading in all of the screened data files and making summarized data tables for the Beckster. 
#Started July 14th, 2014
#Author: Adam Sibley


#Libraries and functions
#---------------------------------------#
source('/home/adam/Dropbox/sapflow/r_scripts/fun/climread.r')
library(WriteXLS)
#---------------------------------------#


#Directories
#---------------------------------------#
cdir = '/home/adam/Dropbox/sapflow/clim_screened/'
destdir = '/home/adam/Dropbox/sapflow/clim/summarized_vars/'
dates = '20141210'
#---------------------------------------#


#Station names
#---------------------------------------#
statnames = c('Laupahoehoe','Mamalahoa','Palamanui','PuuWaawaa')
#---------------------------------------#

#Reading the files in 
#---------------------------------------#
load(paste(cdir,'all_stations_',dates,'.Rdat',sep=''))
#---------------------------------------#

#Calculating daily averages
#---------------------------------------#
daylist = list()

for(i in 1:length(slist)){

	dat = slist[[i]]
	
	#First, change the -9999 to NA
	wonkers <- dat == -9999
	dat[wonkers] = NA
	
	#Make an index of each month
	dayfac = sort(unique(substr(dat$yyyy.mm.dd.hh.mm,6,7)))
	
	#Starting the table to store the data in
	daydat = c()
	
	#Loop through each date
	for(j in 1:length(dayfac)){
	
		#ID of date location in table
		dID = which(substr(dat$yyyy.mm.dd.hh.mm,6,7) == dayfac[j])
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
		tab[which(tab == Inf)] = NA
		tab[which(tab == -Inf)] = NA
		
		#Adding todays values to the table
		daydat = rbind(daydat,cbind(rep(dayfac[j],nrow(tab)),yID,tab))
		
		#Printing some information
		print(paste(statnames[i],'-',dayfac[j]))
		
	}
	
	#Adding the column names
	colnames(daydat) = c('Month','Year',colnames(tab))
		
	#Adding this to the list
	daylist[[i]] = daydat	
}

#Making a readme file
#readme                   = 'README FOR PRECEEDING DATASETS'
#readme[length(readme)+1] = 'AUTHOR: Adam Sibley'
#readme[length(readme)+1] = 'CONTACT: adam.m.sibley@gmail.com'
#readme[length(readme)+1] = paste('LAST MODIFIED:',round(Sys.time(),'days'))
#readme[length(readme)+1] = ''
#readme[length(readme)+1] = 'VARIABLE NAMES'
#readme[length(readme)+1] = '#------------#'
#readme[length(readme)+1] = 'SWup  = Downwelling Shortwave Radiation (up-looking sensor)'
#readme[length(readme)+1] = 'SWdn  = Upwelling Shortwave Radiation (down-looking sensor)'
#readme[length(readme)+1] = 'LWup  = Downwelling Longwave Radiation (up-looking sensor)'
#readme[length(readme)+1] = 'LWdn  = Upwelling Longwave Radiation (down-looking sensor)'
#readme[length(readme)+1] = 'PAR   = Photosynthetically Active Radiation (I assume downwelling?)'
#readme[length(readme)+1] = 'Tair  = Air Temperature'
#readme[length(readme)+1] = 'RH    = Relative Humidity'
#readme[length(readme)+1] = 'WS    = Wind Speed'
#readme[length(readme)+1] = 'Tsoil = Soil Temperature'
#readme[length(readme)+1] = 'SM    = Soil Mositure'
#readme[length(readme)+1] = 'RF    = Rainfall'
#readme[length(readme)+1] = '#------------#'
#readme[length(readme)+1] = ''
#readme[length(readme)+1] = 'NOTES'
#readme[length(readme)+1] = '#------------#'
#readme[length(readme)+1] = '#------------#'

#adding readme to the list
#daylist[[length(daylist)+1]] = as.data.frame(readme)
	
#names(daylist) = c(statnames,'README')

names(daylist) = names(slist)

#Making em all data frames
for(i in 1:length(daylist)){daylist[[i]] = as.data.frame(daylist[[i]])}


#Writing her out!
WriteXLS(x = 'daylist',ExcelFileName = paste(destdir,'Month_by_year_means_version_20141210.xls',sep=''),SheetNames = names(daylist),AdjWidth = T)
	
	


