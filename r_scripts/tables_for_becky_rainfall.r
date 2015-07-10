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


#Time intervals
#---------------------------------------#
intlist = c()
intlist[[1]] = c('11','12','01','02','03','04')
intlist[[length(intlist)+1]] = c('11','12','01')
intlist[[length(intlist)+1]] = c('05','06','07','08','09','10')
intlist[[length(intlist)+1]] = c('02','03','04')
intlist[[length(intlist)+1]] = c('05','06','07')
intlist[[length(intlist)+1]] = c('08','09','10')
intlist[[length(intlist)+1]] = sprintf('%02d',1:12)
names(intlist) = c('nov2apr','nov2jan','may2oct','feb2apr','may2jul','aug2oct','calyr')
#---------------------------------------#

#We also want to do a count of the number of months below a threshold of rain. 
#---------------------------------------#
thresh = c(50,100)
threshnames = c('less50mm','less100mm')

#Looping through all the stations
#---------------------------------------#
yrlist = list()
threshlist = list()

for(i in 1:length(slist)){

	dat = slist[[i]]
	
	#First, change the -9999 to NA
	wonkers <- dat == -9999
	dat[wonkers] = NA
	
	#Make an index of each day, month and year
	dayfac = sort(unique(substr(dat$yyyy.mm.dd.hh.mm,9,10)))
	mofac = sort(unique(substr(dat$yyyy.mm.dd.hh.mm,6,7)))
	yrfac = sort(unique(substr(dat$yyyy.mm.dd.hh.mm,1,4)))
	nyear = length(yrfac)
	
	#TAKE CARE OF THE ODDBALLS FIRST
	#Names of the things that will be in this table
	cnames = c(names(intlist),rep('no.days',length(intlist)))
	tab = matrix(data = NA, ncol = length(cnames), nrow = nyear)
	colnames(tab) = cnames
	
	
	for(j in 1:length(yrfac)){
	
		#The nov to april and nov to jan one
		if(j != length(yrfac)){
	
			#Get this years november and december
			yID = which(substr(dat$yyyy.mm.dd.hh.mm,1,4) == yrfac[j] & substr(dat$yyyy.mm.dd.hh.mm,6,7) == '11')
			yID = c(yID,which(substr(dat$yyyy.mm.dd.hh.mm,1,4) == yrfac[j] & substr(dat$yyyy.mm.dd.hh.mm,6,7) == '12'))
	
			#Now do Just January of the next year
			yID2 = which(substr(dat$yyyy.mm.dd.hh.mm,1,4) == yrfac[j+1] & substr(dat$yyyy.mm.dd.hh.mm,6,7) == '01')
	
			#Now tack on February, March and April
			yID3 = c(yID2,which(substr(dat$yyyy.mm.dd.hh.mm,1,4) == yrfac[j+1] & substr(dat$yyyy.mm.dd.hh.mm,6,7) == '02'))
			yID3 = c(yID3,which(substr(dat$yyyy.mm.dd.hh.mm,1,4) == yrfac[j+1] & substr(dat$yyyy.mm.dd.hh.mm,6,7) == '03'))
			yID3 = c(yID3,which(substr(dat$yyyy.mm.dd.hh.mm,1,4) == yrfac[j+1] & substr(dat$yyyy.mm.dd.hh.mm,6,7) == '04'))
	
			#OK, adding the rainfall total 
			tab[j,'nov2apr'] = sum(dat[c(yID,yID3),'RF'],na.rm=T)
			tab[j,length(intlist)+1] = length(c(yID,yID3))/(6*24)
	
			tab[j,'nov2jan'] = sum(dat[c(yID,yID2),'RF'],na.rm=T)
			tab[j,length(intlist)+2] = length(c(yID,yID2))/(6*24)
	
		}

	for(k in 3:length(intlist)){
	
		ID = c()
	
		for(l in 1:length(intlist[[k]])){ID = c(ID,which(substr(dat$yyyy.mm.dd.hh.mm,1,4) == yrfac[j] & substr(dat$yyyy.mm.dd.hh.mm,6,7) == intlist[[k]][l]))}
		
		tab[j,k] = sum(dat[ID,'RF'],na.rm=T)
		tab[j,k+length(intlist)] = length(ID)/(6*24)
	}
	
	#OK now a mini table for the # months below the thresholds
	threshtab = matrix(0,nrow = nyear*12,ncol = 2+(length(thresh)*2))
	threshtab[,c(2,1)] = as.numeric(as.matrix(expand.grid(1:12,yrfac)))
	colnames(threshtab) = c('year','mo',threshnames,rep('no.days',length(threshnames)))
	
	
	#Looping through the months, with what to make the thresholds with
	for(k in 1:12){
		ID = which(substr(dat$yyyy.mm.dd.hh.mm,1,4) == yrfac[j] & substr(dat$yyyy.mm.dd.hh.mm,6,7) == sprintf('%02d',k))
	
		for(l in 1:length(thresh)){
		
		#Yet another ID, to put values in the right year/month combo
		place = (((j-1)*12))+k
			
			#If there were no days in that month, skip....
			if(length(ID) == 0){next}		
			
			#Checking the threshold
			if(sum(dat[ID,'RF'],na.rm=T) < thresh[l]){threshtab[place,l+2] = 1
										  threshtab[place,length(thresh)+l+2] = length(ID)/(6*24)
										  }
										  
					  
		}
	}								  
		
	#Printing some information
	print(paste(statnames[i],'-',yrfac[j]))		
	}
	
	#OK rearrange the tables, because I'm dumb like that
	tab2 = matrix(as.numeric(yrfac),ncol=1,nrow=nyear)
	names2 = 'year'
	for(l in 1:length(intlist)){
	
		tab2 = cbind(tab2,tab[,l],tab[,length(intlist)+l])
		names2 = c(names2,colnames(tab)[l],colnames(tab)[length(intlist)+l])
	}
	colnames(tab2) = names2
	
	#rearrange the thresh one
	thresh2 = threshtab[,1:2]
	names2 = c('year','mo')
	for(l in 2+(1:length(thresh))){
	
		thresh2 = cbind(thresh2,threshtab[,l],threshtab[,length(thresh)+l])
		names2 = c(names2,colnames(threshtab)[l],colnames(threshtab)[length(thresh)+l])
	}
	colnames(thresh2) = names2
	
	yrlist[[i]] = tab2
	threshlist[[i]] = thresh2
}

#Making a readme file
readme                   = 'README FOR PRECEEDING DATASETS'
readme[length(readme)+1] = 'AUTHOR: Adam Sibley'
readme[length(readme)+1] = 'CONTACT: adam.m.sibley@gmail.com'
readme[length(readme)+1] = paste('LAST MODIFIED:',round(Sys.time(),'days'))
readme[length(readme)+1] = ''
readme[length(readme)+1] = 'VARIABLE NAMES'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = 'nov2apr  = Total rainfall November THROUGH April'
readme[length(readme)+1] = 'no.days  = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = 'nov2jan  = Total rainfall November through January'
readme[length(readme)+1] = 'no.days  = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = 'may2oct  = Total rainfall May through October'
readme[length(readme)+1] = 'no.days  = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = 'feb2apr  = Total rainfall February through April'
readme[length(readme)+1] = 'no.days  = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = 'may2jul  = Total rainfall May through July'
readme[length(readme)+1] = 'no.days  = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = 'aug2oct  = Total rainfall August through October'
readme[length(readme)+1] = 'no.days  = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = 'calyr    = Total rainfall in the calendar year (Jan to Dec)'
readme[length(readme)+1] = 'no.days  = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = ''
readme[length(readme)+1] = 'NOTES'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = 'The reason for the whole no.days thing: the climate records all start on different'
readme[length(readme)+1] = 'days and have different numbers of years and different amounts of missing data.'
readme[length(readme)+1] = 'This way, you can divide the monthly/3month/6month/yearly totals by the number of days'
readme[length(readme)+1] = 'That went in to that total to get at average amount of rain per day in that'
readme[length(readme)+1] = 'given time interval.'
readme[length(readme)+1] = '#------------#'

#WRITING IT ALL OUT
#---------------------------------------#

#adding readme to the list
yrlist[[length(yrlist)+1]] = as.data.frame(readme)
	
names(yrlist) = c(statnames,'README')

#Making em all data frames
for(i in 1:length(yrlist)){yrlist[[i]] = as.data.frame(yrlist[[i]])}

#Writing her out!
#WriteXLS(x = 'yrlist',ExcelFileName = paste(destdir,'Rainfall_sums.xls',sep=''),SheetNames = names(yrlist),AdjWidth = T)
#---------------------------------------#

	



#Making a readme file
readme                   = 'README FOR PRECEEDING DATASETS'
readme[length(readme)+1] = 'AUTHOR: Adam Sibley'
readme[length(readme)+1] = 'CONTACT: adam.m.sibley@gmail.com'
readme[length(readme)+1] = paste('LAST MODIFIED:',round(Sys.time(),'days'))
readme[length(readme)+1] = ''
readme[length(readme)+1] = 'VARIABLE NAMES'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = 'less50mm   = 1 means total rainfall was less than 50mm in this month (it was a dry month). 0 means > 50mm. NA means no data'
readme[length(readme)+1] = 'no.days    = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = 'less100mm  = 1 means total rainfall was less than 50mm in this month (it was a dry month). 0 means > 100mm. NA means no data'
readme[length(readme)+1] = 'no.days    = number of days that went into the calculation one column to the left'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = ''
readme[length(readme)+1] = 'NOTES'
readme[length(readme)+1] = '#------------#'
readme[length(readme)+1] = 'The reason for the whole no.days thing: the climate records all start on different'
readme[length(readme)+1] = 'days and have different numbers of years and different amounts of missing data.'
readme[length(readme)+1] = 'This way, you can divide the monthly/3month/6month/yearly totals by the number of days'
readme[length(readme)+1] = 'That went in to that total to get at average amount of rain per day in that'
readme[length(readme)+1] = 'given time interval.'
readme[length(readme)+1] = '#------------#'

#WRITING IT ALL OUT
#---------------------------------------#

#adding readme to the list
threshlist[[length(threshlist)+1]] = as.data.frame(readme)
	
names(threshlist) = c(statnames,'README')

#Making em all data frames
for(i in 1:length(threshlist)){threshlist[[i]] = as.data.frame(threshlist[[i]])}

#Writing her out!
WriteXLS(x = 'threshlist',ExcelFileName = paste(destdir,'Dry_month_count.xls',sep=''),SheetNames = names(yrlist),AdjWidth = T)
#---------------------------------------#

