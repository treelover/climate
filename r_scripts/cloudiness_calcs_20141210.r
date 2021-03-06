#This code was written to calculate a cloudiness metric for each of the climate stations.
#The result will be written out as an Rdat file


#Libraries and functions
#---------------------------------------#
source('/home/adam/Dropbox/sapflow/r_scripts/fun/climread.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read_REST2.r')
library(WriteXLS)
#---------------------------------------#


#Directories
#---------------------------------------#
cdir = '/home/adam/Dropbox/sapflow/clim_screened/'
rdir = '/home/adam/Dropbox/sapflow/clim/clear_sky_rad/raw/'
dates = '20141210'
#---------------------------------------#


#Station names
#---------------------------------------#
statnames = c('Hakalau','IPIF','Kiholo','Laupahoehoe','Mamalahoa','Palamanui','PuuWaawaa','Spencer')
#---------------------------------------#

#Reading the files in 
#---------------------------------------#
hak   = read.table(paste(cdir,'Hakalau_screened_20141210.dat' ,sep=''),sep=',',header = T)
ipif  = read.table(paste(cdir,'IPIF_screened_20141214.dat' ,sep=''),sep=',',header = T)
kiho  = read.table(paste(cdir,'KiholoBay_screened_20140929.dat' ,sep=''),sep=',',header = T)
lau   = read.table(paste(cdir,'Laupahoehoe_screened_20141203.dat' ,sep=''),sep=',',header = T)
mama  = read.table(paste(cdir,'Mamalahoa_screened_20141214.dat' ,sep=''),sep=',',header = T)
palam = read.table(paste(cdir,'Palamanui_screened_20141214.dat' ,sep=''),sep=',',header = T)
pww   = read.table(paste(cdir,'Puuwaawaa_screened_20141214.dat' ,sep=''),sep=',',header = T)
spe   = read.table(paste(cdir,'Spencer_screened_20141203.dat' ,sep=''),sep=',',header = T)

#Fixing the timestamps
hak[,1] = as.POSIXct(hak[,1])
ipif[,1] = as.POSIXct(ipif[,1])
kiho[,1] = as.POSIXct(kiho[,1])
lau[,1] = as.POSIXct(lau[,1])
mama[,1] = as.POSIXct(mama[,1])
palam[,1] = as.POSIXct(palam[,1])
pww[,1] = as.POSIXct(pww[,1])
spe[,1] = as.POSIXct(spe[,1])

#Swapping the -9999 for NA
hak[which(hak == -9999,arr.ind = T)] = NA
ipif[which(ipif == -9999,arr.ind = T)] = NA
kiho[which(kiho == -9999,arr.ind = T)] = NA
lau[which(lau == -9999,arr.ind = T)] = NA
mama[which(mama == -9999,arr.ind = T)] = NA
palam[which(palam == -9999,arr.ind = T)] = NA
pww[which(pww == -9999,arr.ind = T)] = NA
spe[which(spe == -9999,arr.ind = T)] = NA

#Making a list of all the stations
slist = list(hak,ipif,kiho,lau,mama,palam,pww,spe)
names(slist) = c('hak','ipif','kiho','lau','mama','palam','pww','spe')
#---------------------------------------#


#READING IN THE R.E.S.T. DATA
#---------------------------------------#
rhak = read_REST2(paste(rdir,'Hakalau_REST2_out.txt',sep=''))
ripif = read_REST2(paste(rdir,'Ipif_Albedo_from_Spencer_REST2_out.txt',sep=''))
rkiho = read_REST2(paste(rdir,'Kiholo_REST2_out.txt',sep=''))
rlau = read_REST2(paste(rdir,'Laupahoehoe_REST2_out.txt',sep=''))
rmama = read_REST2(paste(rdir,'Mamalahoa_REST2_out.txt',sep=''))
rpalam = read_REST2(paste(rdir,'Palamanui_REST2_out.txt',sep=''))
rpww = read_REST2(paste(rdir,'PuuWaaWaa_REST2_out.txt',sep=''))
rspe = read_REST2(paste(rdir,'Spencer_REST2_out.txt',sep=''))

rlist = list(rhak,ripif,rkiho,rlau,rmama,rpalam,rpww,rspe)
names(slist) = c('hak','ipif','kiho','lau','mama','palam','pww','spe')
#---------------------------------------#


#Looping throught the stations
#---------------------------------------#
for(i in 1:length(slist)){

	stat = slist[[i]]
	rest = rlist[[i]]
	
	#Matrix to hold the new data
	temp = matrix(NA,ncol = 3, nrow = nrow(stat))
	
	#Making a compatable list of timestamps
	stat.time = substr(stat$yyyy.mm.dd.hh.mm,6,16)
	
	rest.time = paste(sprintf(fmt = '%02d',rest[,'Mnth']),'-',sprintf(fmt = '%02d',rest[,'Day']),' ',sprintf(fmt = '%02d',floor(rest[,'HrLST'])),':',sprintf(fmt = '%02d',round((rest[,'HrLST']-floor(rest[,'HrLST']))*60)),sep='')                                    

	starttime = Sys.time()
	for(j in 1:nrow(stat)){
	
		ID = which(rest.time == stat.time[j])
		
		if(length(ID) == 0){temp[j,3] = 0; next}
		
		
		if(rest[ID,'Ghor'] <= 0){temp[j,] = c(rest[ID,'Ghor'],1-(stat[j,'SWup']/rest[ID,'Ghor']),0)}else{
								 temp[j,] = c(rest[ID,'Ghor'],1-(stat[j,'SWup']/rest[ID,'Ghor']),1)}
	}
	
	#Adding the column names
	colnames(temp) = c('ClearSky','Cloud','DayFlag')
	
	#Removing any infinite values
	temp[which(is.infinite(temp) == T,arr.ind = T)] = NA
	
	#Putting it all together
	stat = cbind(stat,temp)
	
	#Rectifying the fact that there are gaps in the data
	temp2 = as.matrix(stat[,2:ncol(stat)])
	times = seq(stat[1,1],tail(stat[,1],1),by = 600)
	temp3 = matrix(NA,nrow = length(times),ncol = (ncol(stat)-1))
	missvec = match(stat[,1],times)
	temp3[missvec,] = temp2
	temp3 = cbind(times,as.data.frame(temp3))
	colnames(temp3) = c('yyyy.mm.dd.hh.mm',colnames(temp2))
	
	#Adding the result back in to the list
	slist[[i]] = temp3
	
	#Reporting progress
	print(paste(names(slist)[i],' done in',difftime(Sys.time(),starttime,units = 'mins'),' minutes',sep=''))
}
#---------------------------------------#


#Writing out slist as an Rdat file
#---------------------------------------#
save(slist,file = paste(cdir,'all_stations_20141210.Rdat',sep=''))
#---------------------------------------#











