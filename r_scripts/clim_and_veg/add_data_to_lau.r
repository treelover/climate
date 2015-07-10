
#Libraries and functions
#---------------------------------------#
source('/home/adam/Dropbox/sapflow/r_scripts/fun/climread.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read_REST2.r')
library(WriteXLS)
#---------------------------------------#

#Directories
#---------------------------------------#
cdir = '/home/adam/Dropbox/sapflow/clim_screened/'
dates = '20140404'
#---------------------------------------#

#Load up the climate data
#---------------------------------------#
load(paste(cdir,'all_stations_',dates,'.Rdat',sep=''))
#---------------------------------------#

#Reading in the most recent climate data
#---------------------------------------#
clim1 = climread('/home/adam/Dropbox/sapflow/veg/lau/LaupahoehoeDirect_MetData.dat')
newclim = clim1[['dat']]
colnames(newclim) = clim1[['header2']]

soil1 = climread('/home/adam/Dropbox/sapflow/veg/lau/LauSoilDirect_SoilData.dat')
newsoil = soil1[['dat']]
colnames(newsoil) = soil1[['header2']]

#Matching up timestamps
soilmat = as.data.frame(matrix(NA,nrow = nrow(newclim),ncol = ncol(newsoil)))
soilmat[,1] = newclim[,1]
for(i in 1:nrow(soilmat)){

	ID = which(newsoil[,1] == soilmat[i,1])
	
	if(length(ID) != 0){soilmat[i,] = newsoil[ID,]}
}
	
colnames(soilmat) = colnames(newsoil)
newsoil = soilmat
#That was screwy.....
#---------------------------------------#

#Pulling out the mamalahoa entry in the climate data from before
lau = slist[['lau']]

#Rearraning new data to fit old data
newclim = cbind(newclim[,c(1,3,4,5,6,7,8,13,14,9,11,10,12,15,16,17,19,18)],newsoil[,c(3,4,5:11)],newclim[,20])

colnames(newclim) = colnames(lau)[1:28]

#Cranking through the clear sky stuff
#---------------------------------------------------------#
rdir = '/home/adam/Dropbox/sapflow/clim/clear_sky_rad/raw/'
rest = read_REST2(paste(rdir,'Laupahoehoe_REST2_out.txt',sep=''))

stat = newclim

	#Matrix to hold the new data
	temp = matrix(NA,ncol = 3, nrow = nrow(stat))
	
	#Making a compatable list of timestamps
	stat.time = substr(stat$yyyy.mm.dd.hh.mm,6,16)
	
	rest.time = paste(sprintf(fmt = '%02d',rest[,'Mnth']),'-',sprintf(fmt = '%02d',rest[,'Day']),' ',sprintf(fmt = '%02d',floor(rest[,'HrLST'])),':',sprintf(fmt = '%02d',round((rest[,'HrLST']-floor(rest[,'HrLST']))*60)),sep='')                                    

	for(j in 1:nrow(stat)){
	
		ID = which(rest.time == stat.time[j])
		
		if(length(ID) == 0){temp[j,3] = 0; next}
		
		
		if(rest[ID,'Ghor'] <= 0){temp[j,] = c(rest[ID,'Ghor'],1-(stat[j,'SWup']/rest[ID,'Ghor']),0)}else{
								 temp[j,] = c(rest[ID,'Ghor'],1-(stat[j,'SWup']/rest[ID,'Ghor']),1)}
	}
	
	#Adding the column names
	colnames(temp) = c('ClearSky','Cloud','DayFlag')
	
	#Removing any infinite values and NaNs
	temp[which(is.infinite(temp) == T,arr.ind = T)] = NA
	temp[which(is.nan(temp) == T,arr.ind = T)] = NA
	
	#Putting it all together
	stat = cbind(stat,temp)

#---------------------------------------------------------#

final = rbind(lau,stat)

save(final,file = '/home/adam/Dropbox/sapflow/veg/lau/lau_climdata.Rdat')

plot(stat[,'RH_2'])


