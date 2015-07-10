#This script will, on a daily basis, create summary figures for the past 24 hours of weather data. 


#Setting the home folder
#---------------------------------------#
tmp = system('ls /home/', intern=T)
if(length(which(tmp == 'adam')) > 0){home = 'adam'}
if(length(which(tmp == 'ender')) > 0){home = 'ender'}
#---------------------------------------#


#Functions and libraries
#---------------------------------------#
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/fun/climread.r',sep=''))
library(openair)
library(raster)
library(png)
library(jpeg)
#---------------------------------------#


#Directories
#---------------------------------------#
cdir = paste('/home/',home,'/Dropbox/sapflow/clim/',sep='')
figdir = paste('/home/',home,'/Dropbox/sapflow/figs/clim_1day/',sep='')
picdir = paste('/home/',home,'/Dropbox/sapflow/pictures/',sep='')
#---------------------------------------#

#Reading in the most recent met data
#------------------------------------------------------------#
#hak = poor hakalau
ipif  = climread(paste(cdir,'IPIF_MetData.dat',sep=''))
kiho  = climread(paste(cdir,'Kiholo_MetData.dat',sep=''))
lau   = climread(paste(cdir,'Laupahoehoe_MetData.dat',sep=''))
mama  = climread(paste(cdir,'Mamalahoa_MetData.dat',sep=''))
palam = climread(paste(cdir,'Palamanui_MetData.dat',sep=''))
pww   = climread(paste(cdir,'PuuWaawaa_MetData.dat',sep=''))
spen  = climread(paste(cdir,'Spencer_MetData.dat',sep=''))
dlist = list(ipif,kiho,lau,mama,palam,pww,spen)
names(dlist) = c('ipif','kiho','lau','mama','palam','pww','spen')

#Reading in the system info
ipifs  = climread(paste(cdir,'IPIF_SysInfo.dat',sep=''))
kihos  = climread(paste(cdir,'Kiholo_SysInfo.dat',sep=''))
laus  = climread(paste(cdir,'Laupahoehoe_SysInfo.dat',sep=''))
mamas  = climread(paste(cdir,'Mamalahoa_SysInfo.dat',sep=''))
palams = climread(paste(cdir,'Palamanui_SysInfo.dat',sep=''))
pwws   = climread(paste(cdir,'PuuWaawaa_SysInfo.dat',sep=''))
spens  = climread(paste(cdir,'Spencer_SysInfo.dat',sep=''))
slist = list(ipifs,kihos,laus,mamas,palams,pwws,spens)
names(slist) = c('ipifs','kihos','laus','mamas','palams','pwws','spens')
#------------------------------------------------------------#

#Station names
statnames = c('IPIF','Kiholo','Laupahoehoe','Mamalahoa','Palamanui','PuuWaawaa','Spencer')

#Getting system time
now = Sys.time()
now = as.POSIXct(paste(substr(now,1,11),'00:00:00',sep=''))

#In case you wanna run it for another day....
#now = as.POSIXct('2014-08-08')

dayback = now-(60*60*24)
threeback = now-(60*60*24*3)

pdf(file = paste(figdir,substr(now,1,10),'.pdf',sep=''),width = 8.5, height = 11)

#In case you wanna run it for another day....
#pdf(file = paste(figdir,'2014-08-08.pdf',sep=''),width = 8.5, height = 11)

#PAGE
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#
#Putting the map and title on
par(oma = c(3,2,12,2))

#testp = readPNG(paste(picdir,'station_locations2.png',sep=''))
#pbrick = brick(testp)

testj = readJPEG(paste(picdir,'station_locations2_scaled.jpg',sep=''))
jbrick = brick(testj)

plotRGB(x = jbrick, r = 1, g = 2, b = 3, scale = 1,maxpixels = 1000000,mar = c(3,5,5,4))
mtext('Ho Brah,',cex = 4, font = 2, outer = F,line = 8)
mtext('Big Island',cex = 4, font = 2, outer = F,line = 4)
mtext('Weather Station Data',cex = 4, font = 2, outer = F,line = 0)
#dev.off()
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#









#PAGE
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#

#OH, NOW WE DO DA STATIONS
#-------------------------------------------------------------#
for(i in 1:length(sloop)){
#i=1

par(mfrow = c(2,1),mar = c(0,5,5,4),oma=c(3,0,0,0))

#TEMPERATURE
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#Pull out the two Temperature records
T1 = dlist[[i]]$dat[,which(dlist[[i]]$header2 == 'Tair_1_Avg')]
T2 = dlist[[i]]$dat[,which(dlist[[i]]$header2 == 'Tair_2_Avg')]

T1 = T1[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
T2 = T2[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]

#Time vector
days = dlist[[i]]$time[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]

#The title
title = 'Air Temperature'

plot(x = days,
y = T1,
lwd = 3,
type = 'l',
col = 'black',
#main = title,
#cex.main = 1.5,
xlab = '',
ylab = 'Temperature (deg. C)',
ylim = c(min(c(T1,T2),na.rm=T),max(c(T1,T2),na.rm=T)),
las = 1)

title(main = title, cex.main = 1.4, line = 0.5)

lines(x = days, y = T2, lwd = 3, col = 'red')

abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], col = 'gray')
abline(v = dayback, col = 'black',lwd = 2)

legend(x = 'topleft',fill = c('black','red'),legend = c('Tair 1','Tair 2'),bg='white')

#Adding page title
mtext(text = bquote(underline(paste(.(statnames[i]),' ',.(strftime(threeback,format = '%b %d')),' - ',.(strftime(dayback,format = '%b %d, %Y')),sep=''))),cex = 2, font = 2, outer = F,line = 2)

#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#

#HUMIDITY
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#Pull out the two Temperature records
R1 = dlist[[i]]$dat[,which(dlist[[i]]$header2 == 'RH_1_Avg')]
R2 = dlist[[i]]$dat[,which(dlist[[i]]$header2 == 'RH_2_Avg')]

R1 = R1[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
R2 = R2[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]

#Time vector
days = dlist[[i]]$time[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]

#Title
title = 'Relative Humidity'

plot(x = days,
y = R1,
lwd = 3,
type = 'l',
col = 'blue',
#main = title,
#cex.main = 1.5,
xlab = '',
ylab = 'Relative Humidity (%)',
ylim = c(min(c(R1,R2),na.rm=T),max(c(R1,R2),na.rm=T)),
las = 1)

title(main = title, cex.main = 1.4, line = 1)

lines(x = days, y = R2, lwd = 3, col = 'forestgreen')

abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], col = 'gray')
abline(v = dayback, col = 'black',lwd = 2)

legend(x = 'topleft',fill = c('blue','forestgreen'),legend = c('RH 1','RH 2'),bg='white')
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#

#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#






#FOURTH PAGE
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#
par(mfrow = c(2,1),mar = c(0,5,5,4),oma=c(3,0,0,0))
#RADIATION
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#Deal with the silly ipif case first
if(statnames[i] == 'IPIF'){

	rdat = dlist[[i]]$dat
	colnames(rdat) = dlist[[i]]$header2
	
	R1 = rdat$SWup_Avg[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
	R2 = rdat$PAR_Avg[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
	R3 = rdat$Rnet_Avg[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
	
	#Title
	title = paste('Radiation ',strftime(threeback,format = '%b %d'),' - ',strftime(dayback,format = '%b %d, %Y'),sep='')

	plot(x = days,
	y = R1,
	lwd = 3,
	type = 'l',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Radiation (W/m^2)',
	ylim = c(min(c(R1,R2,R3),na.rm=T),max(c(R1,R2,R3),na.rm=T)),
	las = 1)
	
	title(main = title, cex.main = 1.4, line = 0.5)

	lines(x = days, y = R2, lwd = 3, col = 'red')
	lines(x = days, y = R3, lwd = 3, col = 'forestgreen')

	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = 0, col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)

	legend(x = 'topleft',fill = c('blue','red','forestgreen'),legend = c('SW up','PAR','Rnet'),bg='white')
	
	#Adding page title
	mtext(text = bquote(underline(.(statnames[i]))),cex = 2, font = 2, outer = F,line = 2)


}

#The rest of the stations
if(statnames[i] != 'IPIF'){

	rdat = dlist[[i]]$dat
	colnames(rdat) = dlist[[i]]$header2
	
	R1 = rdat$SWup_Avg[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
	R2 = rdat$SWdn_Avg[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
	R3 = rdat$LWup_Avg[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
	R4 = rdat$LWdn_Avg[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now)]
	
	#Title
	title = paste('Radiation ',strftime(threeback,format = '%b %d'),' - ',strftime(dayback,format = '%b %d, %Y'),sep='')

	plot(x = days,
	y = R1,
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Radiation (W/m^2)',
	ylim = c(min(c(R1,R2,R3,R4),na.rm=T),max(c(R1,R2,R3,R4),na.rm=T)),
	las = 1)
	
	title(main = title, cex.main = 1.4, line = 0.5)
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = seq(-1000,2000,200), col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)

	lines(x = days, y = R1, lwd = 3, col = 'blue')
	lines(x = days, y = R2, lwd = 3, col = 'red')
	lines(x = days, y = R3, lwd = 3, col = 'forestgreen')
	lines(x = days, y = R4, lwd = 3, col = 'darkmagenta')

	legend(x = 'topleft',fill = c('blue','red','forestgreen','darkmagenta'),legend = c('SW up','SW down','LW up','LW down'),bg='white')
	
	#Adding page title
	mtext(text = bquote(underline(.(statnames[i]))),cex = 2, font = 2, outer = F,line = 2)

}
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#


#RAINFALL
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#

rdat = dlist[[i]]$dat
colnames(rdat) = dlist[[i]]$header2
	
R1 = rdat$Rainfall_Tot[which(dlist[[i]]$time == dayback):which(dlist[[i]]$time == now)]
#Making it cumulative
for(j in 2:length(R1)){R1[j] = R1[j]+R1[j-1]}
	
#Title
title = paste('Cumulative Rainfall ',strftime(dayback,format = '%B %d, %Y'),sep='')

plot(x = seconddays,
y = R1,
lwd = 4,
type = 'n',
col = 'dodgerblue4',
#main = title,
#cex.main = 1.5,
xlab = '',
ylab = 'Rainfall (mm)',
las = 1)

title(main = title, cex.main = 1.4, line = 0.5)

abline(h = seq(-20,500,2), col = 'gray')
abline(h = seq(-20,500,10), col = 'black')

lines(x = seconddays, y = R1, lwd = 4, col = 'dodgerblue4')
polygon(x = c(seconddays,rev(seconddays)),y = c(R1,rep(0,length(R1))),col='dodgerblue3', border = NA)

#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#






#PAGE
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#

#WIND SPEED WEEE!
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#testing out this windrose business
par(oma=c(0,0,0,0))
winddat = dlist[[i]]$dat
colnames(winddat) = dlist[[i]]$header2
winddat = winddat[which(dlist[[i]]$time == dayback):which(dlist[[i]]$time == now),]
ws = 'WindSpeed'
wd = 'WindDir'

title = 'Wind Speed/Direction'

windRose(winddat, ws = ws, wd = wd, main = title, cex.main = 1.7, angle = 10,col = c('green','yellow','red','darkred','black'),paddle = FALSE,statistic = 'abs.count')

mtext(text = bquote(underline(paste(.(statnames[i]),' - ',.(strftime(dayback,format = '%B %d, %Y')),sep=''))),cex = 2, font = 2, outer = F,line =29)
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#







#PAGE
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#
par(mfrow = c(2,1),mar = c(0,5,5,4),oma=c(3,0,0,0))
#Taking care of the ones with soil moisture
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#

wettest = which(substr(dlist[[i]]$header2,1,12) == 'SoilMoisture')
if(length(wettest) > 0){

	rdat = dlist[[i]]$dat
	colnames(rdat) = dlist[[i]]$header2
	
	#Title
	title = 'Soil Moisture'

	plot(x = days,
	y = rep(NA,length(days)),
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Soil Moisture (%)',
	ylim = c(0,100),
	las = 1)	
	
	title(main = title, cex.main = 1.4, line = 0.5)
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = seq(0,100,20), col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)
	
	leglab = c()
	cols = c('blue','red','forestgreen','darkmagenta')
	
	for(j in 1:length(wettest)){
		R1 = 100*rdat[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now),paste('SoilMoisture_',j,'_Avg',sep='')]
		lines(x = days, y = R1, lwd = 3, col = cols[j])
		leglab[j] = paste('SM',j)
	}
	
	legend('topleft',fill = cols,legend = leglab,bg='white')
	
	#Putting on the page title
	#Adding page title
	mtext(text = bquote(underline(paste(.(statnames[i]),' ',.(strftime(threeback,format = '%b %d')),' - ',.(strftime(dayback,format = '%b %d, %Y')),sep=''))),cex = 2, font = 2, outer = F,line = 2)

}
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#

#Taking care of the ones with soil heat
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
heattest = which(substr(dlist[[i]]$header2,1,12) == 'SoilHeatFlux')
if(length(heattest) > 0){

	rdat = dlist[[i]]$dat
	colnames(rdat) = dlist[[i]]$header2
	
	#Title
	title = 'Soil Heat Flux'

	plot(x = days,
	y = rep(NA,length(days)),
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Soil Heat Flux (W/m^2)',
	ylim = c(min(rdat[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now),heattest],na.rm=T),max(rdat[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now),heattest],na.rm=T)),
	las = 1)	
	
	title(main = title, cex.main = 1.4, line = 0.5)
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)
	
	leglab = c()
	cols = c('blue','red','forestgreen','darkmagenta')
	
	for(j in 1:length(heattest)){
		R1 = rdat[which(dlist[[i]]$time == threeback):which(dlist[[i]]$time == now),paste('SoilHeatFlux_',j,'_Avg',sep='')]
		lines(x = days, y = R1, lwd = 3, col = cols[j])
		leglab[j] = paste('SHF',j)
	}
	
	legend('topleft',fill = cols,legend = leglab,bg='white')

}
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#
#}




#PAGE
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#
par(mfrow = c(2,1),mar = c(0,5,5,4),oma=c(3,0,0,0))
#FINALLY, taking care of Lau and PWW soil loggers


#LAUPAHERHER
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
if(statnames[i] == 'Laupahoehoe'){
temp = climread(paste(cdir,'LauSoil_SoilData.dat',sep=''))
soil = temp$dat
colnames(soil) = temp$header2

#Soil Moisture
	
	#Title
	title = 'Soil Moisture'

	plot(x = days,
	y = rep(NA,length(days)),
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Soil Moisture (%)',
	ylim = c(0,100),
	las = 1)	
	
	title(main = title, cex.main = 1.4, line = 0.5)
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = seq(0,100,20), col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)
	
	R1 = 100*soil[which(temp$time == threeback):which(temp$time == now),paste('SoilMoisture_1_Avg',sep='')]
	R2 = 100*soil[which(temp$time == threeback):which(temp$time == now),paste('SoilMoisture_2_Avg',sep='')]
	R3 = 100*soil[which(temp$time == threeback):which(temp$time == now),paste('SoilMoisture_3_Avg',sep='')]

	cols = c('blue','red','forestgreen','darkmagenta')
	lines(x = days, y = R1, lwd = 3, col = cols[1])
	lines(x = days, y = R2, lwd = 3, col = cols[2])
	lines(x = days, y = R3, lwd = 3, col = cols[3])
		
	legend('topleft',fill = cols,legend = c('SM 1','SM 2','SM 3'),bg='white')
	
	#Adding page title
	mtext(text = bquote(underline(paste(.(statnames[i]),' ',.(strftime(threeback,format = '%b %d')),' - ',.(strftime(dayback,format = '%b %d, %Y')),sep=''))),cex = 2, font = 2, outer = F,line = 2)

	
#Soil Heat Flux
	
	#Title
	title = 'Soil Heat Flux'

	R1 = soil[which(temp$time == threeback):which(temp$time == now),paste('SoilHeatFlux_1_Avg',sep='')]
	R2 = soil[which(temp$time == threeback):which(temp$time == now),paste('SoilHeatFlux_2_Avg',sep='')]
	R3 = soil[which(temp$time == threeback):which(temp$time == now),paste('SoilHeatFlux_3_Avg',sep='')]
	R4 = soil[which(temp$time == threeback):which(temp$time == now),paste('SoilHeatFlux_4_Avg',sep='')]
	
	plot(x = days,
	y = rep(NA,length(days)),
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Soil Heat Flux (W/m^2)',
	ylim = c(min(c(R1,R2,R3,R4),na.rm=T),max(c(R1,R2,R3,R4),na.rm=T)),
	las = 1)	
	
	title(main = title, cex.main = 1.4, line = 0.5)
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = seq(0,100,20), col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)

	cols = c('blue','red','forestgreen','darkmagenta')
	lines(x = days, y = R1, lwd = 3, col = cols[1])
	lines(x = days, y = R2, lwd = 3, col = cols[2])
	lines(x = days, y = R3, lwd = 3, col = cols[3])
	lines(x = days, y = R4, lwd = 3, col = cols[4])
		
	legend('topleft',fill = cols,legend = c('SHF 1','SHF 2','SHF 3','SHF 4'),bg='white')
	
#Soil Temperature

	par(mfrow = c(2,1),mar = c(0,5,5,4),oma=c(3,0,0,0))
	#Title
	title = 'Soil Temperature'

	R1 = soil[which(temp$time == threeback):which(temp$time == now),paste('Tsoil_1_Avg',sep='')]
	R2 = soil[which(temp$time == threeback):which(temp$time == now),paste('Tsoil_2_Avg',sep='')]

	plot(x = days,
	y = rep(NA,length(days)),
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Temperature (deg. C)',
	ylim = c(min(c(R1,R2),na.rm=T),max(c(R1,R2),na.rm=T)),
	las = 1)	
	
	title(main = title, cex.main = 1.4, line = 0.5)
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = seq(0,100,20), col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)

	cols = c('blue','red','forestgreen','darkmagenta')
	lines(x = days, y = R1, lwd = 3, col = cols[1])
	lines(x = days, y = R2, lwd = 3, col = cols[2])
		
	legend('topleft',fill = cols,legend = c('ST 1','ST 2'),bg='white')
	
	#Adding page title
	mtext(text = bquote(underline(paste(.(statnames[i]),' ',.(strftime(threeback,format = '%b %d')),' - ',.(strftime(dayback,format = '%b %d, %Y')),sep=''))),cex = 2, font = 2, outer = F,line = 2)

}
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#	
	
	
#PUU WAA WAA
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
if(statnames[i] == 'PuuWaawaa'){
temp = climread(paste(cdir,'PwwSoil_SoilData.dat',sep=''))
soil = temp$dat
colnames(soil) = temp$header2

#Soil Moisture
	
	#Title
	title = 'Soil Moisture'

	plot(x = days,
	y = rep(NA,length(days)),
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Soil Moisture (%)',
	ylim = c(0,100),
	las = 1)	
	
	title(main = title, cex.main = 1.4, line = 0.5)
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = seq(0,100,20), col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)
	
	R1 = 100*soil[which(temp$time == threeback):which(temp$time == now),paste('SoilMoisture_1_Avg',sep='')]
	R2 = 100*soil[which(temp$time == threeback):which(temp$time == now),paste('SoilMoisture_2_Avg',sep='')]
	R3 = 100*soil[which(temp$time == threeback):which(temp$time == now),paste('SoilMoisture_3_Avg',sep='')]

	cols = c('blue','red','forestgreen','darkmagenta')
	lines(x = days, y = R1, lwd = 3, col = cols[1])
	lines(x = days, y = R2, lwd = 3, col = cols[2])
	lines(x = days, y = R3, lwd = 3, col = cols[3])
		
	legend('topleft',fill = cols,legend = c('SM 1','SM 2','SM 3'),bg='white')
	
	#Adding page title
	mtext(text = bquote(underline(paste(.(statnames[i]),' ',.(strftime(threeback,format = '%b %d')),' - ',.(strftime(dayback,format = '%b %d, %Y')),sep=''))),cex = 2, font = 2, outer = F,line = 2)

	
#Soil Heat Flux
	
	#Title
	title = 'Soil Heat Flux'

	R1 = soil[which(temp$time == threeback):which(temp$time == now),paste('SoilHeatFlux_1_Avg',sep='')]
	R2 = soil[which(temp$time == threeback):which(temp$time == now),paste('SoilHeatFlux_2_Avg',sep='')]
	R3 = soil[which(temp$time == threeback):which(temp$time == now),paste('SoilHeatFlux_3_Avg',sep='')]
	R4 = soil[which(temp$time == threeback):which(temp$time == now),paste('SoilHeatFlux_4_Avg',sep='')]
	
	plot(x = days,
	y = rep(NA,length(days)),
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Soil Heat Flux (W/m^2)',
	ylim = c(min(c(R1,R2,R3,R4),na.rm=T),max(c(R1,R2,R3,R4),na.rm=T)),
	las = 1)	
	
	title(main = title, cex.main = 1.4, line = 0.5)
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = seq(0,100,20), col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)

	cols = c('blue','red','forestgreen','darkmagenta')
	lines(x = days, y = R1, lwd = 3, col = cols[1])
	lines(x = days, y = R2, lwd = 3, col = cols[2])
	lines(x = days, y = R3, lwd = 3, col = cols[3])
	lines(x = days, y = R4, lwd = 3, col = cols[4])
		
	legend('topleft',fill = cols,legend = c('SHF 1','SHF 2','SHF 3','SHF 4'),bg='white')
	
#Soil Temperature
par(mfrow = c(2,1),mar = c(0,5,5,4),oma=c(3,0,0,0))
	
	#Title
	title = 'Soil Temperature'

	R1 = soil[which(temp$time == threeback):which(temp$time == now),paste('Tsoil_1_Avg',sep='')]
	R2 = soil[which(temp$time == threeback):which(temp$time == now),paste('Tsoil_2_Avg',sep='')]

	plot(x = days,
	y = rep(NA,length(days)),
	lwd = 3,
	type = 'n',
	col = 'blue',
	#main = title,
	#cex.main = 1.5,
	xlab = '',
	ylab = 'Temperature (deg. C)',
	ylim = c(min(c(R1,R2),na.rm=T),max(c(R1,R2),na.rm=T)),
	las = 1)
	
	title(main = title, cex.main = 1.4, line = 0.5)	
	
	abline(v = seq(threeback,now,'hours')[seq(1,length(seq(threeback,now,'hours')),12)], h = seq(0,100,20), col = 'gray')
	abline(v = dayback, col = 'black',lwd = 2)

	cols = c('blue','red','forestgreen','darkmagenta')
	lines(x = days, y = R1, lwd = 3, col = cols[1])
	lines(x = days, y = R2, lwd = 3, col = cols[2])
		
	legend('topleft',fill = cols,legend = c('ST 1','ST 2'),bg='white')
	
	#Adding page title
	mtext(text = bquote(underline(paste(.(statnames[i]),' ',.(strftime(threeback,format = '%b %d')),' - ',.(strftime(dayback,format = '%b %d, %Y')),sep=''))),cex = 2, font = 2, outer = F,line = 2)

}
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#	
}

#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#





#PAGE
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#

#MAKE A BATTERY PLOT! - WILL GO ON ONE PAGE WITH ENC RH PLOT
#-------------------------------------------------------------#
par(mfrow = c(2,1),mar = c(3,5,5,4))

#Make title
title = 'Battery Voltage'

#Make x data 
x = ipifs$time[which(ipifs$time == threeback):which(ipifs$time == now)]

#Start the plot
plot(x = x,
y = rep(NA,length(x)),
ylim = c(10,14),
#main = title,
#cex.main = 1.4,
las = 1,
#mar = c(0,6,5,6),
ylab = 'Volts',
xlab = '')

title(main = title, cex.main = 1.4, line = 0.5)

#Add some lines
abline(h = seq(8,15,1),v = seq(threeback,now,'day'),col = colors()[220])
abline(v = dayback, col = 'black')

#Stations
sloop = c('ipifs','kihos','laus','mamas','palams','pwws','spens')

#Colors for each stations
plotcols = colors()[c(29,552,32,90,99,258,153,254)]

#Looping through the stations
for(i in 1:length(sloop)){

#Coming up with the data to plot

batvar = which(slist[[i]]$header2 == 'BattVolt_Avg')
dat = slist[[i]]$dat[,batvar] 

#First line = the first two days, dashed line
firstdays = slist[[i]]$time[which(slist[[i]]$time == threeback):which(slist[[i]]$time == dayback)]
firstline = dat[which(slist[[i]]$time == threeback):which(slist[[i]]$time == dayback)]

#Second line = last 24 hours
seconddays = slist[[i]]$time[which(slist[[i]]$time == dayback):which(slist[[i]]$time == now)]
secondline = dat[which(slist[[i]]$time == dayback):which(slist[[i]]$time == now)]

#Plot them babies
lines(x = firstdays, y = firstline, col = plotcols[i],lwd = 4, lty = 1)
lines(x = seconddays, y = secondline, col = plotcols[i],lwd = 4, lty = 1)

}

legend('bottomleft',fill = plotcols,legend = statnames,cex = 1,bg='white')

#Adding the page title
mtext(text = bquote(underline(paste('All Stations ',.(strftime(threeback,format = '%b %d')),' - ',.(strftime(dayback,format = '%b %d, %Y')),sep=''))),cex = 2, font = 2, outer = F,line = 2)

#-------------------------------------------------------------#


#HO BRAH, TIME FOR DA HUMIDITY
#-------------------------------------------------------------##Make title
title ='Enclosure Humidity'

#Make x data 
x = ipifs$time[which(ipifs$time == threeback):which(ipifs$time == now)]

#Start the plot
plot(x = x,
y = rep(NA,length(x)),
ylim = c(0,100),
#main = title,
#cex.main = 1.4,
#mar = c(3,6,0,6),
las = 1,
ylab = 'Relative Humidity (%)',
xlab = '')

title(main = title, cex.main = 1.4, line = 1)

#Add some lines
abline(h = seq(0,100,10),v = seq(threeback,now,'day'),col = colors()[220])
abline(v = dayback, col = 'black')

#Looping through the stations
for(i in 1:length(sloop)){

#Coming up with the data to plot
rhvar = which(slist[[i]]$header2 == 'RH_enc')
dat = slist[[i]]$dat[,rhvar] 

#First line = the first two days, dashed line
firstdays = slist[[i]]$time[which(slist[[i]]$time == threeback):which(slist[[i]]$time == dayback)]
firstline = dat[which(slist[[i]]$time == threeback):which(slist[[i]]$time == dayback)]

#Second line = last 24 hours
seconddays = slist[[i]]$time[which(slist[[i]]$time == dayback):which(slist[[i]]$time == now)]
secondline = dat[which(slist[[i]]$time == dayback):which(slist[[i]]$time == now)]

#Plot them babies
lines(x = firstdays, y = firstline, col = plotcols[i],lwd = 4, lty = 1)
lines(x = seconddays, y = secondline, col = plotcols[i],lwd = 4, lty = 1)

}

legend('bottomleft',fill = plotcols,legend = statnames,cex = 1,bg='white')
#-------------------------------------------------------------#

dev.off()
#/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\/*\#

	
	
	
	
	
	
	
	
	
