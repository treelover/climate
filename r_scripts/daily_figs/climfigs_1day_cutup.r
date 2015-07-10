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
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/daily_figs/stations/ipif_plot.r',sep=''))
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/daily_figs/stations/mamalahoa_plot.r',sep=''))
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/daily_figs/stations/palamanui_plot.r',sep=''))
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/daily_figs/stations/puuwaawaa_plot.r',sep=''))
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/daily_figs/stations/spencer_plot.r',sep=''))
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/daily_figs/stations/laupahoehoe_plot.r',sep=''))
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/daily_figs/stations/status_plot.r',sep=''))
library(plotrix)
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
lau_soil   = climread(paste(cdir,'LauSoil_SoilData.dat',sep=''))
mama  = climread(paste(cdir,'Mamalahoa_MetData.dat',sep=''))
palam = climread(paste(cdir,'Palamanui_MetData.dat',sep=''))
pww   = climread(paste(cdir,'PuuWaawaa_MetData.dat',sep=''))
pww_soil = climread(paste(cdir,'PwwSoil_SoilData.dat',sep=''))
spen  = climread(paste(cdir,'Spencer_MetData.dat',sep=''))

#Reading in the system info
ipifs  = climread(paste(cdir,'IPIF_SysInfo.dat',sep=''))
kihos  = climread(paste(cdir,'Kiholo_SysInfo.dat',sep=''))
laus  = climread(paste(cdir,'Laupahoehoe_SysInfo.dat',sep=''))
laus_soil   = climread(paste(cdir,'LauSoil_SysInfo.dat',sep=''))
mamas  = climread(paste(cdir,'Mamalahoa_SysInfo.dat',sep=''))
palams = climread(paste(cdir,'Palamanui_SysInfo.dat',sep=''))
pwws   = climread(paste(cdir,'PuuWaawaa_SysInfo.dat',sep=''))
pwws_soil = climread(paste(cdir,'PwwSoil_SysInfo.dat',sep=''))
spens  = climread(paste(cdir,'Spencer_SysInfo.dat',sep=''))
slist = list(ipifs,kihos,laus,laus_soil,mamas,palams,pwws,pwws_soil,spens)
names(slist) = c('ipifs','kihos','laus','laus_soil','mamas','palams','pwws','pwws_soil','spens')
#------------------------------------------------------------#


#Time inputs
#------------------------------------------------------------#
#Getting system time
now = Sys.time()
now = as.POSIXct(paste(substr(now,1,11),'00:00:00',sep=''))

#In case you wanna run it for another day....
#now = as.POSIXct('2014-08-08')

dayback = now-(60*60*24)
threeback = now-(60*60*24*3)
#------------------------------------------------------------#


#Main Title Page
#------------------------------------------------------------#
pdf(file = paste(figdir,substr(now,1,10),'.pdf',sep=''),width = 8.5, height = 11)
par(oma = c(3,2,12,2))

testj = readJPEG(paste(picdir,'station_locations2_scaled.jpg',sep=''))
jbrick = brick(testj)

plotRGB(x = jbrick, r = 1, g = 2, b = 3, scale = 1,maxpixels = 1000000,mar = c(3,5,5,4))
mtext('Ho Brah,',cex = 4, font = 2, outer = F,line = 8)
mtext('Big Island',cex = 4, font = 2, outer = F,line = 4)
mtext('Weather Station Data',cex = 4, font = 2, outer = F,line = 0)
#------------------------------------------------------------#

#Individual Stations
#------------------------------------------------------------#
ipif_plot(ipif,now,dayback,threeback)                 #IPIF
mamalahoa_plot(mama,now,dayback,threeback)            #MAMALAHOA
palamanui_plot(palam,now,dayback,threeback)           #PALAMANUI
puuwaawaa_plot(pww,pww_soil,now,dayback,threeback)    #PUUWAAWAA
spencer_plot(spen,now,dayback,threeback)              #SPENCER
laupahoehoe_plot(lau,lau_soil,now,dayback,threeback)  #LAUPAHOEHOE
status_plot(slist,now,dayback,threeback)              #STATUS PLOTZ
#------------------------------------------------------------#

dev.off()














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

	
	
	
	
	
	
	
	
	
