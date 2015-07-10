#This script is for exploring the correlation between PAR measurements and 


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
figdir = '/home/adam/Dropbox/sapflow/figs/par/'
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

#Laupahoehoe
#---------------------------------------#
lau[which(lau[,'SWup'] == -9999),'SWup'] = NA
lau[which(lau[,'PAR'] == -9999),'PAR'] = NA

time = as.POSIXct(lau[,'yyyy.mm.dd.hh.mm'])

caldates = as.POSIXct(c('2009-11-06','2010-12-17','2011-12-12'))

cal1 = which(time < caldates[1])
cal2 = which(time < caldates[2] & time >= caldates[1])
cal3 = which(time < caldates[3] & time >= caldates[2])
cal4 = which(time < (caldates[3]+(60*60*24*(365+90))) & time >= caldates[3])

#Only the last calibration period had both SW and PAR measurements
jpeg(filename = paste(figdir,'lau_par.jpeg',sep=''), quality = 100, width = 1200,height = 500)
par(mfrow = c(1,2))

	pdatx = lau[cal4,'SWup']
	pdaty = lau[cal4,'PAR']
	pdatz = floor(as.numeric((time[cal4]-(caldates[3]))/(60*60*24*30)))

	#First plot, points
	plot(x = pdatx,
	     y = pdaty,
	     xlab = 'Downwelling Short Wave (W/m^2)',
	     ylab = 'PAR (umol/s/m^2)',
	     #xlim = c(1000,1500),
	     #ylim = c(1500,2500),
	     type = 'n',
	     main = 'Laupahoehoe - from PAR cal date 2011-12-12')
	     
	abline(v = seq(0,5000,500),h = seq(0,5000,500),col = 'gray')

	ploop(x = pdatx, y = pdaty, z = pdatz)

	#Second plot, lines
	plot(x = pdatx,
	     y = pdaty,
	     xlab = 'Downwelling Short Wave (W/m^2)',
	     ylab = 'PAR (umol/s/m^2)',
	     #xlim = c(1000,1500),
	     #ylim = c(1500,2500),
	     type = 'n',
	     main = 'Laupahoehoe - from PAR cal date 2011-12-12')




###STOPPED HERE. ALL I'VE DONE IS SCANNED THROUGH UP TO THIS POINT AND REPLACED "HAK" WITH "LAU" AND "HAKALAU" WITH "LAUPAHOEHOE"







	abline(v = seq(0,5000,500),h = seq(0,5000,500),col = 'gray')

	regloop(x = pdatx, y = pdaty, z = pdatz)

	savePlot(filename = paste(figdir,'hak_par2.jpeg',sep=''),type = 'jpeg')

	dev.off()
	#####################
#---------------------------------------#


#Now, using the hakalau dates after cal4, come up with 
#an RMSE and SE for bins of SW down
#---------------------------------------#
t = time[cal4]
x = hak[cal4,'SWup']
y = hak[cal4,'PAR']
z = floor(as.numeric((time[cal4]-(caldates[3]))/(60*60*24*30)))
	
dat = data.frame(t,x,y,z)

#Taking out any data below 20 W/m^2 or 20 umol/s*m^2
dat$x[dat$x < 20] = NA	
dat$y[dat$y < 20] = NA

#A vector of boundaries for the bins
radbin = seq(0,1500,100)

#A vector of months since cal
calmo = sort(unique(dat$z))

#Making a matrix to store results
RMSE = matrix(NA, nrow = length(unique(dat$z)),ncol = length(radbin)-1)
colnames(RMSE) = paste(radbin[1:(length(radbin)-1)],' - ',radbin[2:length(radbin)],sep='')
rownames(RMSE) = sort(calmo)

#Making a matrix to store the # of samples that went in to the RMSE
sampno = RMSE

#Making a list of all the linear models 
linlist = list()

#Month loop
for(i in 1:length(calmo)){

	calID = which(dat$z == calmo[i])
	temp = dat[calID,]
	
	linlist[[i]] = lm(temp$y ~ temp$x)

	#Bin loop
	for(j in 1:(length(radbin)-1)){

		xID = which(temp$x > radbin[j] & temp$x <= radbin[j+1])
		
		#Pulling out the residuals for this bin
		residuals = resid(linlist[[i]])[match(as.character(xID),names(resid(linlist[[i]])))]
		
		#Filling in sampno - Length of xID minus # of NAs in Y data
		sampno[i,j] = length(xID)-length(which(is.na(dat$y[xID])))
		
		#Computing the RMSE
		RMSE[i,j] = sqrt(mean(residuals^2,na.rm=T))
	}
}
#---------------------------------------#

#Plotting dat bugga
#---------------------------------------#
plot(x = radbin[1:15]+50,
y = seq(0,max(RMSE,na.rm=T),
length.out = ncol(RMSE)),
type='n',
xlab = 'Intensity of Radiation - Binned by 100 W/m^2',
ylab = 'RMSE (umol/s*m^2)',
main = 'Hakalau - RMSE of monthly linear fits (LI-190 cal date = 2011-12-12)')

#cols = c(rainbow(ncol(RMSE)-1),'black')
cols = rainbow(16)
abline(h = seq(0,1000,10),v = seq(0,10000,100),col = 'gray')

for(i in 1:nrow(RMSE)){lines(x = radbin[1:15]+50,y = RMSE[i,],col = cols[i],lwd = 3,type = 'b',pch = 15)}

legend(x = 'topleft',fill = cols, title = 'Months since cal',
		legend = paste(calmo,' - ',calmo+1,sep = ''),bg = 'white',cex = 0.8,ncol = 3)

savePlot(filename = paste(figdir,'hak_RMSE.jpeg',sep=''),type = 'jpeg')
#---------------------------------------#



#The same operation as before, only the regressions are done adding one 
#month at a time until all the data is included
#---------------------------------------#

#Making a matrix to store results
RMSE2 = matrix(NA, nrow = length(unique(dat$z)),ncol = length(radbin)-1)
colnames(RMSE2) = paste(radbin[1:(length(radbin)-1)],' - ',radbin[2:length(radbin)],sep='')
rownames(RMSE2) = sort(calmo)

#Making a matrix to store the # of samples that went in to the RMSE2
sampno2 = RMSE2

#Making a list of all the linear models 
linlist = list()

#Month loop
for(i in 1:length(calmo)){

	calID = which(dat$z <= calmo[i])
	temp = dat[calID,]
	
	linlist[[i]] = lm(temp$y ~ temp$x)

	#Bin loop
	for(j in 1:(length(radbin)-1)){

		xID = which(temp$x > radbin[j] & temp$x <= radbin[j+1])
		
		#Pulling out the residuals for this bin
		residuals = resid(linlist[[i]])[match(as.character(xID),names(resid(linlist[[i]])))]
		
		#Filling in sampno - Length of xID minus # of NAs in Y data
		sampno2[i,j] = length(xID)-length(which(is.na(dat$y[xID])))
		
		#Computing the RMSE
		RMSE2[i,j] = sqrt(mean(residuals^2,na.rm=T))
	}
}
#---------------------------------------#


#Plotting dat bugga.....2!
#---------------------------------------#
plot(x = radbin[1:15]+50,
y = seq(0,max(RMSE2,na.rm=T),
length.out = ncol(RMSE2)),
type='n',
xlab = 'Intensity of Radiation - Binned by 100 W/m^2',
ylab = 'RMSE (umol/s*m^2)',
main = 'Hakalau - RMSE of linear fits, adding one month at a time (LI-190 cal date = 2011-12-12)')

#cols = c(rainbow(ncol(RMSE)-1),'black')
cols = rainbow(16)
abline(h = seq(0,1000,10),v = seq(0,10000,100),col = 'gray')

for(i in 1:nrow(RMSE2)){lines(x = radbin[1:15]+50,y = RMSE2[i,],col = cols[i],lwd = 3,type = 'b',pch = 15)}

legend(x = 'topleft',fill = cols, title = 'Months since cal',
		legend = paste(rep(0,length(calmo)),' - ',calmo+1,sep = ''),bg = 'white',cex = 0.8,ncol = 3)

savePlot(filename = paste(figdir,'hak_RMSE2.jpeg',sep=''),type = 'jpeg')
#---------------------------------------#



