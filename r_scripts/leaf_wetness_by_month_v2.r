#Reading in the leaf wetness data
library(gdata)

#Reading in the wet leaf wetness data
wet1 = read.xls('/home/adam/Dropbox/sapflow/leafwet/pww/pww_leaf_2014-08-15_date_corrected.xls',sheet=1)
wet1 = wet1[-c(1,2),]
wet2 = read.xls('/home/adam/Dropbox/sapflow/leafwet/pww/pww_leaf_2014-12-08.xls',sheet=1)
wet2 = wet2[-c(1,2),]

wet = rbind(wet1,wet2)
wet = wet[,c(1,2,5,8,11,14)]     #450 threshold
#wet6 = wet[,c(1,3,6,9,12,15)]   #460 threshold

wet[,1] = as.POSIXct(wet[,1],format = '%m/%d/%Y %I:%M %p')
wet[,2] = as.numeric(as.matrix(wet[,2]))
wet[,3] = as.numeric(as.matrix(wet[,3]))
wet[,4] = as.numeric(as.matrix(wet[,4]))
wet[,5] = as.numeric(as.matrix(wet[,5]))
wet[,6] = as.numeric(as.matrix(wet[,6]))
colnames(wet)[1] = 'time'

#Month index
mID = sort(unique(substr(wet$time,6,7)))

#Hour index
hID = sort(unique(substr(wet$time,12,13)))

ID = expand.grid(hID,mID)
wetave = mat.or.vec(nrow(ID),5)

for(i in 1:nrow(ID)){
	tempID = which(substr(wet$time,12,13) == ID[i,1] & substr(wet$time,6,7) == ID[i,2])
	wetave[i,] = apply(wet[tempID,2:6]/10,2,mean)
	}

#Take mean of sensors and plot by month
#Get rid of July bc there is only 1 day of data there
pdat = rowMeans(wetave)
pdat = pdat[25:144]

#DO IT AGAIN FOR LAU

#Reading in the wet leaf wetness data
wet1 = read.xls('/home/adam/Dropbox/sapflow/leafwet/lau/lau_leaf_2014-09-04.xls',sheet=1)
wet1 = wet1[-c(1,2),]
wet2 = read.xls('/home/adam/Dropbox/sapflow/leafwet/lau/lau_leaf_2014-10-27.xls',sheet=1)
wet2 = wet2[-c(1,2),]
wet3 = read.xls('/home/adam/Dropbox/sapflow/leafwet/lau/lau_leaf_2014-12-03.xls',sheet=1)
wet3 = wet3[-c(1,2),]

wet = rbind(wet1,wet2,wet3)
wet = wet[,c(1,2,5,8,11,14)]     #450 threshold
#wet6 = wet[,c(1,3,6,9,12,15)]   #460 threshold

wet[,1] = as.POSIXct(wet[,1],format = '%m/%d/%Y %I:%M %p')
wet[,2] = as.numeric(as.matrix(wet[,2]))
wet[,3] = as.numeric(as.matrix(wet[,3]))
wet[,4] = as.numeric(as.matrix(wet[,4]))
wet[,5] = as.numeric(as.matrix(wet[,5]))
wet[,6] = as.numeric(as.matrix(wet[,6]))
colnames(wet)[1] = 'time'

#Month index
mID = sort(unique(substr(wet$time,6,7)))

#Hour index
hID = sort(unique(substr(wet$time,12,13)))

ID = expand.grid(hID,mID)
wetave = mat.or.vec(nrow(ID),5)

for(i in 1:nrow(ID)){
	tempID = which(substr(wet$time,12,13) == ID[i,1] & substr(wet$time,6,7) == ID[i,2])
	wetave[i,] = apply(wet[tempID,2:6]/10,2,mean)
	}

#The sensors are pretty similar to each other. Take mean and plot by month

pdatlau = rowMeans(wetave)

#Get both pdats on to a 0 to 100 scale
pdat = pdat*100
pdatlau = pdatlau*100

#PLOTTING
jpeg('/home/adam/Dropbox/sapflow/figs/poster/leafwet.jpeg',width = 1000, height = 1000)
par(mfrow = c(2,2),las = 1,cex.main = 3,cex.lab = 2.5,cex.axis=2.5,mar = c(5,6,4,2))

plot(y = pdatlau[1:24], 
x = 1:24,
lwd = 4,
ylim = c(0,100),
col = colors()[132],
ylab = '% of hour leaf is wet',
xlab = '',
main = 'August',
tck = 0.03,
cex.axis = 2.2,
type = 'l')
lines(y = pdat[1:24], x = 1:24,lwd = 4,col = colors()[149])

legend(x = 13, y = 98,cex=2,legend = c("Laupahoehoe","Pu\'u\ Wa\'awa\'a"),fill = colors()[c(132,149)])

plot(y = pdatlau[25:48], 
x = 1:24,
lwd = 4,
ylim = c(0,100),
col = colors()[132],
ylab = '',
xlab = '',
main = 'September',
tck = 0.03,
type = 'l')
lines(y = pdat[25:48], x = 1:24,lwd = 4,col = colors()[149])

plot(y = pdatlau[49:72], 
x = 1:24,
lwd = 4,
ylim = c(0,100),
col = colors()[132],
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'October',
tck = 0.03,
cex.axis = 2.2,
type = 'l')
lines(y = pdat[49:72], x = 1:24,lwd = 4,col = colors()[149])

plot(y = pdatlau[73:96], 
x = 1:24,
lwd = 4,
ylim = c(0,100),
col = colors()[132],
ylab = '',
xlab = 'Hour of day',
main = 'November',
tck = 0.03,
type = 'l')
lines(y = pdat[73:96], x = 1:24,lwd = 4,col = colors()[149])

dev.off()



