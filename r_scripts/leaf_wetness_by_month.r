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

par(mfrow = c(2,2))

plot(y = pdat[1:24], 
x = 1:24,
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'Aug Averages - PWW',
type = 'l')

plot(y = pdat[25:48], 
x = 1:24,
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'Sept Averages',
type = 'l')

plot(y = pdat[49:72], 
x = 1:24,
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'Oct Averages',
type = 'l')

plot(y = pdat[73:96], 
x = 1:24,
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'Nov Averages',
type = 'l')

savePlot('/home/adam/Dropbox/sapflow/figs/Leaf_wetness_patterns_pww.jpeg',type = 'jpeg')

#DO IT AGAIN FOR PWW

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

pdat = rowMeans(wetave)

par(mfrow = c(2,2))

plot(y = pdat[1:24], 
x = 1:24,
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'Aug Averages - Lau',
type = 'l')

plot(y = pdat[25:48], 
x = 1:24,
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'Sept Averages',
type = 'l')

plot(y = pdat[49:72], 
x = 1:24,
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'Oct Averages',
type = 'l')

plot(y = pdat[73:96], 
x = 1:24,
ylab = '% of hour leaf is wet',
xlab = 'Hour of day',
main = 'Nov Averages',
type = 'l')

savePlot('/home/adam/Dropbox/sapflow/figs/Leaf_wetness_patterns_lau.jpeg',type = 'jpeg')




