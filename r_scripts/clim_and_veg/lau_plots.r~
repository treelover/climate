

#Directories
#---------------------------------------#
cdir = '/home/adam/Dropbox/sapflow/clim_screened/'
vdir = '/home/adam/Dropbox/sapflow/veg/lau/'
climdate = '20140404'
figdir = '/home/adam/Dropbox/sapflow/figs/veg/'
#---------------------------------------#

#Read in the data file 
load(paste(vdir,'cor_outputs.Rdat',sep=''))

win = as.numeric(rownames(Tcors))
lags = c(0,3,6,9,12)

#Species
speciesID = 3:21
species = colnames(Tcors)[speciesID]
sizeID = 22:26
sizes = colnames(Tcors)[sizeID]

#Plot for all data - Temperature
#============================================================================================#
#============================================================================================#
pdat = Tcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Temperature and Growth Rate of All Species')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray')
lines(x = win,y = pdat[,1,1],lwd = 3, col = 'violet')
lines(x = win,y = pdat[,1,2],lwd = 3, col = 'blue')
lines(x = win,y = pdat[,1,3],lwd = 3, col = 'green')
lines(x = win,y = pdat[,1,4],lwd = 3, col = 'red')
lines(x = win,y = pdat[,1,5],lwd = 3, col = 'orange')

legend(x = 'topright',legend = lags,fill = c('violet','blue','green','red','orange'),title = 'Lag (in Months)',bg = 'white')

savePlot(filename = paste(figdir,'Lau_T_all.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for all data - Rainfall
#============================================================================================#
#============================================================================================#
pdat = RFcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Rainfall and Growth Rate of All Species')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray')
lines(x = win,y = pdat[,1,1],lwd = 3, col = 'violet')
lines(x = win,y = pdat[,1,2],lwd = 3, col = 'blue')
lines(x = win,y = pdat[,1,3],lwd = 3, col = 'green')
lines(x = win,y = pdat[,1,4],lwd = 3, col = 'red')
lines(x = win,y = pdat[,1,5],lwd = 3, col = 'orange')

legend(x = 'topright',legend = lags,fill = c('violet','blue','green','red','orange'),title = 'Lag (in Months)',bg = 'white')

savePlot(filename = paste(figdir,'Lau_RF_all.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for all data - Shortwave
#============================================================================================#
#============================================================================================#
pdat = SWcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Solar Radiation and Growth Rate of All Species')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray')
lines(x = win,y = pdat[,1,1],lwd = 3, col = 'violet')
lines(x = win,y = pdat[,1,2],lwd = 3, col = 'blue')
lines(x = win,y = pdat[,1,3],lwd = 3, col = 'green')
lines(x = win,y = pdat[,1,4],lwd = 3, col = 'red')
lines(x = win,y = pdat[,1,5],lwd = 3, col = 'orange')

legend(x = 'topright',legend = lags,fill = c('violet','blue','green','red','orange'),title = 'Lag (in Months)',bg = 'white')

savePlot(filename = paste(figdir,'Lau_SW_all.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#





#Plot for all data minus ferns - Temperature
#============================================================================================#
#============================================================================================#
pdat = Tcors

plot(x = win,
y = pdat[,2,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Temperature and Growth Rate of All Non-Ferns')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray')
lines(x = win,y = pdat[,2,1],lwd = 3, col = 'violet')
lines(x = win,y = pdat[,2,2],lwd = 3, col = 'blue')
lines(x = win,y = pdat[,2,3],lwd = 3, col = 'green')
lines(x = win,y = pdat[,2,4],lwd = 3, col = 'red')
lines(x = win,y = pdat[,2,5],lwd = 3, col = 'orange')

legend(x = 'topright',legend = lags,fill = c('violet','blue','green','red','orange'),title = 'Lag (in Months)',bg = 'white')

savePlot(filename = paste(figdir,'Lau_T_noferns.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for all data minus ferns - Rainfall
#============================================================================================#
#============================================================================================#
pdat = RFcors

plot(x = win,
y = pdat[,2,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Rainfall and Growth Rate of All Non-Ferns')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray')
lines(x = win,y = pdat[,2,1],lwd = 3, col = 'violet')
lines(x = win,y = pdat[,2,2],lwd = 3, col = 'blue')
lines(x = win,y = pdat[,2,3],lwd = 3, col = 'green')
lines(x = win,y = pdat[,2,4],lwd = 3, col = 'red')
lines(x = win,y = pdat[,2,5],lwd = 3, col = 'orange')

legend(x = 'topright',legend = lags,fill = c('violet','blue','green','red','orange'),title = 'Lag (in Months)',bg = 'white')

savePlot(filename = paste(figdir,'Lau_RF_noferns.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for all data minus ferns - Shortwave
#============================================================================================#
#============================================================================================#
pdat = SWcors

plot(x = win,
y = pdat[,2,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Solar Radiation and Growth Rate of All Non-Ferns')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray')
lines(x = win,y = pdat[,2,1],lwd = 3, col = 'violet')
lines(x = win,y = pdat[,2,2],lwd = 3, col = 'blue')
lines(x = win,y = pdat[,2,3],lwd = 3, col = 'green')
lines(x = win,y = pdat[,2,4],lwd = 3, col = 'red')
lines(x = win,y = pdat[,2,5],lwd = 3, col = 'orange')

legend(x = 'topright',legend = lags,fill = c('violet','blue','green','red','orange'),title = 'Lag (in Months)',bg = 'white')

savePlot(filename = paste(figdir,'Lau_SW_noferns.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for each species, lag 0, Temperature 
#============================================================================================#
#============================================================================================#
pdat = Tcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Temperature and Growth Rate by Species - No Lag')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray',lwd=0.5)
cols = primary.colors(length(species))
for(i in 1:length(species)){lines(x = win, y = pdat[,speciesID[i],1],col = cols[i],lwd=2)}

legend(x = 'topright',legend = species,fill = cols,title = 'Species',bg = 'white',ncol = 2)

savePlot(filename = paste(figdir,'Lau_T_byspecies_lag0.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for each species, lag 0, Rainfall
#============================================================================================#
#============================================================================================#
pdat = RFcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Rainfall and Growth Rate by Species - No Lag')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray',lwd=0.5)
cols = primary.colors(length(species))
for(i in 1:length(species)){lines(x = win, y = pdat[,speciesID[i],1],col = cols[i],lwd=2)}

legend(x = 'topright',legend = species,fill = cols,title = 'Species',bg = 'white',ncol = 2)

savePlot(filename = paste(figdir,'Lau_RF_byspecies_lag0.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for each species, lag 0, Shortwave
#============================================================================================#
#============================================================================================#
pdat = SWcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Solar Radiation and Growth Rate by Species - No Lag')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray',lwd=0.5)
cols = primary.colors(length(species))
for(i in 1:length(species)){lines(x = win, y = pdat[,speciesID[i],1],col = cols[i],lwd=2)}

legend(x = 'topright',legend = species,fill = cols,title = 'Species',bg = 'white',ncol = 2)

savePlot(filename = paste(figdir,'Lau_SW_byspecies_lag0.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for size classes, lag 0, Temperature
#============================================================================================#
#============================================================================================#
pdat = Tcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Temperature and Growth Rate by Size Class - No Lag')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray',lwd=0.5)
cols = primary.colors(length(sizes))
for(i in 1:length(species)){lines(x = win, y = pdat[,sizeID[i],1],col = cols[i],lwd=3)}

legend(x = 'topright',legend = sizes,fill = cols,title = 'Species',bg = 'white',ncol = 2)

savePlot(filename = paste(figdir,'Lau_T_bysize_lag0.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#



#Plot for size classes, lag 0, Rainfall
#============================================================================================#
#============================================================================================#
pdat = RFcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Rainfall and Growth Rate by Size Class - No Lag')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray',lwd=0.5)
cols = primary.colors(length(sizes))
for(i in 1:length(species)){lines(x = win, y = pdat[,sizeID[i],1],col = cols[i],lwd=3)}

legend(x = 'topright',legend = sizes,fill = cols,title = 'Species',bg = 'white',ncol = 2)

savePlot(filename = paste(figdir,'Lau_RF_bysize_lag0.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#


#Plot for size classes, lag 0, Shortwave
#============================================================================================#
#============================================================================================#
pdat = SWcors

plot(x = win,
y = pdat[,1,1],
type = 'n',
xlab = 'Window Size (days)',
ylab = 'Correlation Coefficient',
main = 'Laupahoehoe - Correlation Between Average Solar Radiation and Growth Rate by Size Class - No Lag')

abline(v = seq(0,1000,30),h = seq(-1,1,0.5),col = 'gray',lwd=0.5)
cols = primary.colors(length(sizes))
for(i in 1:length(species)){lines(x = win, y = pdat[,sizeID[i],1],col = cols[i],lwd=3)}

legend(x = 'topright',legend = sizes,fill = cols,title = 'Species',bg = 'white',ncol = 2)

savePlot(filename = paste(figdir,'Lau_SW_bysize_lag0.jpeg',sep=''), type = 'jpeg')
#============================================================================================#
#============================================================================================#




