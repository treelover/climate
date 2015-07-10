#This script will, on a daily basis, create summary figures for the past 3 days of sapflow data 
source('/home/adam/Dropbox/sapflow/r_scripts/fun/read.saptab.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/sapread.r')
source('/home/adam/Dropbox/sapflow/r_scripts/fun/sapplot.r')

#Directories
#---------------------------------------#
cdir = '/home/adam/Dropbox/sapflow/'
figdir = '/home/adam/Dropbox/sapflow/figs/sap_1day/'
#---------------------------------------#

#Reading in the most recent sapflow data
#------------------------------------------------------------#
derek = sapread(paste(cdir,'derek/raw/derek_sapdat.dat',sep=''))
pet = sapread(paste(cdir,'petunia/raw/petunia_sapdat.dat',sep=''))
#PETUNIA HAS HEADERS WITH A CAPITAL S IN "SAP_AVG(x)". QUICK STOPGAP HERE. FIX THIS LATER.
colnames(pet)[5:9] = paste('sap_Avg(',1:5,')',sep='')
koa = sapread(paste(cdir,'koa/raw/koa_sapdat.dat',sep=''))
ohia = sapread(paste(cdir,'ohia/raw/ohia_sapdat.dat',sep=''))
#------------------------------------------------------------#

#Reading in the table of info
#------------------------------------------------------------#
saptab = read.saptab(paste(cdir,'tree_params.csv',sep=''))
#------------------------------------------------------------#

#Setting up the times
#------------------------------------------------------------#
now = Sys.time()
now = as.POSIXct(paste(substr(now,1,11),'00:00:00',sep=''))

dayback = now-(60*60*24)
xback = now-(60*60*24*3)

#putting those in a list
times = list(now,dayback,xback)
names(times) = c('now','dayback','xback')
#------------------------------------------------------------#

#Lets start with Koa logger:
#------------------------------------------------------------#
ID = which(saptab$Logger == 'koa')
tab = saptab[ID,]
timeID = which(koa$TIMESTAMP == xback):which(koa$TIMESTAMP == now)

pdat = koa[timeID,'TIMESTAMP']
pdat = cbind(pdat,koa[timeID,match(as.character(saptab$Table_loc[ID]),colnames(koa))])
colnames(pdat)[1] = 'TIMESTAMP'

#Setting up stuff for sapplot
pID = tab$Table_loc[1:5]                            #ID of things to plot
loc = paste(figdir,now,'_koa1.jpeg',sep ='')           #location of file to plot

#Plawtin
sapplot(tab,pdat,pID,times,loc)

#That was so fun lets do it again!
pID = tab$Table_loc[6:10]                            #ID of things to plot
loc = paste(figdir,now,'_koa2.jpeg',sep ='')           #location of file to plot

#Plawtin
sapplot(tab,pdat,pID,times,loc)
#------------------------------------------------------------#

#Ohia logger
#------------------------------------------------------------#
ID = which(saptab$Logger == 'ohia')
tab = saptab[ID,]
timeID = which(ohia$TIMESTAMP == xback):which(ohia$TIMESTAMP == now)

pdat = ohia[timeID,'TIMESTAMP']
pdat = cbind(pdat,ohia[timeID,match(as.character(saptab$Table_loc[ID]),colnames(ohia))])
colnames(pdat)[1] = 'TIMESTAMP'

#Setting up stuff for sapplot
pID = tab$Table_loc[1:5]                                #ID of things to plot
loc = paste(figdir,now,'_ohia1.jpeg',sep ='')           #location of file to plot
ylims = c(0,30)                                         #Ylims for the plot
#Plawtin
sapplot(tab,pdat,pID,times,loc,ylims)

#That was so fun lets do it again!
pID = tab$Table_loc[6:10]                               #ID of things to plot
loc = paste(figdir,now,'_ohia2.jpeg',sep ='')           #location of file to plot
ylims = c(0,15)                                         #Ylims for the plot

#Plawtin
sapplot(tab,pdat,pID,times,loc,ylims)
#------------------------------------------------------------#

#Derek logger
#------------------------------------------------------------#
ID = which(saptab$Logger == 'derek')
tab = saptab[ID,]
timeID = which(derek$TIMESTAMP == xback):which(derek$TIMESTAMP == now)

pdat = derek[timeID,'TIMESTAMP']
pdat = cbind(pdat,derek[timeID,match(as.character(saptab$Table_loc[ID]),colnames(derek))])
colnames(pdat)[1] = 'TIMESTAMP'

#Setting up stuff for sapplot
pID = tab$Table_loc[1:5]                                 #ID of things to plot
loc = paste(figdir,now,'_derek1.jpeg',sep ='')           #location of file to plot
ylims = c(0,15)                                         #Ylims for the plot

#Plawtin
sapplot(tab,pdat,pID,times,loc,ylims)

#That was so fun lets do it again!
pID = tab$Table_loc[6:10]                                #ID of things to plot
loc = paste(figdir,now,'_derek2.jpeg',sep ='')           #location of file to plot
#ylims = c(0,15)                                         #Ylims for the plot

#Plawtin
sapplot(tab,pdat,pID,times,loc,ylims)

#That was so fun lets do it again!
pID = tab$Table_loc[11:15]                                #ID of things to plot
loc = paste(figdir,now,'_derek3.jpeg',sep ='')           #location of file to plot
#ylims = c(0,15)                                         #Ylims for the plot

#Plawtin
sapplot(tab,pdat,pID,times,loc,ylims)
#------------------------------------------------------------#

#Petunia logger
#------------------------------------------------------------#
ID = which(saptab$Logger == 'petunia')
tab = saptab[ID,]
timeID = which(pet$TIMESTAMP == xback):which(pet$TIMESTAMP == now)

pdat = pet[timeID,'TIMESTAMP']
pdat = cbind(pdat,pet[timeID,match(as.character(saptab$Table_loc[ID]),colnames(pet))])
colnames(pdat)[1] = 'TIMESTAMP'

#Setting up stuff for sapplot
pID = tab$Table_loc[1:5]                                 #ID of things to plot
loc = paste(figdir,now,'_petunia1.jpeg',sep ='')           #location of file to plot
ylims = c(0,15)                                         #Ylims for the plot

#Plawtin
sapplot(tab,pdat,pID,times,loc,ylims)
#------------------------------------------------------------#


