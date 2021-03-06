#This script takes the daily produced weather variables and piles them into one easy to read, handy-dandy PDF
#Started on July 17th, 2014
#Author: Adam Sibley

#Setting the home folder
#---------------------------------------#
tmp = system('ls /home/', intern=T)
if(length(which(tmp == 'adam')) > 0){home = 'adam'}
if(length(which(tmp == 'ender')) > 0){home = 'ender'}
#---------------------------------------#

#Libraries and functions
#---------------------------------------#
source(paste('/home/',home,'/Dropbox/sapflow/r_scripts/fun/climread.r',sep=''))
library(openair)
library(jpeg)
#---------------------------------------#

#Directories
#---------------------------------------#
cdir = paste('/home/',home,'/Dropbox/sapflow/clim/',sep='')
figdir = paste('/home/',home,'/Dropbox/sapflow/figs/clim_1day/',sep='')
#---------------------------------------#


#Station names
#---------------------------------------#
statnames = c('IPIF','Kiholo','Laupahoehoe','Mamalahoa','Palamanui','PuuWaawaa','Spencer')
#---------------------------------------#


#Times
#---------------------------------------#
now = Sys.time()
now = as.POSIXct(paste(substr(now,1,11),'00:00:00',sep=''))

dayback = now-(60*60*24)
threeback = now-(60*60*24*3)
#---------------------------------------#


#Starting the PDF
#---------------------------------------#
pdf('/home/adam/junk/test.pdf',width = 8.5, height = 11)
#---------------------------------------#

#Reading in a jpeg to test with
testj = readJPEG(paste(figdir,'2014-07-14_IPIF_Wind.jpeg',sep=''))
testr = as.raster(testj)

rasterImage(image = testj, xleft = 3, ybottom = 7,xright = 6,ytop = 10)

testp = readPNG(paste(figdir,'2014-07-14_IPIF_Wind.png',sep=''))

mtext(text = 'Big Island Weather Station Summary...In Spaaaace!!!',line = 10,cex=5)

plotRGB(x = test2, r = 1, g = 2, b = 3, scale = 1)

dev.off()




