status_plot = function(slist,now,dayback,threeback){

#dlist = dlist[[5]]

#NAME: status
#AUTHOR: Adam Sibley
#LAST MODIFIED: Oct 17th, 2014
#DESCRIPTION: This function plots battery voltage and enclosure humidity for all of the stations. 

#Da title page
plot(1, type="n", axes=F, xlab="", ylab="")
mtext('Status Plots',cex = 4, font = 2, outer = F,line = -10)

#Setting up the page for Battery Voltage and Enclosure Humidity
par(mfrow = c(2,1),mar = c(3,5,5,4))



#BATTERY VOLTAGE
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#

#Make title
title = 'Battery Voltage'

#Timestamps for this period of data
times = as.POSIXlt(seq(threeback,now,60*10))

#Setting up a matrix for all the battery data
humdat = matrix(NA,ncol = length(slist),nrow = length(seq(threeback,now,60*10)))
colnames(humdat) = names(slist)

#Going through the battery voltage data and 
for(i in 1:length(slist)){

	tempdat = slist[[i]]$dat
	colnames(tempdat) = slist[[i]]$header2

	ID = match(times,slist[[i]]$time)
	humdat[,i] = tempdat$BattVolt_Avg[ID]
}


	#Test if all the stations are NA
	#...............................................#
	if(all(is.na(humdat)) == T){
		plot(x = 1:10,y = 1:10, type = 'n',xlab = '',ylab='')
		text(x = c(5,5), y = c(5,4), labels = c('All battery voltages coming through NA.', 'Check data file!'))
	}else{
	#...............................................#

#Start the plot
plot(x = times,
y = rep(NA,length(times)),
type = 'n',
ylim = c(10,14),
las = 1,
ylab = 'Volts',
xlab = '')

title(main = title, cex.main = 1.4, line = 0.5)

#Add some lines
abline(h = seq(8,15,1),v = seq(threeback,now,'day'),col = colors()[220])
abline(v = dayback, col = 'black')

#Colors for each stations
plotcols = colors()[c(29,552,32,33,90,99,258,259,153,254)]

#building the legend
leglab = c()

for(i in 1:ncol(humdat)){
	
	#Testing if the column is all NA
	if(all(is.na(humdat[,i])) == T){leglab[i] = paste(colnames(humdat)[i],' - All NA',sep='');next}
		
	#Building the legend and ploting
	leglab[i] = colnames(humdat)[i]
	lines(x = times, y = humdat[,i],lwd = 2,col = plotcols[i])

}

legend(x = 'bottomleft',legend = leglab,lwd = 2,col = plotcols,ncol = 2,bg = 'white')

}


#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#



#ENCLOSURE HUMIDITY
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#

#Make title
title = 'Enclosure Humidity'

#Timestamps for this period of data
times = as.POSIXlt(seq(threeback,now,60*10))

#Setting up a matrix for all the battery data
humdat = matrix(NA,ncol = length(slist),nrow = length(seq(threeback,now,60*10)))
colnames(humdat) = names(slist)

#Going through the enclosure humidity data and putting it in a matrix
for(i in 1:length(slist)){

	tempdat = slist[[i]]$dat
	colnames(tempdat) = slist[[i]]$header2

	ID = match(times,slist[[i]]$time)
	humdat[,i] = tempdat$RH_enc[ID]
}


	#Test if all the stations are NA
	#...............................................#
	if(all(is.na(humdat)) == T){
		plot(x = 1:10,y = 1:10, type = 'n',xlab = '',ylab='')
		text(x = c(5,5), y = c(5,4), labels = c('All battery voltages coming through NA.', 'Check data file!'))
	}else{
	#...............................................#

#Start the plot
plot(x = times,
y = rep(NA,length(times)),
type = 'n',
ylim = c(0,100),
las = 1,
ylab = 'Humidity (%)',
xlab = '')

title(main = title, cex.main = 1.4, line = 0.5)

#Add some lines
abline(h = seq(-10,110,10),v = seq(threeback,now,'day'),col = colors()[220])
abline(v = dayback, col = 'black')

#Colors for each stations
plotcols = colors()[c(29,552,32,33,90,99,258,259,153,254)]

#building the legend
leglab = c()

for(i in 1:ncol(humdat)){
	
	#Testing if the column is all NA
	if(all(is.na(humdat[,i])) == T){leglab[i] = paste(colnames(humdat)[i],' - All NA',sep='');next}
		
	#Building the legend and ploting
	leglab[i] = colnames(humdat)[i]
	lines(x = times, y = humdat[,i],lwd = 2,col = plotcols[i])

}

legend(x = 'topleft',legend = leglab,lwd = 2,col = plotcols,ncol = 2,bg = 'white')

}
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
#=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+#
}





