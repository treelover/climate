sapplot = function(tab,pdat,pID,times,loc){
#This script creates a plot of the sapflow data. 


#color vector
cols = c('blue','red','forestgreen','darkmagenta','darkorange2','midnightblue','gold','gray48','lawngreen','mediumorchid1')

#title
title = paste('Trees on the ',tab$Logger[1],' logger ',strftime(times$xback,format = '%b %d'),' to ',strftime(times$dayback,format = '%b %d, %Y'),sep='')

#legend labels
leglabs = paste(tab$Name,' - ',tab$Depth,'cm',sep='')[match(pID,tab$Table_loc)]

#Starting the jpeg
jpeg(loc,height = 600, width = 1000)

plot(x = pdat$TIMESTAMP,
     y = rep(NA,length(pdat$TIMESTAMP)),
     ylab = 'Temperature difference (deg. C)',
     xlab = '',
     main = title,
     cex.main = 1.5,
     type = 'n',
     ylim = c(0,20),
     )
     
abline(h = seq(-50,50,5),v = seq(times$xback,times$now,'day'),col = colors()[220])
abline(v = dayback, col = 'black')

for(i in 1:length(pID)){
	y = pdat[,which(colnames(pdat) == pID[i])]
	lines(x = pdat$TIMESTAMP,y = y, col = cols[i],lwd = 3)
}

legend('topleft',legend = leglabs,fill = cols,bg='white')

dev.off()
}
