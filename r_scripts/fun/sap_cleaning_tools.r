#A set of tools to clean up the sapflow data with. 
#First used on petunia_clean3. This is after derek_clean4 was run. derek_clean4 is where these tools were developed/polished.




#REPLOT Function
#---------------------------------------#
replot = function(x){
time = as.POSIXct(data[,1])
startint = as.POSIXct(seq(data[1,1],data[nrow(data),1],(60*60*24*14)))

#Making sure startint is same class as timestamp column in data
#if(class(data[,1]
#class(startint) = class(data[,1])

for(i in 1:(length(startint)-1)){

	for(j in x:y){

		start = startint[i]
		if(i == 1){start = time[1]}

		end = startint[i+1]-(60*10)
		if(i == length(startint)-1){end = time[length(time)]}
	
		pID = which(data[,1] == start):which(data[,1] == end)

		if(all(is.na(data[pID,j+3]))){plot(x = 1,y=1,main = paste('Profile ',j,':   ',start,' - ',end,sep=''))
										savePlot(paste('/home/',home,'/Dropbox/sapflow/figs/sap_diagnose/',logname,'_',j,'_date',i,'.jpeg',sep=''),type = 'jpeg')
										next}

		plot(x = data[pID,1],
		y = data[pID,j+3],
		xlab = 'Date',
		ylab = 'Temp dif (deg.C)',
		type = 'l',
		lwd = 3,
		xaxp = c(start,end,7),
		las = 1,
		main = paste('Profile ',j,':   ',start,' - ',end,sep='')
		)
		
		abline(v = seq(round(start,'days'),round(end,'days'),(60*60*24)),col = 'gray')
		
		savePlot(paste('/home/',home,'/Dropbox/sapflow/figs/sap_diagnose/',logname,'_',j,'_date',i,'.jpeg',sep=''),type = 'jpeg')
	}
}
}
#---------------------------------------#


#DAY function. Returns a days worth of data and timestamps
#---------------------------------------#
day = function(date,i){tid = which(substr(data[,1],1,10) == as.POSIXlt(paste('2014-',date,sep='')));return(data[tid,c(1,i+3)])}
#---------------------------------------#


#PASS function. Turns all data above/below a certain value to NA
#---------------------------------------#
pass = function(direc,value,sap){
		if(direc == 'greater'){data[which(data[,sap+3] > value),sap+3] = NA}
		if(direc == 'less'){data[which(data[,sap+3] < value),sap+3] = NA}
		return(data)
		}
#---------------------------------------#	


#CH function - changes data in the specified range to NA
#---------------------------------------#	
ch = function(rows,sap){data[rows,sap+3] = NA;return(data)}	
#---------------------------------------#	


#SCHEDJ1 function - filter for probes that are on thurs thru sat
#---------------------------------------#	
schedj1 = function(sap){

	#vector of NA the length of time
	multip = rep(NA,length(time))
	
	#Day of Week string
	dow = strftime(time,'%a')
	
	#Which ones are Thursday after 2:30 am? (switches on at 2, throw out first half hour as heater warms)
	#Then frisat, then same routine for sunday before 2
	thu = which(dow == 'Thu' & substr(time,12,19) >= '02:30:00')
	frisat = which(dow == 'Fri' | dow == 'Sat')
	sun = which(dow == 'Sun' & substr(time,12,19) <= '02:00:00')
	alldays = c(thu,frisat,sun)
	
	#Putting 1's in where valid data is
	multip[alldays] = 1
	
	data[,sap+3] = data[,sap+3]*multip
	
	return(data)
	}
#---------------------------------------#	


#SCHEDJ2 function - filter for probes that are on mon thru thurs
#---------------------------------------#
schedj2 = function(sap){
	multip = rep(NA,length(time))
	dow = strftime(time,'%a')	
	mon = which(dow == 'Mon' & substr(time,12,19) >= '02:30:00')
	tuewed = which(dow == 'Tue' | dow == 'Wed')
	thu = which(dow == 'Thu' & substr(time,12,19) <= '02:00:00')
	alldays = c(mon,tuewed,thu)
	multip[alldays] = 1
	data[,sap+3] = data[,sap+3]*multip
	return(data)
	}
#---------------------------------------#	






		
