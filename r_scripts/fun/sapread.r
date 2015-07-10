sapread = function(loc,lowlim = NA,hilim = NA){

#loc = '/home/adam/Data/sapflow/Koa/Koa_Sapdat.dat'

#Raw files will have the extention .dat. Cleaned up files will have the extension .csv. 

#For dat files
#----------------------------------------------------------------------------------------------#
if(substr(loc,nchar(loc)-2,nchar(loc)) == 'dat'){

	#Reading in the data - as a dataframe
	dat = read.table(loc, sep = ',', skip = 4, header = F)

	#Reading in the data - as a matrix
	dat2 = as.numeric(as.matrix(read.table(loc, sep = ',', skip = 4, header = F)))
	dat2 = matrix(dat2,ncol = ncol(dat),nrow = nrow(dat),byrow=F)

	#Header of the file
	heads = as.character(as.matrix(read.table(loc, sep = ',', skip = 1, nrows = 1, header = F)))
	
	#Fixing the header
	fixheads = which(substr(heads,1,3) == 'sap')
	#heads[fixheads] = paste('sap_',fixheads-(head(fixheads,1)-1),sep='')
	colnames(dat) = heads
	
	#Doing the filtering on the numeric data
	dat2 = dat2[,fixheads]
	dat2[which(is.nan(dat2) == T)] = NA
	if(is.na(lowlim) == F){ dat2[which(dat2 <= lowlim)] = NA} #Low filter
	if(is.na(hilim) == F){ dat2[which(dat2 >= hilim)] = NA} #Hi filter

	#Turning the factor bullshit into numeric
	dat[,fixheads] = dat2
}
#----------------------------------------------------------------------------------------------#

#For CSVs
#----------------------------------------------------------------------------------------------#
if(substr(loc,nchar(loc)-2,nchar(loc)) == 'csv'){

	#Reading in the data - as a dataframe
	dat = read.table(loc, sep = ',', skip = 1, header = F)

	#Reading in the data - as a matrix
	dat2 = as.numeric(as.matrix(read.table(loc, sep = ',', skip = 1, header = F)))
	dat2 = matrix(dat2,ncol = ncol(dat),nrow = nrow(dat),byrow=F)

	#Header of the file
	heads = as.character(as.matrix(read.table(loc, sep = ',', nrows = 1, header = F)))
	
	#Doing the filtering on the numeric data
	dat2 = dat2[,which(substr(heads,1,3) == 'sap')]
	dat2[which(is.nan(dat2) == T)] = NA
	if(is.na(lowlim) == F){ dat2[which(dat2 <= lowlim)] = NA} #Low filter
	if(is.na(hilim) == F){ dat2[which(dat2 >= hilim)] = NA} #Hi filter
	
	#Putting the filtered data back in 
	dat[,which(substr(heads,1,3) == 'sap')] = dat2
	colnames(dat) = heads
}
#----------------------------------------------------------------------------------------------#


#Fixing up the timestring stuff
ptime = as.POSIXct(strptime(dat[,1],'%Y-%m-%d %H:%M:%S'))
dat$TIMESTAMP = ptime

return(dat)
}













