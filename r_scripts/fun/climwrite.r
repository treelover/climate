climwrite = function(x,loc){
#loc = '/home/adam/Dropbox/sapflow/test.dat'
#This function uses the sink function to write tables back out in the campbell format. 
#Input "x" is a list object created by climread.r

#Extracting data from x
dat = x$dat

sink(loc) 
cat(dQuote(x$header1),sep=',')
cat("\n") 
cat(dQuote(x$header2),sep=',')
cat("\n") 
cat(dQuote(x$header3),sep=',')
cat("\n") 
cat(dQuote(x$header4),sep=',')
cat("\n") 

#Writing the data line by line
for(i in 1:nrow(dat)){
	cat(as.character(dat[i,]),sep=',')
	cat("\n") 
}

#Closing the file
sink() 
}
