read_REST2 = function(fname){

#fname = '/home/adam/Dropbox/sapflow/clim/clear_sky_rad/raw/Hakalau_REST2_out.txt'

dat = as.matrix(read.table(file = fname,header = F, skip = 15))
dat = matrix(as.numeric(dat),ncol = ncol(dat),nrow = nrow(dat))

headers = as.matrix(read.table(file = fname,header = F, skip = 13,nrows = 1))

colnames(dat) = headers

dat[which(dat == -9.999, arr.ind = T)] = NA

return(dat)}






