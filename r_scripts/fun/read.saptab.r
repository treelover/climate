read.saptab = function(loc){

#loc = '/home/adam/Dropbox/sapflow/tree_params.csv'

dat = read.table(loc,sep=',',header = T)

dat$Install_date = strptime(as.character(dat$Install_date),format = '%m/%d/%Y')
dat$Start_date = strptime(as.character(dat$Start_date),format = '%m/%d/%Y')

return(dat)
}
