## yearly averages and st dev - for surface and bottom
## not station or platform specific
## if files won't read in - the data is not a true .csv file despite the name - open and save fixes the issue
## read.table skips a line b/c data have a description in the 1st row - hopefully this doesn't change in future
## data are sent as zipped file from DATRAS data center - one file per strata for surface, 1 per strata for bottom

## code is verbose 

## there was an issues with data being split over 2 rows (have partial data in 2 rows)
## not solving this because aggregating over all data in each area

##-------------------------------------------------------------------------
require(data.table)

where<-"C:\\Users\\jenniferd\\Documents\\_2017\\WGINOSE\\oceanogr data\\"

filer<-list.files(path = where)
filer.s<-filer[substr(filer,nchar(filer)-4,nchar(filer)-4)=='e']
filer.b<-filer[substr(filer,nchar(filer)-4,nchar(filer)-4)=='m']

#surface data
for(i in 1:length(filer.s)) {
  fil<-read.table(paste(where,filer.s[i],sep=''),skip=1,header=T,sep=',')
  fil$area<-substr(filer.s[i],1,nchar(filer.s[i])-12)
#  print(i);print(nrow(fil))        # check
  if(i==1) x<-fil
    else   x<-merge(x,fil,all=T)   #merge in case there are different variables in files
}
#length(filer.s)                    # check

for(i in 1:length(filer.b)) {
  fil<-read.table(paste(where,filer.b[i],sep=''),skip=1,header=T,sep=',')
  fil$area<-substr(filer.s[i],1,nchar(filer.s[i])-12)
#  print(i);print(nrow(fil))           # check
  if(i==1) y<-fil
    else   y<-merge(y,fil,all=T)
}
#length(filer.b)                       # check

# replace -9 with NA in entire data set for both surface and bottom
x[x=="-9"]<-NA
y[y=="-9"]<-NA

# removing unnecessary columns b/c aggregate doesn't always work seamlessly otherwise
x<-x[,c(5,11:ncol(x))];y<-y[,c(5,11:ncol(y))]

# estimating mean and std deviation
xx<-setDT(x)[, as.list(unlist(lapply(.SD, function(x) list(mn=mean(x,na.rm=T),std=sd(x,na.rm=T))))), by=c('area','Year'),.SDcols=c(names(x))]
yy<-setDT(y)[, as.list(unlist(lapply(.SD, function(x) list(mn=mean(x,na.rm=T),std=sd(x,na.rm=T))))), by=c('area','Year'),.SDcols=c(names(y))]
# kicking out a few columns
xx<-xx[,-c(3:4,33:34)];yy<-yy[,-c(3:4,33:34)]
# adding surface and bottom to names
names(xx)[-c(1:2)]<-paste('surf',names(xx)[-c(1:2)],sep='.')
names(yy)[-c(1:2)]<-paste('bot',names(yy)[-c(1:2)],sep='.')

# merging data, reodering by area & year, write out file
xy<-merge(xx,yy,by=c('Year','area'),all=T)
xy<-xy[order(xy$area,xy$Year),]

write.table(xy,"C:\\Users\\jenniferd\\Documents\\_2017\\WGINOSE\\data\\oceanogr data 1983_2016.csv",row.names=F,quote=F,sep=',')


