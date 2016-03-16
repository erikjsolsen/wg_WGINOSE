#' @title Script for plotting time-series of mean and SD of top ranked variables
#' @description based on PCA analysis of NS ecosystem data
#' @description  requires running 'PCA ordination and heat plot' script that generates PCA .csv file
#' 16.03.2016


#' LIBRARIES
#' -------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

#' looping through all WGINOSE AREAS
#' ---------------------

area<-c(1:11)
area.names<-c("Orkney-Shetland", "Fladen", "Utsira", "Long Forties", "Dogger Bank", "Norfolk Banks", "German Bight", "Oyster Ground", "Southern Bight", "Skagerrak", "Kattegat" )
# Add area names to the plots and file names

#for (i in 1:length(area)){ 
for (i in 8:length(area)){  # not the same number of StDev and mean columns
  

  #' SET WORKING DIRECTORY
  setwd("~/ownCloud/Research/WGINOSE/PCA IEA analysis/TS plots") 
  
  
  #' IMPORT FILES
  #' ----------------
  PCAtable<-tbl_df(read.csv(paste("../WGINOSE16_Varloadings_",area.names[i], area[i], ".csv", sep="")))
  CPUEsd<-tbl_df(read.csv(paste("../../data/CPUE st_dev area ",area[i],".csv", sep="")))
  CPUEmean<-tbl_df(read.csv(paste("../../data/CPUE mean area ",area[i],".csv", sep="")))
  Other<-tbl_df(read.csv(paste("../../data/wgi",area[i],"_mean_std.csv", sep="")))
  Other<-filter(Other, Year>1983)
  
  cn<-colnames(CPUEsd)
  for (j in 1:length(cn)){ cn[j]<-paste(cn[j], "_std", sep="")  }
  colnames(CPUEsd)<-cn
  
  cn<-colnames(CPUEmean)
  for (j in 1:length(cn)){ cn[j]<-paste(cn[j], "_mean", sep="")  }
  colnames(CPUEmean)<-cn
  
  #' combine all data sets into one df
  means<-cbind(CPUEmean, Other[grep("_mean_", colnames(Other))])
  stds<-cbind(CPUEsd, Other[grep("_std_", colnames(Other))])
  
  #' identify 2 varibles with highest PC1 ranks
  PCAtable<-arrange(PCAtable, pc1)
  High.rank<- PCAtable$variable[c(1,2,length(PCAtable$variable),length(PCAtable$variable) -1 ) ]
  
  #' make combined data set of High Ranked variables and StDev for plotting
  HR1<-cbind(means$Year_mean, means[grep(High.rank[1], colnames(means))], stds[grep(High.rank[1], colnames(means))])
  HR1$HR<-c(1)
  HR1$variable<-colnames(HR1[2])
  colnames(HR1)<-c("year", "data", "StDev", "HR", "variable")
  
  HR2<-cbind(means$Year_mean, means[grep(High.rank[2], colnames(means))], stds[grep(High.rank[2], colnames(means))])
  HR2$HR<-c(2)
  HR2$variable<-colnames(HR2[2])
  colnames(HR2)<-c("year", "data", "StDev", "HR", "variable")
  
  HR3<-cbind(means$Year_mean, means[grep(High.rank[3], colnames(means))], stds[grep(High.rank[3], colnames(means))])
  HR3$HR<-c(3)
  HR3$variable<-colnames(HR3[2])
  colnames(HR3)<-c("year", "data", "StDev", "HR", "variable")
  
  HR4<-cbind(means$Year_mean, means[grep(High.rank[4], colnames(means))], stds[grep(High.rank[4], colnames(means))])
  HR4$HR<-c(4)
  HR4$variable<-colnames(HR4[2])
  colnames(HR4)<-c("year", "data", "StDev", "HR", "variable")
  
  HRdata<-rbind(HR1, HR2, HR3, HR4)
  #HRdata<-rbind(HR1, HR2) special case for Area 7 with too few observations to calculate StDev for many of the variables

  #' create plot using geom_ribbon + geom_line
  HRplot<-ggplot(HRdata, aes(year))
  
  HRplot<-HRplot + geom_ribbon(aes(ymin=(data-StDev), ymax=(data+StDev)), fill = "grey70") +  geom_line(aes(y = data)) + theme_bw() + facet_wrap(~variable, ncol=1, scales="free") + ggtitle(paste(area[i], area.names[i]))
  HRplot
  ggsave(paste("TSplot_area_",area[i], area.names[i], ".png", sep="" ))

  
}
  
  