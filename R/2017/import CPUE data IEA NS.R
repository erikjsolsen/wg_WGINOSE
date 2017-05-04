#' Creating fisheries input to NS IEA analysis
#' @description   Importing and wrangling very large (34 MB) CPUE file
#' @author Erik Olsen
#' 15.03.2017
#' @note Updated to work with 2017 data for WGINOSE analysis

#' LIBRARIES
#' --------------------
library(dplyr)
library(tidyr)



#' IMPORT DATA
#' --------------------
setwd("~/ownCloud/Research/WGINOSE/2017/data") 
CPUE.data<-read.csv2("WGINOSE_Age_2017_v3.csv", na.strings="NULL")

#' make table_dataframe (dplyr)
CPUE.data<-tbl_df(CPUE.data)

CPUE.data<-mutate(CPUE.data, SumCPUE = rowSums(CPUE.data[14:29], na.rm=TRUE))

#' Cacluate mean CPUE pr species pr year
#' use only Q1 data
CPUE.q_base<-filter(CPUE.data, Quarter==1)
#CPUE.q_base<-filter(CPUE.q_base, Year<2015)
CPUE.q_base<-select(CPUE.q_base, Year, ScientificName_WoRMS, Description, areaname, SumCPUE)
CPUE.q_base<-arrange(CPUE.q_base, ScientificName_WoRMS)



years<-as.integer(levels(as.factor(CPUE.q_base$Year)))
species<-levels(CPUE.q_base$ScientificName_WoRMS)

#' Aggregate pr WGINOSE Area
k<-as.integer(levels(as.factor(CPUE.data$areaname)))

for (j in 1:length(k)) { 
  CPUE.q1<-filter(CPUE.q_base, areaname==k[j])

  #' aggregate sum CPUE by species pr year
  for (i in 1:length(species)) {
    Cq1<-filter(CPUE.q1, ScientificName_WoRMS==species[i])
    SpAvg<-Cq1 %>% group_by(Year) %>% summarise(sum = sum(SumCPUE))
    colnames(SpAvg)<-c("Year", species[i])
    if(i==1){
      SpTable<-SpAvg
    }
    if(i>1){
      if (length(SpAvg$Year)<length(SpTable$Year)){
        d<-data.frame(x=1:(length(SpTable$Year)-length(SpAvg$Year)), y=0)
        colnames(d)<-colnames(SpAvg)
        SpAvg<-rbind(d, SpAvg)
        
      }
    {
      SpTable<-cbind(SpTable, SpAvg[2])
    }
    }
  }
  
  write.csv(SpTable, paste("CPUE sum area ",k[j],".csv", sep=""))
  
  
  #' aggregate mean CPUE by species pr year
  for (i in 1:length(species)) {
    Cq1<-filter(CPUE.q1, ScientificName_WoRMS==species[i])
    SpAvg<-Cq1 %>% group_by(Year) %>% summarise(avg = mean(SumCPUE))
    colnames(SpAvg)<-c("Year", species[i])
    if(i==1){
      SpTable<-SpAvg
    }
    if(i>1){
      if (length(SpAvg$Year)<length(SpTable$Year)){
        d<-data.frame(x=1:(length(SpTable$Year)-length(SpAvg$Year)), y=0)
        colnames(d)<-colnames(SpAvg)
        SpAvg<-rbind(d, SpAvg)
        
      }
      {
      SpTable<-cbind(SpTable, SpAvg[2])
      }
    }
  }
  
  write.csv(SpTable, paste("CPUE mean area ",k[j],".csv", sep=""))
  
  
  #' aggregate standard deviation of CPUE by species pr year
  for (i in 1:length(species)) {
    Cq1<-filter(CPUE.q1, ScientificName_WoRMS==species[i])
    SpAvg<-Cq1 %>% group_by(Year) %>% summarise(sd = sd(SumCPUE))
    colnames(SpAvg)<-c("Year", species[i])
    if(i==1){
      SpVariance<-SpAvg
    }
    if(i>1){
      if (length(SpAvg$Year)<length(SpTable$Year)){
        d<-data.frame(x=1:(length(SpTable$Year)-length(SpAvg$Year)), y=0)
        colnames(d)<-colnames(SpAvg)
        SpAvg<-rbind(d, SpAvg)
      }
    SpVariance<-cbind(SpVariance, SpAvg[2])
    }
  }
  
  write.csv(SpVariance, paste("CPUE st_dev area ",k[j],".csv", sep=""))
}

