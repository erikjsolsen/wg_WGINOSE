' @description   Importing and wrangling very large (34 MB) CPUE file

#' @author Erik Olsen

#' updated June 2017 for WGINOSE 2018 report



#' LIBRARIES

#' --------------------

library(dplyr)

library(tidyr)







#' IMPORT DATA

#' --------------------

setwd("~/ownCloud/Research/WGINOSE/2018/data")

#' FIX corrupt DATRAS data
#' run 'fixDatras.R'



#cls  <- c(Survey="factor", Year="numeric", Quarter="numeric", Ship="factor", Gear="factor", HaulNo="numeric", DateTime="Date", Depth="numeric", Area="numeric", SubArea="Factor", DayNight="factor", ScientificName_WoRMS="character", Sex="factor", Age_0="numeric", Age_1="numeric", Age_2="numeric", Age_3="numeric", Age_4="numeric", Age_5="numeric", Age_6="numeric", Age_7="numeric",Age_8="numeric", Age_9="numeric", Age_10="numeric", Age_11="numeric", Age_12="numeric", Age_13="numeric", Age_14="numeric", Age_15="numeric", ICESArea="factor", areaname="numeric", Decscription="character", Division="character", ShootLat="numeric", ShootLon="numeric", WoRMS_AphiaID_Valid="numeric")


CPUE.data <- read.csv("./WGINOSE/WGINOSE_Age_DATRAS.csv_fixed.csv")

#convert factor columns to numeric

CPUE.data$Year <- as.integer(CPUE.data$Year)+1983
CPUE.data$Age_6 <- as.numeric(CPUE.data$Age_6)
CPUE.data$Age_7 <- as.numeric(CPUE.data$Age_7)
CPUE.data$Age_8 <- as.numeric(CPUE.data$Age_8)
CPUE.data$Age_9 <- as.numeric(CPUE.data$Age_9)
CPUE.data$Age_10 <- as.numeric(CPUE.data$Age_10)
CPUE.data$Age_11 <- as.numeric(CPUE.data$Age_11)
CPUE.data$Age_12 <- as.numeric(CPUE.data$Age_12)
CPUE.data$Age_13 <- as.numeric(CPUE.data$Age_13)
CPUE.data$Age_14 <- as.numeric(CPUE.data$Age_14)
CPUE.data$Age_15 <- as.numeric(CPUE.data$Age_15)

CPUE.data <- subset(CPUE.data, Year<=2018)

summary(CPUE.data)



#' make table_dataframe (dplyr)

CPUE.data<-tbl_df(CPUE.data)



CPUE.data<-mutate(CPUE.data, SumCPUE = rowSums(CPUE.data[14:29], na.rm=TRUE))



#' Cacluate mean CPUE pr species pr year

#' use only Q1 data

CPUE.q_base<-filter(CPUE.data, Quarter==1)

CPUE.q_base<-filter(CPUE.q_base, Year<2019)

CPUE.q_base<-select(CPUE.q_base, Year, ScientificName_WoRMS, areaname, SumCPUE)

CPUE.q_base<-arrange(CPUE.q_base, ScientificName_WoRMS)







years<-as.integer(levels(as.factor(CPUE.q_base$Year)))

species<-levels(CPUE.q_base$ScientificName_WoRMS)



#' Aggregate pr WGINOSE Area

k<-c(1:11)



for (j in 1:length(k)) { 

  CPUE.q1<-filter(CPUE.q_base, areaname==k[j])



  #' aggregate sum CPUE by species pr year

  for (i in 1:length(species)) {

    Cq1<-filter(CPUE.q1, ScientificName_WoRMS==species[i])
    

    SpAvg<-Cq1 %>% group_by(Year) %>% summarise(sum = sum(SumCPUE, na.rm = TRUE))
    

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

