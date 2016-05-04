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

#area<-c(1:11)
#area.names<-c("Orkney-Shetland", "Fladen", "Utsira", "Long Forties", "Dogger Bank", "Norfolk Banks", "German Bight", "Oyster Ground", "Southern Bight", "Skagerrak", "Kattegat" )

#select only areas w benthos data
area<-c(7:9)
area.names<-c("German Bight", "Oyster Ground", "Southern Bight")

# Add area names to the plots and file names

#for (i in 1:length(area)){ 
for (i in 1:length(area)){  # not the same number of StDev and mean columns
  

  #' SET WORKING DIRECTORY
  setwd("~/ownCloud/Research/WGINOSE/PCA IEA analysis/TS plots") 
  
  
  #' IMPORT FILES
  #' ----------------
  PCAtable<-tbl_df(read.csv(paste("../WGINOSE16_Varloadings_",area.names[i], area[i], ".csv", sep="")))
  CPUEsd<-tbl_df(read.csv(paste("../../data/CPUE st_dev area ",area[i],".csv", sep="")))
  colnames(CPUEsd)[2]<-c("year")
  CPUEmean<-tbl_df(read.csv(paste("../../data/CPUE mean area ",area[i],".csv", sep="")))
  colnames(CPUEmean)[2]<-c("year")
  #Other<-tbl_df(read.csv(paste("../../data/wgi",area[i],"_mean_std.csv", sep="")))
  Other<-tbl_df(read.csv(paste("../../data/wgi",area[i],"_mean_std.csv", sep="")))
  Other<-filter(Other, Year>1983)
  colnames(Other)[1]<-c("year")
  
  benthos<-tbl_df(read.csv("../../data/ISIS_key_species_nha_station_mean_std.csv"))
  benthos.sel<-filter(benthos, AreaName==area[i])
  benthos.sel<-arrange(benthos.sel, year)
  
  benthos.sel$SCIENTIFIC_NAME<-gsub(" Sp.", "_Sp_", benthos.sel$SCIENTIFIC_NAME)
  benthos.sel$SCIENTIFIC_NAME<-gsub(" sp.", "_sp_", benthos.sel$SCIENTIFIC_NAME)

  bl<-levels(as.factor((benthos.sel$SCIENTIFIC_NAME)))
  
  #select benthos species included in PCA analysis
  #bl<- bl[c(3,5,6,8,9,11,13)]
  
  for (j in 1:length(bl)){ 
    benthos.sel.sp<-filter(benthos.sel, SCIENTIFIC_NAME==bl[j])
    
    Other<-full_join(Other, benthos.sel.sp[,c(2,5,6)], by="year")

    colnames(Other)[c(dim(Other)[2]-1,dim(Other)[2])]<-c(paste(bl[j], "_mean", sep=""), paste(bl[j], "_std", sep=""))
  }
  
  colnames(Other)<-gsub(" ", "_", colnames(Other))

  
  
  cn<-colnames(CPUEsd)
  for (j in 3:length(cn)){ cn[j]<-paste(cn[j], "_std", sep="")  }
  colnames(CPUEsd)<-cn
  
  cn<-colnames(CPUEmean)
  for (j in 3:length(cn)){ cn[j]<-paste(cn[j], "_mean", sep="")  }
  colnames(CPUEmean)<-cn
  
  #' combine all data sets into one df
  means<-full_join(CPUEmean, cbind(Other[1], Other[grep("_mean", colnames(Other))]), by="year")
  stds<-full_join(CPUEsd, cbind(Other[1], Other[grep("_std", colnames(Other))]), by="year")
  #means<-cbind(CPUEmean, Other[grep("_mean", colnames(Other))])
  #stds<-cbind(CPUEsd, Other[grep("_std", colnames(Other))])
  
  #' identify 2 varibles with highest PC1 ranks
  PCAtable<-arrange(PCAtable, pc1)
  High.rank<- PCAtable$variable[c(1,2,length(PCAtable$variable),length(PCAtable$variable) -1 ) ]
  
  #' make combined data set of High Ranked variables and StDev for plotting
  HR1<-cbind(means$year, means[grep(High.rank[1], colnames(means))], stds[grep(High.rank[1], colnames(means))])
  HR1$HR<-c(1)
  HR1$variable<-colnames(HR1[2])
  colnames(HR1)<-c("year", "data", "StDev", "HR", "variable")
  
  HR2<-cbind(means$year, means[grep(High.rank[2], colnames(means))], stds[grep(High.rank[2], colnames(means))])
  HR2$HR<-c(2)
  HR2$variable<-colnames(HR2[2])
  colnames(HR2)<-c("year", "data", "StDev", "HR", "variable")
  
  HR3<-cbind(means$year, means[grep(High.rank[3], colnames(means))], stds[grep(High.rank[3], colnames(means))])
  HR3$HR<-c(3)
  HR3$variable<-colnames(HR3[2])
  colnames(HR3)<-c("year", "data", "StDev", "HR", "variable")
  
  HR4<-cbind(means$year, means[grep(High.rank[4], colnames(means))], stds[grep(High.rank[4], colnames(means))])
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
  



#' Extract RANK from PCA table and add it to a common PCA table
for (i in 1:length(area)){  # not the same number of StDev and mean columns
  setwd("~/ownCloud/Research/WGINOSE/PCA IEA analysis/") 
  PCAtable<-tbl_df(read.csv(paste("WGINOSE16_Varloadings_",area.names[i], area[i], ".csv", sep="")))
  PCAtable<-arrange(PCAtable, variable)
  
  if (i==1) {
    RankTable<-PCAtable[c(2,5,6)]
    colnames(RankTable)[2]<-c("Orkney-Shetland_1")
    colnames(RankTable)[3]<-c("Orkney-Shetland_2")
  }
  
  if (i>1) {
    Pt<-select(PCAtable, variable, Rank1, Rank2)
    RankTable<-full_join(RankTable, Pt, by = "variable")
    colnames(RankTable)[c(2*i)]<-c(paste(area.names[i], "_1", sep=""))
    colnames(RankTable)[c(2*i+1)]<-c(paste(area.names[i], "_2", sep=""))
  }
   
}

#for (i in 1:ncol(RankTable)){
 # if (i==1){ TRank<-rbind(RankTable[1])}
  #if (i>1) {Trank <-rbind(TRank, RankTable[2]) }
  
#}

TRank<-t(RankTable)
colnames(TRank)<-RankTable$variable
TRank<-TRank[2:nrow(TRank),]
class(TRank)<-"numeric"

TRank1<-TRank[grep(1, rownames(TRank)),]
TRank2<-TRank[grep(2, rownames(TRank)),]

#calculate average rank, median rank etc
#BoxPlot Rank

png("Rank boxplot PC1.png", width=1000, height=1000)
par(mai=c(4,1,1,1), cex=1.4)
boxplot(TRank1, las=2)
title("Absolute rank PC1")
dev.off()

png("Rank boxplot PC2.png", width=1000, height=1000)
par(mai=c(4,1,1,1), cex=1.4)
boxplot(TRank2, las=2)
title("Absolute rank PC2")
dev.off()

write.csv(TRank, "Rank tables PCA loadings.csv")

boxplot.stats(TRank2)


#'------------------ REST NOT USED --------
#' cluster analysis & Dendrogram
#' -----------------------------------
#' Not appropriate to use this when combining data from all the different areas. 

RankClust1 <- hclust(dist(TRank1))
RankClust2 <- hclust(dist(TRank2))
# very simple dendrogram
png("Dedrogram PC1 ranks.png")
plot(RankClust1, hang = -1, ann=FALSE)
title("Dendrogram of ranked PC1 scores")
dev.off()

png("Dendrogram PC2 ranks.png")
plot(RankClust2, hang = -1, ann=FALSE)
title("Dendrogram of ranked PC2 scores")
dev.off()


#' Extract PC1 and PC2 from PCA table and add it to a common PCA table
for (i in 1:length(area)){  # not the same number of StDev and mean columns
  setwd("~/ownCloud/Research/WGINOSE/PCA IEA analysis/") 
  PCAtable<-tbl_df(read.csv(paste("WGINOSE16_Varloadings_",area.names[i], area[i], ".csv", sep="")))
  PCAtable<-arrange(PCAtable, variable)
  
  if (i==1) {
    RankTable<-PCAtable[c(2,3,4)]
    colnames(RankTable)[2]<-c("Orkney-Shetland_1")
    colnames(RankTable)[3]<-c("Orkney-Shetland_2")
  }
  
  if (i>1) {
    Pt<-select(PCAtable, variable, pc1, pc2)
    RankTable<-full_join(RankTable, Pt, by = "variable")
    colnames(RankTable)[c(2*i)]<-c(paste(area.names[i], "_1", sep=""))
    colnames(RankTable)[c(2*i+1)]<-c(paste(area.names[i], "_2", sep=""))
  }
  
}

TPC<-t(RankTable)
colnames(TPC)<-RankTable$variable
TPC<-TPC[2:nrow(TPC),]
TPC1<-TPC[grep(1, rownames(TPC)),]
TPC2<-TPC[grep(2, rownames(TPC)),]

write.csv(TPC, "PCA loadings all areas.csv")

#' cluster analysis & Dendrogram

RankClust1 <- hclust(dist(TPC1))
RankClust2 <- hclust(dist(TPC2))
# very simple dendrogram
png("Dedrogram PC1 loadings.png")
plot(RankClust1, hang = -1, ann=FALSE)
title("Dendrogram of  PC1 loadings")
dev.off()

png("Dendrogram PC2 loadings.png")
plot(RankClust2, hang = -1, ann=FALSE)
title("Dendrogram of ranked PC2 loadings")
dev.off()
  