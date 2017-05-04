#' @title  PCA based Integrated Ecological analysis of the North Sea - Norwegian Trench
#' @description Special version for the Norwegian Trench data WGINOSE for 2017 report
#' @description  Based on earlier script developed by Andrew Kerry, CEFAS, UK   
#' @date: 15.03.2017 - updated for WGINOSE analysis
#'      
#' and produce a 'shade' plot  of the original data matrix (sorted against PC1)


#' LIBRARIES
#' ---------------------
library (lattice)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(data.table)

#' SETTING WORKING DIRECTORIES
#' ---------------------
#' setwd("/TEMP/WGINOSE 2016")
setwd("~/ownCloud/Research/WGINOSE/2017/data/NorTrench") 


#' IMPORT DATA

fish<-tbl_df(read.csv("Nor Trench_reketokt data.csv"))
plank<-tbl_df(read.csv("Nor Trench_zoop data by month + year.csv"))
ocean<-tbl_df(read.csv("../oceanogr data 1983_2016.csv"))


#FISH DATA
# Create data.table w one line pr year, and one column pr species
fish.dt<-data.table(fish)
fish.dt.wide<-dcast(fish.dt, year ~ species, value.var="scaled.ct")

#FISH select only columns (variables) with data for all years
wntg<-which(colSums(is.na(fish.dt.wide))>0)
cols<-names(wntg)

fish.year.sel<-fish.dt.wide[, !cols, with=FALSE]
rownames(fish.year.sel)<-c(fish.year.sel$year)
colnames(fish.year.sel)[1]<-c("Year")
colnames(fish.year.sel)<-c("Year", "DW.prawn", "R. cimbrius", "American plaice", "Chimaera", "Haddock", "Thornback ray", "Blue whiting", "Norway Redfish", "Saithe", "Grenadier", "Witch flounder", "Velvet belly", "Silvery pout", "Cod", "Greater Argentine", "Norway Pout")


#PLANKTON DATA
plank.dt<-data.table(plank)
plank.dt.wide<-dcast(plank.dt, Year~Month, value.var="Sum_dw")
plank.dt.wide$PhytoMnthSum<-rowSums(plank.dt.wide[,2:length(plank.dt.wide)], na.rm=TRUE)

#OCEANOGRAPHIC DATA
ocean.dt<-data.table(ocean)
ocean.NT<-ocean.dt[area=="wgi12"]

#OCEAN select only columns (variables) with data for all years
wntg<-which(colSums(is.na(ocean.NT))>1)
cols<-names(wntg)
ocean.NT<-ocean.NT[, !cols, with=FALSE]

#Merge FISH, PLANKTON and OCEANOGRAPHIC data into one tabel
nt.joint.data<-merge(fish.year.sel, plank.dt.wide[,c(1,14)], by="Year")
nt.joint.data<-merge(nt.joint.data, ocean.NT[,c(1,3:42)], by="Year")

nt.data<-nt.joint.data[Year <2016]
row.names(nt.data)<-nt.data$Year

trans.std <- data.table(scale(log10(nt.data[,2:length(nt.data)]+1)))
rownames(trans.std)<-rownames(nt.data)

#' ANALYSES
#' ---------------------
#' Perform PCA

data.pca <- prcomp(trans.std, cor = F)
summary(data.pca)

  
  
#' Extract loadings (eigenvalues) for PC1 & PC2 vectors

pc1 <- data.pca$rotation[,1]
pc2 <- data.pca$rotation[,2]
loadings <- data.frame(pc1,pc2)
pc1.order <- order(pc1, na.last = TRUE, decreasing = TRUE)
loadings.order <- loadings[pc1.order,]
loadings.order

#' add two columns with rank (based on absolute scores) in relation to PC1 and PC2
loadings.order <- cbind(rownames(loadings.order), loadings.order[1:2])
colnames(loadings.order)[1]<-c("variable")
loadings.order <- tbl_df(loadings.order)
loadings.order <- mutate(loadings.order, Rank1 = percent_rank(abs(pc1)) )
loadings.order <- mutate(loadings.order, Rank2 = percent_rank(abs(pc2)) )

write.csv(loadings.order, file = "WGINOSE17_NorwTrench_Varloadings.csv")
  
  
  #' Plotordination using PC1 & PC2 scores
  
  #plot(data.pca$x[,1],data.pca$x[,2], xlab = "PC1", xlim = c(-8,11), ylab = "PC2", type = "both", main =paste("PCA", area.names[i]))
  #text(data.pca$x[,1], data.pca$x[,2], row.names(data), cex=0.9, pos=4, col="red") # add labels
  
pdf("WGINOSE17_PCA_ordniation_Norw_trench.pdf")
plot(data.pca$x[,1],data.pca$x[,2], xlab = "PC1", xlim = c(-10,11), ylab = "PC2", type = "both", main ="PCA Norwegian Trench")
text(data.pca$x[,1], data.pca$x[,2], row.names(nt.data), cex=0.9, pos=4, col="red") # add labels
dev.off()

#' reorder the data matrix stnddata using the ranked order of PC1 loadings pc1.order



setcolorder(trans.std, pc1.order)

trans.std.order <- as.matrix(trans.std)
rownames(trans.std.order)<-rownames(trans.std)

#' Heat plot of PCA scores

# levelplot(trans.std.order, col.regions = rainbow(100, start = 0, end = 0.325), scales = list(x=list(cex=0.8, rot = 90), y=list(cex = 0.7)), main=list(label=paste("PCA Heatplot", area.names[i]), cex = 2), xlab=list(label="Year",cex=2), ylab=list(label="Variables",cex=2), aspect = 1.25)

pdf("WGINOSE17_PCA_heat_Norw_Trench.pdf")
#png(paste("WGINOSE16_PCA_heat", area.names[i], area[i], ".png", sep=""))
p1<-levelplot(trans.std.order, col.regions = rainbow(100, start = 0, end = 0.325), scales = list(x=list(cex=0.8, rot = 90), y=list(cex = 0.4)), main=list(label=("PCA Heatplot Norwegian trench"), cex = 1), xlab=list(label="Year",cex=1), ylab=list(label="Variables",cex=1), aspect = 1.25)

#p1<-levelplot(trans.std.order, col.regions = rainbow(100, start = 0, end = 0.325), scales = list(x=list(cex=0.8, rot = 90), y=list(cex = 0.4)), main=list(label=paste("PCA Heatplot", area.names[i]), cex = 1), xlab=list(label="Year",cex=1), ylab=list(label="Variables",cex=1), aspect = 1.25)
              
print(p1)
dev.off()

 