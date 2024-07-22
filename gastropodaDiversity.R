################################################################################
#Compare diversities of 3 different environmental depositions 
#using different Diversity parameters with the divDyn package
#
#
# Author: Alissa Watson
# Date: July 22 2024
#
################################################################################
#setwd
setwd("C:/Users/Alissa/Documents/School/GraduateSchool/Summer2024/AnalyticalPaleobiology/Project/data")

#install packages
#install.packages("divDyn")
#install.packages("chronosphere")
library(divDyn)
library(chronosphere)

#get data (July 15, 2024)
gastropoda <- read.csv("gastropodaData.csv", sep= ",")
time <- read.csv("timeScale.csv", sep=",")
#extract only columns needed
gastropoda <- gastropoda[, c(1, 5, 14:19, 38, 45, 55, 58)]
#Remove occurrences with genus not specified
gastropoda <- gastropoda[gastropoda$genus != "", ]

#find diversity for each time bin
#get preloaded data
data(stages)
str(stages)
data("keys") #finds all possible variations of a time interval
# Categroize data using the stg entries
#lower number = earlier stage
stgMin<-categorize(gastropoda[,"early_interval"],keys$stgInt)
stgMax<-categorize(gastropoda[,"late_interval"],keys$stgInt)
stgBin<-categorize(gastropoda[,"time_bins"],keys$stgInt)

#make sure numbers are numbers (instead of a character string)
stgMin<-as.numeric(stgMin) 
stgMax<-as.numeric(stgMax)
stgBin<-as.numeric(stgBin)

#Make one bin for every entry
gastropoda$stg <- rep(NA, nrow(gastropoda))

## select entries, where
stgCondition <- c(
  # the early and late interval fields indicate the same stg
  which(stgMax==stgMin),
  # or the late interval field is empty
  which(stgMax==-1))

#in these conditions use the minimum stage
gastropoda$stg[stgCondition] <- stgMin[stgCondition]

#select entries that already have time bins
stgNa <- which(is.na(gastropoda$stg))
#in this condition use time bin already in data
gastropoda$stg[stgNa] <- stgBin[stgNa]

#So I am not accounting for intervals that aren't narrowed down to a time bin
#even though they have max and min units (but are usually for the entire thing)
#e.g. Ordovician is 40 million years

################################################################################
#Fix Ordovician stages using code modified from Adam Kocsis
# load data 
# load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))
# correct it with this function
# source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R")
# Script to assign Ordovician collections to 'stg' stages based on tables format, max.int, and zones
# last checked with data of 2018-08-31 - Adam Kocsis

# Transform to collections
new <- unique(gastropoda[is.na(gastropoda$stg), c(
  "early_interval", 
  "late_interval", 
  "formation",
  "time_bins",
  "max_ma",
  "min_ma",
  "collection_no",
  "reference_no",
  "stg")])

# Looping formation (added condition that period is same)
for (i in 1:nrow(format))  {
  ix <- which((as.character(new$formation) == as.character(format$formation[i])))
  new$stg[ix] <- format$stg[i]
}

# Looping early_intervals 
for (i in 1:nrow(max.int))  {
  ix <- which(as.character(new$early_interval) == as.character(max.int$Max.int[i]))
  new$stg[ix] <- max.int$stg.1[i]
}

#  Looping late intervals (to check if different)
stg2 <- rep(NA, nrow(new))
for (i in 1:nrow(max.int))  {
  ix <- which(as.character(new$late_interval) == as.character(max.int$Max.int[i]))
  stg2[ix] <- max.int$stg.1[i]
}

ix <- which(new$stg<stg2) # should ignore NAs in second column
new$stg[ix] <- NA
  
# Looping zones
for (i in 1:nrow(zones))  {
  ix <- which(as.character(new$zone) == as.character(zones$zone[i]))
  new$stg[ix] <- zones$stg[i]
}
	
# only that part, which has stg assignments now
new2 <- new[!is.na(new$stg), ]

# vector: stg numbers, names:collection numbers
ord <- new2$stg
names(ord) <- new2$collection_no

# the collection identifiers of occurrences in the total dataset
colls <- as.character(gastropoda$collection_no)

# which are present in the newly gathered data?
bool <- colls%in%names(ord)

# collection identifiers of the occurrences of only these collections
subColls <- colls[bool]

# order/assign the stg accordingly
subStg<-ord[subColls]

# copy original
newStg <- gastropoda$stg

# replace the missing entries
newStg[bool]  <- subStg

# make sure things are OK
origTab <- table(gastropoda$stg)
newTab <- table(newStg)

# add to the full table
gastropoda$stg <- newStg

################################################################################
#Show overall amount of occurrences of the data
#create new column in gastropoda data placing which env.
#paralic
parCon<- c(which(gastropoda$environment == "lagoonal"), which(gastropoda$environment == "estuary/bay"),
           which(gastropoda$environment== "paralic indet."))
#deltaic
parDel<-c(which(gastropoda$environment == "delta plain"), which(gastropoda$environment == "interdistributary bay"),
          which(gastropoda$environment == "delta front"), which(gastropoda$environment == "prodelta"),  
          which(gastropoda$environment == "deltaic indet."))
#non-deltaic
parNon<- c(which(gastropoda$environment == "foreshore"), which(gastropoda$environment == "shoreface"),
           which(gastropoda$environment == "transition zone/lower shoreface"), 
           which(gastropoda$environment == "offshore"), which(gastropoda$environment == "coastal indet."))
#Have to run all 3 and then run it again
gastropoda$newEnv[parCon] <- "paralic"
gastropoda$newEnv[parDel] <- "deltaic"
gastropoda$newEnv[parNon] <- "nonDeltaic"
#to get full data
gastropoda$newEnv[parCon] <- "paralic"
gastropoda$newEnv[parDel] <- "deltaic"
gastropoda$newEnv[parNon] <- "nonDeltaic"
# numerical ages, as bins
gastropoda$stgMid <- stages$mid[gastropoda$stg]
windows()
#Make it look good
cols <- c("#8ea8c3", "#819e57", "#0b2545")
# reorder too
reord <- c("paralic","deltaic","nonDeltaic")
plotnames <-c("Paralic", "Deltaic", "Non-Deltaic")
tsplot(stages, shading="series", boxes="sys", xlim = 14:95,
       ylab="Number of occurrences", ylim=c(0,10500))
parts(gastropoda$stgMid, gastropoda$newEnv, col=cols, ord = reord, labs = F)
legend("topleft", inset=c(0.01, 0.01), 
       legend= plotnames, fill=cols, bg="white")
################################################################################
#Subset data by 3 main environments
#paralic = lagoonal, estuary/bay, paralic indet.
#deltaic = delta plain, interdistributary bay, delta front, prodelta, deltaic indet.
#non-deltaic = foreshore, shoreface, transition zone/lower shoreface, offshore, coastal indet. 
#subset paralic data
paralic <- subset(gastropoda, environment == "lagoonal" | environment == "estuary/bay" | environment== "paralic indet.")
#subset deltaic data
deltaic <- subset(gastropoda, environment == "delta plain" | environment == "interdistributary bay" |
                    environment == "delta front" | environment == "prodelta" |  environment == "deltaic indet.")
#subset non-deltaic data
nonDeltaic <- subset(gastropoda, environment == "foreshore" | environment == "shoreface" |
                       environment == "transition zone/lower shoreface" | environment == "offshore" |  environment == "coastal indet.")
################################################################################
#Do sqs sampling in each time bin 
#set seed
set.seed(95532)
#define quorum for sqs analysis
#qu <- 0.3
it <- 100 #change to 5 to test

#try corrected sqs - doesn't work?
#sqs0.5ParCorrected<-subsample(paralic, iter=it, q=0.5,  
#                    tax="genus", bin="stg", ref="reference_no", coll="collection_no",
#                    type="sqs",  singleton="ref",, excludeDominant=T, largestColl=T, na.rm=T)


#All with uncorrected sqs
#Paralic
sqs0.3Par<-subsample(paralic, bin="stg", tax="genus", type="sqs", q=0.3,
                     iter=it, na.rm=TRUE)
sqs0.5Par<-subsample(paralic, bin="stg", tax="genus", type="sqs", q=0.5,
                     iter=it, na.rm=TRUE)
#Failed tests still show
sqs0.3FailedPar<-subsample(paralic, bin="stg", tax="genus", type="sqs", q=0.3,
                     iter=it, na.rm=TRUE, useFailed = TRUE)
sqs0.5FailedPar<-subsample(paralic, bin="stg", tax="genus", type="sqs", q=0.5,
                           iter=it, na.rm=TRUE, useFailed = TRUE)
#Deltaic
sqs0.3Del<-subsample(deltaic, bin="stg", tax="genus", type="sqs", q=0.3,
                  iter=it, na.rm=TRUE)
sqs0.5Del<-subsample(deltaic, bin="stg", tax="genus", type="sqs", q=0.5,
                     iter=it, na.rm=TRUE)
#Failed tests still show
sqs0.3FailedDel<-subsample(deltaic, bin="stg", tax="genus", type="sqs", q=0.3,
                     iter=it, na.rm=TRUE, useFailed = TRUE)
sqs0.5FailedDel<-subsample(deltaic, bin="stg", tax="genus", type="sqs", q=0.5,
                     iter=it, na.rm=TRUE, useFailed = TRUE)
#Non-Deltaic
sqs0.3Non<-subsample(nonDeltaic, bin="stg", tax="genus", type="sqs", q=0.3,
                  iter=it, na.rm=TRUE)
sqs0.5Non<-subsample(nonDeltaic, bin="stg", tax="genus", type="sqs", q=0.5,
                     iter=it, na.rm=TRUE)
#Failed tests still show
sqs0.3FailedNon<-subsample(nonDeltaic, bin="stg", tax="genus", type="sqs", q=0.3,
                     iter=it, na.rm=TRUE, useFailed = TRUE)
sqs0.5FailedNon<-subsample(nonDeltaic, bin="stg", tax="genus", type="sqs", q=0.5,
                     iter=it, na.rm=TRUE, useFailed = TRUE)
################################################################################
#Prepare for plotting 
#merge with time scale
sqs0.3Par <- merge(stages, sqs0.3Par, by="stg")
sqs0.5Par <- merge(stages, sqs0.5Par, by="stg")
sqs0.3FailedPar <- merge(stages, sqs0.3FailedPar, by="stg")
sqs0.5FailedPar <- merge(stages, sqs0.5FailedPar, by="stg")
sqs0.3Del <- merge(stages, sqs0.3Del, by="stg")
sqs0.5Del <- merge(stages, sqs0.5Del, by="stg")
sqs0.3FailedDel <- merge(stages, sqs0.3FailedDel, by="stg")
sqs0.5FailedDel <- merge(stages, sqs0.5FailedDel, by="stg")
sqs0.3Non <- merge(stages, sqs0.3Non, by="stg")
sqs0.5Non <- merge(stages, sqs0.5Non, by="stg")
sqs0.3FailedNon <- merge(stages, sqs0.3FailedNon, by="stg")
sqs0.5FailedNon <- merge(stages, sqs0.5FailedNon, by="stg")
################################################################################
#Basic plot - RT comparing q levels
windows()
tsplot(stages, boxes="sys", shading="sys", xlim=14:95, ylim=c(0,285), 
       ylab="Range-through Richness (Diversity)")
#Simple diversity (using RT (range-through))
lines(sqs0.3Par$mid, sqs0.3Par$divRT, col="#8ea8c3", lwd=2)
lines(sqs0.5Par$mid, sqs0.5Par$divRT, col="#8ea8c3", lwd=2, lty=2)
lines(sqs0.3Del$mid, sqs0.3Del$divRT, col="#819e57", lwd=2)
lines(sqs0.5Del$mid, sqs0.5Del$divRT, col="#819e57", lwd=2, lty=2)
lines(sqs0.3Non$mid, sqs0.3Non$divRT, col="#0b2545", lwd=2)
lines(sqs0.5Non$mid, sqs0.5Non$divRT, col="#0b2545", lwd=2, lty=2)

#legend
reord <- c("paralic", "deltaic", "nonDeltaic")
plotnames <-c("Paralic, q=0.3","Paralic, q=0.5", "Deltaic, q=0.3","Deltaic, q=0.5",
              "Non-Deltaic, q=0.3", "Non-Deltaic, q=0.5")
qColors<-c("#8ea8c3", "#8ea8c3", "#819e57", "#819e57", "#0b2545", "#0b2545" )
legend("topleft", inset=c(0.01, 0.01), legend= plotnames, col =qColors, 
       lty=c(1,2,1,2,1,2),bg="white")
################################################################################
#Basic plot - RT comparing q levels with Failed tests
tsplot(stages, boxes="sys", shading="sys", xlim=14:95, ylim=c(0,285), 
       ylab="Range-through Richness (Diversity) with Failed Tests")
#Simple diversity (using RT (range-through))
lines(sqs0.3FailedPar$mid, sqs0.3FailedPar$divRT, col="#8ea8c3", lwd=2)
lines(sqs0.5FailedPar$mid, sqs0.5FailedPar$divRT, col="#8ea8c3", lwd=2, lty=2)
lines(sqs0.3FailedDel$mid, sqs0.3FailedDel$divRT, col="#819e57", lwd=2)
lines(sqs0.5FailedDel$mid, sqs0.5FailedDel$divRT, col="#819e57", lwd=2, lty=2)
lines(sqs0.3FailedNon$mid, sqs0.3FailedNon$divRT, col="#0b2545", lwd=2)
lines(sqs0.5FailedNon$mid, sqs0.5FailedNon$divRT, col="#0b2545", lwd=2, lty=2)

#legend
legend("topleft", inset=c(0.01, 0.01), legend= plotnames, col =qColors, 
       lty=c(1,2,1,2,1,2),bg="white")
################################################################################
#Basic plot - RT just 0.3 q level
tsplot(stages, boxes="sys", shading="sys", xlim=14:95, ylim=c(0,135), 
       ylab="Range-through Richness (Diversity)")
#Simple diversity (using RT (range-through))
lines(sqs0.3Par$mid, sqs0.3Par$divRT, col="#8ea8c3", lwd=2)
lines(sqs0.3Del$mid, sqs0.3Del$divRT, col="#819e57", lwd=2)
lines(sqs0.3Non$mid, sqs0.3Non$divRT, col="#0b2545", lwd=2)

#abline(v=234, col="hotpink", lwd=2)

#legend
plotnames2 <-c("Paralic","Deltaic","Non-Deltaic")
legend("topleft", inset=c(0.01, 0.01), legend= plotnames2, fill = cols ,bg="white")
################################################################################
#Basic plot - RT just 0.3 q level with Failed tests
windows()
tsplot(stages, boxes="sys", shading="sys", xlim=14:95, ylim=c(0,165), 
       ylab="Range-through Richness (Diversity) with Failed Tests")
#Simple diversity (using RT (range-through))
lines(sqs0.3FailedPar$mid, sqs0.3FailedPar$divRT, col="#8ea8c3", lwd=2)
lines(sqs0.3FailedDel$mid, sqs0.3FailedDel$divRT, col="#819e57", lwd=2)
lines(sqs0.3FailedNon$mid, sqs0.3FailedNon$divRT, col="#0b2545", lwd=2)

#legend
legend("topleft", inset=c(0.01, 0.01), legend= plotnames2, fill = cols ,bg="white")
################################################################################
#See if pull of the recent is effecting graph
tsplot(stages, boxes="sys", shading="sys", xlim=82:95, ylim=c(0,140), 
       ylab="RT Diversity (Looking at Pull-of-the-Recent))")
#Simple diversity (using RT (range-through))
lines(sqs0.3Par$mid, sqs0.3Par$divRT, col="#8ea8c3", lwd=2)
lines(sqs0.3Par$mid[1:94], sqs0.3Par$divRT[1:94], col="#FCF7DA", lwd=2, lty=2)
lines(sqs0.3Del$mid, sqs0.3Del$divRT, col="#819e57", lwd=2)
lines(sqs0.3Del$mid[1:94], sqs0.3Del$divRT[1:94], col="#FCF7DA", lwd=2, lty=2)
lines(sqs0.3Non$mid, sqs0.3Non$divRT, col="#0b2545", lwd=2)
lines(sqs0.3Non$mid[1:94], sqs0.3Non$divRT[1:94], col="#FCF7DA", lwd=2, lty=2)

# legend
legend("topleft", inset=c(0.01, 0.01), legend=c("Paralic with recent", "Deltaic with recent",
      "Non-Deltaic with recent","Without recent"),
      col=c("#8ea8c3", "#819e57","#0b2545", "#FCF7DA" ), lty=c(1,1,1,2), bg="#E0E0E0")
################################################################################
#Basic plot - CSIB
tsplot(stages, boxes="sys", shading="sys", xlim=14:95, ylim=c(0,170), 
       ylab="CSIB Diversity")
#Simple diversity (using RT (range-through))
lines(sqs0.3Par$mid, sqs0.3Par$divCSIB, col="#8ea8c3", lwd=2)
lines(sqs0.5Par$mid, sqs0.5Par$divCSIB, col="#8ea8c3", lwd=2, lty=2)
lines(sqs0.3Del$mid, sqs0.3Del$divCSIB, col="#819e57", lwd=2)
lines(sqs0.5Del$mid, sqs0.5Del$divCSIB, col="#819e57", lwd=2, lty=2)
lines(sqs0.3Non$mid, sqs0.3Non$divCSIB, col="#0b2545", lwd=2)
lines(sqs0.5Non$mid, sqs0.5Non$divCSIB, col="#0b2545", lwd=2, lty=2)

#legend
legend("topleft", inset=c(0.01, 0.01), legend= plotnames, col =qColors, 
       lty=c(1,2,1,2,1,2),bg="white")
################################################################################
#Basic plot - CSIB with Failed Tests
tsplot(stages, boxes="sys", shading="sys", xlim=14:95, ylim=c(0,170), 
       ylab="CSIB Diversity with Failed Tests")
#Simple diversity (using RT (range-through))
lines(sqs0.3FailedPar$mid, sqs0.3FailedPar$divCSIB, col="#8ea8c3", lwd=2)
lines(sqs0.5FailedPar$mid, sqs0.5FailedPar$divCSIB, col="#8ea8c3", lwd=2, lty=2)
lines(sqs0.3FailedDel$mid, sqs0.3FailedDel$divCSIB, col="#819e57", lwd=2)
lines(sqs0.5FailedDel$mid, sqs0.5FailedDel$divCSIB, col="#819e57", lwd=2, lty=2)
lines(sqs0.3FailedNon$mid, sqs0.3FailedNon$divCSIB, col="#0b2545", lwd=2)
lines(sqs0.5FailedNon$mid, sqs0.5FailedNon$divCSIB, col="#0b2545", lwd=2, lty=2)
#legend
legend("topleft", inset=c(0.01, 0.01), legend= plotnames, col =qColors, 
       lty=c(1,2,1,2,1,2),bg="white")
################################################################################
#Basic plot - Carnian-pluvial episode - RT 0.3 q level
tsplot(stages, boxes="sys", shading="sys", xlim=43:69, ylim=c(0,16), 
       ylab="Range-through Richness (Diversity)")
#draw line where carnian-pluvial event begins
abline(v=234, col="hotpink", lwd=2)
#Simple diversity (using RT (range-through))
lines(sqs0.3Par$mid, sqs0.3Par$divRT, col="#8ea8c3", lwd=2)
lines(sqs0.3Del$mid, sqs0.3Del$divRT, col="#819e57", lwd=2)
lines(sqs0.3Non$mid, sqs0.3Non$divRT, col="#0b2545", lwd=2)
#legend
plotnames3 <-c("Paralic","Deltaic","Non-Deltaic")
legend("topleft", inset=c(0.01, 0.01), legend= plotnames2, fill = cols,
       bg="white")
################################################################################
#Basic plot - Carnian-pluvial episode - RT 0.3 q level using Failed Trials
tsplot(stages, boxes="sys", shading="sys", xlim=43:69, ylim=c(0,18), 
       ylab="Range-through Richness (Diversity)")
#draw line where carnian-pluvial event begins
abline(v=234, col="hotpink", lwd=2)
#Simple diversity (using RT)
lines(sqs0.3FailedPar$mid, sqs0.3FailedPar$divRT, col="#8ea8c3", lwd=2)
lines(sqs0.3FailedDel$mid, sqs0.3FailedDel$divRT, col="#819e57", lwd=2)
lines(sqs0.3FailedNon$mid, sqs0.3FailedNon$divRT, col="#0b2545", lwd=2)

#legend
plotnames3 <-c("Paralic","Deltaic","Non-Deltaic")
legend("topleft", inset=c(0.01, 0.01), legend= plotnames3, fill = cols, bg="white")
################################################################################
#Basic plot - Carnian-pluvial episode - CSIB 0.3 q level 
#No data points for paralic of deltaic
tsplot(stages, boxes="sys", shading="sys", xlim=43:69, ylim=c(0,16), 
       ylab="CSIB Diversity")
#draw line where carnian-pluvial event begins
abline(v=234, col="hotpink", lwd=2)
#Simple diversity (using RT (range-through))
lines(sqs0.3Par$mid, sqs0.3Par$divCSIB, col="#8ea8c3", lwd=2)
lines(sqs0.3Del$mid, sqs0.3Del$divCSIB, col="#819e57", lwd=2)
lines(sqs0.3Non$mid, sqs0.3Non$divCSIB, col="#0b2545", lwd=2)
#legend
plotnames3 <-c("Paralic","Deltaic","Non-Deltaic")
legend("topleft", inset=c(0.01, 0.01), legend= plotnames2, fill = cols,
       bg="white")
################################################################################
#No autocorrelation
#Time bin data is consistently missing (So can't correlate)
################################################################################