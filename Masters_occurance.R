#convert .txt files in .csv files
#convert any blank cells with na values

#install all r packages needed - this only needs to be done once
install.packages("readr", "maptools", "raster")

#loading data
library(readr)
library(maptools)
#this would be your file path way to where the data is kept
SN <- read_csv("C:/Users/etelford.IC.002/Dropbox/Phd/R/Masters/SN.csv")
VEX <- read_csv("C:/Users/etelford.IC.002/Dropbox/Phd/R/Masters/VEX.csv")
VS <- read_csv("C:/Users/etelford.IC.002/Dropbox/Phd/R/Masters/VS.csv")
VE <- read_csv("C:/Users/etelford.IC.002/Dropbox/Phd/R/Masters/VE.csv")


p1<-plot(wrld_simpl)
p1 <- plot(wrld_simpl[region==2], axes=TRUE)
p1 <- wrld_simpl[wrld_simpl$REGION==2,]
p1 <- box()
p1 <- points(SN$decimalLongitude, SN$decimalLatitude, col='light grey', pch=19, cex=0.75)
p1 <- points(VEX$decimalLongitude, VEX$decimalLatitude, col='grey15', pch=21, cex=0.75)
p1 <- points(VS$decimalLongitude, VS$decimalLatitude, col='grey25', pch=20, cex=0.75)


#make sure the coordinates are numeric factors
as.numeric(SN)
as.numeric(as.character(SN$decimalLatitude))
as.numeric(as.character(SN$decimalLongitude))

#creating the histograms
library(raster)
wc <- raster::getData('worldclim', res=10, var='bio')
#in the brackets you can put which measurements you need - I only used bio1 and bio12
plot(wc[[c(1, 12)]], nr=2)
#extracting the climate data
SNbfc <- extract(wc, SN[,2:1])
#look at a sample of the data
head(SNbfc)

p1 <- SNbfc[, 'bio12']
na.omit(x)
h1<-hist(SNbfc[, 'bio12'],
         main = "Senegalia nigrescens",
         xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
         col = "light grey", xlim=c(0,2000), breaks = 20)

#VEX
#make sure the coordinates are numeric factors
as.numeric(VEX)
as.numeric(as.character(VEX$decimalLatitude))
as.numeric(as.character(VEX$decimalLongitude))

#creating the histograms
library(raster)
wc <- raster::getData('worldclim', res=10, var='bio')
#in the brackets you can put which measurements you need - I only used bio1 and bio12
plot(wc[[c(1, 12)]], nr=2)
#extracting the climate data


#VEX
VEXbfc <- extract(wc, VEX[,2:1])
#look at a sample of the data
head(VEXbfc)
p1 <- VEXbfc[, 'bio12']
na.omit(x)
h1<-hist(VEXbfc[, 'bio12'],
         main = "Vachellia exuvialis",
         xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
         col = "light grey", xlim=c(0,2000), breaks = 5)


#SN
SNbfc <- extract(wc, SN[,2:1])
#look at a sample of the data
head(SNbfc)
p1 <- SNbfc[, 'bio12']
na.omit(x)
h1<-hist(SNbfc[, 'bio12'],
         main = "Senegalia nigrescens",
         xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
         col = "grey15", xlim=c(0,2000), breaks = 20)

#VS
VSbfc <- extract(wc, VS[,2:1])
#look at a sample of the data
head(VSbfc)
p1 <- VSbfc[, 'bio12']
na.omit(x)
h1<-hist(VSbfc[, 'bio12'],
         main = "Vachellia sieberiana",
         xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
         col = "grey25", xlim=c(0,2000), breaks = 20)

