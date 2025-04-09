#This script by created as part of the PCHES (Program on Coupled Human and Earth Systems) project
#studying the impact of unsustainable groundwater restrictions on crop production in the US West
#The project is funded by the Department of Energy

#This script were developed throughout the course of the project (2021-2025)
#Last updated in March 2025

# The script creates GCAMland figures that are provided in the submitted manuscript (including the supplemental information)
# The maps show relative change in irrigated and rainfed land areas for iterations 1 and 10 for different states in the US West
# All the input files needed to run this script and output files generated are listed in the MetaData file


### Libraries:
# Install the following packages if not already installed using  install.packages("packagename")

# required for convergence barplots and point plots:
library(ggplot2) 
library(tidyr)
library(sp)

# required for maps:
library(rgdal)
library(rgeos)
library(data.table)
library(RColorBrewer)

################################################################################################################################
### Load and format data #######################################################################################################

#create new folders for figure outputs
dir.create("figures/gcam_delta_land")

# load GCAMland output
nfiles = 10          # number of iterations or files to read

for(i in 1:nfiles){ # this loop puts all the data from nfiles into one data frame called dat
  d = read.csv(paste("data/GCAM/DREM_land_iteration_", i, ".csv", sep=""))
  d = as.data.frame(cbind(d, rep(i, nrow(d))))
  colnames(d)[7] = "iteration"
  
  if(i == 1){
    dat = d
  }else if(i > 1){
    dat = rbind(dat, d)
  }
}

# create a reference(BAU) land dataset for 2015 from iteration 1 results
d1 = read.csv(paste("data/GCAM/pches_output_Iter1", ".csv", sep=""))
d1 <- d1[order(d1[,"State"], d1[,"Crop"], d1[, "Management"], decreasing = TRUE),]

# subset data so only iteration (not baseline) years are included
dat = subset(dat, dat$year == 2015)

# subset to WECC States
dat = subset(dat, dat$State %in% c("Washington",
                                   "New_Mexico", 
                                   "Colorado",
                                   "Utah",
                                   "Wyoming",
                                   "Nevada",
                                   "Montana",
                                   "Arizona",
                                   "Oregon",
                                   "Idaho",
                                   "California"))


# Set a lower limit on land values from GCAM such that tiny values get set to 0
dat$land.allocation[dat$land.allocation < 1e-6] <- 0
d1$BAU_land[d1$BAU_land < 1e-6] <- 0

# CALCULATE DELTA_LAND so that it is the ratio between BAU land allocation (2015) and current iteration land allocation (2015).
#If irrigated land area is zero to begin with, use NA for delta land
d1_tmp <- d1[rep(1:nrow(d1), times=10),]
delta.land <- ifelse(d1_tmp$BAU_land == 0, NA, dat$land.allocation / d1_tmp$BAU_land) 

# put iteration 0 data at the beginning of the data frame dat
dat$delta_land = delta.land
d0 <- d1 #making iteration 0 matrix with curriteration land to be the same as BAU land
d0$CurrIteration_Land <- d1$BAU_land
d0$delta_land = ifelse(d0$CurrIteration_Land == 0, NA, 1) #modified to add NA when irrigated area = 0
colnames(d0)[colnames(d0)=="CurrIteration_Land"] <- "land.allocation"
colnames(d0)[colnames(d0)=="BAU_land"] <- "expectedYield"
d0$expectedYield = NA
d0 <- cbind(d0[,1:(ncol(d0)-1)], rep(0,nrow(d0)),d0[,ncol(d0)])
colnames(d0)[ncol(d0)-1]<- "iteration"
colnames(d0)[ncol(d0)]<- "delta_land"
dat = rbind(d0, dat)
dat = as_tibble(dat)  # turn the data frame into a tibble for easy use of ggplot functions below
dat$iteration = as.factor(dat$iteration)  # make iteration a factor so it is treated as a discrete variable in the plots below

################################################################################################################################
### Create tables showing levels of irrigated and rainfed land for each iteration ##############################################

dat.irr = subset(dat, dat$Management == "Irrigated")  # subset the tibble to irrigated land area only
dat.rfd = subset(dat, dat$Management == "Rainfed")  # subset the tibble to rainfed land area only

################################################################################################################################
### Create tables showing % change in irrigated and rainfed land from one iteration to the next ###########################

# calculate % change from previous iteration
for(i in 1:nfiles){
  d.sub.i2 = subset(dat, dat$iteration == i)
  d.sub.i1 = subset(dat, dat$iteration == i-1)
  
  delta = 100*(d.sub.i2$land.allocation - d.sub.i1$land.allocation)/d.sub.i1$land.allocation
  d.sub.i2$percent_change = delta
  
  if(i == 1){
    dat.p = d.sub.i2
  }else if(i > 1){
    dat.p = rbind(dat.p, d.sub.i2)
  }
}

dat.p = as_tibble(dat.p)

# report maximum change in irrigated land by ending iteration (prints to screen)
d.end = subset(dat.irr, dat.irr$iteration == nfiles)
d.end[which(d.end$percent_change == max(d.end$percent_change, na.rm=T)),]   # prints row with max % change to screen

# report maximum change in rainfed land by ending iteration (prints to screen)
d.end = subset(dat.rfd, dat.rfd$iteration == nfiles)
d.end[which(d.end$percent_change == max(d.end$percent_change, na.rm=T)),]  # prints row with max % change to screen

################################################################################################################################
# load US state shapefile

states = readOGR("data/cb_2015_us_state_500k/", "cb_2015_us_state_500k") 

################################################################################################################################
#### Delta_land Maps #############################################################################################################


### IRRIGATED LAND #############################################################################################################

# iteration 1
dat.p.irr = subset(dat.p, dat.p$Management == "Irrigated")
dat.p.irr.1 = subset(dat.p.irr, dat.p.irr$iteration == 1)
dat.p.irr.1 = dat.p.irr.1[,-c(3:4,6:7)] # remove management, year, expectedYield and iteration columns

# delta_land variable = ratio of current iteration land value to BAU land value
# Make this variable into a wide data frame to enable merging with the shapefile attribute table
dat.wide.delta_land.1 = dcast(setDT(dat.p.irr.1), State ~ Crop, value.var = "delta_land")
colnames(dat.wide.delta_land.1)[2:7] = paste(colnames(dat.wide.delta_land.1)[2:7], "iter1", sep="_")

# iteration 10
dat.p.irr.10 = subset(dat.p.irr, dat.p.irr$iteration == 10)
dat.p.irr.10 = dat.p.irr.10[,-c(3:4,6:7)] # remove management, year, and iteration columns

# delta_land variable = ratio of current iteration land value to BAU land value
# Make this variable into a wide data frame to enable merging with the shapefile attribute table
dat.wide.delta_land.10 = dcast(setDT(dat.p.irr.10), State ~ Crop, value.var = "delta_land")
colnames(dat.wide.delta_land.10)[2:7] = paste(colnames(dat.wide.delta_land.10)[2:7], "iter10", sep="_")

# combine iteration 1 and iteration 10 delta_land data tables
delta_land = merge(dat.wide.delta_land.1, dat.wide.delta_land.10, by = "State")

# make a subset of just the WECC states for plotting
WECC_state_names = c("AZ","CA","ID","NV","OR","UT","WA", "MT", "WY", "CO", "NM")
states.WECC = states[states$STUSPS %in% WECC_state_names,]

# merge delta_land data with state shapefile attribute table
states.WECC@data[,10:21] = mat.or.vec(nr=nrow(states.WECC@data), nc=(ncol(delta_land)-1))
colnames(states.WECC@data)[10:21] = colnames(delta_land)[2:13]

for(i in 1:nrow(states.WECC@data)){
  if(as.character(states.WECC$NAME[i]) == "New Mexico"){  # catch the mismatch in state name characters for New Mexico
    delta_land.match = which(delta_land$State == as.character("New_Mexico"))
  }else{
    delta_land.match = which(delta_land$State == as.character(states.WECC$NAME[i]))
  }
  if(length(delta_land.match > 0)){
    states.WECC@data[i,10:21] = delta_land[delta_land.match, 2:13]
  }
}

# define a color scheme
# map.col = colorRampPalette(c(brewer.pal(n=9, name='RdYlBu')))(1000)
map.col = colorRampPalette(c("red","white","green"))(1000)

# set 0s to NA so they don't plot as colors
states.WECC@data[,10:21][states.WECC@data[,10:21] == 0] = NA 

# plot each irrigated crop area's delta_land value
for(i in 10:21){
  
  # identify the column to plot
  zname = colnames(states.WECC@data)[i]
  
  # make labels for the polygons
  l1 = list("sp.text", 
            coordinates(states.WECC), 
            as.character(signif(states.WECC@data[,i], 3)), 
            col="black", cex=0.8,font=1)
  
  # save plot to file
  png(paste("figures/gcam_delta_land/Irr_", zname, ".png", sep=""),
      height = 4, width = 4, units = "in", res=300, type = "cairo")
  p = spplot(states.WECC, 
             zname, 
             main = paste("Irr", zname), 
             col.regions = map.col,
             at = seq(0.3, 1.7, 0.01),
             par.settings = list(axis.line = list(col = 'transparent')),
             sp.layout=list(l1))
  print(p)
  dev.off()
}


### Total irrigated land

# sum irrigated land in each state for BAU, iteration 1, and iteration 10

irr.bau = subset(dat, dat$iteration == 0 & dat$Management == "Irrigated")
irr.total.bau = tapply(irr.bau$land.allocation, INDEX = irr.bau$State, FUN = sum)
irr.total.1   = tapply(dat.p.irr.1$land.allocation, INDEX = dat.p.irr.1$State, FUN = sum)
irr.total.10   = tapply(dat.p.irr.10$land.allocation, INDEX = dat.p.irr.10$State, FUN = sum)

# calculate delta_land for total irrigated land
irr.delta.1 = irr.total.1/irr.total.bau
irr.delta.10 = irr.total.10/irr.total.bau

# append delta_land values to shapefile attribute table
states.WECC@data[,22:23] = mat.or.vec(nr=nrow(states.WECC@data), nc=2)
colnames(states.WECC@data)[22:23] = c("irr.delta.1", "irr.delta.10")

# iteration 1
for(i in 1:nrow(states.WECC@data)){
  if(as.character(states.WECC$NAME[i]) == "New Mexico"){  # catch the mismatch in state name characters for New Mexico
    delta_land.match = which(names(irr.delta.1) == as.character("New_Mexico"))
  }else{
    delta_land.match = which(names(irr.delta.1) == as.character(states.WECC$NAME[i]))
  }
  if(length(delta_land.match > 0)){
    states.WECC@data[i,22] = irr.delta.1[delta_land.match]
  }
}

# iteration 10
for(i in 1:nrow(states.WECC@data)){
  if(as.character(states.WECC$NAME[i]) == "New Mexico"){  # catch the mismatch in state name characters for New Mexico
    delta_land.match = which(names(irr.delta.10) == as.character("New_Mexico"))
  }else{
    delta_land.match = which(names(irr.delta.10) == as.character(states.WECC$NAME[i]))
  }
  if(length(delta_land.match > 0)){
    states.WECC@data[i,23] = irr.delta.10[delta_land.match]
  }
}


# Plot delta_land for total irrigated land

# iteration 1
l1 = list("sp.text", 
          coordinates(states.WECC), 
          as.character(signif(states.WECC$irr.delta.1, 3)), 
          col="black", cex=0.8,font=1)

png("figures/gcam_delta_land/Irr_total_iter1.png",
    height = 4, width = 4, units = "in", res=300, type = "cairo")
p1=spplot(states.WECC, 
       "irr.delta.1", 
       main =  "Irr Total delta_land_iter1",  
       col.regions = map.col,
       at = seq(0.3, 1.7, 0.01),
       par.settings = list(axis.line = list(col = 'transparent')),
       sp.layout=list(l1))
print(p1)
dev.off()


# iteration 10
l1 = list("sp.text", 
          coordinates(states.WECC), 
          as.character(signif(states.WECC$irr.delta.10, 3)), 
          col="black", cex=0.8,font=1)

png("figures/gcam_delta_land/Irr_total_iter10.png",
    height = 4, width = 4, units = "in", res=300, type = "cairo")
p2=spplot(states.WECC, 
       "irr.delta.10", 
       main =  "Irr Total delta_land_iter10",  
       col.regions = map.col,
       at = seq(0.3, 1.7, 0.01),
       par.settings = list(axis.line = list(col = 'transparent')),
       sp.layout=list(l1))
print(p2)
dev.off()




### RAINFED LAND #############################################################################################################

# iteration 1
dat.p.rfd = subset(dat.p, dat.p$Management == "Rainfed")
dat.p.rfd.1 = subset(dat.p.rfd, dat.p.rfd$iteration == 1)
dat.p.rfd.1 = dat.p.rfd.1[,-c(3:4,6:7)] # remove management, year, and iteration columns

# delta_land variable = ratio of current iteration land value to BAU land value
# Make this variable into a wide data frame to enable merging with the shapefile attribute table
dat.wide.delta_land.1 = dcast(setDT(dat.p.rfd.1), State ~ Crop, value.var = "delta_land")
colnames(dat.wide.delta_land.1)[2:7] = paste(colnames(dat.wide.delta_land.1)[2:7], "iter1", sep="_")

# iteration 10
dat.p.rfd = subset(dat.p, dat.p$Management == "Rainfed")
dat.p.rfd.10 = subset(dat.p.rfd, dat.p.rfd$iteration == 10)
dat.p.rfd.10 = dat.p.rfd.10[,-c(3:4,6:7)] # remove management, year, and iteration columns

# delta_land variable = ratio of current iteration land value to BAU land value
# Make this variable into a wide data frame to enable merging with the shapefile attribute table
dat.wide.delta_land.10 = dcast(setDT(dat.p.rfd.10), State ~ Crop, value.var = "delta_land")
colnames(dat.wide.delta_land.10)[2:7] = paste(colnames(dat.wide.delta_land.10)[2:7], "iter10", sep="_")

# combine iteration 1 and iteration 5 delta_land data tables
delta_land = merge(dat.wide.delta_land.1, dat.wide.delta_land.10, by = "State")

# merge delta_land data with state shapefile attribute table
states.WECC@data[,24:35] = mat.or.vec(nr=nrow(states.WECC@data), nc=(ncol(delta_land)-1))
colnames(states.WECC@data)[24:35] = paste("rfd", colnames(delta_land)[2:13], sep="_")

for(i in 1:nrow(states.WECC@data)){
  if(as.character(states.WECC$NAME[i]) == "New Mexico"){  # catch the mismatch in state name characters for New Mexico
    delta_land.match = which(delta_land$State == as.character("New_Mexico"))
  }else{
    delta_land.match = which(delta_land$State == as.character(states.WECC$NAME[i]))
  }
  if(length(delta_land.match > 0)){
    states.WECC@data[i,24:35] = delta_land[delta_land.match, 2:13]
  }
}

# set 0s to NA so they don't plot as colors
states.WECC@data[,24:35][states.WECC@data[,24:35] == 0] = NA 

# plot each irrigated crop area's delta_land value
for(i in 24:35){
  
  # identify the column to plot
  zname = colnames(states.WECC@data)[i]
  
  # make labels for the polygons
  l1 = list("sp.text", 
            coordinates(states.WECC), 
            as.character(signif(states.WECC@data[,i], 3)), 
            col="black", cex=0.8,font=1)
  
  # save plot to file
  png(paste("figures/gcam_delta_land/", zname, ".png", sep=""),
      height = 4, width = 4, units = "in", res=300, type = "cairo")
  p = spplot(states.WECC, 
             zname, 
             main = zname, 
             col.regions = map.col,
             at = seq(0.3, 1.7, 0.01),
             par.settings = list(axis.line = list(col = 'transparent')),
             sp.layout=list(l1))
  print(p)
  dev.off()
}

### Total rainfed land

# sum rainfed land in each state for BAU, iteration 1, and iteration 5
rfd.bau = subset(dat, dat$iteration == 0 & dat$Management == "Rainfed")
rfd.total.bau = tapply(rfd.bau$land.allocation, INDEX = rfd.bau$State, FUN = sum)
rfd.total.1   = tapply(dat.p.rfd.1$land.allocation, INDEX = dat.p.rfd.1$State, FUN = sum)
rfd.total.10   = tapply(dat.p.rfd.10$land.allocation, INDEX = dat.p.rfd.10$State, FUN = sum)

# calculate delta_land for total irrigated land
rfd.delta.1 = rfd.total.1/rfd.total.bau
rfd.delta.10 = rfd.total.10/rfd.total.bau

# append delta_land values to shapefile attribute table
states.WECC@data[,36:37] = mat.or.vec(nr=nrow(states.WECC@data), nc=2)
colnames(states.WECC@data)[36:37] = c("rfd.delta.1", "rfd.delta.10")

# iteration 1
for(i in 1:nrow(states.WECC@data)){
  if(as.character(states.WECC$NAME[i]) == "New Mexico"){  # catch the mismatch in state name characters for New Mexico
    delta_land.match = which(names(rfd.delta.1) == as.character("New_Mexico"))
  }else{
    delta_land.match = which(names(rfd.delta.10) == as.character(states.WECC$NAME[i]))
  }
  if(length(delta_land.match > 0)){
    states.WECC@data[i,36] = rfd.delta.1[delta_land.match]
  }
}

# iteration 10
for(i in 1:nrow(states.WECC@data)){
  if(as.character(states.WECC$NAME[i]) == "New Mexico"){  # catch the mismatch in state name characters for New Mexico
    delta_land.match = which(names(rfd.delta.10) == as.character("New_Mexico"))
  }else{
    delta_land.match = which(names(rfd.delta.10) == as.character(states.WECC$NAME[i]))
  }
  if(length(delta_land.match > 0)){
    states.WECC@data[i,37] = rfd.delta.10[delta_land.match]
  }
}


# Plot delta_land for total rainfed land
# iteration 1
l1 = list("sp.text", 
          coordinates(states.WECC), 
          as.character(signif(states.WECC$rfd.delta.1, 3)), 
          col="black", cex=0.8,font=1)

png("figures/gcam_delta_land/Rfd_total_iter1.png",
    height = 4, width = 4, units = "in", res=300, type = "cairo")
p1=spplot(states.WECC, 
       "rfd.delta.1", 
       main =  "Rfd Total delta_land_iter1",  
       col.regions = map.col,
       at = seq(0.3, 1.7, 0.01),
       par.settings = list(axis.line = list(col = 'transparent')),
       sp.layout=list(l1))
print(p1)
dev.off()


# iteration 10
l1 = list("sp.text", 
          coordinates(states.WECC), 
          as.character(signif(states.WECC$rfd.delta.10, 3)), 
          col="black", cex=0.8,font=1)

png("figures/gcam_delta_land/Rfd_total_iter10.png",
    height = 4, width = 4, units = "in", res=300, type = "cairo")
p2=spplot(states.WECC, 
       "rfd.delta.10", 
       main =  "Rfd Total delta_land_iter10",  
       col.regions = map.col,
       at = seq(0.3, 1.7, 0.01),
       par.settings = list(axis.line = list(col = 'transparent')),
       sp.layout=list(l1))
print(p2)
dev.off()
