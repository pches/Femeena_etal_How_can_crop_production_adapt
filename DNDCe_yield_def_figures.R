#This script by created as part of the PCHES (Program on Coupled Human and Earth Systems) project
#studying the impact of unsustainable groundwater restrictions on crop production in the US West
#The project is funded by the Department of Energy

#This script were developed throughout the course of the project (2021-2025)
#Last updated in March 2025

# This script creates yield deficit maps provided in the submitted manuscript (including the supplemental information)
# The maps show yield deficits for for different states in the US West for six crop categories
# All the input files needed to run this script and output files generated are listed in the MetaData file


### Libraries:
# Install the following packages if not already installed using  install.packages("packagename")
library(rgdal)
library(rgeos)
library(RColorBrewer)

library(tidyr)

################################################################################################################################
### Create output folder #######################################################################################################

if (!file.exists("figures/DNDCe_yield_def")) {
  dir.create("figures/DNDCe_yield_def", recursive = TRUE)
}

### Functions

agg.wbm.to.drem = function(wbm.dat, drem.crop.list, crop_list){
  out = data.frame(matrix(nr=length(drem.crop.list), nc=12))
  out[,1] = drem.crop.list
  for(i in 1:length(drem.crop.list)){
    crop.list.wbm = subset(crop_list, crop_list$drem.crop == drem.crop.list[i])
    wbm.dat.sub = subset(wbm.dat, wbm.dat$crop %in% crop.list.wbm$wbm.crop)
    out[i,2:12] = as.numeric(colSums(wbm.dat.sub[,2:12], na.rm=T))
  }
  colnames(out) = c("crop", colnames(wbm.dat.sub[,2:12]))
  out
}

######################################################################################################
### Load data

### State shapefile (needed for plotting)
states = readOGR("data/cb_2015_us_state_500k/", "cb_2015_us_state_500k")
WECC_state_names = c("AZ","CA","ID","NV","OR","UT","WA", "MT", "WY", "CO", "NM")
states.WECC = states[states$STUSPS %in% WECC_state_names,]

#######################################################################################################
### Crop ID mapping files to aggregate WBM crop list to DREM crop list
wbm_to_dndc = read.csv("data/WoCD_DNDC_CropSystem_id_name_WBM_crop_id_name.csv", 
                       header=T, stringsAsFactors = FALSE, colClasses=c("character","numeric","character","numeric"), 
                       sep =",")
dndc_to_drem = read.csv(file = 'data/alpha_i_j_s_SprWinWht_WECC.csv', 
                        header=T, stringsAsFactors = FALSE, sep = ",")


### WBM output
# Gross irrigation by state and WBM crop
gross.irr.s = read.csv("data/DNDCe/IrrGross_mmYr_states_iteration_10.csv")


######################################################################################################
### Aggregate WBM crops to DREM crops

# make 1 file with all 3 crop lists
crop_list = data.frame(matrix(nr=nrow(wbm_to_dndc), ncol=4))
colnames(crop_list) = c("dndc.crop", "dndc.id", "wbm.crop", "drem.crop")
crop_list[,1] = wbm_to_dndc$DNDC_crop_name
crop_list[,2] = wbm_to_dndc$DNDC_crop_system

for(i in 1:nrow(crop_list)){
  crop_list[i,3] = wbm_to_dndc[which(wbm_to_dndc$DNDC_crop_system == crop_list[i,2]), 3]
  crop_list[i,4] = dndc_to_drem[which(dndc_to_drem$DNDC_crop_id   == crop_list[i,2]), 1]
}


# sum wbm output
drem.crop.list = unique(crop_list$drem.crop)
drem.crop.list = drem.crop.list[-6] # remove "not included"

# scen 1 it 10 gross irr
gross.irr.s.drem = agg.wbm.to.drem(wbm.dat = gross.irr.s, 
                                   drem.crop.list, crop_list)
###########################################################################################################
###########################################################################################################
# DNDCe output: Yield deficit by state and crop
iter = 10           # Choose the iteration to load
yld.def = read.csv(paste("data/DNDCe/DNDCe_to_IMPLAN_yield_deficit_STATE_iteration_", iter, ".csv", sep=""))

# remove the row with the "not_included" crop category
rm.row = which(yld.def$IMPLAN_crop == "not_included")
yld.def = yld.def[-rm.row,]
row.names(yld.def) <- NULL

#rearrange rows and columns in gross.irr file to match yld.def file
col_names <- names(gross.irr.s.drem)[-1]
sorted_col_names <- sort(col_names)
gross.irr.s.drem <- gross.irr.s.drem[, c(1, match(sorted_col_names, col_names)+1)]
gross.irr.s.drem <- gross.irr.s.drem[match(yld.def[,1], gross.irr.s.drem[,1]), , drop = FALSE]

#change 1 to NA, if gross irrigation for that state-crop combo is zero.
yld.def[gross.irr.s.drem[,1:12] == 0] <- NA

# load GCAMland cropland area data. Use this to identify which states have 0 ha of irrigated land for each crop
gcam_land = read.csv("data/GCAM/pches_output_Iter10.csv")
gcam_land_0 = subset(gcam_land, gcam_land$CurrIteration_Land < 1e-6  & gcam_land$Management == "Irrigated")

for(i in 1:nrow(gcam_land_0)){
  r = which(yld.def$IMPLAN_crop == as.character(gcam_land_0$Crop[i]))  # dndce row that matches the crop
  c = which(colnames(yld.def)   == as.character(gcam_land_0$State[i])) # dndce column that matches the state

  yld.def[r,c] = NA  # set matching row and column to NA
}


# load US state shapefile
states = readOGR("data/cb_2015_us_state_500k/", "cb_2015_us_state_500k")

# subset to WECC states only
WECC_state_names = c("AZ","CA","ID","NV","OR","UT","WA", "MT", "WY", "CO", "NM")
states.WECC = states[states$STUSPS %in% WECC_state_names,]

# Add DNDCe data to states.WECC attribute table
yld.def.t = t(yld.def[,2:12])          # transpose data so the states are the rows and the crops are the columns
colnames(yld.def.t) = yld.def[,1]      # set crops as column names
rownames(yld.def.t)[7] = 'New Mexico'  # fix mismatch in state naming between DNDCe and states.WECC

# append DNDCe values to shapefile attribute table
states.WECC@data[,10:15] = mat.or.vec(nr=nrow(states.WECC@data), nc=6)
colnames(states.WECC@data)[10:15] = colnames(yld.def.t)

# iteration 1
for(i in 1:nrow(states.WECC@data)){
  yld.def.match = which(rownames(yld.def.t) == as.character(states.WECC$NAME[i]))
  if(length(yld.def.match > 0)){
    states.WECC@data[i,10:15] = yld.def.t[yld.def.match,]
  }
}

################################################################################################################################
### Make maps ##################################################################################################################

# define a color scheme
map.col = colorRampPalette(c(brewer.pal(n=9, name='Greens')))(5001)
# map.col = c(map.col, "goldenrod1") # use a different color for the value 1, to help show where there is NOT deficit irrigation. goldenrod1 = no deficit irr

# plot each irrigated crop area's delta_land value
for(i in 10:15){
  
  # identify the column to plot
  zname = colnames(states.WECC@data)[i]
  
  # make labels for the polygons
  l1 = list("sp.text", 
            coordinates(states.WECC), 
            as.character(signif(states.WECC@data[,i], 3)), 
            col="black", cex=0.8,font=1)
  
  # save plot to file
  png(paste("figures/DNDCe_yield_def/", zname, "_yield_def.png", sep=""),
      height = 4, width = 4, units = "in", res=300, type = "cairo")
  p = spplot(states.WECC, 
             zname, 
             main = paste(zname, "yield deficit"), 
             col.regions = map.col,
             at = seq(0.5, 1.0001, 0.0001),
             par.settings = list(axis.line = list(col = 'transparent')),
             sp.layout=list(l1))
  print(p)
  dev.off()
}



