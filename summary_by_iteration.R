#This script by created as part of the PCHES (Program on Coupled Human and Earth Systems) project
#studying the impact of unsustainable groundwater restrictions on crop production in the US West
#The project is funded by the Department of Energy

#This script were developed throughout the course of the project (2021-2025)
#Last updated in March 2025

# This script creates summary files for each state and crop combination and derives the values for the following variables:
# Gross irrigation water demand
# Irrigation deficit (% of gross irr not met)
# Yield deficit due to irrigation deficit
# Irrigated area
# Rainfed area
# Irrigated production (area x yield)
# Rainfed production (area x yield)
#These summary files are used in the other scripts to create plots provided in the submitted manuscript (including the supplemental information)
# All the input files needed to run this script and output files generated are listed in the MetaData file

#Libraries:
# Install the following packages if not already installed using  install.packages("packagename")
library(tidyr)
library(ggplot2)
library(dplyr)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
n_iter = 10 #change the number of iterations here
########################################################################################################################
### function: aggregate DNDCe crops to DREM/IMPLAN crops

dndc_to_implan = function(dat,  # data to convert, as data frame
                          dndc.crops_wbm.crops, 
                          implan_crops.dndc_crops){
  
  # set DNDCe "output" file (just intermediary, this doesn't actually get saved to output)
  DNDC_out = dat
  IMPLAN_crop_names = unique(implan_crops.dndc_crops$IMPLAN_crop)
  
  # IMPLAN/DREM Output 
  IMPLAN_out = data.frame(matrix(nr = length(IMPLAN_crop_names), nc = ncol(DNDC_out)))
  IMPLAN_out[,1] = IMPLAN_crop_names
  
  # sort column names so all states are alphabetical in both files (NB: assumes same state list in DNDC_out and implan_crops.dndc_crops)
  implan_crops.dndc_crops = implan_crops.dndc_crops[, c(1:3, (order(colnames(implan_crops.dndc_crops)[4:ncol(implan_crops.dndc_crops)])+3))]  # assumes first 3 columns are NOT states 
  colnames(IMPLAN_out) = c("IMPLAN_crop", colnames(implan_crops.dndc_crops)[4:ncol(implan_crops.dndc_crops)])
  DNDC_out = DNDC_out[, c(1, (order(colnames(DNDC_out)[2:ncol(DNDC_out)])+1))]   # first column is IDs, NOT states  # SHOULD NOT BE NECESSARY, but doesn't hurt to double check
  colnames(DNDC_out)[1] = "DNDC_crop_id"
  
  # loop through IMPLAN crops
  for(i in 1:length(IMPLAN_crop_names)){
    
    # subset the implan-dndc crop mapping file to IMPLAN_crop_names[i]
    crop_map_sub = subset(implan_crops.dndc_crops, implan_crops.dndc_crops$IMPLAN_crop == IMPLAN_crop_names[i])
    
    # subset the yield deficit file to the crops that match IMPLAN_crop_names[i]
    dndc_yld_def_sub = subset(DNDC_out, DNDC_out$DNDC_crop_id %in% crop_map_sub$DNDC_crop_id)
    
    # sort the crop mapping file so the DNDC_crop_id list is in the same order
    crop_map_sub.sort = crop_map_sub[with(crop_map_sub, order(crop_map_sub$DNDC_crop_id)),]
    
    # # apply scaling factors from implan_crops.dndc_crops to the IMPLAN crop i
    IMPLAN_out[i,2:ncol(IMPLAN_out)] = colSums(crop_map_sub.sort[,4:ncol(crop_map_sub.sort)] * dndc_yld_def_sub[,2:ncol(dndc_yld_def_sub)])
    
   }
  return(IMPLAN_out) # return values
}
########################################################################################################################
### function: aggregate WBM crops to DREM crops
# use this function for both UGW (km3) by crop and state and Gross Irr (km3) by crop and state 

# first, convert WBM to DNDCe crop list

wbm_to_implan = function(dat,  # data to convert, as data frame
                         dndc.crops_wbm.crops, 
                         implan_crops.dndc_crops){
  
  ### 1. WBM to DNDCe crops
  
  # set DNDCe "output" file (just intermediary, this doesn't actually get saved to output)
  DNDC_out = as.data.frame(matrix(nr=nrow(dndc.crops_wbm.crops), nc=ncol(dat)))
  DNDC_out[,1] = dndc.crops_wbm.crops[,2]
  colnames(DNDC_out) = c("DNDC_crop_id", colnames(dat)[2:ncol(dat)])
  
  DNDC_crop_ids = sort(dndc.crops_wbm.crops[,2])
  DNDC_ncrops = length(unique(dndc.crops_wbm.crops$DNDC_crop_system))
  
  for (icrop in 1:DNDC_ncrops) {
    WBM_crop_name = dndc.crops_wbm.crops[icrop,3]
    DNDC_out[icrop,2:ncol(DNDC_out)] = subset(dat, dat$crop == WBM_crop_name)[2:ncol(dat)]
  }
  
  
  ### 2. DNDCe crops to IMPLAN/DREM crops
  
  IMPLAN_crop_names = unique(implan_crops.dndc_crops$IMPLAN_crop)
  
  # IMPLAN/DREM Output 
  IMPLAN_out = data.frame(matrix(nr = length(IMPLAN_crop_names), nc = ncol(DNDC_out)))
  IMPLAN_out[,1] = IMPLAN_crop_names
  
  # sort column names so all states are alphabetical in both files (NB: assumes same state list in DNDC_out and implan_crops.dndc_crops)
  implan_crops.dndc_crops = implan_crops.dndc_crops[, c(1:3, (order(colnames(implan_crops.dndc_crops)[4:ncol(implan_crops.dndc_crops)])+3))]  # assumes first 3 columns are NOT states 
  colnames(IMPLAN_out) = c("IMPLAN_crop", colnames(implan_crops.dndc_crops)[4:ncol(implan_crops.dndc_crops)])
  DNDC_out = DNDC_out[, c(1, (order(colnames(DNDC_out)[2:ncol(DNDC_out)])+1))]   # first column is IDs, NOT states  # SHOULD NOT BE NECESSARY, but doesn't hurt to double check
  
  # loop through IMPLAN crops
  for(i in 1:length(IMPLAN_crop_names)){
    
    # subset the implan-dndc crop mapping file to IMPLAN_crop_names[i]
    crop_map_sub = subset(implan_crops.dndc_crops, implan_crops.dndc_crops$IMPLAN_crop == IMPLAN_crop_names[i])
    
    # subset the yield deficit file to the crops that match IMPLAN_crop_names[i]
    dndc_yld_def_sub = subset(DNDC_out, DNDC_out$DNDC_crop_id %in% crop_map_sub$DNDC_crop_id)
    
    # sort the crop mapping file so the DNDC_crop_id list is in the same order
    crop_map_sub.sort = crop_map_sub[with(crop_map_sub, order(crop_map_sub$DNDC_crop_id)),]
    
    #get corresponding WBM cropIds to modify the scaling factors to be multiplied for each crop category
    crop_map_sub.sort2 <- merge(crop_map_sub.sort,dndc.crops_wbm.crops, by.x = "DNDC_crop_id", by.y = "DNDC_crop_system", all.x = TRUE)
    crop_map_sub.sort3 <- cbind(DNDC_crop_id=crop_map_sub.sort2$DNDC_crop_id, WBM_Crop_id = crop_map_sub.sort2$WBM_Crop_id, crop_map_sub.sort2[, 2:ncol(crop_map_sub.sort2)])
    crop_map_sub.sort3 <- crop_map_sub.sort3[,-c((ncol(crop_map_sub.sort3)-2):ncol(crop_map_sub.sort3))]
    
    crop_map_sub.sort_subset <- crop_map_sub.sort3[, 5:ncol(crop_map_sub.sort3)]
    
    group_sums <- lapply(crop_map_sub.sort_subset, function(x) ave(x, crop_map_sub.sort3$WBM_Crop_id, FUN = sum))
    
    # Calculate modified fractions for each group for each column, such that sum of fractions for each crop category sums to 1
    fractions_list <- lapply(seq_along(group_sums), function(i) {
      ifelse(group_sums[[i]] == 0, 0, crop_map_sub.sort_subset[, i] / group_sums[[i]])
    })
    
    # Combine the news fractions into a new dataframe
    crop_map_sub.newfrac <- data.frame(DNDC_crop_id= crop_map_sub.sort3$DNDC_crop_id, WBM_Crop_id = crop_map_sub.sort3$WBM_Crop_id, sapply(fractions_list, unlist))
    
    # apply scaling factors from implan_crops.dndc_crops to the IMPLAN crop i
    IMPLAN_out[i,2:ncol(IMPLAN_out)] = colSums(crop_map_sub.newfrac[,3:ncol(crop_map_sub.newfrac)] * dndc_yld_def_sub[,2:ncol(dndc_yld_def_sub)])
  }
  return(IMPLAN_out) # return values
  
}

########################################################################################################################

wbm_to_drem_long = function(nfiles, 
                            pth,
                            prefx,
                            var.nm,
                            wbm.to.implan,
                            dndc.to.implan = 0){
  for(i in 0:nfiles){
    d = read.csv(paste(pth, prefx,"iteration_", i, ".csv", sep=""))
    
    if(wbm.to.implan == 1){
      d = wbm_to_implan(d, 
                        dndc.crops_wbm.crops, 
                        implan_crops.dndc_crops) 
    }
    
    if(dndc.to.implan == 1){
      d = dndc_to_implan(d, 
                         dndc.crops_wbm.crops, 
                         implan_crops.dndc_crops) 
    }
    d = d[-2,]
    
    for(crp in 1:nrow(d)){
      d.crp = subset(d, d$IMPLAN_crop == unique(d$IMPLAN_crop)[crp])
      
      d.long = gather(data = d.crp, 
                      key  =  State,
                      value = Value,
                      Arizona:Wyoming,
                      factor_key = TRUE)
      if(crp == 1){
        d.crops = d.long
      }else{
        d.crops = rbind(d.crops, d.long)
      }
    }
    d.crops$iteration = i
    
    if(i == 0){
      d.all = d.crops
    }else{
      d.all = rbind(d.all, d.crops)
    }
  }
  d.all$variable = var.nm
  return(d.all)
  
}


########################################################################################################################
# load data needed to convert WBM to IMPLAN/DREM crops
dndc.crops_wbm.crops = read.csv("data/WoCD_DNDC_CropSystem_id_name_WBM_crop_id_name.csv",  # dndc and wbm crops
                                header=T, 
                                stringsAsFactors = FALSE,
                                colClasses=c("character","numeric","character","numeric"), 
                                sep =",")
implan_crops.dndc_crops = read.csv(file = 'data/alpha_i_j_s_SprWinWht_WECC.csv',   # dndc and implan (aka DREM) crops
                                   header=T, 
                                   stringsAsFactors = 
                                     FALSE, sep = ",")



########################################################################################################################
# 1. Gross Irrigation Water, km3/yr
########################################################################################################################
pth    = "data/DNDCe/" # path to files
nfiles = n_iter           # number of files to read
prefx  = "IrrGross_mmYr_states_"  # file name prefix
var.nm = "IrrGross_km3Yr"
wbm.to.implan = 1

gross.irr.all = wbm_to_drem_long(nfiles, 
                                 pth,
                                 prefx,
                                 var.nm,
                                 wbm.to.implan)

write.csv(gross.irr.all, "results/gross_irr_long.csv", row.names=F)


########################################################################################################################
# 2. UGW, km3/yr
########################################################################################################################
pth    = "data/DNDCe/" # path to files
nfiles = n_iter            # number of files to read
prefx  = "UGW_mmYr_states_"  # file name prefix
var.nm = "UGW_km3Yr"
wbm.to.implan = 1


ugw.km3.all = wbm_to_drem_long(nfiles, 
                              pth,
                              prefx,
                              var.nm,
                              wbm.to.implan)

write.csv(ugw.km3.all, "results/ugw_km3_long.csv", row.names=F)

########################################################################################################################
# 3. UGW, fraction
# 3b: sustainable irrigation water

g = gross.irr.all
u = ugw.km3.all

colnames(g)[3] = "GrossIrr_km3"
colnames(u)[3] = "UGW_km3"

g = g[,-5]
u = u[,-5]

b = merge(g, u)
b$irr_sust = b$GrossIrr_km3 - b$UGW_km3

b$variable = "irr_sust_km3"
b=b[,-c(4:5)]
colnames(b)[4] = "Value"

write.csv(b, "results/sust_irr_km3_long.csv", row.names=F)

# 3b. Sustainable irrigation water as fraction of gross
gross.irr = read.csv('results/gross_irr_long.csv')
sust.irr  = read.csv('results/sust_irr_km3_long.csv')
dat = merge(gross.irr, sust.irr, by=c("IMPLAN_crop", "State", "iteration"))
dat$sust.irr.frac = dat$Value.y / dat$Value.x
dat$variable = "irr_sust_frac"
dat = dat[,c(1,2,3,8,9)]
write.csv(dat, "results/sust_irr_frac_long.csv", row.names=F)

dat$unsust.irr.frac = 1-dat$sust.irr.frac
dat$variable = "irr_unsust_frac"
dat = dat[,c(1,2,3,5,6)]
write.csv(dat, "results/unsust_irr_frac_long.csv", row.names=F)
########################################################################################################################
# 4. deficit irrigation
########################################################################################################################

nfiles = n_iter           # number of files to read
prefx  = "DNDCe_to_IMPLAN_yield_deficit_STATE_"  # file name prefix
var.nm = "yield_deficit"
wbm.to.implan = 0

deficit_yld = wbm_to_drem_long(nfiles, 
                               pth,
                               prefx,
                               var.nm,
                               wbm.to.implan)
deficit_yld$State = as.character(sub("New_Mexico", "New.Mexico", as.character(deficit_yld$State)))
write.csv(deficit_yld, "results/deficit_yld_long.csv", row.names=F)

########################################################################################################################
# 5. Irrigated and rainfed area
########################################################################################################################

pth = "data/GCAM/" # path to files
nfiles = n_iter               # number of files to read
prefx  = "DREM_land_"  # file name prefix

wecc.states = c("Washington",
                "New_Mexico", 
                "Colorado",
                "Utah",
                "Wyoming",
                "Nevada",
                "Montana",
                "Arizona",
                "Oregon",
                "Idaho",
                "California")

# get reference land area per crop
iter.reference = read.csv(paste(pth,"GCAM_2010_reference_land.csv", sep=""))
iter.reference = subset(iter.reference, iter.reference$State %in% wecc.states)
iter.reference = subset(iter.reference, iter.reference$year == 2010) 
iter.reference = iter.reference[,-4] # remove year column
iter.reference$iteration = "reference"

dat.land = iter.reference
# get iteration 0 land area per crop
iter.0 = read.csv(paste(pth,"GCAM_2015_reference_land.csv", sep=""))
iter.0 = subset(iter.0, iter.0$State %in% wecc.states)
iter.0 = subset(iter.0, iter.0$year == 2015)
iter.0 = iter.0[,-4] # remove year column
iter.0$iteration = 0

dat.land = rbind(dat.land,iter.0)

for(i in 1:nfiles){
  d = read.csv(paste(pth, prefx, "iteration_", i,".csv", sep=""))
  d = subset(d, d$State %in% wecc.states)
  d = subset(d, d$year == 2015)
  d = d[,-4] # remove year column
  d$iteration = i
  
  dat.land = rbind(dat.land, d)
}
colnames(dat.land)[1] = "IMPLAN_crop"
colnames(dat.land)[4] = "Value"
dat.land$variable = "land_area"
dat.land$State = as.character(sub("New_Mexico", "New.Mexico", as.character(dat.land$State)))
write.csv(dat.land, "results/land_area_long.csv", row.names=F)

# 5b. Separate irrigated and rainfed land
dat.land = read.csv("results/land_area_long.csv")

dat.irr = subset(dat.land, dat.land$Management == "Irrigated")
dat.rfd = subset(dat.land, dat.land$Management == "Rainfed")

dat.irr$variable = "irr_land"
dat.rfd$variable = "rfd_land"

dat.irr = dat.irr[,c(1,2,4,6,7)]
dat.rfd = dat.rfd[,c(1,2,4,6,7)]

write.csv(dat.irr, "results/irr_land_area_long.csv", row.names=F)
write.csv(dat.rfd, "results/rfd_land_area_long.csv", row.names=F)

########################################################################################################################
# 6. Yield values (max and deficit in kgC/ha)

#### a. identify maximum possible irrigated yields
yld.max = read.csv("data/wecc_merra2_full_sw_c13_2006-2015_YieldVsDefIrrig_QuadFitParams_v1.csv")
yld.max$StateName = sub("New Mexico", "New.Mexico", as.character(yld.max$StateName))

dat = read.csv("data/DNDCe/DNDC_yield_deficit_iteration_0.csv")

# make a matrix of max irrigated yields: rows = crop id, cols = states
yld.max.matrix = as.data.frame(matrix(nr = nrow(dat), ncol = ncol(dat)))
yld.max.matrix[,1] = dat$DNDC_crop_id
colnames(yld.max.matrix) = colnames(dat)

for(i in 1:nrow(yld.max.matrix)){
  crop.id = yld.max.matrix$DNDC_crop_id[i]
  
  for(s in 2:ncol(yld.max.matrix)){
    st = colnames(yld.max.matrix)[s]
    
    match.row = which(yld.max$crop_id == crop.id  &  yld.max$StateName == st)
    
    yld.max.matrix[i,s] = yld.max$intercept[match.row]
  }
}

write.csv(yld.max.matrix, "results/yield_max_matrix.csv", row.names=F)

## Convert DNDCe max yields to IMPLAN/DREM crop categories.
### save as long format (just for purposes of comparisons by iteration and plots)
yld.max.implan = dndc_to_implan(yld.max.matrix, 
                                dndc.crops_wbm.crops, 
                                implan_crops.dndc_crops)
yld.max.implan = yld.max.implan[-2,]

yld.max.long = gather(yld.max.implan,
                      key = State,
                      value = Max_yield_kg.C.ha,
                      Arizona:Wyoming,
                      factor_key = TRUE)

# repeat for 0 through nfile iterations
for(i in 0:nfiles){
  yld.max.long$iteration = i
  if(i == 0){
    yld.max.all = yld.max.long
  }else{
    yld.max.all = rbind(yld.max.all, yld.max.long)
  }
}

write.csv(yld.max.all, "results/max_yield_kg.C.ha.csv", row.names=F)

#### b. calculate the sustainable water-based yield for each crop/state, each iteration. save to file as "result"
pth    = "data/DNDCe/" # path to files
nfiles = n_iter           # number of files to read
prefx  = "DNDC_yield_deficit_"  # file name prefix 

for(i in 0:nfiles){
  d = read.csv(paste(pth, prefx, "iteration_", i, ".csv", sep=""))
  d.yld.kg.C.ha = d[,2:12] * yld.max.matrix[,2:12]
  d.yld.kg.C.ha = cbind(d$DNDC_crop_id, d.yld.kg.C.ha)
  
  write.csv(d.yld.kg.C.ha,
            paste("data/DNDCe/DNDC_yield_kg.C.ha", "_iteration_", i, ".csv", sep=""),
            row.names=F)
}

### c. convert to DREM/IMPLAN crops using weighting factors
pth    = "data/DNDCe/" # path to files
nfiles = n_iter           # number of files to read
prefx  = "DNDC_yield_kg.C.ha_"  # file name prefix
var.nm = "sust_yield"
wbm.to.implan = 0
dndc.to.implan = 1


sust_yld_kg.C.ha = wbm_to_drem_long(nfiles, 
                                    pth,
                                    prefx,
                                    var.nm,
                                    wbm.to.implan,
                                    dndc.to.implan)

write.csv(sust_yld_kg.C.ha, "results/sust_yld_kg.C.ha_long.csv", row.names=F)

# 6b. Unsustainable yield (kgC/ha)
dat = merge(sust_yld_kg.C.ha, yld.max.all, by=c("IMPLAN_crop", "State", "iteration"))
dat$unsust_yld = dat$Max_yield_kg.C.ha - dat$Value
unsust_yld_kg.C.ha = dat[,c(1,2,3,7)]
unsust_yld_kg.C.ha$variable = "unsust_yld"
write.csv(unsust_yld_kg.C.ha, "results/unsust_yld_kg.C.ha_long.csv", row.names=F)
########################################################################################################################

### write data in long format

# load data
gross.irr          = read.csv("results/gross_irr_long.csv")
ugw.km3             = read.csv("results/ugw_km3_long.csv")
sust.wtr.km3        = read.csv("results/sust_irr_km3_long.csv")
max.yld.kgC.ha     = read.csv("results/max_yield_kg.C.ha.csv")
unsust.yld.kgC.ha  = read.csv("results/unsust_yld_kg.C.ha_long.csv")
sust.yld.kg.C.ha   = read.csv("results/sust_yld_kg.C.ha_long.csv")
irr.land.area      = read.csv("results/irr_land_area_long.csv")
rfd.land.area      = read.csv("results/rfd_land_area_long.csv")

# make sure all column names match to allow row binding
colnames(max.yld.kgC.ha)[3] = "Value"
max.yld.kgC.ha$variable = "max_yield"

colnames(unsust.yld.kgC.ha)[4] = "Value"
unsust.yld.kgC.ha = unsust.yld.kgC.ha[, c(1,2,4,3,5)]

# convert land areas from 2.4 thousand hectares to thousand hectares
irr.land.area$Value = irr.land.area$Value / 2.4
rfd.land.area$Value = rfd.land.area$Value / 2.4

# merge all
dat.all = rbind(gross.irr, 
                ugw.km3, 
                sust.wtr.km3,
                max.yld.kgC.ha, 
                unsust.yld.kgC.ha,
                sust.yld.kg.C.ha,
                irr.land.area,
                rfd.land.area)

# break into wide format
states = unique(dat.all$State)
crops  = unique(dat.all$IMPLAN_crop)
mod.list = c(rep("WBM", 3), rep("DNDCe", 3), rep("GCAMland", 2))
for(s in states){
  for(r in crops){
    sr = subset(dat.all, dat.all$IMPLAN_crop == r & dat.all$State == s)
    sr = sr[,c(3,4,5)]
    sr.wide = reshape(sr, idvar = "variable", timevar = "iteration", direction = "wide")
    sr.wide$variable = c("Total_irr_water_km3",
                         "Unsustainable_irr_water_km3",
                         "Sustainable_irr_water_km3",
                         "Max_irrigated_yield_kgC_per_ha",
                         "Unsustainable_yield_kgC_per_ha",
                         "Sustainable_yield_kgC_per_ha",
                         "Irrigated_land_area_1000_ha",
                         "Rainfed_land_area_1000_ha")
    
    # add columns for scenario, state, crop, model, and reference
    num_iter = n_iter; #number of iterations used in the run
    scenario=1
    sr.wide$scenario = scenario
    sr.wide$state = s
    sr.wide$crop = r
    sr.wide$model = mod.list
    sr.wide$reference = c(rep(NA, 6), sr.wide$Value.reference[7:8])
    sr.wide = sr.wide[,c((num_iter+4):(num_iter+7),1,(num_iter+8),2:(num_iter+2))]
    colnames(sr.wide)[7:(num_iter+7)] = c("iteration_0", paste("iteration", seq(1:num_iter), sep="_"))
    
    # without drem data:
    m = sr.wide
    
    # save to file
    write.csv(m, paste("results/crop_state_tables/", s, "_", r, "_all_models.csv", sep=""), row.names=F) 
  }
}
