#This script by created as part of the PCHES (Program on Coupled Human and Earth Systems) project
#studying the impact of restrictions of extracting groundwater beyond recharge on crop production in the US West
#The project is funded by the Department of Energy

#This script were developed throughout the course of the project (2021-2025)
#Last updated in March 2025

# This script creates irrigation deficit maps that are provided in the submitted manuscript (including the supplemental information)
# The maps show irrigation deficit at the end of iteration runs for different states in the US West for six crop categories
# The script also creates irrigation v/s yield deficit maps for the three major crop categories (grain, vegetables and fruits, and fodder crops)
# and an irrigation deficit map for all the crops combined
# All the input files needed to run this script and output files generated are listed in the MetaData file


######################################################################################################
### Libraries:
# Install the following packages if not already installed using  install.packages("packagename")

library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(dplyr)

#create new folder for figure outputs
dir.create("figures/irr_def_maps_by_crop")


######################################################################################################
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


### Crop ID mapping files to aggregate WBM crop list to DREM crop list
wbm_to_dndc = read.csv("data/WoCD_DNDC_CropSystem_id_name_WBM_crop_id_name.csv", 
                       header=T, stringsAsFactors = FALSE, colClasses=c("character","numeric","character","numeric"), 
                       sep =",")
dndc_to_drem = read.csv(file = 'data/alpha_i_j_s_SprWinWht_WECC.csv', 
                        header=T, stringsAsFactors = FALSE, sep = ",")


### WBM output
# Gross irrigation by state and WBM crop
gross.irr.b = read.csv("data/DNDCe/IrrGross_mmYr_states_iteration_1.csv")
gross.irr.s = read.csv("data/DNDCe/IrrGross_mmYr_states_iteration_10.csv")

# GBR irrigation by state and WBM crop
gbr.irr.b = read.csv("data/DNDCe/GBR_mmYr_states_iteration_1.csv")
gbr.irr.s = read.csv("data/DNDCe/GBR_mmYr_states_iteration_10.csv")


### DNDCe output
# Yield (kg C/ha) by state and DREM crop
yld.b = read.csv("data/DNDCe/DNDCe_to_IMPLAN_yield_deficit_STATE_iteration_1.csv")
yld.s = read.csv("data/DNDCe/DNDCe_to_IMPLAN_yield_deficit_STATE_iteration_10.csv")

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

# baseline gross irr
gross.irr.b.drem = agg.wbm.to.drem(wbm.dat = gross.irr.b, 
                                   drem.crop.list, crop_list)

# scen 1 it 10 gross irr
gross.irr.s.drem = agg.wbm.to.drem(wbm.dat = gross.irr.s, 
                                   drem.crop.list, crop_list)


# baseline GBR
gbr.b.drem = agg.wbm.to.drem(wbm.dat = gbr.irr.b, 
                             drem.crop.list, crop_list)

# scen 1 it 10 GBR
gbr.s.drem = agg.wbm.to.drem(wbm.dat = gbr.irr.s, 
                             drem.crop.list, crop_list)

######################################################################################################
### Show deficit irrigation by crop and state: yield and water
# yield deficit irr is already calculated
yld.s = read.csv("data/DNDCe/DNDCe_to_IMPLAN_yield_deficit_STATE_iteration_10.csv")
yld.s = yld.s[-2,] # remove "not_included" crop
irr.def = cbind(gbr.s.drem$crop, (1 - (gbr.s.drem[,2:12]/gross.irr.s.drem[,2:12])))
irr.def[is.na(irr.def)] = NA 
colnames(irr.def)[1] = "crop"

x = as.numeric(unlist(irr.def[,2:12]))
plot(as.numeric(unlist(yld.s[,2:12])) ~ x, xlim = rev(range(x, na.rm = TRUE)))

# convert tables from wide to long for use in ggplot
yld.long = gather(yld.s, 
                  key = state,
                  value = yield,
                  Arizona:Wyoming,
                  factor_key = TRUE)
colnames(yld.long)[1] = "crop"

yld.long$state <- gsub("_", ".", yld.long$state)
 
irr.def.long = gather(irr.def, 
                  key = state,
                  value = deficit.irrigation,
                  Washington:California,
                  factor_key = TRUE)

merge.long = merge(yld.long, irr.def.long)

merge.long <- merge.long %>%
  mutate(crop=recode(crop, "other"="fodder crops"))

p =  ggplot(merge.long, aes(x=deficit.irrigation, y=yield, shape=crop, color=state)) +
  geom_point(size=5) + 
  scale_x_reverse() +
  ylim(0.6, 1.0) +
  theme_minimal()+
  theme(
    text=element_text(size=32),
    axis.text=element_text(size=32),
    axis.line = element_line(color="black")
  )+
  labs(title="Yield deficit v/s Irrigation deficit (all crops)", x="Irrigation deficit", y="Fraction of maximum yield",
       shape = "crop",
       color = "State")+
  scale_color_brewer(palette="Paired")



ggsave("figures/Yld_vs_IrrDef_i10.png",
       p,
       device = "png", width=14,height=10, units="in",
       dpi=600)

pgrain =  ggplot(filter(merge.long, crop=="grain"), aes(x=deficit.irrigation, y=yield, shape=crop, color=state)) +
  geom_point(size=10) + 
  scale_x_reverse() +
  ylim(0.7, 1.0) +
  theme_minimal()+
  theme(
    text=element_text(size=32),
    axis.text=element_text(size=32),
    axis.line = element_line(color="black")
  )+
  labs(title="Yield deficit v/s Irrigation deficit (grain)", x="Irrigation deficit", y="Fraction of maximum yield",
       color = "State")+
  scale_shape_manual(values=15)+
  scale_color_brewer(palette="Paired")



ggsave("figures/Yld_vs_IrrDef_grain_i10.png",
       pgrain,
       device = "png", width=14,height=10, units="in",
       dpi=600)

pvegfruit =  ggplot(filter(merge.long, crop=="vegfruit"), aes(x=deficit.irrigation, y=yield, shape=crop, color=state)) +
  geom_point(size=10) + 
  scale_x_reverse() +
  ylim(0.6, 1.0) +
  theme_minimal()+
  theme(
    text=element_text(size=32),
    axis.text=element_text(size=32),
    axis.line = element_line(color="black")
  )+
  labs(title="Yield deficit v/s Irrigation deficit (vegfruit)", x="Irrigation deficit", y="Fraction of maximum yield",
       color = "State")+
  scale_shape_manual(values=16)+
  scale_color_brewer(palette="Paired")



ggsave("figures/Yld_vs_IrrDef_vegfruit_i10.png",
       pvegfruit,
       device = "png", width=14,height=10, units="in",
       dpi=600)

pfoddercrops =  ggplot(filter(merge.long, crop=="fodder crops"), aes(x=deficit.irrigation, y=yield, shape=crop, color=state)) +
  geom_point(size=10) + 
  scale_x_reverse() +
  ylim(0.6, 1.0) +
  theme_minimal()+
  theme(
    text=element_text(size=32),
    axis.text=element_text(size=32),
    axis.line = element_line(color="black")
  )+
  labs(title="Yield deficit v/s Irrigation deficit (fodder crops)", x="Irrigation deficit", y="Fraction of maximum yield",
       color = "State")+
  scale_shape_manual(values=17)+
  scale_color_brewer(palette="Paired")



ggsave("figures/Yld_vs_IrrDef_foddercrops_i10.png",
       pfoddercrops,
       device = "png", width=14,height=10, units="in",
       dpi=600)

### maps
# 1. by crop, based on water

# make plots
plot_list = list()
for(k in 1:length(irr.def$crop)){
  crop = irr.def$crop[k]
  crop.def = irr.def[k,]
  states.WECC$crop.def = mat.or.vec(nr=nrow(states.WECC@data), nc=1)
  states.WECC$NAME = sub(" ", ".", as.character(states.WECC$NAME)) # fix inconsistent state name for New Mexico
  
  for(i in 1:nrow(states.WECC@data)){
    states.WECC$crop.def[i] = as.numeric(crop.def[which(colnames(crop.def) == states.WECC$NAME[i])])

  }

  map.col = colorRampPalette(c(brewer.pal(n=9, name='RdYlBu')))(1000)
  # make labels for the polygons
  l1 = list("sp.text",
            coordinates(states.WECC),
            as.character(signif(states.WECC$crop.def, 3)),
            col="black", cex=2,font=1)

  
  p = spplot(states.WECC, zcol = "crop.def", main = list(paste(as.character(crop), "deficit irrigation water use")),col.regions = map.col,
             at = seq(0.4,1.01,0.01),
             par.settings = list(axis.line = list(col = 'transparent')),
             sp.layout=list(l1),
             colorkey=list(
               space="right",
               labels=list(
                 cex=2
               )
             ))
  plot_list[[k]] = p
}

# save files
for (k in 1:length(irr.def$crop)) {
  crop = irr.def$crop[k]
  file_name = paste("figures/irr_def_maps_by_crop/", crop, "_irr_def_map.png", sep="")
  png(file_name, res=150,width = 8, height = 8, unit = "in", type = "cairo")
  print(plot_list[[k]])
  dev.off()
}

# 2. by total water amount (combine all crops)
irr.gross.b.sum = colSums(gross.irr.b.drem[,2:12])
irr.gross.s.sum = colSums(gross.irr.s.drem[,2:12])

gbr.b.sum = colSums(gbr.b.drem[,2:12])
gbr.s.sum = colSums(gbr.s.drem[,2:12])

gbr.s.frac = 1 - (gbr.s.sum/irr.gross.s.sum)


states.WECC$irr.def = mat.or.vec(nr=nrow(states.WECC@data), nc=1)
states.WECC$NAME = sub(" ", ".", as.character(states.WECC$NAME)) # fix inconsistent state name for New Mexico
for(i in 1:nrow(states.WECC@data)){
  states.WECC$irr.def[i] = as.numeric(gbr.s.frac[which(names(gbr.s.frac) == states.WECC$NAME[i])])
}

map.col = colorRampPalette(c(brewer.pal(n=9, name='RdYlBu')))(1000)


# make labels for the polygons
  l2 = list("sp.text", 
            coordinates(states.WECC), 
            as.character(signif(states.WECC@data[,i], 3)), 
            col="black", cex=2,font=1)
  
  # save plot to file
  png("figures/Irr_def_map_allCrops_i10.png", res=150, width = 8, height = 8, unit = "in", type = "cairo")
  p=spplot(states.WECC, "irr.def", main = list("deficit irrigation fraction - all crops",cex=2),col.regions = map.col,
         at = seq(0.4, 1, 0.01),
         par.settings = list(axis.line = list(col = 'transparent')),
         sp.layout=list(l2),
         colorkey=list(
         space="right",
             labels=list(
               cex=2
               )
           ))
  print(p)
  dev.off()


