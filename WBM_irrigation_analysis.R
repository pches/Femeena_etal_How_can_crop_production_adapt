#This script by created as part of the PCHES (Program on Coupled Human and Earth Systems) project
#studying the impact of restrictions of groundwater beyond recharge (GBR) on crop production in the US West
#The project is funded by the Department of Energy

#This script were developed during the final stages of the project (2024-2025)
#Last updated in March 2025

# This script is used to create the grid-scale irrigation water requests plot provided in the submitted manuscript 
# The maps show GBR irrigation water request for the reference scenario, as well as
# change in GBR irrigation water request (Iteration 10 - Iteration 0) for the state of California
# All the input files needed to run this script and output files generated are listed in the MetaData file


#Load libraries
# Install the following packages if not already installed using  install.packages("packagename")
library(ncdf4)
library(raster)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(RColorBrewer)
library(lattice)
library(maps)
library(sp)
library(sf)

#Plot gross irrigation map
#Load the WBM grid-level irrigation data for iteration 0
nc_data1 <- nc_open("data/WBM/wbm_irrigationGross_dc_iteration_0.nc")
lon <- ncvar_get(nc_data1, "lon")
lat <- ncvar_get(nc_data1,"lat", verbose = F)

irrig.array1 <- ncvar_get(nc_data1, "irrigationGross")
fillvalue1 <- ncatt_get(nc_data1, "irrigationGross", "_FillValue")
nc_close(nc_data1)

irrig.array1[irrig.array1 == fillvalue1$value] <- NA

sum_irrigation1 <- round(apply(irrig.array1, MARGIN = c(1,2), FUN=sum),6)

r1 <- raster(t(sum_irrigation1), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs = CRS("+proj=longlat +datum=WGS84"))
r1<-flip(r1,direction='y')
r1_df <- as.data.frame(r1,xy=TRUE,na.rm=TRUE)
state_boundaries <- map_data("state")
WECC_states <- c("arizona","california","idaho","nevada","oregon","utah","washington","montana","wyoming","colorado","new mexico")
state_boundaries <- state_boundaries[state_boundaries$region %in% WECC_states,]

#Plot change in gross irrigation
#Load irrigation data for iteration 10
nc_data2 <- nc_open("data/WBM/wbm_irrigationGross_dc_iteration_10.nc") #results for final iteration
irrig.array2 <- ncvar_get(nc_data2, "irrigationGross")
fillvalue2 <- ncatt_get(nc_data2, "irrigationGross", "_FillValue")
nc_close(nc_data2)
irrig.array2[irrig.array2 == fillvalue2$value] <- NA

sum_irrigation2 <- round(apply(irrig.array2, MARGIN = c(1,2), FUN=sum),6)
tot_irrig.diff <- round((sum_irrigation2-sum_irrigation1)*100/sum_irrigation1)

r2 <- raster(t(tot_irrig.diff), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs = CRS("+proj=longlat +datum=WGS84"))
r2<-flip(r2,direction='y')
r2_df <- as.data.frame(r2,xy=TRUE,na.rm=TRUE)

#Plot GBR irrigation maps
#Load data for GBR irrigation for iteration 0
nc_data3 <- nc_open("data/WBM/wbm_irrigationExtra_dc_iteration_0.nc")
lon <- ncvar_get(nc_data3, "lon")
lat <- ncvar_get(nc_data3,"lat", verbose = F)

irrig.array3 <- ncvar_get(nc_data3, "irrigationExtra")
fillvalue3 <- ncatt_get(nc_data3, "irrigationExtra", "_FillValue")
nc_close(nc_data3)

irrig.array3[irrig.array3 == fillvalue3$value] <- NA

sum_irrigation3 <- round(apply(irrig.array3, MARGIN = c(1,2), FUN=sum),6)

r3 <- raster(t(sum_irrigation3), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs = CRS("+proj=longlat +datum=WGS84"))
r3<-flip(r3,direction='y')
r3_df <- as.data.frame(r3,xy=TRUE,na.rm=TRUE)

#Load data for GBR irrigation for iteration 10
nc_data4 <- nc_open("data/WBM/wbm_irrigationExtra_dc_iteration_10.nc")
irrig.array4 <- ncvar_get(nc_data4, "irrigationExtra")
fillvalue4 <- ncatt_get(nc_data4, "irrigationExtra", "_FillValue")
nc_close(nc_data4)
irrig.array4[irrig.array4 == fillvalue4$value] <- NA

sum_irrigation4 <- round(apply(irrig.array4, MARGIN = c(1,2), FUN=sum),6)

gbr_irrig.diff <- round((sum_irrigation4-sum_irrigation3)*100/sum_irrigation3,6)

r4 <- raster(t(gbr_irrig.diff), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs = CRS("+proj=longlat +datum=WGS84"))
r4<-flip(r4,direction='y')
r4_df <- as.data.frame(r4,xy=TRUE,na.rm=TRUE)
myColors1 <- colorRampPalette(c("red","yellow","darkgreen"))(100)

#save the map
png("figures/WBM_irrigation_maps/Change_gbr_irrig.png",
    height = 2000, width = 1500,type = "cairo")

p=ggplot()+
  geom_tile(data=r4_df,aes(x=x,y=y,fill=layer),alpha=0.7)+
  scale_fill_gradientn(colors=myColors1,limits = c(-100,100),na.value=NA,
                       oob = scales::squish,           # Handle values outside the limits
                       breaks = c(-100, 0, 100),       # to improve the legend 
                       labels = c("-100", "0", "100")) +
  geom_path(data = state_boundaries, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_fixed(1.3)+
  theme_minimal()+
  theme(panel.grid.minor=element_blank(),
        axis.title =element_text(size=42),
        axis.text=element_text(size=42),
        plot.title = element_text(size = 40),  
        legend.title = element_text(size = 42),  
        legend.text = element_text(size = 40),
        legend.key.height = unit(1, "cm")
  )+
  labs(fill="Change in Groundwater-Beyond-Recharge
Irrigation Water Request(%)
       ",
       x = "Longitude",
       y="Latitude")
print(p)
dev.off()


##Plots for iterations 0,1, and 10 (iteration numbers in file names can be changed as needed to plot these)

nc_data1 <- nc_open("data/WBM/wbm_irrigationGross_dc_iteration_0.nc")
lon <- ncvar_get(nc_data1, "lon")
lat <- ncvar_get(nc_data1,"lat", verbose = F)

irrig.array1 <- ncvar_get(nc_data1, "irrigationGross")
fillvalue1 <- ncatt_get(nc_data1, "irrigationGross", "_FillValue")
nc_close(nc_data1)

irrig.array1[irrig.array1 == fillvalue1$value] <- NA

sum_irrigation1 <- round(apply(irrig.array1, MARGIN = c(1,2), FUN=sum),6)
r1 <- raster(t(sum_irrigation1), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs = CRS("+proj=longlat +datum=WGS84"))
r1<-flip(r1,direction='y')
r1_df <- as.data.frame(r1,xy=TRUE,na.rm=TRUE)

nc_data3 <- nc_open("data/WBM/wbm_irrigationExtra_dc_iteration_0.nc")
lon <- ncvar_get(nc_data3, "lon")
lat <- ncvar_get(nc_data3,"lat", verbose = F)

irrig.array3 <- ncvar_get(nc_data3, "irrigationExtra")
fillvalue3 <- ncatt_get(nc_data3, "irrigationExtra", "_FillValue")
nc_close(nc_data3)

irrig.array3[irrig.array3 == fillvalue3$value] <- NA

sum_irrigation3 <- round(apply(irrig.array3, MARGIN = c(1,2), FUN=sum),6)

r3 <- raster(t(sum_irrigation3), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs = CRS("+proj=longlat +datum=WGS84"))
r3<-flip(r3,direction='y')
r3_df <- as.data.frame(r3,xy=TRUE,na.rm=TRUE)

merged_df <- merge(r1_df, r3_df, by = c("x", "y"), suffixes = c("_r1_df", "_r3_df"))

# Create new dataframe with ratio
r5_df_iter0 <- data.frame(x = merged_df$x, 
                          y = merged_df$y, 
                          layer = merged_df$layer_r3_df / merged_df$layer_r1_df) #fraction of GBR irrigation water

myColors2 <- colorRampPalette(c("darkgreen","yellow","red"))(100)

#save map of fraction of GBR irrigation water for reference scenario
png("figures/WBM_irrigation_maps/Frac_gbr_irrig_iter0.png",
    height = 4000, width = 3000, type = "cairo")

p=ggplot()+
  geom_tile(data=r5_df_iter0,aes(x=x,y=y,fill=layer),alpha=0.7)+
  scale_fill_gradientn(colors=myColors2,limits = c(0,1),na.value=NA,
                       breaks = c(0, 0.5, 1),       # to improve the legend 
                       labels = c("0", "0.5", "1"))+
  geom_path(data = state_boundaries, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_fixed(1.3)+
  theme_minimal()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title =element_text(size=52),
        axis.text=element_text(size=52),
        plot.title = element_text(size = 50),  
        legend.title = element_text(size = 52),  
        legend.text = element_text(size = 50),
        legend.key.height = unit(1, "cm")
  )+
  labs(title = " Reference scenario",
       fill="Fraction of GBR water use
       ",
       x = "Longitude",
       y="Latitude")
print(p)
dev.off()


###Iteration 10
nc_data1 <- nc_open("data/WBM/wbm_irrigationGross_dc_iteration_10.nc")
lon <- ncvar_get(nc_data1, "lon")
lat <- ncvar_get(nc_data1,"lat", verbose = F)

irrig.array1 <- ncvar_get(nc_data1, "irrigationGross")
fillvalue1 <- ncatt_get(nc_data1, "irrigationGross", "_FillValue")
nc_close(nc_data1)

irrig.array1[irrig.array1 == fillvalue1$value] <- NA

sum_irrigation1 <- round(apply(irrig.array1, MARGIN = c(1,2), FUN=sum),6)
r1 <- raster(t(sum_irrigation1), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs = CRS("+proj=longlat +datum=WGS84"))
r1<-flip(r1,direction='y')
r1_df <- as.data.frame(r1,xy=TRUE,na.rm=TRUE)

nc_data3 <- nc_open("data/WBM/wbm_irrigationExtra_dc_iteration_10.nc")
lon <- ncvar_get(nc_data3, "lon")
lat <- ncvar_get(nc_data3,"lat", verbose = F)

irrig.array3 <- ncvar_get(nc_data3, "irrigationExtra")
fillvalue3 <- ncatt_get(nc_data3, "irrigationExtra", "_FillValue")
nc_close(nc_data3)

irrig.array3[irrig.array3 == fillvalue3$value] <- NA

sum_irrigation3 <- round(apply(irrig.array3, MARGIN = c(1,2), FUN=sum),6)

r3 <- raster(t(sum_irrigation3), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs = CRS("+proj=longlat +datum=WGS84"))
r3<-flip(r3,direction='y')
r3_df <- as.data.frame(r3,xy=TRUE,na.rm=TRUE)

merged_df <- merge(r1_df, r3_df, by = c("x", "y"), suffixes = c("_r1_df", "_r3_df"))

# Create new dataframe with ratio
r5_df_iter10 <- data.frame(x = merged_df$x, 
                           y = merged_df$y, 
                           layer = merged_df$layer_r3_df / merged_df$layer_r1_df) #fraction of GBR irrigation water


merged_df_diff <- merge(r5_df_iter0, r5_df_iter10, by = c("x", "y"), suffixes = c("_iter0_df", "_iter10_df"))

r6_df <- data.frame(x = merged_df_diff$x, 
                    y = merged_df_diff$y, 
                    layer = merged_df_diff$layer_iter10_df - merged_df_diff$layer_iter0_df) #difference in fraction of GBR water from iter 0 to iter 10


lon_min <- -125  # Minimum longitude
lon_max <- -114  # Maximum longitude
lat_min <- 32     # Minimum latitude
lat_max <- 42     # Maximum latitude

# Filter r5_df for points within the bounding box for California
r6_df_filtered <- r6_df[r6_df$x >= lon_min & r6_df$x <= lon_max &
                          r6_df$y >= lat_min & r6_df$y <= lat_max, ]

myColors3 <- colorRampPalette(c("darkblue", "blue", "lightblue"))(100)

#save the CA map for change in fraction of GBR irrigation
png("figures/WBM_irrigation_maps/Frac_gbr_irrig_iter10-iter0_CA.png", type = "cairo")

p=ggplot()+
  geom_tile(data=r6_df_filtered,aes(x=x,y=y,fill=layer),alpha=0.7)+
  scale_fill_gradientn(colors=myColors3,limits = c(-0.2,0),na.value=NA,
                       breaks = c(-0.2, -0.1, 0),       # to improve the legend 
                       labels = c("-0.2", "-0.1", "0"))+
  geom_path(data = state_boundaries, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
  coord_fixed(1.3)+
  theme_minimal()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title =element_text(size=52),
        axis.text=element_text(size=52),
        plot.title = element_text(size = 50),  
        legend.title = element_text(size = 52),  
        legend.text = element_text(size = 50),
        legend.key.height = unit(1, "cm")
  )+
  labs(title = "Change in fraction of GBR water use",
       fill="Iteration 10 - Iteration 0
       ",
       x = "Longitude",
       y="Latitude")
print(p)
dev.off()
