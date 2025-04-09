# Femeena_etal_How_can_crop_production_adapt

## Code for: How can crop production adapt to growing groundwater restrictions in the West? by Femeena et al.

**Repository Purpose:** This repository contains the code necessary to reproduce the figures in the supplemental information document accompanying the research paper, "How can crop production adapt to growing groundwater restrictions in the West?"

**Paper Citation:** Pandara Valappil Femeena, Kathryn Daenzer, Steve Frolking, Danielle Grogan, Jeff Nucciarone, Kate Calvin, Richard B. Lammers, Karen Fisher-Vanden.

**Publication Status:** The supplemental information document and its associated paper are currently under review for publication. A full citation and link will be provided upon acceptance.

**Code Authors:** Pandara Valappil Femeena and Kathryn Daenzer

**Input Data:** The input data required to run the code in this repository is available at:

MSD-LIVE Data Repository -  
"Supporting Information - How Can Crop Production Adapt to Growing Groundwater Restrictions in the West?", authors: Daenzer, Kathryn; Femeena, Pandara Valappil; Frolking, Steve; Grogan, Danielle; Nucciarone, Jeffrey; Calvin, Kate; Lammers, Richard; Fisher-Vanden, Karen - https://doi.org/10.57931/2530930

**Contact info:** 
- Jeffrey Nucciarone, [https://orcid.org/0000-0002-1092-9142] https://orcid.org/0000-0002-1092-9142 (data curation),
- Femeena Pandara Valappil, [https://orcid.org/0000-0002-9120-8493] https://orcid.org/0000-0002-9120-8493 (code),
- Kathryn Daenzer, katie.daenzer@gmail.com (code)

## System requirements:
This code is written in standard R coding language, using R version 4.2.1 (Funny Looking Kid). No special hardware or system requirements are necessary. The R code requires the use of the following libraries for processing:

|              |              |               |
| ------------ | ------------ | ------------- |
| RColorBrewer | lattice      | rgeos         |
| data.table   | maps         | rnaturalearth |
| dplyr        | ncdf4        | sf            |
| ggplot2      | raster       | sp            |
| ggspatial    | rgdal        | tidyr         |

All processing was performed on a Red Hat Enterprise Linux 8 system but should be reproducible on a regular Windows/Mac/Linux computer.  


## Scripts:

The code provided here to reproduce the figures and tables in the paper requires the data available on MSD-Live at https://doi.org/10.57931/2530930. A description file available in this repo, in the form of a spreadsheet, lists the required input files for each script. The output file description lists each associated figure generated in the paper.

Here, we describe each dataset:

File name: script_and_file_description.xlsx

A Microsoft XL spreadsheet with a list of all scripts and their:
- required input files and  descriptions 
- listing of each output file and description 

---

### File Name: summary_by_iteration.R	

This script creates summary files for each state and crop combination and derives the values for the following variables:

- Gross irrigation water demand
- Irrigation deficit (% of gross irr not met)
- Yield deficit due to irrigation deficit
- Irrigated area
- Rainfed area
- Irrigated production (area x yield)
- Rainfed production (area x yield)

This script must be run first as these generated summary files are used in the other scripts to create plots provided in the submitted manuscript (including the supplemental information).	

Output written to folders: results/crop_state_tables

The following input files are required:

| Input File	                                                                 | Input File Description                        	          | 
| -------------------------------------------------------                      | ------------------------------------------------         |
| /data/WoCD_DNDC_CropSystem_id_name_WBM_crop_id_name.csv	                     | Provides mapping from WBM crops to DNDC crops.         	| 
|	/data/alpha_i_j_s_SprWinWht_WECC.csv                                         | Provides mapping from DNDC crops to IMPLAN/DREM crops.	| 
|	/data/wecc_merra2_full_sw_c13_2006-2015_YieldVsDefIrrig_QuadFitParams_v1.csv | DNDC-emulator quadratic curve fit parameters, relating irrigation deficit to yield deficit by crop and state. This is for all 11 Western Electricity Coordinating Council (WECC) states and all 26 DNDC crops.	| 
| /data/GCAM/GCAM_2010_reference_land.csv	                                     | GCAMland starting point for rainfed and irrigated cropland areass, created from FAO GAEZ 2010 data (units are in 2.4 thousand hectares). |
|/data/GCAM/GCAM_2015_reference_land.csv                                       | GCAMland starting point for rainfed and irrigated cropland areass, created from FAO GAEZ 2015 data (units are in 2.4 thousand hectares). |
|/data/DNDCe/IrrGross_mmYr_states_ iteration_X.csv	                           | Total crop irrigation water usage (mm/year), by crop and state, for each iteration X. |
|/data/DNDCe/UGW_mmYr_states_ iteration_X.csv	                                 | Unsustainable ground water usage (mm/year) for irrigation, by crop and state, for each iteration X. |
|/data//DNDCe/DNDCe_to_IMPLAN_yield_deficit_STATE_ iteration_X.csv	           | DNDC emulator output per iteration X for IMPLAN defined crops: values represent the proportion of maximum yield achieved.  A value of 1 = 100% of maximum yield achieved indicating no reduction due to water shortages.  All values < 1 indicate that deficit irrigation caused a reduction in yields. |
|/data/DNDCe/DNDC_yield_deficit_ iteration_X.csv	                             | DNDC emulator output per iteration X for DNDC defined crops:  crop yield deficit based on irrigation deficits. A value of 1 = 100% of maximum yield achieved indicating no reduction due to irrigation shortages.  All values < 1 indicate that deficit irrigation caused a reduction in yields. |
|/data/DNDCe/DNDC_yield_kg.C.ha_ iteration_X.csv	                             | DNDC absolute yield values in kg Carbon/hectare per iteration X. |
|/data/GCAM/DREM_land_ iteration_X.csv	                                       | GCAM calculated irrigated and rainfed land allocation (x 2.4 thousand ha) and crop yields (kg C/ha). Output is by crop and state for 2010 and 2015, with separate files created for each iteration step X. This output from GCAM is what is sent to DREM for each iteration. | 
	
This script produces the following output:

| Output File	                                            | Output File Description                        	          | 
| --------------------------------------------------------- | -----------------------------------------------------------|
| /results/gross_irr_ long.csv	                            | Gross irrigation water (km^3/yr) |
| /results/ugw_km3_long.csv	                            | Unsustainable ground water (km^3/yr) |
| /results/sust_irr_km3_ long.csv	                    | Sustainable irrigation water (km^3/yr) |
| /results/sust_irr_frac_ long.csv	                    | Fraction of irrigation water sourced from sustainable groundwater |
| /results/unsust_irr_frac_ long.csv	                    | Fraction of irrigation water sourced from unsustainable groundwater |
| /results/deficit_yld_ long.csv	                    | Yield deficit or fraction of maximum yield |
| /results/irr_land_area_ long.csv	                    | Irrigated land area (x 2.4 thousand hectares)Note: GCAM gives land area outputs in units that are different from other models. A factor of 2.4 is used to convert them to thousand hectares and report it in the final summary tables inside the ""crop_state_tables"" folder" |
| /results/rfd_land_area_ long.csv	                    | Rainfed land area (x 2.4 thousand hectares)  Note: GCAM gives land area outputs in units that are different from other models. A factor of 2.4 is used to convert them to thousand hectares and report it in the final summary tables inside the ""crop_state_tables"" folder" |
| /results/yield_max_matrix.csv	                            | Matrix of max irrigated yields (kg C/ha), crop x state |
| /results/max_yield_kg.C.ha.csv	                    | Maximum yield in kg Carbon/hectare (kgC/ha) |
| /results/sust_yld_kg.C.ha_long.csv	                    | Yield corresponding to susytainable irrigation water in kgC/ha |
| /results/unsust_yld_kg.C.ha_ long.csv	                    | Yield corresponding to unsustainable irrigation water in kgC/ha |
| /results/crop_state_tables/[state]_[crop]_all_models.csv  | Per iteration output for all crops for all models |

---

### File Name: irr_def_map.R	

This script creates irrigation deficit maps that are provided in the submitted manuscript (including the supplemental information).

The maps show irrigation deficit at the end of iteration runs for different states in the US West for six crop categories

The script also creates irrigation v/s yield deficit maps for the three major crop categories (grain, vegetables and fruits, and fodder crops) and an irrigation deficit map for all the crops combined. 

Creates output folder: figures/irr_def_maps_by_crop

The following input files are required:

| Input File	                                                                 | Input File Description                        	          | 
| --------------------------------------------------------------------           | ------------------------------------------------         |
| /data/cb_2015_us_state_500k/ cb_2015_us_state_500k (state shapefile)	         | An OGR data source obtained from the US Census Bureau (https://www2.census.gov/geo/tiger/GENZ2015/shp/). This data is used to load a spatial vector object of US states into R map plots. |
| /data/WoCD_DNDC_CropSystem_id_name_WBM_crop_id_name.csv	                 | Provides mapping from WBM crops to DNDC crops. |
| /data/alpha_i_j_s_SprWinWht_WECC.csv	                                         | Provides mapping from DNDC crops to IMPLAN/DREM crops. |
| /data/DNDCe/IrrGross_mmYr_states_iteration_1.csv	                         | Total crop irrigation water usage (mm/year), by crop and state, for iteration 1. |
| /data/DNDCe/IrrGross_mmYr_states_iteration_10.csv 	                         | Total crop irrigation water usage (mm/year), by crop and state, for iteration 10. |
| /data/DNDCe/UGW_mmYr_states_iteration_1.csv	                                 | Unsustainable ground water usage (mm/year) for irrigation, by crop and state, for iteration 1. |
| /data/DNDCe/UGW_mmYr_states_iteration_10.csv	                                 | Unsustainable ground water usage (mm/year) for irrigation, by crop and state, for iteration 10. |
| /data/DNDCe/DNDCe_to_IMPLAN_yield_deficit_STATE_iteration_1.csv	         | DNDC emulator output  for IMPLAN defined crops, Itration 1: values represent the proportion of maximum yield achieved.  A value of 1 = 100% of maximum yield achieved indicating no reduction due to water shortages.  All values < 1 indicate that deficit irrigation caused a reduction in yields. |
| /data/DNDCe/DNDCe_to_IMPLAN_yield_deficit_STATE_iteration_10.csv	         | DNDC emulator output  for IMPLAN defined crops, Itration 10: values represent the proportion of maximum yield achieved.  A value of 1 = 100% of maximum yield achieved indicating no reduction due to water shortages.  All values < 1 indicate that deficit irrigation caused a reduction in yields. |

This script produces the following output:

| Output File	                                            | Output File Description                        	          | 
| --------------------------------------------------------- | -----------------------------------------------------------|
| /figures/ irr_def_maps_by_crop /[crop]_irr_def_map.png (for each of the six  crop categories)	| Multiple Plots, used to produce: Figure S2, Bottom image- Optimal Deficit Irrigation strategy by crop category |
| figures/Irr_def_map_allCrops_i10.png	                    | Figure S2, Top image - Optimal deficit irrigation strategy (all crops) |
| figures/Yld_vs_IrrDef_i10.png	                            | Plot - Yield deficit v/s Irrigation deficit (all crops) Note: Not used in the manuscript and replaced by Figure 3 |
| figures/Yld_vs_IrrDef_grain_i10.png	                    | Figure S4 - Yield deficit v/s Irrigation deficit (grain) |
| figures/Yld_vs_IrrDef_vegfruit_i10.png	            | Figure S4 - Yield deficit v/s Irrigation deficit (vegetables and fruits) |
| figures/Yld_vs_IrrDef_foddercrops_i10.png	            | Figure S4 - Yield deficit v/s Irrigation deficit (fodder crops) |

---

### File name: gcamland_plots.R	

This script creates plots for the GCAM change in land. 	

Creates output folder: figures/gcam_delta_land

The following input files are required:

| Input File	                                                        | Input File Description                        	          | 
| --------------------------------------------------------------------  | ------------------------------------------------         |
| /data/cb_2015_us_state_500k/ cb_2015_us_state_500k (state shapefile)  | An OGR data source obtained from the US Census Bureau (https://www2.census.gov/geo/tiger/GENZ2015/shp/). This data is used to load a spatial vector object of US states into R map plots. |
| /data/gcamland/DREM_land_iteration_X.csv	                        | GCAM calculated irrigated and rainfed land allocation (x 2.4 thousand ha) and crop yields (kg C/ha). Output is by crop and state for 2010 and 2015, with separate files created for each iteration step X. This output from GCAM is what is sent to DREM for each iteration. |
| /data/GCAM/pches_output_Iter1.csv	                                | GCAM output sent to WBM, change in irrigated and rainfed cropland area, results for Iteration 1. |


This script produces the following output:

| Output File	                                                                 | Output File Description                        	          | 
| ------------------------------------------------------------------------------ | -----------------------------------------------------------|
| /figures/gcam_delta_land/Irr_[crop]_iter1.png (for all six crop categories)	 | Change in irrigated land areas at the end of iteration 1 for each crop category Note: Not used in the manuscript |
| /figures/gcam_delta_land/Irr_[crop]_iter10.png (for all six crop categories)	 | Change in irrigated land areas at the end of iteration 10 for each crop category Figure 4, Top plots and Figure S6, Top plots - Depicted for three major crop categories (grain, vegetables and fruits, and fodder crops) |
| /figures/gcam_delta_land/Irr_total_iter1.png	                                 | Change in total irrigated land area for all crops after iteration 1. Change in irrigated land areas at the end of iteration 1 for each crop category.  Note: Not used in the manuscript |
| /figures/gcam_delta_land/Irr_total_iter10.png	                                 | Figure S5, Left image - Change in total irrigated land area for all crops after iteration 10 |
/figures/gcam_delta_land/Rfd_[crop]_iter1.png (for all six crop categories)	 | Change in rainfed land areas at the end of iteration 1 for each crop category Change in irrigated land areas at the end of iteration 1 for each crop category Note: Not used in the manuscript |
| /figures/gcam_delta_land/Rfd_[crop]_iter10.png (for all six crop categories)	 | Change in rainfed land areas at the end of iteration 10 for each crop category Figure 4, Bottom plots and Figure S6, Bottom plots - Depicted for three major crop categories (grain, vegetables and fruits, and fodder crops) |
| /figures/gcam_delta_land/Rfd_total_iter1.png	                                 | Change in total rainfed land area for all crops after iteration 1 Change in irrigated land areas at the end of iteration 1 for each crop category Note: Not used in the manuscript |
| /figures/gcam_delta_land/Rfd_total_iter10.png	                                 | Figure S5, Right image- Change in rainfed land area at the end of iteration 10 |

---

### File name: DNDCe_yield_def_figures.R	

This script creates plots of crop yield deficits.  	

Creates output folder: figures/DNDCe_yield_def

The following input files are required:

| Input File	                                                                 | Input File Description                        	          | 
| --------------------------------------------------------------------           | ------------------------------------------------         |
| /data/WBM/wbm_irrigationGross_dc_iteration_0.nc	                         | Water withdrawals for agriculture for iteration o (warmup step), daily climatology (mm). Results for iterations 0 and 10 are used in the processing scripts to estimate the change in irrigation water use between reference and optimal scenarios. |
| /data/WBM/wbm_irrigationGross_dc_iteration_10.nc	                         | Water withdrawals for agriculture for iteration 10, daily climatology (mm). Results for iterations 0 and 10 are used in the processing scripts to estimate the change in irrigation water use between reference and optimal scenarios. |
| /data/WBM/wbm_irrigationExtra_dc_iteration_0.nc	                         | Water withdrawals from unsustainable groundwater for agriculture for iteration 0 (warmup step), daily climatology (mm). Results for iterations 0 and 10 are used in the processing scripts to estimate the change in irrigation water use between reference and optimal scenarios. |
| /data/WBM/wbm_irrigationExtra_dc_iteration_10.nc	                         | Water withdrawals from unsustainable groundwater for agriculture for iteration 10, daily climatology (mm). Results for iterations 0 and 10 are used in the processing scripts to estimate the change in irrigation water use between reference and optimal scenarios. |


This script produces the following output:

| Output File	                                            | Output File Description                        	          | 
| --------------------------------------------------------- | -----------------------------------------------------------|
| /figures/WBM_irrigation_maps/Change_unsust_irrig.png	    | Change in unsustainable irrgation water request between iteration 0 and 10 for the US West Change in irrigated land areas at the end of iteration 1 for each crop category Note: Not used in the manuscript |
| /figures/WBM_irrigation_maps/Frac_unsust_irrig_iter0.png  | Figure 2a - Fraction of rrigation water coming from unssutainable ground water for reference (iteration 0) scenario |
| /figures/WBM_irrigation_maps/Frac_unsust_irrig_iter10-iter0_CA.png	| Figure 2c - Change in fraction of unsustainable ground water use for California between iteration 0 and 10 |
---

### File name: WBM_irrigation_analysis.R	

This script creates plots of grid-scale unsustainable irrigation fraction.	

Creates output folder: /figures/WBM_irrigation_maps

The following input files are required:

| Input File	                                             | Input File Description                        	          | 
| ---------------------------------------------------------- | ------------------------------------------------         |
| /data/WBM/wbm_irrigationGross_dc_iteration_0.nc	     | Water withdrawals for agriculture for iteration o (warmup step), daily climatology (mm). Results for iterations 0 and 10 are used in the processing scripts to estimate the change in irrigation water use between reference and optimal scenarios. |
| /data/WBM/wbm_irrigationGross_dc_iteration_10.nc	     | Water withdrawals for agriculture for iteration 10, daily climatology (mm). Results for iterations 0 and 10 are used in the processing scripts to estimate the change in irrigation water use between reference and optimal scenarios. |
| /data/WBM/wbm_irrigationExtra_dc_iteration_0.nc	     | Water withdrawals from unsustainable groundwater for agriculture for iteration 0 (warmup step), daily climatology (mm). Results for iterations 0 and 10 are used in the processing scripts to estimate the change in irrigation water use between reference and optimal scenarios. |
| /data/WBM/wbm_irrigationExtra_dc_iteration_10.nc	     | Water withdrawals from unsustainable groundwater for agriculture for iteration 10, daily climatology (mm). Results for iterations 0 and 10 are used in the processing scripts to estimate the change in irrigation water use between reference and optimal scenarios.  |


This script produces the following output:

| Output File	                                                      | Output File Description                        	          | 
| ------------------------------------------------------------------- | -----------------------------------------------------------|
| /figures/WBM_irrigation_maps/Change_unsust_irrig.png	              | Change in unsustainable irrgation water request between iteration 0 and 10 for the US West Change in irrigated land areas at the end of iteration 1 for each crop category Note: Not used in the manuscript |
| /figures/WBM_irrigation_maps/Frac_unsust_irrig_iter0.png	      | Figure 2a - Fraction of rrigation water coming from unssutainable ground water for reference (iteration 0) scenario |
| /figures/WBM_irrigation_maps/Frac_unsust_irrig_iter10-iter0_CA.pn   | Figure 2c - Change in fraction of unsustainable ground water use for California between iteration 0 and 10 |

---

### File name: Yield_irr_def_plot.R	

This script creates a plot of sustainable irrigation fraction versus fraction of maximum yield.	

| Input File	                             | Input File Description                        	          | 
| --------------------------------------     | ------------------------------------------------         |
data/Yield_irr_deficit_figure_data.csv       | This file was externally created in Microsoft Excel using the summary data in results/crop_state_tables. We obtained the values for each state and crop combination by taking the ratio of sustainable irrigation water and total irrigation water (= fraction of full irrigation) and sustainable yield to maximum irrigated yield (= Fraction of maximum yield). Values were derived for iterations 0 and 10 and used to create the plot. |


This script produces the following output:

| Output File	                        | Output File Description                                              | 
| ------------------------------------- | ---------------------------------------------------------------------|
| /figures/Yielddef_irrdef_plot.png     | Figure 3 - Fraction of maximum yield v/s fraction of total irrigation that is from sustainable groundwater |

---

### File name: ag_bar_chart.R	

This script creates a stacked bar chart displaying the economic value of agricuture output for three crops across the western region.

The following input files are required:

| Input File	                   | Input File Description                        	       | 
| -------------------------------- | --------------------------------------------------------- |
| data/DREM_figure_data.csv	   | This file was created using General Algebraic Modeling Software (GAMS).  software. The state-level production values are direct output from the DREM model, and they are aggregated to provide a total value for each of the western sub-regions (Southwest, Pacific Northwest, Mountain States, and California). |


This script produces the following output:

| Output File	                   | Output File Description                        	          | 
| -------------------------------- | -----------------------------------------------------------|
| drem_production_barchart.png	   | Figure 5 - Crop production in billion USD by crop and region before the water shock and after the water shock and farmer adaptation. | 
---


