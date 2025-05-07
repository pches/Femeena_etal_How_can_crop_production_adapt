#This script by created as part of the PCHES (Program on Coupled Human and Earth Systems) project
#studying the impact of restrictions of extracting groundwater beyond recharge on crop production in the US West
#The project is funded by the Department of Energy

#This script was developed during the final stages of the project (2024-2025)
#Last updated in March 2025

# This script is used to plot fraction of maximum yield versus groundwater within recharge (GWR) fraction of full irrigation for multiple states in the US West, as provided in Figure 3 of the submitted manuscript 
# All the input files needed to run this script and output files generated are listed in the MetaData file


# Load necessary libraries
# Install the following packages if not already installed using  install.packages("packagename")
library(ggplot2)

# Your dataset
data <- read.csv("data/Yield_irr_deficit_figure_data.csv")
select_states <- c("Arizona", "California", "New Mexico", "Nevada", "Washington")



# Split the data into iteration 0 and iteration 10
data_iter_0 <- data[data$Iteration == 0, ]
data_iter_10 <- data[data$Iteration == 10, ]

#filter by states
data_iter_0 <- subset(data_iter_0, State %in% select_states)
data_iter_10 <- subset(data_iter_10, State %in% select_states)


# Create a custom shape mapping for filled symbols
# Create a custom shape mapping for both iterations
shape_mapping <- c(
  "Grain" = 17,        
  "Vegfruit" = 18,     
  "FodderCrops" = 16
)

# Create the plot
png("figures/Yielddef_irrdef_plot.png",
    height = 3000, width = 3500, res = 300, type = "cairo")

p <- ggplot() +
  # Points for iteration 0 (white filled with black outline)
  geom_point(data = data_iter_0, 
             aes(x = IrrigationDeficit, y = YieldDeficit, color = State, shape = Crop), 
             size = 4, fill = "white", color = "black", shape = 21) +  
  # Points for iteration 10 (filled based on shape mapping)
  geom_point(data = data_iter_10, 
             aes(x = IrrigationDeficit, y = YieldDeficit, color = State, shape = Crop), 
             size = 4) +  
  # Arrows from iteration 0 to 10
  geom_segment(data = merge(data_iter_0, data_iter_10, by = c("State", "Crop")),
               aes(x = IrrigationDeficit.x, y = YieldDeficit.x,
                   xend = IrrigationDeficit.y, yend = YieldDeficit.y, color = State),
               arrow = arrow(length = unit(0.05, "inches")), size = 0.5) +  
  geom_point(aes(x = 1, y = 1), size = 8, shape = 19, color = "black", alpha=0.5) +  # Add point at (1, 1)
  annotate("text", x = 1, y = 1, label = "Maximum yield, Full irrigation", 
           hjust = 1.33, vjust = 0.1, size = 4, color = "black") +  # Add annotation
  # Labels and themes
  geom_segment(aes(x = 0.97, y = 1, xend = 0.99, yend = 1),  # Arrow from annotation to the point
               arrow = arrow(length = unit(0.05, "inches")), size = 0.8) +  
  labs(title = "",
       x = "Fraction of full irrigation",
       y = "Fraction of maximum yield") +
  theme_minimal() +
  scale_shape_manual(values = shape_mapping) +  # Assign shapes based on custom mapping for both iterations
  scale_x_continuous(limits = c(0.5, 1)) +  # Set x-axis limits from 1 to 0.5
  scale_y_continuous(limits = c(0.6, 1)) + # Set y-axis limits from 0.6 to 1
  theme_bw()+
  theme(
    axis.title = element_text(size = 14),   # Increase axis title size
    axis.text = element_text(size = 12),    # Increase axis text size
    legend.title = element_text(size = 14), # Increase legend title size
    legend.text = element_text(size = 12),  # Increase legend text size
    plot.title = element_text(size = 16)    # Increase plot title size
  )
print(p)
dev.off()