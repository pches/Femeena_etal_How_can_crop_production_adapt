##########################
# DREM Production Figure
#########################

library(dplyr)

ag_bar_chart <- read.csv("DREM_figure_data.csv", header = T)
#changing the order of the rows because it matters for the order of the bars in the bar chart
ag_bar_chart_reordered <- ag_bar_chart %>% slice(3,4,1,2,5,6)


california_data=ag_bar_chart_reordered[['California']]
southwest_data=ag_bar_chart_reordered[['Southwest']]
mountains_data=ag_bar_chart_reordered[['Mountains']]
pacific_data=ag_bar_chart_reordered[['PNW']]

#create the input vectors
colors = c("#000000","#E69F00","#56B4E9","#009E73")
categories = c("Southwest","California","Mountains","PNW")
#this will combine the data vectors, and you have to include "byrow=true" because it'll keep the data in order
stackeddata <- matrix(c(southwest_data,california_data,mountains_data,pacific_data), ncol=6,byrow=TRUE)

custom_space <- c(0.4, 0.4, 1.2, 0.4, 1.2, 0.4)

png(file="drem_production_barchart.png")
par(mar = c(5,4,4,2) + 0.5)
barplot(stackeddata,beside = FALSE, space = custom_space,
        main="Crop Production",
#        sub = "Column 1: Reference, Column 2: Water Shock",
        ylab="Billion US$",col=colors,ylim=c(0,40))
#Add legend to the chart
legend("bottom", inset = c(0,-0.2), legend = categories,horiz = TRUE, xpd = TRUE, cex=1,fill=colors, bty = "n", x.intersp=0.5)

text(x=5,y=42, #location
     label='Column 1: Reference, Column 2: Water Shock',col='black',
     cex=1, #size
     xpd = TRUE
)

text(x=1.55,y=-1.5, #location
     label='vegetables & fruit',col='black',
     cex=1.2, #size
     xpd = TRUE
)

text(x=5.25,y=-1.5, #location
     label='grain',col='black',
     cex=1.2, #size
     xpd = TRUE
)

text(x=8.8,y=-1.5, #location
     label='fodder',col='black',
     cex=1.2, #size
     xpd = TRUE
)

text(x=2.27,y=37, #location
     label='(+5%)',col='black',
     cex=1, #size
     xpd = TRUE
)

text(x=5.87,y=6.5, #location
     label='(+0.2%)',col='black',
     cex=1, #size
     xpd = TRUE
)

text(x=9.45,y=20.5, #location
     label='(+2%)',col='black',
     cex=1, #size
     xpd = TRUE
)

text(x=8,y=28, #location
     label='(+N.n%) is the change in national
prices relative to the reference.',
     col='black',
     cex=0.75, #size
     xpd = TRUE
)

dev.off()
