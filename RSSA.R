#Package for SSA
install.packages("Rssa")

#library(lubricate)
library(Rssa)

#Package for importing excel sheet
install.packages("xlsx")
library("xlsx")

#Package for plotting
install.packages("ggplot")
library(ggplot2)

setwd("path to working folder")
data <- read.xlsx("filename.xlsx",1, header = TRUE)
date <- as.Date(data$Date)
PiezometricSeries <- data$Piezometric.Level
charttitle<- basename(getwd()) #this extracts the file name and makes it the plot title

# Piezometric deconstrction
#The window length was taken as 50% of the number of observations. 
#You can specify the window length and the decomposition method. For this, I assumed the default. 
piezo_SSA <- ssa(PiezometricSeries)

jpeg("eigenval.jpg") #save plot in the directory
plot(piezo_SSA)
dev.off()

#Reconstruction
#the trend and seasonality can be calculated from the eigenvalues plot
r1 = reconstruct(piezo_SSA, groups = list(Trend=c(1:3),  Seasonality=c(3:12)))

#Plotting with ggplot
PiezometricTrend <- r1$Trend
Seasonality <- r1$Seasonality
Residuals <- attr(r1,"residuals")
df<- data.frame(date, PiezometricSeries, PiezometricTrend, Seasonality, Residuals)
colors <- c("Piezometric Series" = "black", "Piezometric Trend" = "orange", "Seasonality" = "red", "Residuals" = "green")


jpeg("outputplot.jpg",units="cm", width=25, height=15, res=300)
ggplot(df, aes(date)) +                    # basic graphical object
  geom_line(aes(y = PiezometricSeries, color ="Piezometric Series")) +  # series plot
  geom_line(aes(y=PiezometricTrend,color="Piezometric Trend")) + # trend plot
  geom_line(aes(y=Seasonality,color = "Seasonality")) + #seasonality plot
  geom_line(aes(y=Residuals,color = "Residuals")) + #residual plot
  labs(y= "Piezometric Level(m)", x = "Year" ,colour = " ") +
  scale_color_manual(values=colors) +
  theme_update(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + #update legend and other positions
  ggtitle(charttitle) + #title for the plot
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")  #change the x scale
dev.off()

#saving the outputs
sink("summary.txt")
summary(piezo_SSA)
sink()

write.xlsx(PiezometricTrend, file = "RSSAoutput.xlsx", sheetName = "Trend", append = FALSE)
write.xlsx(Residuals, file = "RSSAoutput.xlsx", sheetName = "Residual", append = TRUE)
write.xlsx(PiezometricSeries, file = "RSSAoutput.xlsx", sheetName = "Original_Series", append = TRUE)
