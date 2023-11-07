
#####################################################################
#1 Date:	(DD/MM/YYYY)
#2 Time: 	(HH.MM.SS)
#3 CO.GT. : True hourly averaged concentration CO in mg/m^3  (reference analyzer)
#4 PT08.S1.CO.: PT08.S1 (tin oxide) hourly averaged sensor response (nominally  CO targeted)	
#5 NMHC.GT. : True hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer)
#6 C6H6.GT. : True hourly averaged Benzene concentration  in microg/m^3 (reference analyzer)
#7 PT08.S2.NMHC.: PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted)	
#8 NOx.GT. : True hourly averaged NOx concentration  in ppb (reference analyzer)
#9 PT08.S3.NOx. : PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted) 
#10  NO2.GT.: True hourly averaged NO2 concentration in microg/m^3 (reference analyzer)	
#11 PT08.S4.NO2.: PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted)	
#12 PT08.S5.O3.: PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted)
#13 T: Temperature in Â°C	
#14 RH: Relative Humidity (%) 	
#15 AH Absolute Humidity
#16 X
#17 X.1
#####################################################################

setwd("C:/courses/Air-Quality-Project")
air.quality.data <- read.csv("AirQualityUCI.csv",sep = ";")
air.quality.data <- data.frame(air.quality.data[1:15])

str(air.quality.data) #There are 9471 observations

########################################
# Reformat the data:
########################################

#convert Date column to date class:
air.quality.data$Date <- as.Date(air.quality.data$Date, format = "%d/%m/%Y")

#convert Time column to num
air.quality.data$Time <- as.numeric(substr(air.quality.data$Time, 1, 2))

#convert CO.GT. to num:
air.quality.data$CO.GT. <- as.numeric(gsub(",", ".", air.quality.data$CO.GT.))

#convert C6H6.GT. to num:
air.quality.data$C6H6.GT. <- as.numeric(gsub(",", ".", air.quality.data$C6H6.GT.))

#convert T column from Char to num:
air.quality.data$T <- as.numeric(gsub(",", ".", air.quality.data$T))

#convert RH column from Char to num:
air.quality.data$RH <- as.numeric(gsub(",", ".", air.quality.data$RH))

#convert AH column from Char to num:
air.quality.data$AH <- as.numeric(gsub(",", ".", air.quality.data$AH))

###################################
# Missing data - Empty rows
###################################

number_of_missing_data <- sum(is.na(air.quality.data))
print(number_of_missing_data)

library(naniar)
library(visdat)

# Create a pattern plot
vis_dat(air.quality.data)

# Create a missing data heatmap
gg_miss_var(air.quality.data)

# Create a missing data plot
gg_miss_upset(air.quality.data)

# Assess missing data mechanisms
miss_case_summary(air.quality.data) 

# It seems clearly that these missing data are just empty rows, hence they can be excluded
air.quality.data <- na.omit(air.quality.data)

########################################
# Create a new column (Season) to classify seasons based on dates.
# It will be used to visualise the data rather than using (Date)
########################################

library(lubridate)

# Create a function to classify the season based on the date
classify_season <- function(date) {
  
  month_value <- as.numeric(month(date))
  
  if (month_value %in% c(12,1,2)){
    return("Winter")
  } else if (month_value %in% c(3,4,5)){
    return("Spring")
  } else if (month_value %in% c(6,7,8)) {
    return("Summer")
  } else if (month_value %in% c(9,10,11)){
    return("Autumn")
  } 
}

air.quality.data$Season <- sapply(air.quality.data$Date, classify_season)

########################################
#Create a new column classify different parts of the day.
# It will be used to visualise the data and for filling the missing data marked at -200
########################################

day <- function(time){
  
  if (time %in% c(1:6)) {
    return("Night")
  } else if (time %in% c(7:12)){
    return("Morning")
  } else if (time %in% c(13:18)){
    return("Afternoon")
  } else if (time %in% c(19:23,0)){
    return("Evening")
  }
}

air.quality.data$Day <- sapply(air.quality.data$Time, day)

########################################
# Categorical the data
########################################

air.quality.data$Season <- factor(air.quality.data$Season)

air.quality.data$Day <- factor(air.quality.data$Day)

air.quality.data$Time <- factor(air.quality.data$Time)

########################################

str(air.quality.data)

########################################
# Exploring sensors responses
########################################

library(ggplot2)
day_colors <- c("Night" = "#3b3561", "Morning" = "#fde74c", "Afternoon" = "#f1a66a", "Evening" = "#3891a6")

CO.GT.histo <- ggplot(air.quality.data, aes(CO.GT.)) + 
  geom_histogram(aes(fill = Day), color = "black")+ 
  scale_fill_manual(values = day_colors)
#print(CO.GT.histo)

PT08.S1.CO.histo <- ggplot(air.quality.data, aes(PT08.S1.CO.)) + 
  geom_histogram(aes(fill = Day), color = "black")+ 
  scale_fill_manual(values = day_colors)
#print(PT08.S1.CO.histo)

NMHC.GT.histo <- ggplot(air.quality.data, aes(NMHC.GT.)) + 
  geom_histogram(aes(fill = Day), color = "black") + 
  scale_fill_manual(values = day_colors)
#print(NMHC.GT.histo)

C6H6.GT.histo <- ggplot(air.quality.data, aes(C6H6.GT.)) + 
  geom_histogram(aes(fill = Day), color = "black")+ 
  scale_fill_manual(values = day_colors)
#print(C6H6.GT.histo)

PT08.S2.NMHC.histo <- ggplot(air.quality.data, aes(PT08.S2.NMHC.)) + 
  geom_histogram(aes(fill = Day), color = "black")+ 
  scale_fill_manual(values = day_colors)
#print(PT08.S2.NMHC.histo)

NOx.GT.histo <- ggplot(air.quality.data, aes(NOx.GT.)) + 
  geom_histogram(aes(fill = Day), color = "black") + 
  scale_fill_manual(values = day_colors)
#print(NOx.GT.histo)

PT08.S3.NOx.histo <- ggplot(air.quality.data, aes(PT08.S3.NOx.)) + 
  geom_histogram(aes(fill = Day), color = "black") + 
  scale_fill_manual(values = day_colors)
#print(PT08.S3.NOx.histo)

NO2.GT.histo <- ggplot(air.quality.data, aes(NO2.GT.)) + 
  geom_histogram(aes(fill = Day), color = "black")+ 
  scale_fill_manual(values = day_colors)
#print(NO2.GT.histo)

PT08.S4.NO2.histo <- ggplot(air.quality.data, aes(PT08.S4.NO2.)) + 
  geom_histogram(aes(fill = Day), color = "black")+ 
  scale_fill_manual(values = day_colors)
#print(PT08.S4.NO2.histo)

ozon.histo <- ggplot(air.quality.data, aes(PT08.S5.O3.)) + 
  geom_histogram(aes(fill = Day), color = "black") + 
  scale_fill_manual(values = day_colors)
#print(ozon.histo)

t.histo <- ggplot(air.quality.data, aes(T)) + 
  geom_histogram(aes(fill = Day), color = "black") + 
  scale_fill_manual(values = day_colors)
#print(t.histo)

rh.histo <- ggplot(air.quality.data, aes(RH)) + 
  geom_histogram(aes(fill = Day), color = "black") + 
  scale_fill_manual(values = day_colors)
#print(rh.histo)

ah.histo <- ggplot(air.quality.data, aes(AH)) + 
  geom_histogram(aes(fill = Day), color = "black") + 
  scale_fill_manual(values = day_colors)
#print(ah.histo)

library(gridExtra)
grid.arrange(CO.GT.histo, PT08.S1.CO.histo, NMHC.GT.histo, C6H6.GT.histo, PT08.S2.NMHC.histo, NOx.GT.histo, 
             PT08.S3.NOx.histo, NO2.GT.histo, PT08.S4.NO2.histo, ozon.histo, t.histo, rh.histo, ah.histo, ncol = 3)

###################################
# Missing data - the one tagged at -200 value (as mentioned in UCI repository description, and as shows in exploring the data)
# First step is to replace (-200) with NA
###################################

invalid_data <- function(variable){
  replace(variable, variable < -100 , NA)
}

air.quality.data$CO.GT. <- invalid_data(air.quality.data$CO.GT.)
air.quality.data$PT08.S1.CO. <- invalid_data(air.quality.data$PT08.S1.CO.)
air.quality.data$NMHC.GT. <- invalid_data(air.quality.data$NMHC.GT.)
air.quality.data$C6H6.GT. <- invalid_data(air.quality.data$C6H6.GT.)
air.quality.data$PT08.S2.NMHC. <- invalid_data(air.quality.data$PT08.S2.NMHC.)
air.quality.data$NOx.GT. <- invalid_data(air.quality.data$NOx.GT.)
air.quality.data$PT08.S3.NOx. <- invalid_data(air.quality.data$PT08.S3.NOx.)
air.quality.data$NO2.GT. <- invalid_data(air.quality.data$NO2.GT.)
air.quality.data$PT08.S4.NO2. <- invalid_data(air.quality.data$PT08.S4.NO2.)
air.quality.data$PT08.S5.O3. <- invalid_data(air.quality.data$PT08.S5.O3.)
air.quality.data$T <- invalid_data(air.quality.data$T)
air.quality.data$RH <- invalid_data(air.quality.data$RH)
air.quality.data$AH <- invalid_data(air.quality.data$AH)

number_of_missing_data <- sum(is.na(air.quality.data))
print(number_of_missing_data) #There are 16701 missing data. This is a lot of data, and omitting them would affect the model accuracy

# Create a missing data heatmap
gg_miss_var(air.quality.data)

###################################
# Replace missing data based on the average value at the corresponding Day
###################################

fillMissing <- function(variable, day) {
    
  out <- variable
    
  for (i in 1:length(variable)) {
    
    if (is.na(variable[i])) {
      
      if (day[i] == "Morning") {
        morning_average <- mean(variable[day == "Morning"],na.rm = TRUE)
        out[i] <- morning_average
      } else if (day[i] == "Afternoon") {
        afternoon_average <- mean(variable[day == "Afternoon"], na.rm = TRUE)
        out[i] <- afternoon_average
      } else if (day[i] == "Evening") {
        evening_average <- mean(variable[day == "Evening"], na.rm = TRUE)
        out[i] <- evening_average
      } else if (day[i] == "Night") {
        night_average <- mean(variable[day == "Night"], na.rm = TRUE)
        out[i] <- night_average
      }
    }else{
      out[i]<-variable[i]
    }
  }
  return(out)
}

air.quality.data$CO.GT. <- fillMissing(air.quality.data$CO.GT., air.quality.data$Day)
air.quality.data$PT08.S1.CO. <- fillMissing(air.quality.data$PT08.S1.CO., air.quality.data$Day)
air.quality.data$NMHC.GT. <- fillMissing(air.quality.data$NMHC.GT., air.quality.data$Day)
air.quality.data$C6H6.GT. <- fillMissing(air.quality.data$C6H6.GT., air.quality.data$Day)
air.quality.data$PT08.S2.NMHC. <- fillMissing(air.quality.data$PT08.S2.NMHC., air.quality.data$Day)
air.quality.data$NOx.GT. <- fillMissing(air.quality.data$NOx.GT., air.quality.data$Day)
air.quality.data$PT08.S3.NOx. <- fillMissing(air.quality.data$PT08.S3.NOx., air.quality.data$Day)
air.quality.data$NO2.GT. <- fillMissing(air.quality.data$NO2.GT., air.quality.data$Day)
air.quality.data$PT08.S4.NO2. <- fillMissing(air.quality.data$PT08.S4.NO2., air.quality.data$Day)
air.quality.data$PT08.S5.O3. <- fillMissing(air.quality.data$PT08.S5.O3., air.quality.data$Day)
air.quality.data$T <- fillMissing(air.quality.data$T, air.quality.data$Day)
air.quality.data$RH <- fillMissing(air.quality.data$RH, air.quality.data$Day)
air.quality.data$AH <- fillMissing(air.quality.data$AH, air.quality.data$Day)

number_of_missing_data <- sum(is.na(air.quality.data))
print(number_of_missing_data)

#####################################
# Check Correlation features
#####################################

num.cols <- sapply(air.quality.data, is.numeric) # to exclude categorical data

cor.data <- cor(air.quality.data[,num.cols])

print (cor.data)

library(corrplot)

# Specify the color palette for the correlation plot
palette <- colorRampPalette(c("#F3F0CA", "#3876BF", "#192655"))(50)

# Create the correlation plot
corrplot(cor.data, 
         method="circle", 
         type="upper", 
         col=palette, 
         tl.col="#192655", 
         tl.srt=45, 
         p.mat = cor.data, 
         sig.level = 0.9999, 
         insig = "blank")

# Add a title to the correlation plot
mtext("Correlation Plot of Variables", side = 3, line = 2, col = "black")

# Draw rectangular to highlight (PT08.S5.O3.)
var_index <- which(colnames(cor.data) == "PT08.S5.O3.")

rect(var_index - 0.5, var_index - 6.5, var_index + 0.5, var_index + 3.5, col = NA, border = "#E55604")
rect(var_index - 0.5, var_index - 6.5 , var_index + 3.5, var_index - 5.5, col = NA, border = "#E55604")

########################################
# Explore the data:
########################################

season_colors <- c("Spring" = "#53a548", "Summer" = "#ffd151", "Autumn" = "#d77a61", "Winter" = "#208aae")
day_colors <- c("Night" = "#3b3561", "Morning" = "#fde74c", "Afternoon" = "#f1a66a", "Evening" = "#3891a6")

ozone.theme <- theme(plot.title = element_text(hjust = 0.5), 
                     panel.background = element_rect(fill = "white"), 
                     panel.grid.major = element_line(colour = "#EEEEEE"),
                     axis.line = element_line(linewidth = 0.3, colour = "black")) 

ozone.plot1 <- ggplot(air.quality.data, aes(PT08.S1.CO., PT08.S5.O3.)) + 
  geom_point(aes(color = Season), alpha = 0.5) + 
  scale_color_manual(values = season_colors) + 
  ggtitle("Carbon monoxide (CO) vs. Ozon (O3)") + 
  ozone.theme
print(ozone.plot1)

ozone.plot2 <- ggplot(air.quality.data, aes(PT08.S3.NOx., PT08.S5.O3.)) + 
  geom_point(aes(color = Day), alpha = 0.5) +
  scale_color_manual(values = day_colors) +
  ggtitle("Nitrogen Oxides (NOx.) vs ozon (O3)") + 
  ozone.theme
print(ozone.plot2)

########################################
# Building the model:
########################################

#Train and Test Sets
library(caTools)

split.df <- sample.split(air.quality.data$PT08.S5.O3., SplitRatio = 0.7)
train <- subset(air.quality.data, split.df == TRUE)
test <- subset(air.quality.data, split.df == FALSE)
str(air.quality.data)

#Linear regression model
model <- lm(PT08.S5.O3. ~ . , train)

summary(model)

########################################
# Predictions:
########################################

predicted_ozone <- predict(model, test)

#Plotting the model vs. real data for evaluation

results <- cbind(predicted_ozone, test$PT08.S5.O3., test$PT08.S1.CO.)

colnames(results) <- c('Predicted.O3', 'Real.O3', 'PT08.S1.CO.')

results <- as.data.frame(results)

plot.model <- ggplot(results, aes(x = PT08.S1.CO.)) +
  geom_point(aes(y = Predicted.O3, color = "Predicted"), size = 1) +
  geom_point(aes(y = Real.O3, color = "Real"), size = 1) +
  scale_color_manual(values = c("Predicted" = "red", "Real" = "blue")) +
  labs(color = "O3", y = "Ozone", x = "Carbon Monoxide") +
  theme_bw()
print(plot.model)

#Evaluation
mse <- mean((results$Real.O3-results$Predicted.O3)^2)
print(mse)

# The relatively high Mean Squared Error (MSE) could be due to outliers, but a
# favorable distribution in the predicted vs. real values plot suggests that the
# model captures the data well. As such, no further investigate will be done,
# as the model's performance suffices for this project.


