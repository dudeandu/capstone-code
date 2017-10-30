setwd("C:/Users/dudea/Desktop/Ryerson/CKME 136 Capstone Project/data analisys surface/fireFighters")

file_list <- list.files()
file_list

safety <- read.csv("WB-Safety.csv" , header = TRUE)
housing <- read.csv("WB-Housing.csv" , header = TRUE)
economics <- read.csv("WB-Economics.csv" , header = TRUE)
environment<- read.csv("WB-Environment.csv" , header = TRUE)
demographics <- read.csv("WB-Economics.csv" , header = TRUE)
culture <- read.csv("WB-Culture.csv" , header = TRUE)
pop_change <- read.csv("Wellbeing_TO_2016 Census_Total Pop_Total Change.csv" , header = TRUE)

head(safety)
head(housing)
head(economics)
head(environment)
head(demographics)
head(culture)
head(pop_change)

sapply(safety, class)
sapply(housing, class)
sapply(economics, class)
sapply(environment, class)
sapply(demographics, class)
sapply(culture, class)
sapply(pop_change, class)

summary(safety)
summary(housing)
summary(economics)
summary(environment)
summary(demographics)
summary(culture)
summary(pop_change)
pop_names <- list("Neighbourhood.Id","Neighbourhood","Pop2016","Pop2011","PopChg11t16")
names(pop_change)<- pop_names
as.numeric(gsub(",","",as.character(pop_change$Pop2011)))

safety[,c(1,5)]

safety$Neighbourhood
boxplot(safety$Fire.Vehicle.Incidents)
hist(safety$Fire.Vehicle.Incidents)
hist(safety$Fires...Fire.Alarms)

##### Merged all data frames
neighborhood_merged <- merge(safety,housing, by = c("Neighbourhood", "Neighbourhood.Id"))
neighborhood_merged <- merge(neighborhood_merged,economics, by = c("Neighbourhood", "Neighbourhood.Id"))
neighborhood_merged <- merge(neighborhood_merged,environment, by = c("Neighbourhood", "Neighbourhood.Id"))
neighborhood_merged <- merge(neighborhood_merged,demographics, by = c("Neighbourhood", "Neighbourhood.Id"))
neighborhood_merged <- merge(neighborhood_merged,culture, by = c("Neighbourhood", "Neighbourhood.Id"))
neighborhood_merged <- merge(neighborhood_merged,pop_change, by = c("Neighbourhood.Id"))

str(neighborhood_merged)

##### CLEANING unnecessary / repeated columns
neighborhood_merged$X <- NULL
neighborhood_merged$Neighbourhood.y <- NULL
neighborhood_merged$Social.Assistance.Recipients.y <- NULL
neighborhood_merged$Home.Prices.y <- NULL
neighborhood_merged$Home.Prices.x <- NULL
neighborhood_merged$Debt.Risk.Score.y <- NULL
neighborhood_merged$Local.Employment.y <- NULL
neighborhood_merged$Child.Care.Spaces.y <- NULL
neighborhood_merged$Businesses.y <- NULL

#####CLEANING Changing names to eliminate the names resulting from merging
colnames(neighborhood_merged)[2] <- "Neighborhood"
colnames(neighborhood_merged)[25] <- "Businesses"
colnames(neighborhood_merged)[26] <- "Child.Care.Spaces"
colnames(neighborhood_merged)[27] <- "Debt.Risk.Score"
colnames(neighborhood_merged)[28] <- "Local.Employment"
colnames(neighborhood_merged)[29] <- "Social.Assistance.Recipients"

##### This loop assigns a new column called class that will be used in classification
for (i in 1:nrow(neighborhood_merged)){
  if(neighborhood_merged$Fires...Fire.Alarms[i]<120){
    neighborhood_merged$class[i] <- "Low"
  } else if(neighborhood_merged$Fires...Fire.Alarms[i]>=120 && neighborhood_merged$Fires...Fire.Alarms[i]<=240){
    neighborhood_merged$class[i] <- "Medium"
  } else if(neighborhood_merged$Fires...Fire.Alarms[i]>240){
    neighborhood_merged$class[i] <- "High"
  } 
}

##### turns the characters into factor
neighborhood_merged$class = factor(neighborhood_merged$class, levels = c("Low","Medium","High"))
levels(neighborhood_merged$class)

##### Writes a csv file for use outside R
write.csv(neighborhood_merged, file = "neighborhood_merged.csv", sep = ",", col.names = TRUE )
