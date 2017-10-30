install.packages("XML")
library(XML)

setwd("C:/Users/dudea/Desktop/Ryerson/CKME 136 Capstone Project/data analisys surface/fireFighters/data/TFS_OPEN_DATA-2011-2016")

file_list <- list.files()
file_list

###fileURL <- "http://www.w3schools.com/xml/simple.xml"
###doc <- xmlTreeParse(fileURL,useInternal=TRUE)
###xmlName(rootNode)


## doc = doc
## top = rootNode
doc <- xmlTreeParse(file_list[7],useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
length(names(rootNode))

## Gets the name for all the nodes from the first incident
names(rootNode[[1]])
incident_attr <- names(rootNode[[1]])
incident_attr[[1]]

## give the names of all the nodes in rootNode
testa = rootNode[["INCIDENT"]]
length(names(testa))

## gets the contents of all the names 
xmlSApply(testa, xmlValue)

xmlValue(testa[["DISPATCH_DATE"]])
as.POSIXct(xmlValue(testa[["DISPATCH_DATE"]]))
xmlValue(testa[["ARRIVE_DATE"]])
as.POSIXct(xmlValue(testa[["ARRIVE_DATE"]]))

as.POSIXct(xmlValue(testa[["ARRIVE_DATE"]])) - as.POSIXct(xmlValue(testa[["DISPATCH_DATE"]]))


#### making a data frame with incidents civ injury and fatality, firefighter injury and fatality
civ_injury = as.numeric(as.character(xpathSApply(rootNode,"//CIVILIAN_FIRE_INJURY", xmlValue)))
civ_fatality = as.numeric(as.character(xpathSApply(rootNode,"//CIVILIAN_FIRE_FATALITY", xmlValue)))
ff_injury = as.numeric(as.character(xpathSApply(rootNode,"//FF_INJURIES",xmlValue)))
ff_fatality = as.numeric(as.character(xpathSApply(rootNode,"//FF_FATALITIES",xmlValue)))
incident = xpathSApply(rootNode,"//INCIDENT_NUMBER",xmlValue)


injuries = data.frame(cbind(incident,civ_injury,civ_fatality,ff_injury,ff_fatality))
rm(incident,civ_injury,civ_fatality,ff_injury,ff_fatality)

#### making a data frame with incidents and arrival/dispatch times
arrivals = xpathSApply(rootNode,"//ARRIVE_DATE", xmlValue)
dispatches = xpathSApply(rootNode,"//DISPATCH_DATE", xmlValue)
incident = xpathSApply(rootNode,"//INCIDENT_NUMBER",xmlValue)

incident_times = data.frame(cbind(incident,dispatches,arrivals))
rm(incident,dispatches,arrivals)

######## makes a data frame of both incident times, and injuries
merge_test = cbind(incident_times,injuries[,2:5])


######## modified ############################################
######## do not merge to xml data after this
incident_times$incident <- as.character(incident_times$incident)
incident_times$dispatches <- as.character(incident_times$dispatches)
incident_times$arrivals <- as.character(incident_times$arrivals)

merge_test$incident <- as.character(merge_test$incident)
merge_test$dispatches <- as.character(merge_test$dispatches)
merge_test$arrivals <- as.character(merge_test$arrivals)

## transform all "" instances to NA
incident_times$arrivals[which(incident_times$arrivals == "")] <- NA
incident_times$arrivals[which(incident_times$dispatches == "")] <- NA

merge_test$arrivals[which(merge_test$arrivals == "")] <- NA
merge_test$arrivals[which(merge_test$dispatches == "")] <- NA

## remove all columns with NAs
incident_times <- incident_times[which(complete.cases(incident_times)),]
incident_times$dispatches <- as.POSIXct(incident_times$dispatches)
incident_times$arrivals <- as.POSIXct(incident_times$arrivals)

merge_test <- merge_test[which(complete.cases(merge_test)),]
merge_test$dispatches <- as.POSIXct(merge_test$dispatches)
merge_test$arrivals <- as.POSIXct(merge_test$arrivals)

##### adds a new column with arrivals minus dipatch times
incident_times$difference = incident_times$arrivals - incident_times$dispatches

merge_test$difference = merge_test$arrivals - merge_test$dispatches

##### too many outliers
summary(as.numeric(incident_times$difference))
boxplot(as.numeric(incident_times$difference))

summary(as.numeric(merge_test$difference))
boxplot(as.numeric(merge_test$difference))

### remove outliers and false instances 
incident_times$difference[which(incident_times$difference <= 0)] <- NA
incident_times$difference[which(incident_times$difference >= 15000)] <- NA
incident_times <- incident_times[which(complete.cases(incident_times)),]

merge_test$difference[which(merge_test$difference <= 0)] <- NA
merge_test$difference[which(merge_test$difference >= 2000)] <- NA
merge_test$difference[which(merge_test$difference <= 60)] <- NA
merge_test <- merge_test[which(complete.cases(merge_test)),]

summary(as.numeric(merge_test$difference))
boxplot(as.numeric(merge_test$difference))
hist(as.numeric(merge_test$difference))

###################creating a logical column for injury 
#### 0 = NO injury
#### 1 = yes injury
merge_test$civ_injury <- as.numeric(as.character(merge_test$civ_injury))
merge_test$civ_fatality <- as.numeric(as.character(merge_test$civ_fatality))
merge_test$ff_injury <- as.numeric(as.character(merge_test$ff_injury))
merge_test$ff_fatality <- as.numeric(as.character(merge_test$ff_fatality))

#####not working yet!!!!!!!!!!!!!!!!!
injurycheck.function <- function(x){
  injurylist <- numeric()
  for (i in 1:nrow(x)){
    if(x[i,"civ_injury"] >= 1){
      injurylist <- c(injurylist, 1)
    } else if(x[i,"civ_fatality"] >= 1){
      injurylist <- c(injurylist, 1)
    } else if(x[i,"ff_injury"] >= 1){
      injurylist <- c(injurylist, 1)
    } else if(x[i,"ff_fatality"] >= 1){
      injurylist <- c(injurylist, 1)
    } else {
      injurylist <- c(injurylist, 0)
    }
  }
  injurylist
}

merge_test$injury <- injurycheck.function(merge_test)

############################## THIS IS THE dispatch/arrival difference DATAFRAME ##
write.csv(incident_times, file = "incident_times.csv", sep = ",", col.names = TRUE )
#######################################################

###### REGRESSion test for time and injury
g=glm(injury~as.numeric(difference), family = binomial,merge_test)
curve(predict(g,data.frame(difference=x),type="resp"),add=TRUE)



length(which(incident_times$arrivals == "")) + length(which(incident_times$dispatches == ""))

length(complete.cases(incident_times)) == length(incident)

arrivals[1] <- as.POSIXct(arrivals[1])

nodes = getNodeSet(rootNode,"//INCIDENT")
nodes = getNodeSet(rootNode,"//INCIDENT/INCIDENT_NUMBER")
nodes

xpathSApply(rootNode,"//INCIDENT",xmlValue)
xpathSApply(rootNode,"//EST_LOSS", xmlValue) ###### <- yeah!!!
xpathSApply(rootNode,"//EST_VALUE_AT_RISK", xmlValue)
xpathSApply(rootNode,"//INSURANCE_ESTIMATE", xmlValue)
xpathSApply(rootNode,"//TOTAL_NUM_PERSONNEL",xmlValue)
xpathSApply(rootNode,"//INCIDENT_NUMBER",xmlValue)
xpathSApply(rootNode,"//FF_INJURIES",xmlValue)
xpathSApply(rootNode,"//FF_FATALITIES",xmlValue)
xpathSApply(rootNode,"//OBJECT_OR_MATERIAL_FIRST_IGNITED",xmlValue)
xpathSApply(rootNode,"//AGE_OF_STRUCTURE",xmlValue)
xpathSApply(rootNode,"//EVENT_TYPE",xmlValue)
xpathSApply(rootNode,"node[@OBJECT_OR_MATERIAL_FIRST_IGNITED='55']",xmlValue)

inc_number <- xpathSApply(rootNode,"//INCIDENT_NUMBER",xmlValue)

material_ign <- xpathSApply(rootNode,"//OBJECT_OR_MATERIAL_FIRST_IGNITED",xmlValue)
material_ign <- as.numeric(material_ign)

xmlValue(doc[["//INCIDENT"]])

data.frame(cbind(inc_number,material_ign))

## maximum number of personnel deployed to incident
max(as.integer(xpathSApply(rootNode,"//TOTAL_NUM_PERSONNEL",xmlValue)))

## maximum number of injuries on one incident
max(as.numeric(xpathSApply(rootNode,"//FF_INJURIES",xmlValue)))

###### ESTIMATED LOSS from ???
sum(as.numeric(xpathSApply(rootNode,"//EST_LOSS", xmlValue)))
