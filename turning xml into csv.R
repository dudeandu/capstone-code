## install.packages("XML")
require(XML)

setwd("C:/Users/dudea/Desktop/Ryerson/CKME 136 Capstone Project/data analisys surface/fireFighters/data/TFS_OPEN_DATA-2011-2016")

file_list <- list.files()
file_list

### reading 2011
doc <- xmlTreeParse(file_list[3],useInternal=TRUE)
rootNode <- xmlRoot(doc)

### INCIDENTS total
xmlName(rootNode)
names(rootNode["INCIDENT"])
length(names(rootNode))

### incident Attributes
names(rootNode[[1]])
incident_attr <- names(rootNode[[1]])
length(incident_attr)
incident_attr[[101]]

### this for loop extracts all the attribute names as character
for(i in 1:length(incident_attr)) {
  print(incident_attr[[i]])
}

#### loop creates a list of names for every attribute, in the correct order
att_names <- NULL
for(i in 1:length(incident_attr)) {
  att_names <- c(att_names,incident_attr[[i]])
  print(incident_attr[[i]])
}

length(att_names)

### this variable creates a sequeence to identifiy the rownumber/incidentnumber
datrows <- seq(1,length(names(rootNode)))

##### this works / comented to test the next loop
# tor_incident_attr <- data.frame(CASE_NUMBER = datrows)
# for(i in 1:length(incident_attr)) {
#   columni <- xpathSApply(rootNode,paste("//", incident_attr[[i]], sep = ""),xmlValue)
#   print(class(columni))
#   # print(length(columni))
#   tor_incident_attr <- cbind(tor_incident_attr,columni)
#   print(i)
# }

### This loop creates a data frame and converts empty characters ("") to NA
tor_incident_attr <- data.frame(CASE_NUMBER = datrows)
for(i in 1:length(incident_attr)) {
  columni <- xpathSApply(rootNode,paste("//", incident_attr[[i]], sep = ""),xmlValue)
  print(class(columni))
  columni[which(columni == "")] <- NA
  # print(length(columni))
  tor_incident_attr <- cbind(tor_incident_attr,columni)
  print(i)
}

names(tor_incident_attr) <- c("row_number", att_names[1:length(incident_attr)-1]) ###list of attribute names here
names(tor_incident_attr)

##### creating an injury column for cases where there is firefighter or civilian injury / death
tor_incident_attr$FF_INJURIES <- as.numeric(as.character(tor_incident_attr$FF_INJURIES))
tor_incident_attr$FF_FATALITIES <- as.numeric(as.character(tor_incident_attr$FF_FATALITIES))
tor_incident_attr$CIVILIAN_FIRE_INJURY <- as.numeric(as.character(tor_incident_attr$CIVILIAN_FIRE_INJURY))
tor_incident_attr$CIVILIAN_FIRE_FATALITY <- as.numeric(as.character(tor_incident_attr$CIVILIAN_FIRE_FATALITY))
  
tor_incident_attr$INJURIES <- ifelse(
  tor_incident_attr$FF_INJURIES > 0 | tor_incident_attr$FF_FATALITIES > 0 | 
    tor_incident_attr$CIVILIAN_FIRE_INJURY > 0 |
    tor_incident_attr$CIVILIAN_FIRE_FATALITY > 0, 1, 0) 

sum(tor_incident_attr$INJURIES)

#### test of attribute names and values
head(tor_incident_attr)

###exports data frame to a csv file
write.csv(tor_incident_attr, file = "fire_incidents_2016.csv", sep = ",", col.names = TRUE )

######################## INDIVIDUAL INCIDENTS / INTERESTING INFO

sapply(tor_incident_attr,class)

table(tor_incident_attr$EVENT_TYPE)
table(tor_incident_attr$BLD_HEIGHT)
table(tor_incident_attr$AGE_OF_STRUCTURE)
xpathSApply(rootNode,"//INCIDENT",xmlValue)
xpathSApply(rootNode,"rootNode[@OBJECT_OR_MATERIAL_FIRST_IGNITED='55']",xmlValue)
xpathSApply(rootNode,"//EST_LOSS", xmlValue) ###### <- yeah!!!
sum(as.numeric(xpathSApply(rootNode,"//EST_LOSS", xmlValue))) #<<< est. loss to fire 

## maximum number of personnel deployed to incident
max(as.integer(xpathSApply(rootNode,"//TOTAL_NUM_PERSONNEL",xmlValue)))

## maximum number of injuries on one incident
max(as.numeric(xpathSApply(rootNode,"//FF_INJURIES",xmlValue)))

###### ESTIMATED LOSS from ???
sum(as.numeric(xpathSApply(rootNode,"//EST_LOSS", xmlValue)))
