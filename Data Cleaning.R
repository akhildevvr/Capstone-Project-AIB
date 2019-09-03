setwd("H:/UCD BA/Capstone")
data<-read.csv("output.csv", header = TRUE,stringsAsFactors = TRUE,na.strings = c("", "NA"))
str(data)
View(data)
names(data) <- make.names(names(data))
data$Price<-gsub("???","",data$Price)
data$Price<-gsub(",","",data$Price)
data$Price<-gsub("???","",data$Price)
data$Price<-gsub("???","", data$Price)


summary(data)
data$Date.of.Purchase <- format(as.Date(data$Date.of.Purchase, format = "%Y-%m-%d"), "%Y-%m-%d")
data$Price<-as.integer(data$Price)
data$Type<-as.factor(data$Type)
unique(data$No.of.Bedrooms)
unique(data$Number.of.Bathrooms)
data$No.of.Bedrooms<-as.factor(data$No.of.Bedrooms)
data$Number.of.Bathrooms<-as.factor(data$Number.of.Bathrooms)
#data[data$No.of.Bedrooms=="3 Bathrooms",]
datasub <- data[grep("Bathroom", data$No.of.Bedrooms), ]
data$Number.of.Bathrooms[data$X %in% datasub$X] <- datasub$No.of.Bedrooms
data$No.of.Bedrooms[!data$X %in% datasub$X]
n<-data$X[grep("Bathroom", data$No.of.Bedrooms)]
data$No.of.Bedrooms[data$X %in% n]<- NA

data$No.of.Bedrooms<-gsub("Bedrooms","",data$No.of.Bedrooms) 
data$No.of.Bedrooms<-gsub("Bedroom","",data$No.of.Bedrooms) 
data$Number.of.Bathrooms<-gsub("Bathrooms","",data$Number.of.Bathrooms) 
data$Number.of.Bathrooms<-gsub("Bathroom","",data$Number.of.Bathrooms)
data$No.of.Bedrooms<-as.factor(data$No.of.Bedrooms)
data$Number.of.Bathrooms<-as.factor(data$Number.of.Bathrooms)
data$Date.of.Purchase<- as.Date(data$Date.of.Purchase, format = "%Y-%m-%d")
data$County<-as.factor(data$County)

library(VIM)
res<-summary(aggr(data, sortVar=TRUE))$combinations
colSums(is.na(data))
list_na <- colnames(data)[ apply(data, 2, anyNA) ]
list_na

number.of.mv <-function(a){
  mv.list <- c()
  for(i in 1:ncol(a))
  {
    mv.list[i] <- sum(is.na(a[,i]))
  }
  return(mv.list)
}
number.of.mv(data)

get.mv <- function(a){
  missing.values<-cbind(colnames(a[which(number.of.mv(a)!=0)]),number.of.mv(a)[!(number.of.mv(a)==0)])
  return(missing.values)
}

get.mv(data)

data1<-data[,-2]




data1<-data1[,-c(1,3)]
#Data Imputation using MICE
library(mice)
miceMod <- mice(data1[, !names(data1) %in% "Price"], method="pmm")  # perform mice imputation, based on random forests.

miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)
actuals <- data$No.of.Bedrooms[is.na(data$No.of.Bedrooms)]
predicteds <- miceOutput[is.na(data$No.of.Bedrooms), "No.of.Bedrooms"]
mean(actuals != predicteds)




library(MASS)
library(caret)
data2<-data
data2<-data2[,-1]
data2<-data2[,-c(4,5,6,7)]
miceOutput<- read.csv("ImputedOutput.csv", header = TRUE,stringsAsFactors = TRUE ,na.strings = c("", "NA"))
data2<-cbind(data2, miceOutput)
data2<-data2[,-4]
training1 <- createDataPartition(data2$Price, p=0.60,list=FALSE)
train<-data2[training1,]
test<-data2[-training1,]

data2$Address<- str_replace(data2$Address," ",",")
data2$Address<-gsub(' ','',data2$Address) 

write.csv(data2, "output1.csv")
data<- read.csv("output1.csv", header = TRUE,stringsAsFactors = TRUE ,na.strings = c("", "NA"))
c <- ncol(train)
#Intializing the vector which will contain the p-values of all variables
pvalues <- numeric(c)
# Getting the p-values
for (i in 1:c)
{
  fit <- lm(train$Price ~ train[,i])
  summ <- summary(fit)
  pvalues[i] <- summ$coefficients[2,4]
}

library(dplyr)
library(lubridate)
data3<-data2 %>%
  select(Address, Price, Date.of.Purchase, Type, No.of.Bedrooms, Number.of.Bathrooms, County) %>%
  filter(Date.of.Purchase >= as.Date("2016-01-01"))

data4<-data2 %>%
  select(Address, Price, Date.of.Purchase, Type, No.of.Bedrooms, Number.of.Bathrooms, County) %>%
  filter(Date.of.Purchase <= as.Date("2016-01-01"))

data5<-data4
data5$Full.Address<- paste(data5$Address, sep= ",",data5$County)

rpz<-c("Dublin", "Dun Laoghaire-Rathdown", "Fingal" , "South Dublin County Council","Cork City", "Douglas", "Ballincollig", "Carrigaline", "Passage West", "Cobh")
patt <- c("1")
for (i in 1: length(patt)) {
  data5[grep(rpz[i], data5$Full.Address), "Rent Pressure Zones"] <- patt
}



library(splitstackshape)
data5<-stratified(data3, group = 7, size = 0.5)

library(FSelector)
information.gain(Price~., data =data2)




#Data from PRSA
housedata<-read.csv("PPR-ALL.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c("", "NA"))
str(housedata)
View(housedata)

#Data cleaning
names(housedata)[names(housedata) == 'Date.of.Sale..dd.mm.yyyy.'] <- 'Date.of.Purchase'
names(housedata)[names(housedata) == 'Price....'] <- 'Price'
names(housedata) <- make.names(names(housedata))
data$Price<-gsub("???","",data$Price)
housedata$Price<-gsub(",","",housedata$Price)
data$Price<-gsub("???","",data$Price)
housedata$Price<-gsub("???","", housedata$Price)
housedata$Date.of.Purchase <- format(as.Date(housedata$Date.of.Purchase, format = "%d/%m/%Y"), "%d/%m/%Y")
housedata$Price<-as.numeric(housedata$Price)
housedata$Date.of.Purchase<- as.Date(housedata$Date.of.Purchase, format = "%d/%m/%Y")
unique(housedata$County)
sort(table(housedata$County))

#Subsetting the data based on counties
library(dplyr)
Dublin<-filter(housedata, County %in% c("Dublin"))
Cork<-filter(housedata, County %in% c("Cork"))
Galway<-filter(housedata, County %in% c("Galway"))
Kildare<-filter(housedata, County %in% c("Kildare"))
Meath<-filter(housedata, County %in% c("Meath"))
Limerick<-filter(housedata, County %in% c("Limerick"))
Wexford<-filter(housedata, County %in% c("Wexford"))
Wicklow<-filter(housedata, County %in% c("Wicklow"))

#Only data that has full market price
housedata1<-housedata[housedata$Not.Full.Market.Price == "Yes",]

#Getting the year of purchase
dateyear <- format(housedata1$Date.of.Purchase, "%Y")
housedata1$dateyear<-dateyear
table(housedata1$dateyear)
 
table(housedata1$County)

#Merging of data from daft and PRSA
newdata2<-merge(x= data, y = housedata1, by = c( "Date.of.Purchase", "County"), all = TRUE)
newdata2<-smartbind(housedata1, data)
newdata2<-newdata2[order(as.Date(newdata2$Date.of.Purchase , format="%Y-%m-%d")),]
newdata3<-newdata2[,-c(1,2,3,9,10,11,12)]

#Imputation of missing values
library(mice)
miceMod <- mice(newdata3[, !names(newdata3) %in% "Price"], method="pmm")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)
newdata4<-cbind(newdata2[ ,!names(newdata2) %in% c("County" , "Not.Full.Market.Price" ,  "VAT.Exclusive"  ,   "Description.of.Property", "No.of.Bedrooms" , "Number.of.Bathrooms"    )],miceOutput)
newdata4<-newdata4[,-c(7,8)]
str(newdata4)

newdata4$Date.of.Purchase<- as.Date(as.character(newdata4$Date.of.Purchase), format = "%Y-%m-%d")

dateyear <- format(newdata4$Date.of.Purchase, "%Y")
newdata4$dateyear<-dateyear
newdata4<-newdata4[,-c(1,2)]


#Data Cleaning 
stopwords = c("co ","Co ","Co.")
counties<-c("Carlow", "Dublin", "Wexford", "Wicklow", "Louth", "Kildare", "Meath","Westmeath", "Kilkenny", "Laois", "Offaly", "Longford",
            "Clare", "Cork", "Kerry", "Limerick", "Tipperary", "Waterford","Galway", "Leitrim", "Mayo", "Roscommon", "Sligo","Cavan", "Donegal", "Monaghan",
            "Antrim", "Armagh", "Down", "Derry", "Fermanagh", "Tyrone")
#co<-paste(newdata4$County[!grepl(counties, newdata4$County)], collapse = " ")
county<-gsub(paste0(stopwords,collapse = "|"),"", newdata4$County)
county<-gsub(paste0("counties{1}",collapse = "|"),"", county)



# create a list of vectors of place names
broken <- strsplit(county, ",\\s*")

# paste each broken vector of place names back together
# .......kicking out duplicated instances of the chosen names
county <- sapply(broken, FUN = function(x)  paste(x[!duplicated(x) | !x %in% counties ], collapse = ", "))

newdata4$County<-county
newdata6<-newdata4[sapply(gregexpr("\\w+", newdata4$County), length) <2,]

gsub("e{3,}","e", origStr )
table(newdata6$County)
newdata6$County[newdata6$County == "k"]<-"Cork"

library(splitstackshape)
newdata7<-stratified(newdata6, group = c(6,7), size = 0.5)
table(newdata7$County)
table(newdata7$dateyear, newdata7$County)

write.csv(newdata7, "output2.csv")
newdata7<-read.csv("output2.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c("", "NA"))

setwd("C:/Users/Jose Chiramel/Desktop/Python Programs")

datax<-read.csv("output_final.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c("", "NA"))

omitted<-datax[datax$status == c("ZERO_RESULTS","INVALID_REQUEST","REQUEST_DENIED" ),]

newdata7$ID<- seq.int(nrow(newdata7))
datax<-datax[,-1]
datax$ID<-seq.int(nrow(datax))
omitted<-datax[datax$status == c("ZERO_RESULTS","INVALID_REQUEST","REQUEST_DENIED" ),]
omitted$ID
newdata7<-newdata7[-omitted$ID,]
datax<-datax[-omitted$ID,]

newdata7<-cbind(newdata7, datax)

colnames(newdata7)
newdata7<-newdata7[ ,-c("accuracy"        ,          "google_place_id",          
                        "type"            ,          "postcode" ,                
                        "status"          ,          "ID"          )]
newdata7<-newdata7[ ,-c(18:23)]
newdata7<-newdata7[ ,-c(1,14)]


newdata7<- newdata7[!is.na(newdata7$latitude),]
newdata7<- newdata7[!is.na(newdata7$longitude),]

newdata7<-newdata7[newdata7$latitude > quantile(newdata7$latitude, .25) - 1.5*IQR(newdata7$latitude) & 
                     newdata7$latitude < quantile(newdata7$latitude, .75) + 1.5*IQR(newdata7$latitude), ] #rows

newdata7<-newdata7[newdata7$longitude > quantile(newdata7$longitude, .25) - 1.5*IQR(newdata7$longitude) & 
                     newdata7$longitude < quantile(newdata7$longitude, .75) + 1.5*IQR(newdata7$longitude), ] #rows

setwd("C:/Users/Jose Chiramel/Desktop/Python Programs")
write.csv(newdata7, "outputfinal.csv")
write.csv(newdata7, "outputfinal1.csv")
newdata7<-read.csv("outputfinal.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c("", "NA"))
newdata7<-read.csv("outputfinal1.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c("", "NA"))
str(newdata7)
newdata7$Date.of.Purchase<- as.Date(as.character(newdata7$Date.of.Purchase), format = "%Y-%m-%d")
newdata7<-newdata7[,-c(1,3,4,6)]

library(dplyr)
library(lubridate)
newdata8<-newdata7 %>%
  select(formatted_address, Price, Date.of.Purchase,Description.of.Property , No.of.Bedrooms, Number.of.Bathrooms, County,Not.Full.Market.Price,VAT.Exclusive,latitude,longitude) %>%
  filter(Date.of.Purchase >= as.Date("2016-01-01"))
str(newdata7)
table(newdata8$County)


newdata8<-newdata8[newdata8$Price > quantile(newdata8$Price, .25) - 1.5*IQR(newdata8$Price) & 
  newdata8$Price < quantile(newdata8$Price, .75) + 1.5*IQR(newdata8$Price), ] #rows





library(dplyr)
Dublin<-filter(newdata8, County %in% c("Dublin"))
write.csv(Dublin, "Dublinfinal1.csv")
Dublin<-Dublin[,-c(1)]

#Removing outliers
Dublin<-Dublin[Dublin$latitude > quantile(Dublin$latitude, .25) - 1.5*IQR(Dublin$latitude) & 
                 Dublin$latitude < quantile(Dublin$latitude, .75) + 1.5*IQR(Dublin$latitude), ] #rows
Dublin<- Dublin[!is.na(Dublin$latitude),]
Dublin<-Dublin[Dublin$longitude > quantile(Dublin$longitude, .25) - 1.5*IQR(Dublin$longitude) & 
                 Dublin$longitude < quantile(Dublin$longitude, .75) + 1.5*IQR(Dublin$longitude), ] #rows
Dublin<- Dublin[!is.na(Dublin$longitude),]

#Initialiasing the variable "RPZ"
Dublin$`Rent Pressure Zones`<-"Yes"
Dublin<-Dublin[,-c(12)]
write.csv(Dublin, "Dublinfinal1.csv")
sort(Dublin$latitude, decreasing = FALSE)
sort(Dublin$latitude, decreasing = TRUE)
sort(Dublin$longitude, decreasing = FALSE)
sort(Dublin$longitude, decreasing = TRUE)


Cork<-filter(newdata8, County %in% c("Cork"))

#Removing outliers
Cork<-Cork[Cork$latitude > quantile(Cork$latitude, .25) - 1.5*IQR(Cork$latitude) & 
             Cork$latitude < quantile(Cork$latitude, .75) + 1.5*IQR(Cork$latitude), ] #rows
Cork<- Cork[!is.na(Cork$latitude),]
Cork<-Cork[Cork$longitude > quantile(Cork$longitude, .25) - 1.5*IQR(Cork$longitude) & 
             Cork$longitude < quantile(Cork$longitude, .75) + 1.5*IQR(Cork$longitude), ] #rows
Cork<- Cork[!is.na(Cork$longitude),]
Cork<-Cork[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Bandon','Kinsale','Carrigaline','Cobh','East Cork','Midleton','Fermoy','Kanturk','Mallow','Macroom','Bantry','Skibbereen','Ballincollig')
patt <- c("Yes")
for (i in 1: length(rpz)) {
  Cork[grep(rpz[i], Cork$formatted_address), "Rent Pressure Zones"] <- patt
}
Cork$`Rent Pressure Zones`[is.na(Cork$`Rent Pressure Zones`)]<-"No"
write.csv(Cork, "Corkfinal.csv")
write.csv(Cork, "Corkfinal1.csv")

Galway<-filter(newdata8, County %in% c("Galway"))

#Removing outliers
Galway<-Galway[Galway$latitude > quantile(Galway$latitude, .25) - 1.5*IQR(Galway$latitude) & 
                 Galway$latitude < quantile(Galway$latitude, .75) + 1.5*IQR(Galway$latitude), ] #rows
Galway<-Galway[Galway$longitude > quantile(Galway$longitude, .25) - 1.5*IQR(Galway$longitude) & 
                 Galway$longitude < quantile(Galway$longitude, .75) + 1.5*IQR(Galway$longitude), ] #rows
Galway<-Galway[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Athenry','Oranmore','Ballinasloe','Conamara','Loughrea','Gort','Kinvara','Tuam',"Claddagh", "Dangan", "Eyre Square", "Mionlach", "Newcastle", "Nuns Island", "Rahoon", "Shantalla", "Toghroinn San Niocláis",
       "Baile an Bhriotaigh", "Ballybaan", "Lough Atalia", "Mervue", "Murroogh", "Renmore" , "Wellpark","Bearna", "Cnoc na Cathrach", "Rockbarton", "Salthill" , "Taylors Hill")
patt <- c("Yes")
for (i in 1: length(rpz)) {
  Galway[grep(rpz[i], Galway$formatted_address), "Rent Pressure Zones"] <- patt
}
Galway$`Rent Pressure Zones`[is.na(Galway$`Rent Pressure Zones`)]<-"No"
write.csv(Galway, "Galwayfinal.csv")

b<-boxplot(Galway$longitude)$out
Galway<-Galway[-which(Galway$longitude %in% b),]
b<-boxplot(Galway$latitude)$out
Galway<-Galway[-which(Galway$latitude %in% b),]


Kildare<-filter(newdata8, County %in% c("Kildare"))

#Removing outliers
Kildare<-Kildare[Kildare$latitude > quantile(Kildare$latitude, .25) - 1.5*IQR(Kildare$latitude) & 
                   Kildare$latitude < quantile(Kildare$latitude, .75) + 1.5*IQR(Kildare$latitude), ] #rows
Kildare<-Kildare[Kildare$longitude > quantile(Kildare$longitude, .25) - 1.5*IQR(Kildare$longitude) & 
                   Kildare$longitude < quantile(Kildare$longitude, .75) + 1.5*IQR(Kildare$longitude),] #rows
Kildare<- Kildare[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Naas','Newbridge','Celbridge','Leixlip','Maynooth')
patt <- c("Yes")
for (i in 1: length(rpz)) {
  Kildare[grep(rpz[i], Kildare$formatted_address), "Rent Pressure Zones"] <- patt
}
Kildare$`Rent Pressure Zones`[is.na(Kildare$`Rent Pressure Zones`)]<-"No"
write.csv(Kildare, "Kildarefinal.csv")


Meath<-filter(newdata8, County %in% c("Meath"))

#Removing outliers
Meath<-Meath[Meath$latitude > quantile(Meath$latitude, .25) - 1.5*IQR(Meath$latitude) & 
               Meath$latitude < quantile(Meath$latitude, .75) + 1.5*IQR(Meath$latitude), ] #rows
Meath<-Meath[Meath$longitude > quantile(Meath$longitude, .25) - 1.5*IQR(Meath$longitude) & 
               Meath$longitude < quantile(Meath$longitude, .75) + 1.5*IQR(Meath$longitude), ] #rows
Meath<- Meath[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Ashbourne','Laytown','Bettystown','Ratoath','Navan','Kells','Trim')
patt <- c("1")
for (i in 1: length(rpz)) {
  Meath[grep(rpz[i], Meath$formatted_address), "Rent Pressure Zones"] <- patt
}
Meath$`Rent Pressure Zones`[is.na(Meath$`Rent Pressure Zones`)]<-"No"
write.csv(Meath, "Meathfinal.csv")

Limerick<-filter(newdata8, County %in% c("Limerick"))

#Removing outliers
Limerick<-Limerick[Limerick$latitude > quantile(Limerick$latitude, .25) - 1.5*IQR(Limerick$latitude) & 
                     Limerick$latitude < quantile(Limerick$latitude, .75) + 1.5*IQR(Limerick$latitude), ] #rows
Limerick<-Limerick[Limerick$longitude > quantile(Limerick$longitude, .25) - 1.5*IQR(Limerick$longitude) & 
                     Limerick$longitude < quantile(Limerick$longitude, .75) + 1.5*IQR(Limerick$longitude), ] #rows
Limerick<- Limerick[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Limerick City','Borris-in-Ossory','Mountmellick','Graiguecullen','Portarlington','Portlaoise')
patt <- c("1")
for (i in 1: length(rpz)) {
  Limerick[grep(rpz[i], Limerick$formatted_address), "Rent Pressure Zones"] <- patt
}
Limerick$`Rent Pressure Zones`[is.na(Limerick$`Rent Pressure Zones`)]<-"No"
write.csv(Limerick, "Limerickfinal.csv")

Wexford<-filter(newdata8, County %in% c("Wexford"))

#Removing outliers
Wexford<-Wexford[Wexford$latitude > quantile(Wexford$latitude, .25) - 1.5*IQR(Wexford$latitude) & 
                   Wexford$latitude < quantile(Wexford$latitude, .75) + 1.5*IQR(Wexford$latitude), ] #rows
Wexford<-Wexford[Wexford$longitude > quantile(Wexford$longitude, .25) - 1.5*IQR(Wexford$longitude) & 
                   Wexford$longitude < quantile(Wexford$longitude, .75) + 1.5*IQR(Wexford$longitude), ] #rows
Wexford<- Wexford[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Gorey','Kilmuckridge')
patt <- c("1")
for (i in 1: length(rpz)) {
  Wexford[grep(rpz[i], Wexford$formatted_address), "Rent Pressure Zones"] <- patt
}
Wexford$`Rent Pressure Zones`[is.na(Wexford$`Rent Pressure Zones`)]<-"No"
write.csv(Wexford, "Wexfordfinal.csv")


Wicklow<-filter(newdata8, County %in% c("Wicklow"))

#Removing outliers
Wicklow<- Wicklow[Wicklow$latitude > quantile(Wicklow$latitude, .25) - 1.5*IQR(Wicklow$latitude) & 
                    Wicklow$latitude < quantile(Wicklow$latitude, .75) + 1.5*IQR(Wicklow$latitude), ] #rows
Wicklow<- Wicklow[Wicklow$longitude > quantile(Wicklow$longitude, .25) - 1.5*IQR(Wicklow$longitude) & 
                    Wicklow$longitude < quantile(Wicklow$longitude, .75) + 1.5*IQR(Wicklow$longitude), ] #rows
Wicklow<- Wicklow[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Arklow','Bray','Greystones','Wicklow')
patt <- c("1")
for (i in 1: length(rpz)) {
  Wicklow[grep(rpz[i], Wicklow$formatted_address), "Rent Pressure Zones"] <- patt
}
Wicklow$`Rent Pressure Zones`[is.na(Wicklow$`Rent Pressure Zones`)]<-"No"
write.csv(Wicklow, "Wicklowfinal.csv")


Laois<-filter(newdata8, County %in% c("Laois"))

#Removing outliers
Laois<- Laois[Laois$latitude > quantile(Laois$latitude, .25) - 1.5*IQR(Laois$latitude) & 
                Laois$latitude < quantile(Laois$latitude, .75) + 1.5*IQR(Laois$latitude), ] #rows
Laois<- Laois[Laois$longitude > quantile(Laois$longitude, .25) - 1.5*IQR(Laois$longitude) & 
                Laois$longitude < quantile(Laois$longitude, .75) + 1.5*IQR(Laois$longitude), ] #rows
Laois<- Laois[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Portlaoise','PORTLAOIGHISE','PORTLAOIGHISE','PORTARLINGTON')
patt <- c("1")
for (i in 1: length(rpz)) {
  Laois[grep(rpz[i], Laois$formatted_address), "Rent Pressure Zones"] <- patt
}
Laois$`Rent Pressure Zones`[is.na(Laois$`Rent Pressure Zones`)]<-"No"
write.csv(Laois, "Laoisfinal.csv")


Louth<-filter(newdata8, County %in% c("Louth"))

#Removing outliers
Louth<- Louth[Louth$latitude > quantile(Louth$latitude, .25) - 1.5*IQR(Louth$latitude) & 
                Louth$latitude < quantile(Louth$latitude, .75) + 1.5*IQR(Louth$latitude), ] #rows
Louth<- Louth[!is.na(Louth$latitude),]
Louth<- Louth[Louth$longitude > quantile(Louth$longitude, .25) - 1.5*IQR(Louth$longitude) & 
                Louth$longitude < quantile(Louth$longitude, .75) + 1.5*IQR(Louth$longitude), ] #rows
Louth<- Louth[!is.na(Louth$longitude),]
Louth<- Louth[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Drogheda','Dundalk','Carlingford','Dundalk South','Ardee')
patt <- c("1")
for (i in 1: length(rpz)) {
  Louth[grep(rpz[i], Louth$formatted_address), "Rent Pressure Zones"] <- patt
}
Louth$`Rent Pressure Zones`[is.na(Louth$`Rent Pressure Zones`)]<-"No"
write.csv(Louth, "Louthfinal.csv")



Waterford<-filter(newdata8, County %in% c("Waterford"))

#Removing outliers
Waterford<- Waterford[Waterford$latitude > quantile(Waterford$latitude, .25) - 1.5*IQR(Waterford$latitude) & 
                        Waterford$latitude < quantile(Waterford$latitude, .75) + 1.5*IQR(Waterford$latitude), ] #rows
Waterford<- Waterford[!is.na(Waterford$latitude),]
Waterford<- Waterford[Waterford$longitude > quantile(Waterford$longitude, .25) - 1.5*IQR(Waterford$longitude) & 
                        Waterford$longitude < quantile(Waterford$longitude, .75) + 1.5*IQR(Waterford$longitude), ] #rows
Waterford<- Waterford[!is.na(Waterford$longitude),]
Waterford<- Waterford[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Comeragh','Portlaw','Kilmacthomas','Dungarvan','Lismore')
patt <- c("1")
for (i in 1: length(rpz)) {
  Waterford[grep(rpz[i], Waterford$formatted_address), "Rent Pressure Zones"] <- patt
}
Waterford$`Rent Pressure Zones`[is.na(Waterford$`Rent Pressure Zones`)]<-"No"
write.csv(Waterford, "Waterfordfinal.csv")


Westmeath<-filter(newdata8, County %in% c("Westmeath"))

#Removing outliers
Westmeath<- Westmeath[Westmeath$latitude > quantile(Westmeath$latitude, .25) - 1.5*IQR(Westmeath$latitude) & 
                        Westmeath$latitude < quantile(Westmeath$latitude, .75) + 1.5*IQR(Westmeath$latitude), ] #rows
Westmeath<- Westmeath[!is.na(Westmeath$latitude),]
Westmeath<- Westmeath[Westmeath$longitude > quantile(Westmeath$longitude, .25) - 1.5*IQR(Westmeath$longitude) & 
                        Westmeath$longitude < quantile(Westmeath$longitude, .75) + 1.5*IQR(Westmeath$longitude), ] #rows
Westmeath<- Westmeath[!is.na(Westmeath$longitude),]
Westmeath<- Westmeath[,-c(1)]

#Initialiasing the variable "RPZ"
rpz<-c('Athlone','Moate')
patt <- c("1")
for (i in 1: length(rpz)) {
  Westmeath[grep(rpz[i], Westmeath$formatted_address), "Rent Pressure Zones"] <- patt
}
Westmeath$`Rent Pressure Zones`[is.na(Westmeath$`Rent Pressure Zones`)]<-"No"
write.csv(Westmeath, "Westmeathfinal.csv")


#Row Binding of data of significant counties 
RPZdata<-rbind(Dublin,Cork,Galway,Laois,Limerick,Wicklow,Wexford,Kildare,Meath,Westmeath, Louth, Waterford)
RPZdata$Date.of.Purchase<- as.Date(as.character(RPZdata$Date.of.Purchase), format = "%Y-%m-%d")
write.csv(RPZdata,"RPZFINAL1.csv")


newdata9<-newdata7 %>%
  select(formatted_address, Price, Date.of.Purchase,Description.of.Property , No.of.Bedrooms, Number.of.Bathrooms, County,Not.Full.Market.Price,VAT.Exclusive,latitude,longitude) %>%
  filter(Date.of.Purchase >= as.Date("2013-01-01"),Date.of.Purchase <= as.Date("2015-12-31"))
table(newdata9$County)
write.csv(newdata9, "HousenotRPZ.csv")


#Selection of non significant counties
newdata10<-select(filter(newdata8,!County  %in% c("Dublin","Cork" ,"Galway" ,"Laois","Limerick","Wicklow"  , "Wexford"  , "Kildare"  , "Meath"  ,   "Westmeath","Louth"   ,  "Waterford") ),c(formatted_address, Price, Date.of.Purchase,Description.of.Property , No.of.Bedrooms, Number.of.Bathrooms, County,Not.Full.Market.Price,VAT.Exclusive,latitude,longitude))
#Initialiasing of variable "Rent Pressure Zone" for Non Rent Pressure Zone counties
newdata10$`Rent Pressure Zones`<-"No"
names(newdata10)[names(newdata10) == 'Rent Pressure Zones'] <- 'Rent.Pressure.Zones'
RPZdata<-RPZdata[,-c(1)]

#Data Cleaning
RPZdata$Date.of.Purchase<- as.Date(as.character(RPZdata$Date.of.Purchase), format = "%Y-%m-%d")
RPZdata<-rbind(RPZdata,newdata10)
RPZdata<-RPZdata[order(as.Date(RPZdata$Date.of.Purchase , format="%Y-%m-%d")),]
RPZdata<-RPZdata[,-c(8)]
unique(RPZdata$`Rent Pressure Zones`)
RPZdata$Rent.Pressure.Zones<-as.factor(RPZdata$Rent.Pressure.Zones)
RPZdata$Rent.Pressure.Zones[RPZdata$Rent.Pressure.Zones== 1]<- "Yes"
write.csv(RPZdata,"RPZFINAL2.csv")
RPZdata<-read.csv("RPZFINAL.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c("", "NA"))
avgprice<-aggregate(newdata8$Price~newdata8$County,FUN = mean)

write.csv(newdata8,"housefinal.csv")
newdata8 <- read.csv("housefinal.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))




p<-ggplot(data = test, aes(x = Date.of.Purchase, y = Price))+
    geom_smooth(color = "#00AFBB", size = 0.5) + geom_smooth(color = "#FC4E07", fill = "#FC4E07",aes(y= new4$predict)) +
    ggtitle("Multiple Linear Regression") + labs(y = "Date of Purchase", x = "Price")
p



#Normalization of numeric variables
RPZ<-RPZdata
nidx <- grep(paste(c("numeric","integer"), collapse="|"), lapply(RPZdata, class)) 
norm <- function(x) { (x - min(x)) / (max(x) - min(x)) } 
RPZ[,c(3,6,7,10,11,15,16,17)] <- apply(RPZ[,c(3,6,7,10,11,15,16,17)], MARGIN=2, FUN=norm )
str(RPZdata)


RPZdata$Price<-as.numeric(RPZdata$Price)
RPZdata$No.of.Bedrooms<-as.numeric(RPZdata$No.of.Bedrooms)
RPZdata$Number.of.Bathrooms<-as.numeric(RPZdata$Number.of.Bathrooms)
RPZdata$latitude<-as.numeric(RPZdata$latitude)
RPZdata$longitude<-as.numeric(RPZdata$longitude)
str(RPZdata)
RPZdata <- read.csv("RPZdata.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))

RPZdata <- RPZdata[RPZdata$Price > quantile(RPZdata$Price, .25) - 1.5*IQR(RPZdata$Price) & 
                    RPZdata$Price < quantile(RPZdata$Price, .75) + 1.5*IQR(RPZdata$Price), ]
RPZdata$Date.of.Purchase <- as.Date(as.character(RPZdata$Date.of.Purchase), format = "%Y-%m-%d")
RPZdata$dateyear<-as.factor(RPZdata$dateyear)
RPZdata$Price_Log <- log10(RPZdata$Price)
RPZdata<-RPZdata[,-c(1,2)]
RPZdata$ID<-seq.int(nrow(RPZ))

#Splitting variable "Date of Purchase" into Day, Month and Year
RPZdata$month <- as.numeric(format(RPZdata$Date.of.Purchase, "%m"))
RPZdata$dateyear <- as.numeric(format(RPZdata$Date.of.Purchase, "%Y"))
RPZdata$day <- as.numeric(format(RPZdata$Date.of.Purchase, "%d"))
RPZdata$dateyear<-as.factor(RPZdata$dateyear)
RPZdata$month<-as.factor(RPZdata$month)
RPZdata$day<-as.factor(RPZdata$day)
write.csv(RPZdata,"RPZdata.csv")

#Splitting the dataset into training and test dataset
library(caret)
training1 <- createDataPartition(RPZdata$Price, p=0.60,list=FALSE)
train<-RPZdata[training1,]
test<-RPZdata[-training1,]




library(h2o)

#Initiliasing the package H2O
h2o.init()

#Converting train dataset into H2o dataset 
train1<-as.h2o(train)
model1<-h2o.glm(y = "Price", x = c( "No.of.Bedrooms","Number.of.Bathrooms", "VAT.Exclusive","County","latitude","longitude", "dateyear","month","day"), training_frame = train1, family = "gaussian",
        nfolds = 5, alpha = 0.1, lambda_search = FALSE)
#Converting test dataset into H2o dataset 
test1<-as.h2o(test)

#Model performance on test dataset
perf1<-h2o.performance(model1,newdata=test1)
perf2<-h2o.performance(model1,newdata=test1)

#Predictio on the test dataset
new1<- as.data.frame(h2o.predict(object=model1,newdata=test1))

#Finding correlation between predicted values and actual values
cor(new1, test$Price)

#Adding predicted values to test dataset
test$LR_Pred<-new1$predict

#Finding variable importance
v<-h2o.varimp(model1)

#Variable importance plot
h2o.varimp_plot(model1)


#Plot of actual vs predicted values over time
p<-ggplot(data = test, aes(x = Date.of.Purchase))+
  geom_smooth(aes( y = Price_Log,color = "Actual"), size = 0.5) + geom_smooth(aes(y= new4$predict,color = "Predicted")) +
  ggtitle("Neural Net") + labs(y = "Price", x = "Date of Purchase")

#Gradient Boosting Machine
model2<-h2o.gbm(y= "Price", x = c( "No.of.Bedrooms","Number.of.Bathrooms", "VAT.Exclusive","County","latitude","longitude","Rent.Pressure.Zones", "dateyear","month","day"), training_frame = train1, ntrees = 10,
        max_depth = 3,min_rows = 2, learn_rate = 0.01,nfolds = 5,sample_rate = 0.6,
        distribution= "gaussian")
h2o.performance(model2,newdata=test1)
new2<- as.data.frame(h2o.predict(object=model2,newdata=test1))
cor(new2, test$Price)
test$GBM_Pred<-new2$predict
v<-h2o.varimp(model2)
h2o.varimp_plot(model2)


#Variable Importance Plots
ggplot(imp1, aes(x=reorder(variable, percentage), weight=percentage,fill = percentage )) + 
  geom_bar() +
  ylab("Percentage") +
  xlab("Variable Name")

ggplot(imp2, aes(x=reorder(variable, percentage), weight=percentage,fill = percentage )) + 
  geom_bar() +
  ylab("Percentage") +
  xlab("Variable Name")

ggplot(imp3, aes(x=reorder(variable, percentage), weight=percentage,fill = percentage )) + 
  geom_bar() +
  ylab("Percentage") +
  xlab("Variable Name")

#Random Forest Regression
model3 <- h2o.randomForest(y= "Price", x = c( "No.of.Bedrooms","Number.of.Bathrooms", "County","latitude","longitude","Rent.Pressure.Zones","month","dateyear","day"), #targets
                           training_frame = train1, #training data
                           #validation data 
                           ntrees = 100, #default trees is 50
                           max_depth = 20, #default is 20,
                           score_each_iteration = TRUE,## Predict against training and validation for each tree
                           fold_assignment = 'Random', #startified sampling
                           nfolds=2,
                           balance_classes = TRUE
                           #balance class
)

test1<-as.h2o(test)
h2o.performance(model3, newdata=test1)
new3<- as.data.frame(h2o.predict(object=model3,newdata=test1))
cor(new3, test$Price)
test$RF_Pred<-new3$predict
v<-h2o.varimp(model3)
h2o.varimp_plot(model3)



#Neural Network
n_model <- h2o.deeplearning( # Neural Network Model
  model_id="dl_model_nn",     # Model Name
  training_frame=train1,       # training data
  #validation_frame = valid,                            # validation data 
  y= "Price", x = c("No.of.Bedrooms","Number.of.Bathrooms", "VAT.Exclusive","County","latitude","longitude","Rent.Pressure.Zones","month","dateyear","day"),                   # dependent variable
  hidden=c(30),               # No of hidden layers and hidden nodes
  epochs=10,
  variable_importances = TRUE,
  score_validation_samples=10000,
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  max_w2=10                       # number of runs
)

nn_train_perf<-h2o.performance(h2o.getModel('dl_model_nn'))
# Validation data performance
nn_vali_perf<-n_model@model$validation_metrics 
# Test data performance
nn_test_perf<-h2o.performance(h2o.getModel('dl_model_nn'), newdata = test1)
# Model Summary
nn_sum<-summary(n_model) 
# Model prediction
nn_pred<-h2o.predict(n_model, test1)
new4<-as.data.frame(nn_pred)
cor(new4, test$Price)
test$NN_Pred<-new4$predict
v<-h2o.varimp(n_model)
h2o.varimp_plot(n_model)

#Neural Network Hyperparameter tuning using Grid Search
hidden_opt <- list(c(32,32), c(32,16,8), c(100)) 
l1_opt <- c(1e-4, 1e-3) 
hyper_params <- list(hidden = hidden_opt, l1 = l1_opt) 
Model_Grid1 <- h2o.grid("deeplearning", grid_id = "mygrid",training_frame = train1, y= "Price", x = c( "Date.of.Purchase", "No.of.Bedrooms","Number.of.Bathrooms", "VAT.Exclusive","County","latitude","longitude","Rent.Pressure.Zones"),
                        hyper_params = hyper_params)
summary(Model_Grid1)
model_ids <- Model_Grid1@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})
nn<-h2o.getModel('mygrid_model_5')
nn_pred_1<-h2o.predict(nn, test1)
new5<-as.data.frame(nn_pred_1)
cor(new4, test$Price)


number.of.mv <-function(a){
  mv.list <- c()
  for(i in 1:ncol(a))
  {
    mv.list[i] <- sum(is.na(a[,i]))
  }
  return(mv.list)
}
number.of.mv(housedata)

get.mv <- function(a){
  missing.values<-cbind(colnames(a[which(number.of.mv(a)!=0)]),number.of.mv(a)[!(number.of.mv(a)==0)])
  return(missing.values)
}

get.mv(housedata)


library(mice)
miceMod <- mice(newdata[, !names(newdata) %in% "Price"], method="pmm")  # perform mice imputation, based on random forests.

miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)


newdata<-merge(data, housedata, by.x = "Address", by.y = "Address")
newdata<-newdata[order(as.Date(newdata$Date.of.Purchase.x , format="%Y-%m-%d")),]

dateyear <- format(newdata$Date.of.Purchase.x, "%Y")
newdata$dateyear<-dateyear
table(newdata$dateyear)

library(gtools)
newdata1<-smartbind(housedata, data)
newdata1<-newdata1[order(as.Date(newdata1$Date.of.Purchase , format="%Y-%m-%d")),]

newdata1<-newdata1[,-2]
newdata1<-newdata1[ , -which(names(newdata1) %in% c("X","Postal.Code","Property.Size.Description","Type"))]





RPZdata$month <- as.numeric(format(RPZdata$Date.of.Purchase[1], "%m"))
yr <- as.numeric(format(RPZdata$Date.of.Purchase[1], "%Y"))
da <- as.numeric(format(RPZdata$Date.of.Purchase[1], "%d"))
train2<-ts(train$Price, start = c(yr, mo,da), freq = 7)





ggplot(test, aes(x = Price_Log, y = MLR)) +
  geom_point()

