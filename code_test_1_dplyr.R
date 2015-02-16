setwd("F:/xangars/More project/ABRL2014/DC_data_test")
data <- read.csv("F:/xangars/More project/ABRL2014/DC_data_test/DC_test_1.csv",stringsAsFactors=FALSE)
library(dplyr)
names(data)
str(data)
head(select(data, DC_City, ItemNo)) # To see colums

select(data, -DC.Code) # select all exept col name DC.Code

filter(data, DC_City=="Bangalore") # filter rows giving col name

filter(data, DC_City=="Bangalore" & ItemNo == 100019018) # filter rows giving col name

data <- arrange(data, Date ) # arrange the data by date small to max

data <- rename(data, DC_City = DCCity)

data <- mutate(data, SalesQty_c = SalesQuantity>100 ) # add new column

head(arrange(data, desc(Date) ), 10) # arrange the data by date maax to Small
#data1<- data[data$DC_City=="Bangalore" & data$DC_City=="Chennai", ]
#datacity[which(datacity$DC_City =="Bangalore" & datacity$ItemNo == 100019018), ] different colums

datacity <- data[data$DC_City %in% c("Bangalore","Chennai"), ] # same colums

data <- mutate(data, city = factor(SalesQuantity>100, labels=c("low","high")) # giving lables
               
data <- group_by(data, city) # group the values

summarize(data, value = mean(SalesQuantity)) # calcul the mean by catagory low and high

data %>% mutate(SalesQty_c=SalesQuantity>100) %>% group_by (SalesQty_c)%>%summarize(value = mean(SalesQuantity))
# pipeline fn
data[ which(data$DC_City=="Bangalore" & data$DC_City == "Delhi"),]
datacity$DC_City <- as.factor(datacity$DC_City)
is.factor(datacity$DC_City)
split(datacity, datacity$DC_City)
scity<- split(datacity, datacity$DC_City)
length(scity)
#scity[1] gives the value in $Bangalore $chennai
#scity[[1]] gives the value inside the $Bangalore
scity[[1]][,4] # take sales qquan which is col 3 of banglore

z<-nrow(scity[[1]]) # no of rows


for(i in 1:length(scity))
 
  {
    A = matrix(scity[[1]][,5], nrow = z, ncol= 1 )

   }

scity[1]
colnames(scity[1])
blore <- data.frame(scity[1],stringsAsFactors=FALSE)




factors=unique(datacity$DC_City)
factors2=unique(datacity$ItemNo)
factors
length(factors)
factors2
length(factors2)

for(i in 1:length(factors))
{
  #   i=1
  cat(i," ",paste(factors[[i]]),"\n")
  log <- rbind(log,paste(i," ",paste(factors[[i]]),"\n"))
  Datapercity <- datacity[datacity$DC_City==factors[[i]],]
}
  Cast.Data <- dcast(Datapercity,Year + Month ~ ItemNo,value.var="SalesQuantity",sum)
  #   sorteddata=Cast.Data[order(as.Date(Cast.Data$date, format="%m/%d/%Y")),]
  part1<- Cast.Data[c(1:9),]
  part2 <- Cast.Data[c(14:16),]
  part3 <- Cast.Data[c(10:13),]
  
  sorteddata=rbind(part1,part2,part3)
  