#### 1. kickerstart data ####
#### 1. Packages ####
library(dplyr)
library(ggplot2)
library(plotly)
#### 2. Load data  ####

setwd("/Users/ishraq/Desktop/Project RShiny/Kickstarter")

getwd()
Main<-read.csv("Subset of Main.csv",header= T) #Load data 
names(Main) #shows you the header 

 #creating the subsection of the data#
set.seed(1)
sMain<-Main[sample(1:nrow(Main),10000, replace=FALSE),]
write.csv(sMain,file="Subset of Main.csv", row.names = FALSE)

## First look at the category
unique(Main$category)
length(unique(Main$category)) # How many unique category

CategoryCountData<-as.data.frame(table(Main$category))# create count dataframe 
CategoryCountData<-CategoryCountData[order(CategoryCountData$Freq, decreasing = T),]
barplot(CategoryCountData$Freq, names.arg = CategoryCountData$Var1,col = "red", main="Counts For Category", space = 2)

CatStaTab<-table(select(Main,category,state))
CatStaDataf<-as.data.frame(table(select(Main,category,state)))

plot(Main$category)

plot(Main$category,Main$state)


plot(Main$usd_goal_real,Main$state)

plot(Main$state,Main$backers)

Main$state[Main$state==0]<- NA
hist(Main$state)

#### 06/11/2018 ####

ggplot(data=Main,aes(x= backers,y= usd.pledged))+geom_point()


ggplot(data=Main,aes(x= backers,y= usd.pledged, colour=state))+geom_point()

ggplot(data=Main,aes(x= backers,y= usd.pledged))+geom_bar(stat="identity")

ggplot(data=Main,aes(x = state,y= usd.pledged))+geom_bar(stat="identity")


Main$newstate<- ifelse(Main$state=="live" | Main$state=="successful", "successful","fail" )
ggplot(data = Main,aes(x=backers,y= pledged,shape= state))+geom_point()
ggplot(data = Main,aes(x=backers,y= pledged,shape= newstate))+geom_point()

# pplotly#
p<-ggplot(data=Main,aes(x = newstate,y= usd.pledged, colour=newstate))+geom_bar(stat="identity")
ggplotly(p)


#Summary of data#
summary(Main)
