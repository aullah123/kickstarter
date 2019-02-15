

#require("sos")
#findFn("computeEstimate")

Main<-read.csv("Subset of Main.csv",header= T) #Load data
names(Main) # Shows you the headers (column names)

str(Main) # 

summary(Main)

Main$ID<-as.factor(Main$ID)
Main$deadline<-as.Date(Main$deadline)
Main$launched<-as.Date(Main$launched)
Main<-cbind.data.frame(Main,no_cases=1)
Main$deadline.YM<-substr(Main$deadline,1,7)
Main$launched.YM<-substr(Main$launched,1,7)

unique(Main[[4]])

length(unique(Main[[4]]))

xstate<- aggregate(as.formula(paste("no_cases","~","state")), data = Main, FUN=sum, na.rm=TRUE)

xtabs(~state,data = Main)

xtabs(~state,data = Main)


library(ggplot2)
library(plotly)
m <- list(
  l = 25,
  r = 25,
  b = 150,
  t = 150,
  pad = 5
)

xstate %>%
  plot_ly(labels = ~state, values = as.formula(paste("~",names(xstate[2]))), width = 600, height = 600) %>%
  add_pie(hole = 0.5, textinfo = 'label+percent') %>%
  layout(showlegend = T,
         xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
         yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
         autosize = F, margin = m)

Main$successful<-ifelse(Main$state=="successful"|Main$state=="live","Yes","No")

Main$successfulNo<-ifelse(Main$state=="successful"|Main$state=="live",1,0)

aggmain<-aggregate(cbind(no_cases, successfulNo, goal, pledged, backers, usd.pledged, usd_pledged_real, usd_goal_real)~main_category+category+currency+country+deadline.YM+launched.YM+successful, data = Main, FUN=sum, na.rm=TRUE)%>%
  arrange(main_category,category)


CountCS<-aggregate(no_cases~main_category+category, data = Main, FUN=sum, na.rm=TRUE)%>%
  arrange(main_category)
library(scales)
p<-ggplot(CountCS, aes(x = reorder(main_category,-no_cases), y = no_cases, fill = category))+
  geom_bar(position = "fill",stat = "identity", width = 0.5) +
  xlab('Category')+ ylab('Number of Cases')+
  scale_y_continuous(labels = percent_format()) + 
  theme(axis.text.x=element_text(angle=-90, hjust=0.001)) # "angle" will tilt the labels
ggplotly(p, tooltip = c("y", "colour", "text", "label", "fill"))


#TS<-sample(1:nrow(Main), round(nrow(Main)/5))
#MainTr<-Main[-TS,]
#MainTe<-Main[TS,]

library(caTools)
set.seed(123)
split = sample.split(Main$state, SplitRatio = 0.8)
training_set = subset(Main, split == TRUE)
test_set = subset(Main, split == FALSE)



library.dynam(dplyr)
minus<- select(training_set,-ID,-name)
head(training_set,3)

minus$state2<-as.factor(minus$state2)
minus$successful<-as.factor(minus$successfulNo)
str(minus)



qplot(backers,successfulNo,data =training_set )
model <- glm(successfulNo ~ backers , family=binomial(link='logit'),data=training_set)
model <- glm(successfulNo ~ main_category + currency , family=binomial(link='logit'),data=training_set)
summary(model)
anova(model, test="Chisq")
library(pscl)
pR2(model)
fitted.results <- predict(model,newdata=select(training_set,main_category, currency),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

length(fitted.results-training_set$successfulNo)

sum((fitted.results-training_set$successfulNo)==0)/length(fitted.results-training_set$successfulNo)

sum((fitted.results-training_set$successfulNo)==-1)/length(fitted.results-training_set$successfulNo)

sum((fitted.results-training_set$successfulNo)==1)/length(fitted.results-training_set$successfulNo)


table(fitted.results,training_set$successfulNo)

#confusion matrix
cm = table(training_set[,"successfulNo"], fitted.results)
cm

fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != training_set$successfulNo)
print(paste('Accuracy ',round(1-misClasificError,2),'%', sep = ''))

Diff<- training_set$successfulNo-fitted.results
mean(Diff)
hist(Diff)


# visual of missing data
library(Amelia)
missmap(training_set,Main = 'Missing Map', col = c('yellow', 'black'), legend = FALSE) 

# Taking care of missing data #

training_set$usd.pledged = ifelse(is.na(training_set$usd.pledged),
                              ave(training_set$usd.pledge, FUN= function(x) mean(x, na.rm = TRUE)),
                              training_set$usd.pledged)


missmap(training_set,Main = 'Missing Map', col = c('yellow', 'black'), legend = FALSE) 









library(ggplot2)
stplot <- ggplot(training_set, aes(state)) + geom_bar()
ggplotly(stplot)


# Joining the successful, live etc together. and fail together
Main$state2<- ifelse(Main$state=="live" | Main$state=="successful", "successful","fail" )

training_set$state2<- ifelse(training_set$state=="live" |training_set$state=="successful", "successful","fail" )
library(ggplot2)
stplot2 <- ggplot(training_set, aes(state2)) + geom_bar() +ggtitle("State") + xlab("")
ggplotly(stplot2)

# percentage of fail and successful
stplot3 <- ggplot(training_set, aes(state2)) + geom_bar(aes(y = (..count..)/sum(..count..))) +ggtitle("State") + xlab("") + ylab("%")
ggplotly(stplot3)

stplot4 <- ggplot(training_set, aes(state2)) + geom_bar(aes(fill=factor(main_category))) +ggtitle("State") + xlab("")
ggplotly(stplot4)


cont<-ggplot(training_set,aes(country)) + geom_bar(aes(fill=factor(state2)))
ggplotly(cont)
count2 <- ggplot(training_set, aes(country)) + geom_bar(aes(fill=factor(state2), y = (..count..)/sum(..count..))) +ggtitle("State") + xlab("") + ylab("%")
ggplotly(count2)

