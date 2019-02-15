
# pipline
2 %>%
     log()%>%
     sin()
Main<-read.csv("ks-projects-201801.csv",header= T) #Load data
names(Main) # Shows you the headers (column names)

str(Main) # 

summary(Main) ## Show Ashrak

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


TS<-sample(1:nrow(Main), round(nrow(Main)/5))
MainTr<-Main[-TS,]
MainTe<-Main[TS,]

qplot(backers,successfulNo,data =MainTr )
model <- glm(successfulNo ~ backers , family=binomial(link='logit'),data=MainTr)
model <- glm(successfulNo ~ main_category + currency , family=binomial(link='logit'),data=MainTr)
summary(model)
anova(model, test="Chisq")
library(pscl)
pR2(model)
fitted.results <- predict(model,newdata=select(MainTe,main_category, currency),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

length(fitted.results-MainTe$successfulNo)

sum((fitted.results-MainTe$successfulNo)==0)/length(fitted.results-MainTe$successfulNo)
 
sum((fitted.results-MainTe$successfulNo)==-1)/length(fitted.results-MainTe$successfulNo)

sum((fitted.results-MainTe$successfulNo)==1)/length(fitted.results-MainTe$successfulNo)


table(fitted.results,MainTe$successfulNo)

cm = table(MainTe[,"successfulNo"], fitted.results)
cm

fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != MainTe$successfulNo)
print(paste('Accuracy ',round(1-misClasificError,2),'%', sep = ''))

Diff<- MainTe$successfulNo-fitted.results
mean(Diff)
hist(Diff)

library(ggplot2)
ggplot() +
  geom_point(aes(x = MainTe$backers, y = training_set$main_category),
             colour = 'red') +
  geom_line(aes(x = training_set$backers, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('backers') +
  ylab('main_category')

