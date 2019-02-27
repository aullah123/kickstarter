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

p<-ggplot(CountCS, aes(x = reorder(main_category,-no_cases), y = no_cases, fill = category))+
  geom_bar(position = "fill",stat = "identity", width = 0.5) +
  xlab('Category')+ ylab('Number of Cases')+
  scale_y_continuous(labels = percent_format()) + 
  theme(axis.text.x=element_text(angle=-90, hjust=0.001)) # "angle" will tilt the labels
ggplotly(p, tooltip = c("y", "colour", "text", "label", "fill"))


print(head(Main))
print(' ')
print(str(Main))
library(Amelia)
print(missmap(Main,main = 'Missing Map', col = c('yellow','black'),legend = FALSE))


Main$usd.pledged <- ifelse(is.na(Main$usd.pledged),
                           ave(Main$usd.pledge, FUN= function(x) mean(x, na.rm = TRUE)),
                           Main$usd.pledged)
str(Main)





# logistical part 

print(head(Main))
print(' ')
print(str(Main))
library(Amelia)
print(missmap(Main,main = 'Missing Map', col = c('yellow','black'),legend = FALSE))


Main$usd.pledged <- ifelse(is.na(Main$usd.pledged),
                           ave(Main$usd.pledge, FUN= function(x) mean(x, na.rm = TRUE)),
                           Main$usd.pledged)
str(Main)

Main$successfulNo<-as.factor(Main$successfulNo)





#Main$state <- ifelse(Main$state=="successful"|Main$state=="live",1,0)

library(caTools)
set.seed(101)
split <- sample.split(Main$state, SplitRatio = 0.75)
final.train <- subset(Main,split == TRUE)
final.test <- subset(Main, split == FALSE)


classfier = glm(formula = successfulNo ~  category + main_category +  goal + pledged + backers + usd.pledged + usd_pledged_real + usd_goal_real , family=binomial(link='logit'),data=final.test)
summary(classfier)


prob_pred <- predict(classfier, type = 'response', newdata = final.test[-20])
ypred <- ifelse(prob_pred >0.5,1,0) 

cm <- table(final.test[,20], ypred)
print(cm)






stplot4 <- ggplot(Main, aes(state)) + geom_bar(aes(fill=factor(main_category))) +ggtitle("State") + xlab("")
ggplotly(stplot4)


cont<-ggplot(Main,aes(country)) + geom_bar(aes(fill=factor(state)))
ggplotly(cont)
count2 <- ggplot(Main, aes(country)) + geom_bar(aes(fill=factor(state), y = (..count..)/sum(..count..))) +ggtitle("State") + xlab("") + ylab("%")
ggplotly(count2)


# data exploring 

# project by subcatgory 
library(summariser)
library(ggthemes)
subcat.freq <- Main%>%
  group_by(category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

subcat.freq$category <- factor(subcat.freq$category, levels=subcat.freq$category)

ggplot(head(subcat.freq, 10), aes(category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Subcategory") + xlab("Project Subcategory") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")


# amount pledged by category

pledged.tot <- Main %>%
  group_by(main_category) %>%
  summarize(total=sum(usd.pledged)) %>%
  arrange(desc(total))

pledged.tot$main_category <- factor(pledged.tot$main_category, levels=pledged.tot$main_category)

total.amount <-ggplot(pledged.tot, aes(main_category, total/1000000, fill=total)) + geom_bar(stat="identity") + 
  ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD millions)")  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

ggplotly(total.amount)



# amount pledged vs. project category  

pledged.v.project <-ggplot(Main, aes(main_category, usd.pledged, fill=main_category)) + geom_boxplot() + 
  ggtitle("Amount Pledged vs. Project Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,20000))

ggplotly(pledged.v.project)



#success vs fail by project catgetgory 
state.pct <- Main %>%
  filter(state %in% c("successful", "failed")) %>%
  group_by(main_category, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state), pct)

state.pct$main_category <- factor(state.pct$main_category, 
                                  levels=state.pct$main_category[1:(nrow(state.pct)/2)])

success.vs.percent <- ggplot(state.pct, aes(main_category, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Project Category") + 
  xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold")) + coord_flip()
ggplotly(success.vs.percent)



#year success and fail
state.pct2 <- Main %>%
  filter(year(launched)!="1970", state %in% c("successful", "failed")) %>%
  group_by(year=year(launched), state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state))

year_succ_fail <- ggplot(state.pct2, aes(year, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Year Launched") + 
  xlab("Year") + ylab("Percentage") + scale_x_discrete(limits=c(2009:2017)) + 
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold"))

ggplotly(year_succ_fail)





# project by year
year.freq <- Main%>%
  filter(year(launched)!="1970") %>%
  group_by(year=year(launched)) %>%
  summarize(count=n())

year_project <- ggplot(year.freq, aes(year, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Number of Projects by Launch Year") + xlab("Year") + ylab("Frequency") + 
  scale_x_discrete(limits=c(2009:2018)) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

ggplotly(year_project)





