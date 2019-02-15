setwd("/Users/ishraq/Desktop/Project RShiny/Kickstarter")

getwd()
library(caTools)
Main<-read.csv("Subset of Main.csv",header= T) #Load data 
names(Main) #shows you the header 

#creating the subsection of the data#
set.seed(1)
sMain<-Main[sample(1:nrow(Main),10000, replace=FALSE),]
write.csv(sMain,file="Subset of Main.csv", row.names = FALSE)

# Taking care of missing data #
Main$pledged = ifelse(is.na(Main$pledged),
                         ave(Main$pledge, FUN= function(x) mean(x, na.rm = TRUE)),
                      Main$pledged)
sum(is.na(Main$backers))
Main$backers = ifelse(is.na(Main$backers),
                      ave(Main$backers, FUN= function(x) mean(x, na.rm = TRUE)),
                      Main$backers)

Main$usd.pledged = ifelse(is.na(Main$usd.pledged),
                      ave(Main$usd.pledged, FUN= function(x) mean(x, na.rm = TRUE)),
                      Main$usd.pledged)

Main$usd_pledged_real = ifelse(is.na(Main$usd_pledged_real),
                          ave(Main$usd_pledged_real, FUN= function(x) mean(x, na.rm = TRUE)),
                          Main$usd_pledged_real)

Main$goal <- ifelse(is.na(Main$goal),
                      ave(Main$goal, FUN= function(x) mean(x, na.rm = TRUE)),
                      Main$goal )



# Joining the successful, live etc together. and fail together
Main$state<- ifelse(Main$state=="live" | Main$state=="successful", "successful","fail" )

#Categorical data
Main$state<- factor(Main$state,
                    levels = c('successful', 'fail'),
                    labels = c(1, 0))

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





# Main$country<- factor(Main$country,
#                       levels = c('AT', 'AU', 'BE', 'CA', 'CH', 'DE', 'DK', 'ES', 'FR', 'GB', 'HK', 'IE', 'IT', 'LU', 'MX', 'N,0"', 'NL', 'NO', 'NZ', 'SE', 'SG', 'US'), 
#                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
#                       
#                      
# Main$currency<- factor(Main$currency,
#                        levels = c('AUD', 'CAD', 'CHF', 'DKK', 'EUR', 'GBP', 'HKD', 'MXN', 'NOK', 'NZD', 'SEK', 'SGD', 'USD'),
#                        labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) 
# 
# Main$main_category<- factor(Main$main_category,
#                        levels = c('Art', 'Comics', 'Crafts', 'Dance', 'Design', 'Fashion', 'Film & Video', 'Food', 'Games', 'Journalism', 'Music', 'Photography', 'Publishing', 'Technology', 'Theater'),
#                        labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) 
# 

Main$launched<- substr(Main$launched,1,4)


#splitting the data into training set and test set

set.seed(123)
split = sample.split(Main$state, SplitRatio = 0.8)
training_set = subset(Main, split == TRUE)
test_set = subset(Main, split == FALSE)

#feature scales 
training_set[c('goal', 'pledged', 'backers', 'usd.pledged', 'usd_pledged_real', 'usd_goal_real')] = scale(training_set[c('goal', 'pledged', 'backers', 'usd.pledged', 'usd_pledged_real', 'usd_goal_real')])
test_set[c('goal', 'pledged', 'backers', 'usd.pledged', 'usd_pledged_real', 'usd_goal_real')] = scale(test_set[c('goal', 'pledged', 'backers', 'usd.pledged', 'usd_pledged_real', 'usd_goal_real')])


#logitical regression 
classifier = glm(formula = state ~ main_category + backers + usd_pledged_real,
                 family = binomial,
                 data = training_set)
summary(classifier)



library(xtable)
xtable(summary(classifier))

# predcting the test set result

prob_pred = predict(classifier, type = 'response', newdata = test_set[-10])
y_pred = ifelse(prob_pred >0.5, 1, 0)


# confusion matrix
cm = table(test_set[, 10], y_pred)


# visulising the trainning set result


# Visualising the Regression Model results 

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$backers, y = training_set$main_category),
             colour = 'red') +
  geom_line(aes(x = training_set$backers, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('backers') +
  ylab('main_category')



