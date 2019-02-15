setwd("/Users/ishraq/Desktop/Project RShiny/Kickstarter")

getwd()
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

Main$country<- factor(Main$country,
                      levels = c('AT', 'AU', 'BE', 'CA', 'CH', 'DE', 'DK', 'ES', 'FR', 'GB', 'HK', 'IE', 'IT', 'LU', 'MX', 'N,0"', 'NL', 'NO', 'NZ', 'SE', 'SG', 'US'), 
                      labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
                      
                     
Main$currency<- factor(Main$currency,
                       levels = c('AUD', 'CAD', 'CHF', 'DKK', 'EUR', 'GBP', 'HKD', 'MXN', 'NOK', 'NZD', 'SEK', 'SGD', 'USD'),
                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) 

Main$main_category<- factor(Main$main_category,
                       levels = c('Art', 'Comics', 'Crafts', 'Dance', 'Design', 'Fashion', 'Film & Video', 'Food', 'Games', 'Journalism', 'Music', 'Photography', 'Publishing', 'Technology', 'Theater'),
                       labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) 


Main$launched<- substr(Main$launched,1,4)
Main$launched.Y <- substr(Main$launched,1,4)


#splitting the data into training set and test set

set.seed(123)
split = sample.split(Main$state, SplitRatio = 0.8)
training_set = subset(Main, split == TRUE)
test_set = subset(Main, split == FALSE)

#feature scales 
training_set[c('goal', 'pledged', 'backers', 'usd.pledged', 'usd_pledged_real', 'usd_goal_real')] = scale(training_set[c('goal', 'pledged', 'backers', 'usd.pledged', 'usd_pledged_real', 'usd_goal_real')])
test_set[c('goal', 'pledged', 'backers', 'usd.pledged', 'usd_pledged_real', 'usd_goal_real')] = scale(test_set[c('goal', 'pledged', 'backers', 'usd.pledged', 'usd_pledged_real', 'usd_goal_real')])


#logitical regression 
classifier = glm(formula = state ~.,
                 family = binomial,
                 data = training_set)

# predcting the test set result

prob_pred = predict(classifier, type = 'response', newdata = test_set[-10])
y_pred = ifeles(prob_pred >0.5, 1, 0)


# confusion matrix
cm = table(test_set[, 10], y_pred)


