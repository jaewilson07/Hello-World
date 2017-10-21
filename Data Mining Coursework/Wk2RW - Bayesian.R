install.packages('naivebayes')
require('naivebayes')

df <- read.csv("https://raw.githubusercontent.com/jaewilson07/Hello-World/master/Datasets/DataCamp/locations.csv")
head(df,10)
where9am <- subset(df, df$hour==9)
where9am <- where9am[, c(4,7)]
#refactor to adust for limited number of locations
where9am$location <- factor(as.character(where9am$location))


#compute P(office)
p_Office <- nrow(subset(where9am, where9am$location == 'office')) / nrow(where9am)

#compute P(weekday)
p_Weekday <- nrow(subset(where9am, where9am$daytype == 'weekday')) / nrow(where9am)

#compute P(weekday and office)
p_OfficeAndWeekday <- nrow(subset(where9am, where9am$location == 'office' & where9am$daytype =='weekday')) / nrow(where9am)

#computep(Office|Weekday)
p_OfficeGivenWeekday <- p_OfficeAndWeekday /p_Weekday

print(p_OfficeGivenWeekday)


#build location prediction model

daytype <- c(1,2)
daytype<- factor(daytype, levels = c(1,2), labels = c("weekday", "weekend") )

daytype <- c(1)
thursday9am <- data.frame(daytype)

daytype <- c(2)
saturday9am <- data.frame(daytype)

locmodel <- naive_bayes(location ~ daytype, data = where9am)

# Predict Thursday's 9am location
predict(locmodel, thursday9am)

# Predict Saturdays's 9am location
predict(locmodel, saturday9am)

