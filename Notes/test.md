# test


    df <- read.csv("C:\\Users\\onyxr\\Dropbox (Personal)\\School\\UW\\Data Mining\\Datasets\\DataCamp\\locations.csv")
    
    head(df)
    where9am <- subset(df, df$hour==9)
    where9am <- where9am[, c(4,7)]
    
    #compute P(office)
    p_Office <- nrow(subset(where9am, where9am$location == 'office')) / nrow(where9am)
    
    #compute P(weekday)
    p_Weekday <- nrow(subset(where9am, where9am$daytype == 'weekday')) / nrow(where9am)
    
    #compute P(weekday and office)
    p_OfficeAndWeekday <- nrow(subset(where9am, where9am$location == 'office' & where9am$daytype =='weekday')) / nrow(where9am)
    
    #computep(Office|Weekday)
    p_OfficeGivenWeekday <- p_OfficeAndWeekday /p_Weekday
    
    print(p_OfficeGivenWeekday)


Working w/ probability distributions … 

https://www.youtube.com/watch?v=azPNWoMBYvI&


[https://youtu.be/azPNWoMBYvI](https://youtu.be/azPNWoMBYvI)


| https://www.youtube.com/watch?v=azPNWoMBYvI |  |
| ------------------------------------------- |  |
|                                             |  |


