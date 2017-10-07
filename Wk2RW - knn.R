require(class)

df <- read.csv("C:\\Users\\onyxr\\Dropbox (Personal)\\School\\UW\\Data Mining\\Datasets\\DataCamp\\knn_traffic_signs.csv", stringsAsFactors = FALSE)
df$id <- NULL

signs <- subset(df, sample =="train")
signs_test <- subset(df, sample =="test")

signs$sample <- NULL
signs_test$sample <- NULL
sign_types <- signs$sign_type
test_types <- signs_test$sign_type

signs_pred <- knn( train = signs[,-1] , test = signs_test[,-1] , cl = sign_types, k=7, prob=TRUE)

signs_prob <- attr(signs_pred, "prob")

head(signs_prob, 100)

table(test_types, signs_pred)

