install.packages('xlsx')
install.packages('rJava')

require('rJava')
require('xlsx')

df = read.xlsx("c:\\Temp\\DataSet\\smoking drinking.xlsx", sheetName = "Data", colIndex= 1:5)

head(df[, 1:3])

smoking_con <- table(df$Smoking, df$Drinking)

cTotal <- colSums(smoking_con)
smoking_con <- rbind(smoking_con, cTotal)


rTotal <-rowSums(smoking_con)
smoking_con <- cbind(smoking_con, rTotal)

smoking_con

