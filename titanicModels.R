library(ggplot2) 
library(tidyverse)
library(ggthemes) 
library(dplyr) 
library(mice)
library(scales)
library(randomForest) 


trainingData <- read.csv("train.csv", stringsAsFactors = F)
testingData  <- read.csv("test.csv", stringsAsFactors = F)

combinedData  <- bind_rows(trainingData, testingData) # bind training & test data


combinedData[c(62, 830), 'Embarked']

embark_fare <- combinedData %>%
  filter(PassengerId != 62 & PassengerId != 830)

combinedData$Embarked[c(62, 830)] <- 'C'

combinedData$Fare[1044] <- median(combinedData[combinedData$Pclass == '3' & 
                                 combinedData$Embarked == 'S', ]
                                 $Fare, na.rm = TRUE)

sum(is.na(combinedData$Age))

factor_vars <- c('PassengerId','Pclass','Sex')

combinedData[factor_vars] <- lapply(combinedData[factor_vars],
                                    function(x) as.factor(x))


####changing embarkment values to numerics
for(i in 1:nrow(combinedData)){
  if(as.character(combinedData$Embarked[i] == "S")){
    combinedData$Embarked[i] <- 1
  } else if (as.character(combinedData$Embarked[i] == "C")){
    combinedData$Embarked[i] <- 2
  } else{
    combinedData$Embarked[i] <- 3
  }
}


###distributing men and women into seperate dataframes
menAboard <- data.frame(trainingData[trainingData$Sex == 1, ])
menAboad %>% na.omit()

womenAboard <- data.frame(trainingData[trainingData$Sex == 0, ])
womenAboard %>% na.omit()

####creating two more dataframes by age and survival
maleAgeSurvival <- data.frame(menAboard$Age, menAboard$Survived)

femaleAgeSurvival <- data.frame(womenAboard$Age, womenAboard$Survived)




### Fare frequency distribution
trainingData %>% 
  filter( Fare < 300) %>%
  ggplot( aes(x=Fare)) +
  ggtitle("Frequency of Fares") + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)



####dual histogram featuring Men vs Women aboard by ages
# 
# hist(menAboard$Age,
#      main = "Men's Ages",
#      xlab = "Ages",
#      ylab = "Passenger count",
#      xlim = c(0,400),
#      col = "light blue",
#      breaks = 5)
# 
# 
# hist(womenAboard$Age,
#      main = "Women's Ages",
#      col = "pink",
#      add = TRUE)
# 

set.seed(129)
mice_mod <- mice(combinedData[, !names(combinedData) %in% c('PassengerId','Name',
                                            'Ticket','Survived')], method='rf')

ageImpute <- complete(mice_mod)

combinedData$Age <- ageImpute$Age

###resplitting the data
trainingData <- combinedData[1:891,]

testingData <- combinedData[892:1309,]



trainingData %>% na.omit

set.seed(891)

###random forest model with 500 tree count
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked,
                         data = trainingData, ntree= 500, do.trace = T)

####confusion matrix for quick error check
rf_model$confusion

###plotting model
plot(rf_model, ylim=c(0,0.40))

legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


survivedPredict <- predict(rf_model, testingData)


###writing prediction to csv file

finalPredict <- data.frame(PassengerId = testingData$PassengerId, Survived = survivedPredict)

write.csv(finalPredict, file = 'Survival_Prediction.csv', row.names = F)





###Analyzing prediction

combinedResults <- cbind(finalPredict, testingData)

combinedResults <- combinedResults[ , colSums(is.na(combinedResults)) < nrow(combinedResults)]

predictionSurvValues <- data.frame(combinedResults[combinedResults$Survived == 1, ])

head(predictionSurvValues)

predictionSurvValuesFemale <- data.frame(predictionSurvValues[predictionSurvValues$Sex == "female", ])
nrow(predictionSurvValuesFemale)
predictionSurvValuesMale <- data.frame(predictionSurvValues[predictionSurvValues$Sex == "male", ])
nrow(predictionSurvValuesMale)

head(predictionSurvValues)


hist(predictionSurvValuesFemale$Age,
     main = "Predicted Male vs Female Survivors", xlab = "Ages",
     ylab = "Total Passengers",
     col = "pink", ylim = c(0, 40))
hist(predictionSurvValuesMale$Age, col = "light blue", add = TRUE)
legend("topright", c("Female", "Male"), fill = c("pink", "light blue"))


hist(predictionSurvValues$Fare,
     main = "Fares of Survived Passenger",
     xlab = "Price of Fare",
     ylab = "Number of Fares Paid",
     col = "light green")

survFare <- mean(predictionSurvValuesMale$Fare)

testFare <- mean(testingData$Fare)

differenceFare <- survFare - testFare
print(differenceFare)



mean(as.numeric(predictionSurvValuesMale$Embarked))

mean(as.numeric(predictionSurvValues$Pclass))
table(as.numeric(predictionSurvValues$Pclass))



table(as.numeric(predictionSurvValues$Parch))
table(as.numeric(predictionSurvValues$SibSp))

# ####linear regression implementation and KMeans
# fit.lm <- lm(Survived ~ Sex + Age + Fare + SibSp + Parch, data= trainData)
# print(fit.lm)
# 
# 
# 
#
# fitlog_lg <- glm(Survived ~ Sex + Age + Fare + SibSp + Parch, data= trainData)
# print(fitlog_lg)
# # 
# 
# ###cluster optimizer function
# wssplot <- function(data, nc=15, seed=1234)
# {
#   wss <- (nrow(data)-1)*sum(apply(data,2,var))
#   for (i in 2: nc)
#   {
#     set.seed(seed)
#     wss[i] <- sum(kmeans(data, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab = "Number of clusters", 
#        ylab = "within groups of sum of squares")
# }
# 
# trainDataClusters <- trainData
# trainDataClusters <- dplyr::select(trainDataClusters, -c(Name, Ticket, Cabin, Embarked))
# 
# trainDataClusters <- na.omit(trainDataClusters)
# 
# ###Look at graph for optimum cluster number
# wssplot(trainDataClusters)

# KM = kmeans(trainDataClusters,4)
# ggplot2::autoplot(KM, trainDataClusters, frame=TRUE)
# 




