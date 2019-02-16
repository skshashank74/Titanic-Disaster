
library(ggplot2)
library(dplyr)
library(stringr)
library(ggthemes)
library(randomForest)


train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Combing the train and test for data cleaning

#Rearranging the test as columns in train
test$Survived <- NA
test <- test[,c(1,12,2,3,4,5,6,7,8,9,10,11)]
head(test)
full <- rbind(train, test)


#Check for NAs
sapply(full, function(x) {sum(is.na(x)|x == "")})

#NA values are in: Age, Fare, Cabin and Embarked

#NA - Fare
#Row number 
which(is.na(full$Fare))

#check other details for row number 1044
full[1044,]

mean(full$Fare[full$Pclass == 3 & full$Sex == "male"], na.rm = TRUE)
median(full$Fare[full$Pclass == 3 & full$Sex == "male"], na.rm = TRUE)

ggplot(full[full$Pclass == 3 & full$Sex == "male",],aes(x = Fare, y = Pclass))+
  geom_point()

full$Fare[is.na(full$Fare)] <-mean(full$Fare[full$Pclass == 3 & full$Sex == "male"], na.rm = TRUE)
       
#NA - Cabin
sum(full$Cabin == "")/nrow(full)

#77% of data in Cabin are empty. Cabin will not be used as feature

#NA - Embarked
which(full$Embarked == "")

#62 and 830 are the rows which have NA value for Embarked
full[c(62,830),]

#Only these two person had same ticket no. and Cabin that means they might be together and boarded from same port
full[full$Ticket == 113572,]
full[full$Cabin == "B28",]

ggplot(full[full$Pclass == 1,], aes(x = Embarked))+
  geom_bar()+
  facet_grid(.~Sex)

t <- full[full$Fare >75 & full$Fare <85,]
t %>% group_by(Embarked) %>% summarise(n())

#in the above set around 56% people boarded from C
full$Embarked[full$Embarked == ""] <- "C"

#NA - Age
sum(is.na(full$Age))

#Extract title from name 
full$Title <- str_extract(string = full$Name, pattern = "(Mr|Master|Mrs|Miss)\\.")
table(full$Title)
length(which(is.na(full$Title)))

#Replace Na with Rare
full$Title[is.na(full$Title)] <- "Rare"

#nrow(full[is.na(full$Age) & full$Title == "Mr.",])
#nrow(full[is.na(full$Age) & full$Title == "Mrs.",])
#nrow(full[is.na(full$Age) & full$Title == "Miss.",])
#nrow(full[is.na(full$Age) & full$Title == "Master.",])
#nrow(full[is.na(full$Age) & full$Title == "Rare",])

m <- c("Mr.","Mrs.","Miss.","Master.","Rare")
for (i in 1:length(m)) {
  cat("NA in Age having title",m[i],nrow(full[is.na(full$Age) & full$Title == m[i],]),"\n")
}

#Replace NA with mean grouped by Title

for(i in 1:length(m)){
  full$Age[(is.na(full$Age) & full$Title == m[i])] <- round(mean(full$Age[full$Title == m[i]],na.rm = T),2)
}

#full$Age[(is.na(full$Age) & full$Title == "Mr.")] <- round(mean(full$Age[full$Title == "Mr."], na.rm = T),2)
#full$Age[(is.na(full$Age) & full$Title == "Mrs.")] <- round(mean(full$Age[full$Title == "Mrs."], na.rm = T),2)
#full$Age[(is.na(full$Age) & full$Title == "Miss.")] <- round(mean(full$Age[full$Title == "Miss."], na.rm = T),2)
#full$Age[(is.na(full$Age) & full$Title == "Master.")] <- round(mean(full$Age[full$Title == "Master."], na.rm = T),2)
#full$Age[(is.na(full$Age) & full$Title == "Rare")] <- round(mean(full$Age[full$Title == "Rare"], na.rm = T),2)

#All missing value has been taken care of
#Lets try to extract some fields

#Family size
full$family_size <- full$SibSp + full$Parch +1

ggplot(full[1:891,], aes(x = family_size, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') 

#Family Size = 1 died more
#Family Size >= 2 & <=4 survived more
#Family size > 5 died more

#Lets see the relationship of features with the survived

#Pclass v/s Survived
table(full$Pclass, full$Survived)

ggplot(full[1:891,], aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Pclass')+
  theme_few()

#People in 3rd class died more
#67% of people who did not survived where from 3rd class

#Sex v/s Survived
ggplot(full[1:891,], aes(x = Sex, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Gender')+
  theme_few()

#More number of Female survived than male
# Lets check with Title
ggplot(full[1:891,], aes(x = Title, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Title')+
  theme_few()

#Mr is the only title where the count of death is more than survived
#So in the Titanic movie it is correctly potrayed that preference was given to children and women


# Age vs Survived 
table(cut(full$Age,10), full$Survived)

ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins = 20) + 
  theme_few()

#More number of people in middle age group died
# Lets divide the data through Gender


ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins = 10) + 
  facet_grid(.~Sex) + 
  theme_few()

#Embarked vs Survived

#70% of people boarded from S, and had a higher percentage of death
#people boarded from C survived more
table(full$Embarked, full$Survived)

ggplot(full[1:891,],aes(x = Embarked, fill = factor(Survived)))+
  geom_bar(stat = 'count', position='dodge') +
  labs(x = 'Embarked')+
  theme_few()


#convert to factor level for specific variable
l <- c("Survived", "Pclass", "Sex", "Embarked", "Title")
index <- match(l,names(full))
for(i in index){
  full[,i] <- as.factor(full[,i])
}

# Now we can see the clear picture, most of the Male of middle age group died while proportion of female is very less
train2 <- full[1:891,]
test2 <- full[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)

#Error rate = 16.72%
rf_model_test <- randomForest(factor(Survived) ~ Pclass + Age + Sex + Title + Fare + family_size + Embarked,
                         data = train2)
importance(rf_model_test)

#Emabarked removed (lowest Ginni Index)
#Error rate = 16.5% 
rf_model_test <- randomForest(factor(Survived) ~ Pclass + Age + Sex + Title + Fare + family_size,
                         data = train2)

#Embarked & Family size removed
#Error rate = 16.16%
rf_model_test <- randomForest(factor(Survived) ~ Pclass + Age + Sex + Title + Fare,
                         data = train2)

#title can take care of sex (Embarked & Sex removed)
#Error rate = 15.49%
rf_model_test <- randomForest(factor(Survived) ~ Pclass + Age + Title + Fare + family_size,
                         data = train2)

#Embarked , Pclass & Age
#Error rate = 16.72%
rf_model_test <- randomForest(factor(Survived) ~  Age + Title + Fare + family_size,
                         data = train2)

# Pclass + Age + Title + Fare + family_size has the least error rate 

rf_model <- randomForest(factor(Survived) ~ Pclass + Age + Title + Fare + family_size,
                         data = train2)

plot(rf_model, ylim=c(0,0.40))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test2)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'Survived.csv', row.names = F)















  


