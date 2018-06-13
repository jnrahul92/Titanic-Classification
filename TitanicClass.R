setwd('c:/Users/Pradeep/Desktop/Analytics/Kaggle datasets/')
training_set <- read.csv('train.csv',na.strings = "")
test_set <- read.csv('test.csv',na.strings = "")

library(dplyr)
library(ggplot2)
library(mice)
library(car)

#-----------Data Exploration------
summary(training_set)
names(training_set)
str(training_set)

class(training_set$Survived) # Dependent Variable is integer.
training_set$Survived <- factor(training_set$Survived,
                                labels = c(0,1))

View(training_set)

colSums(is.na(training_set))
#AGE has a lot of missing values
#Cabin aslo has a lot of missing values - Will transform into dummy variable
#Embarked has two missing values

#---------Data Preparation

#Getting title oot of name column
training_set$Title <- gsub('(.*,)|(\\..*)','',training_set$Name)
test_set$Title <- gsub('(.*,)|(\\..*)','',test_set$Name)
training_set$Title <- trimws(training_set$Title)
test_set$Title <- trimws(test_set$Title)

table(training_set$Sex,training_set$Title)
table(test_set$Sex,test_set$Title)

rare_title <- c('Capt','Col','Don','Jonkheer','Lady','Major','Rev'
                ,'Sir','the Countess','Dona','Dr')

training_set$Title[training_set$Title == 'Mlle'] <- 'Miss'
training_set$Title[training_set$Title == 'Ms'] <- 'Miss'
training_set$Title[training_set$Title == 'Mme'] <- 'Mrs'
training_set$Title[training_set$Title %in% rare_title] <- 'Rare Title'
test_set$Title[test_set$Title == 'Mlle'] <- 'Miss'
test_set$Title[test_set$Title == 'Ms'] <- 'Miss'
test_set$Title[test_set$Title == 'Mme'] <- 'Mrs'
test_set$Title[test_set$Title %in% rare_title] <- 'Rare Title'

table(training_set$Sex,training_set$Title)
table(test_set$Sex,test_set$Title)


training_set$familysize <- training_set$Parch + training_set$SibSp + 1
test_set$familysize <- test_set$Parch + test_set$SibSp + 1

ggplot(data = training_set,aes(x=familysize,fill = Survived))+
  geom_histogram(bins = 10,position = 'dodge')

training_set$FSizeD <- ifelse(training_set$familysize==1,'Singleton',
                              ifelse(training_set$familysize<5,'small','large'))
test_set$FSizeD <- ifelse(test_set$familysize==1,'Singleton',
                              ifelse(test_set$familysize<5,'small','large'))


#--------------Missing values-------

#Cabin Missing values are treated
sum(is.na(training_set$Cabin))
sum(is.na(test_set$Cabin))

training_set$Cabin_1 <- ifelse(is.na(training_set$Cabin),'Missing',
                               as.character(training_set$Cabin))
test_set$Cabin_1 <- ifelse(is.na(test_set$Cabin),'Missing',
                               as.character(test_set$Cabin))

sum(is.na(training_set$Cabin_1))
sum(is.na(test_set$Cabin_1))

#Removing the cabin column
training_set <- training_set[,-11] 
test_set <- test_set[,-10]

colSums(is.na(training_set))
colSums(is.na(test_set))

index1 <- which(is.na(training_set$Embarked))
training_set[index1,]

unique(training_set$Pclass)
unique(training_set$Embarked)

ggplot(data = training_set,aes(x=Embarked,y=Fare,fill=factor(Pclass)))+
  geom_boxplot()

#From above plot best value that can be imputed is 'C'

training_set[index1,"Embarked"] <- 'C'


colSums(is.na(training_set))
colSums(is.na(test_set))

#Impute for fare

index2 <- which(is.na(test_set$Fare))
test_set[index2,]

test_set %>% filter(Pclass==3 & Embarked=='S') %>% summarise(median(x = Fare,na.rm = T))

test_set[index2,"Fare"] <- 8.05

colSums(is.na(training_set))
colSums(is.na(test_set))

#Now we have to impute for column AGE
training_set$PassengerId <- as.factor(training_set$PassengerId)
training_set$Sex <- as.factor(training_set$Sex)
training_set$Pclass <- as.factor(training_set$Pclass)
training_set$Embarked <- as.factor(training_set$Embarked)
training_set$Title <- as.factor(training_set$Title)
training_set$FSizeD <- as.factor(training_set$FSizeD)
training_set$Cabin_1 <- as.factor(training_set$Cabin_1)
test_set$PassengerId <- as.factor(test_set$PassengerId)
test_set$Sex <- as.factor(test_set$Sex)
test_set$Pclass <- as.factor(test_set$Pclass)
test_set$Embarked <- as.factor(test_set$Embarked)
test_set$Title <- as.factor(test_set$Title)
test_set$FSizeD <- as.factor(test_set$FSizeD)
test_set$Cabin_1 <- as.factor(test_set$Cabin_1)


str(training_set)

mice_mod <- mice(training_set[,!names(training_set) %in% c('PassengerId',
                                                           'Name','Ticket',
                                                           'Cabin_1','Survived')],method = 'rf')
mice_ouput <- complete(mice_mod)

par(mfrow=c(1,2))
hist(training_set$Age)
hist(mice_ouput$Age)

training_set$Age <- mice_ouput$Age

mice_mod2 <- mice(test_set[,!names(test_set) %in% c('PassengerId',
                                                           'Name','Ticket',
                                                           'Cabin_1')],method = 'rf')
mice_ouput2 <- complete(mice_mod2)

par(mfrow=c(1,2))
hist(test_set$Age)
hist(mice_ouput2$Age)

test_set$Age <- mice_ouput2$Age

colSums(is.na(training_set))
colSums(is.na(test_set))

training_set$Child <- ifelse(training_set$Age<18,1,0)
test_set$Child <- ifelse(test_set$Age<18,1,0)

training_set$Child <- as.factor(training_set$Child)
test_set$Child <- as.factor(test_set$Child)


#-----------Logistic Regression------

mod <- glm(Survived~.,training_set[-c(1,4,9)],family = 'binomial')

summary(mod)

#---------Making dummy variables for Title column
unique(training_set$Title)
training_set$Mr <- ifelse(training_set$Title=='Mr',1,0)
training_set$Mrs <- ifelse(training_set$Title=='Mrs',1,0)
training_set$Miss <- ifelse(training_set$Title=='Miss',1,0)
training_set$Master <- ifelse(training_set$Title=='Master',1,0)
training_set$RareTitle <- ifelse(training_set$Title=='Rare Title',1,0)
training_set <- training_set[-12]

test_set$Mr <- ifelse(test_set$Title=='Mr',1,0)
test_set$Mrs <- ifelse(test_set$Title=='Mrs',1,0)
test_set$Miss <- ifelse(test_set$Title=='Miss',1,0)
test_set$Master <- ifelse(test_set$Title=='Master',1,0)
test_set$RareTitle <- ifelse(test_set$Title=='Rare Title',1,0)
test_set <- test_set[-11]

summary(mod)


mod1 <- glm(Survived~Mr+Mrs+Miss+Master+RareTitle+Sex+Age+SibSp+
              Pclass+Parch+Fare+Embarked+FSizeD+familysize+Child,
            data = training_set,family = 'binomial')
summary(mod1)

mod2 <- glm(Survived~Master+Sex+Age+
              Pclass+Fare+Embarked+FSizeD,
            data = training_set,family = 'binomial')
summary(mod2)

mod3 <- glm(Survived~Master+Sex+Age+Pclass
            +FSizeD,data = training_set,family = 'binomial')
summary(mod3)

vif(mod3)

pred <- predict(object = mod3,newdata = test_set,
                type = 'response')

table(training_set$Survived)/nrow(training_set)
test_set$Survived <- ifelse(pred > 0.5,1,0)

solution <- data.frame(PassengerID = test_set$PassengerId,
                       Survived = test_set$Survived)
write.csv(solution,'Log_Solution.csv',row.names = F)
