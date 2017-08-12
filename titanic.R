full$Title[full$Title == 'Mile'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare title'
train <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)
full <- bind_rows(train, test)
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                                   'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mile'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare title'
table(full$Sex, full$Title)
full$Title[full$Title == 'Mlle'] <- 'Miss'
train1 <- read.csv('train.csv', na.strings = c("", "NA"), stringsAsFactors = F)
test1 <- read.csv('test.csv', na.strings = c("", "NA"), stringsAsFactors = F)
full1 <- bind_rows(train1, test1)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                  +                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mile'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare title'
table(full$Sex, full$Title)
full$Surname <- sapply(full$Name, function(x) strsplit(x,split='[,.]')[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste (full$Surname, full$Fsize, sep='_')
strsplit(full$Cabin[2], NULL)[[1]]
>full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

train <- read.csv('train.csv',na.strings = c("", "NA"),  stringsAsFactors = F)
test <- read.csv('test.csv',na.strings = c("", "NA"),  stringsAsFactors = F)
full <- bind_rows(train, test)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mile'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare title'
table(full$Sex, full$Title)
full$Surname <- sapply(full$Name, function(x) strsplit(x,split='[,.]')[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste (full$Surname, full$Fsize, sep='_')
strsplit(full$Cabin[2], NULL)[[1]]
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


#make tree model
model <- tree(formula = Embarked ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = full)


#predict the NAs with the model
Prediction <- predict(object = model, newdata = missing_embarked, type = "class")

#replace predicted value to NAs
full1$Embarked[is.na(full1$Embarked)] <- prediction








































