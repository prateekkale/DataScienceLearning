#load data from csv file.

train<-read.csv("train.csv", header = TRUE)
test<-read.csv("test.csv",header=TRUE)

#Add survived variable to test data set
test.survived<-data.frame(Survived=rep("None",nrow(test)),test[,])

data.combined<-rbind(train,test.survived)

str(data.combined)

data.combined$Survived<-as.factor(data.combined$Survived)
data.combined$Pclass<-as.factor(data.combined$Pclass)

#survival distribution

table(data.combined$Survived)


#distribution across the classess

table(data.combined$Pclass)


#package loading
library(ggplot2)


#assumption(Hypothesis)- Rich people survived at higher rate

train$Pclass<-as.factor(train$Pclass)

ggplot(train,aes(x=Pclass,fill=factor(Survived)))+
  geom_bar()+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived")



head(as.character((train$Name)))

#how many unique names are there in dataset
length(unique(as.character(data.combined$Name)))

#how to find duplicate names.

duplicates<-as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])


#get the records for duplicates
data.combined[which(data.combined$Name %in% duplicates),]


#load stringr library for string related operations.

library(stringr)
misses<-data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:3,]

mrs<-data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrs[1:3,]



males<-data.combined[which(data.combined$Sex=='male'),]
males[1:3,]


#function to extract titles from data

extractTitles<-function(Name){
  Name<-as.character(Name)
  if (length(grep("Miss.",Name))>0){
  
    return("Miss.")
  }else if (length(grep("Master.",Name))>0){
    
    return("Master.")
  }else if (length(grep("Mr.",Name))>0){
    
    return("Mr.")
  }else if (length(grep("Mrs",Name))>0){
    
    return("Mrs.")
  }else{
    return("Other")
    
  }
  }
  
  
  titles<-NULL
  for(i in 1:nrow(data.combined)){
    titles<-c(titles,extractTitles(data.combined[i,"Name"]))
    
  }

data.combined$title<-as.factor(titles)

ggplot(data.combined[1:891,],aes(x=title,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill="Survived")


ggplot(data.combined[1:891,],aes(x=Sex,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("sex")+
  ylab("Total Count")+
  labs(fill="Survived")


ggplot(data.combined[1:891,],aes(x=Age,fill=Survived))+
  geom_bar()+
  facet_wrap(~Sex+Pclass)+
  ggtitle("Pclass")+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill="Survived")



ggplot(data.combined[1:891,],aes(x=SibSp,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+title)+
  ggtitle("Pclass, Title")+
  xlab("SibSp")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")



data.combined$Parch<-as.factor(data.combined$Parch)

ggplot(data.combined[1:891,],aes(x=Parch,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+title)+
  ggtitle("Pclass, Title")+
  xlab("Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")




temp.sibsp<-c(train$SibSp,test$SibSp)
temp.parch<-c(train$Parch,test$Parch)


data.combined$familysize<-as.factor(temp.sibsp+temp.parch+1)



ggplot(data.combined[1:891,],aes(x=familysize,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+title)+
  ggtitle("Pclass, Title")+
  xlab("familysize")+
  ylab("Total Count")+
  labs(fill="Survived")



#loading Random forest package

library(randomForest)

trainRF1<-data.combined[1:891,c("Pclass","title")]
labelRF<-as.factor(train$Survived)


rf1<-randomForest(x=trainRF1,y=labelRF,importance = TRUE,ntree=1000)
rf1

#The more the accuracy of the random forest decreases due to the exclusion (or permutation) of a single variable, 
#the more important that variable is deemed, and 
#therefore variables with a large mean decrease in accuracy 
#are more important for classification of the data.

varImpPlot(rf1) 

trainRF2<-data.combined[1:891,c("Pclass","title","SibSp")]
rf2<-randomForest(x=trainRF2,y=labelRF,importance = TRUE,ntree=1000)
varImpPlot(rf2) 

testData<-data.combined[892:1309,c("Pclass","title","SibSp")]

#make prediction

predictedSurvival<- predict(rf2,testData,type="class")
predictedSurvival

table(predictedSurvival)

dfprediction<-data.frame(PassangerID=rep(892:1309),Survived=predictedSurvival)

write.csv(dfprediction,"TitanicSurvivalPrediction.csv",row.names = FALSE)



