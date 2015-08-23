
############################################################
#     Human Activity Recognition classification project    #
############################################################

#Step 1
#pml.training.csv text have been edited manually and #DIV/0 have been changed to NA

#Step 2
#Loading Data
pml.training <- read.csv("E:/practicalML/pml-training.csv")
pml.testing <- read.csv("E:/practicalML/pml-testing.csv")

#step 3
#checking data structure and summary
str(pml.training,list.len = 200)
summary(pml.training)

#step 4
#After analysis, the seven first column are just a data index, so we don't need them
training<-pml.training[,7:160]
test<-pml.testing[,7:160]

#step 5
#the summary() command above shows that a lot of variable has a NA value, we will remove them also
na_dataId<-apply(!is.na(training),2,sum)>19000

training<-training[,na_dataId]
test<-test[,na_dataId]

#step 6
#For baseline we will use a tree algo, show model detail and predict test set
model44=train(classe~.,data =training,method='rpart')
print(model44$finalModel)
predict(model44,test)

#step 8
#For proof of concept, we will use only 20% of the training set and 5 cross validation fold and predict test set
littleTrain<-createDataPartition(y=training$classe,p=0.2,list=FALSE)
training1<-training[littleTrain,]
rf_proof<-train(classe~.,data=training1,method="rf",trControl=trainControl(method="cv",number=5))
predict(rf_proof,test)


#step 9
#model for all data and ten cross-validation fold
rf_model<-train(classe~.,data=training,method="rf",trControl=trainControl(method="cv",number=10))
predict(rf_model,test)

#step 10
#create function to export test result and generate file for assignement
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predict(rf_model,test))

