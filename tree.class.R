library(rpart)
library(rpart.plot) 
library(caret)
library(mlearning)
library(randomForest)
library(stats)

train = read.csv("train_abc.csv")
test = read.csv("final_test_abc.csv")

#plot the tree
tree <- rpart(class ~ fixed.acidity+ residual.sugar+ chlorides+ free.sulfur.dioxide+ total.sulfur.dioxide+ density+ pH+alcohol + volatile.acidity + citric.acid + sulphates , data = train, method="class")
rpart.plot(tree)

printcp(tree)

#plot the pruned tree
ptree<-prune(tree,cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
summary(ptree)
rpart.plot(ptree)
printcp(ptree)

#Make predictions
pred<-predict(ptree, test, type="class")
table(pred, test$class)
confusionMatrix(pred, test$class)

#Mlearning
Mod0 <- train(class ~ .,data=train, method="rpart")
conf = confusion(predict(Mod0, test), test$class)
print(conf, sums = FALSE, sort = FALSE)
plot(conf, sort = FALSE)

##############################################
#Random forest
set.seed(1)
Wine_Forest = randomForest(class ~.,  data=train, mtry=sqrt(11),ntree=5000)
Wine_Forest

layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(Wine_Forest, log="x")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(Wine_Forest$err.rate),col=1:4,cex=0.8,fill=1:4)

Wine_Forest$importance
varImpPlot(Wine_Forest)

#Make predictions
pred2<-predict(Wine_Forest, newdata=test, type="class")
summary(pred2)
confusionMatrix(pred2, test$class)

#Mlearning
conf2 = confusion(predict(Wine_Forest, test), test$class)
print(conf2, sums = FALSE, sort = FALSE)
plot(conf2, sort = FALSE)
