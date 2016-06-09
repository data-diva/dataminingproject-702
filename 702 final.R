library(rpart)
install.packages("DMwR")
library("DMwR")
install.packages("MASS")
library(MASS)



######## data and preparation ########
eyes <- read.csv("/Users/emily/school/da exam/eyes.csv", header = TRUE)

# what does my data look like? # 
head(eyes, n = 5)

eyes$Trt <- factor(eyes$Trt, levels = 0:1, labels = c("No", "Yes"))
eyes$diab <- factor(eyes$diab, levels = 0:1, labels = c("No", "Yes"))
eyes$history <- factor(eyes$history, levels = 0:1, labels = c("No", "Yes"))
eyes$race <- factor(eyes$race, levels = 0:1, labels = c("No", "Yes"))
eyes$gender <- factor(eyes$gender, levels = 0:1, labels = c("Female", "Male"))
eyes$stroke <- factor(eyes$stroke, levels = 0:1, labels = c("No", "Yes"))
eyes$myopia <- factor(eyes$myopia, levels = 0:1, labels = c("No", "Yes"))
eyes$PGON <- factor(eyes$PGON, levels = 0:1, labels = c("No", "Yes"))
eyes$heart <- factor(eyes$heart, levels = 0:1, labels = c("No", "Yes"))
eyes$lbp <- factor(eyes$lbp, levels = 0:1, labels = c("No", "Yes"))
eyes$hpb <- factor(eyes$hbp, levels = 0:1, labels = c("No", "Yes"))
eyes$migr <- factor(eyes$migr, levels = 0:1, labels = c("No", "Yes"))
eyes$sysbb <- factor(eyes$sysbb, levels = 0:1, labels = c("No", "Yes"))
eyes$ccb <- factor(eyes$ccb, levels = 0:1, labels = c("No", "Yes"))
eyes$hbp <- factor(eyes$hbp, levels = 0:1, labels = c("No", "Yes"))
summary(eyes)

## average age is 56.3 years, primarily females in the study, primarily white patients in the study # 



# remove ID from data frame as it's an arbitrary primary key and should not be analyzed #
eyes$Patient.ID <- NULL
eyes$eye <- NULL
# check for missing values #
sapply(eyes,function(x) sum(is.na(x)))
# yay there are none # 
dim(eyes)

###### we need a test and training sample ######
# will obtain it using stratified sampling so that the test and training samples reflect #
# the proportion of PGON/no PGON individuals  #

set1 <- eyes[eyes$PGON == "Yes",]
set0 <- eyes[eyes$PGON == "No",]

dim(set1) * 1/2
dim(set0) * 1/2

set.seed(1)
training1 <- sample(1:139,69)
test1 <- (1:139)[-training1]
sum((1:139) == sort(c(training1,test1))) 
length(training1) + length(test1) # confirm that there are 139 observations

training0 <- sample(1:1500,750)
test0<-(1:1500)[-training0]
sum((1:1500==sort(c(training0,test0)))) # confirm there are 1600 - 139 = 1500 observations

train<-rbind(set1[training1,],set0[training0,])
test<-rbind(set1[test1,], set0[test0,])
dim(train)[1] + dim(test)[1] # confirm there are 1639 observations in both

#### model 1 -  fit logistic regression model using train data #####
fit.all.eyes <- glm(formula = PGON ~ ., family = binomial(link = "logit"),data = train)
summary(fit.all.eyes)
# variable selection using stepwise #
stepAIC(fit.all.eyes, trace = FALSE)
# refit with selected variables #
fit.all.eyes <- glm(formula = PGON ~ Trt + IOP + CCT + VCD + MD + CPSD + lbp + diab + myopia, family = binomial(link = "logit"),data = train)
# predictions using model 1 #
pred.all.eyes <- predict(fit.all.eyes, newdata = test, type = "response")
# determine misclassification/accuracy #
pred.all.eyes <- ifelse(pred.all.eyes > .5, "Yes","No")
misclass.all.eyes <- mean(pred.all.eyes != test$PGON)
print(paste('Accuracy', 1 - misclass.all.eyes))
# calculate false negative #
fn <- sum(pred.all.eyes == "No" & test$PGON == "Yes")
fn
n <- sum(pred.all.eyes == "Yes" & test$PGON == "Yes")
n
f.neg.log <- fn/(fn + n)
f.neg.log # ouch :( 

#### model 2 -  fit logistic regression model eliminating eye measures #####
fit.all <- glm(formula = PGON ~ Trt + age + race + gender + history + sysbb + migr + lbp + stroke + heart + hbp + diab + myopia + hbp, family = binomial(link = "logit"),data = train)
summary(fit.all)
# variable selection using stepwise selection #
stepAIC(fit.all, trace = FALSE)
# fit with significant variables #
fit.sig <- glm(formula = PGON ~ Trt + age + lbp + heart, family = binomial(link = "logit"),data = train)
summary(fit.sig)
# predictions using model 2 #
pred.sig <- predict(fit.sig, newdata = test, type = "response")
# misclassifcation error/accuracy # 
pred.sig <- ifelse(pred.sig > .5, "Yes","No")
misclass.sig <- mean(pred.sig != test$PGON)
print(paste('Accuracy', 1 - misclass.sig))
# false negative calculation #
fn.sig <- sum(pred.sig == "No" & test$PGON == "Yes")
fn.sig
n.sig <- sum(pred.sig == "Yes" & test$PGON == "Yes")
n.sig
f.neg.log.sig <- fn.sig/(fn.sig + n.sig)
f.neg.log.sig # even worse ouch :(

#### classification tree #### 

## model 3 - first try - no adjustments ## 
my.control <- rpart.control(cp=0, xval=0)
fit.class <- rpart(PGON ~ ., data=train, method="class", control = my.control)
printcp(fit.class)
# using most complex tree # 
plot(fit.class,uniform=T, margin=0.2)
text(fit.class,use.n=T)
# predict PGON outcome using test data # 
pred.1<-predict(fit.class,newdata=test,type="class")
table.all <- table(test$PGON,pred.1) # table of predictions vs actual values
misclass.class <- (table.all[2,1] + table.all[1,2])/sum(table.all)
misclass.class # misclassification #
1 - misclass.class # accuracy #
false.neg.all <- table.all[2,1] / (table.all[2,1] + table.all[2,2]) # false negative
false.neg.all # ouch :(

## model 4 - eliminate eye measures ##
fit.demo <- rpart(PGON ~ Trt + age + race + gender + history + sysbb + migr + lbp + stroke + heart + hbp + diab + myopia + hbp, data = train, method = "class", control = my.control)
printcp(fit.demo)
# using most complicated tree # 
plot(fit.demo,uniform=T, margin=0.1)
text(fit.demo,use.n=T)
# predictions using model 4 #
pred.demo<-predict(fit.demo,newdata=test,type="class")
table.demo <- table(test$PGON,pred.demo) # misclassification rate
table.demo
misclass.class <- (table.demo[2,1] + table.demo[1,2])/sum(table.demo)
misclass.class
1 - misclass.class
false.neg.demo <- table.demo[2,1] / (table.demo[2,1] + table.demo[2,2]) # false negative 
false.neg.demo # ouch :( 

## model 5 - introduce loss matrix to reduce awful false negative result ##
fit.class.loss <- rpart(PGON ~ Trt  + age + race + gender + history + sysbb + migr + lbp + stroke + heart + hbp + diab + myopia + hbp, data=train, method="class", parms=list(split="information", loss=matrix(c(0,1,10,0), byrow = TRUE,nrow = 2)),control=my.control)
printcp(fit.class.loss)
# pruned based on lowest misclassification error # 
prune.loss <- prune(fit.class.loss, cp = (fit.class.loss$cptable[6,1]))
printcp(prune.loss)
plot(prune.loss,uniform=T, margin=0.1)
text(prune.loss,use.n=T)
# predict model 5 with test data #
pred.loss<-predict(prune.loss,newdata=test,type="class")
# misclassification error #
table.loss <- table(test$PGON,pred.loss) # table of predicted vs actual results
misclass.class <- (table.loss[2,1] + table.loss[1,2])/sum(table.loss)
misclass.class
1 - misclass.class # accuracy
false.neg.loss <- table.loss[2,1] / (table.loss[2,1] + table.loss[2,2]) # false negative
false.neg.loss # getting better

## model 6 - introduce 10 fold cross validation and keep loss matrix from model 5 ## 
cross.validate <- rpart.control(cp = 0, xval= 10)
fit.class.cv<- rpart(PGON ~ Trt  + age + race + gender + history + sysbb + migr + lbp + stroke + heart + hbp + diab + myopia + hbp, data=eyes, method="class",parms=list(split="information", loss=matrix(c(0,1,10,0), byrow = TRUE,nrow = 2)),control=cross.validate)
printcp(fit.class.cv)
plotcp(fit.class.cv)
# prune using 1 - se rule # 
prune.cv <- prune(fit.class.cv,cp=(fit.class.cv$cptable[14,1]))
printcp(prune.cv)
# pruned for the tree plot to be readable #
prune.cv.plot <- prune(fit.class.cv,cp=(fit.class.cv$cptable[7,1]))
plot(prune.cv.plot,uniform=T, margin=0.2)
text(prune.cv.plot,use.n=T)
# predictions using model 6 #
predcv<-predict(prune.cv,newdata=eyes,type="class")
table.cv <- table(eyes$PGON,predcv) # table of predictions vs actual 
# misclassification rate
misclass.cv <- (table.cv[2,1] + table.cv[1,2])/sum(table.cv)
misclass.cv
1 - misclass.cv # accuracy #
false.neg.cv <- table.cv[2,1] / (table.cv[2,1] + table.cv[2,2]) # false negative #
false.neg.cv # better - finally :) still shouldn't be used for predictions though.




printcp(fit.class)
plot(fit.class,uniform=T, margin=0.2)
text(fit.class,use.n=T)

pred8<-predict(fit.class,newdata=test,type="class")

table(test$PGON,pred8)
misclass.class <- (29 + 73)/(498 + 2 + 44 + 3)
misclass.class
1 - misclass.class

tree8<-prune(fit.class,cp=(fit.class$cptable[6,1]+fit.class$cptable[7,1])/2)
plot(tree8,uniform=T, margin=0.2)
text(tree8,use.n=T)
printcp(tree)

table(test$PGON,tree8)
misclass.class <- (29 + 73)/(498 + 2 + 44 + 3)
misclass.class
1 - misclass.class


### random forest ### 

library(randomForest)
fit.rf <- randomForest(as.factor(PGON) ~Trt  + age + race + gender + history + sysbb + migr + lbp + stroke + heart + hbp + diab + myopia + hbp, data=eyes, importance=TRUE, mtry=4, ntree=1000)
fit.rf
varImpPlot(fit.rf, main = "Variable Importance Plot")
importance(fit.rf)
x <- importance(fit.rf)[,4]
x[order(-x)[1:5]]


