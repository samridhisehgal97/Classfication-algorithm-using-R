diabetes= read.csv(file.choose())

View(diabetes)
dim(diabetes)
str(diabetes)
names(diabetes)
summary(diabetes)
pairs(diabetes,col = c("yellow", "red", "green", "blue"))
cor(diabetes)

###############################################
MODEL CREATION BY GLM FUNCTION
###############################################
attach(diabetes)
Logistic.Regression <- glm(Outcome ~ .,data = diabetes, family = "binomial")

windows()
par(mfrow=c(2,2)) 
plot(Logistic.Regression)

summary(Logistic.Regression)
contrasts(as.factor(diabetes$Outcome))

diabetes.Probability.Predicted <- predict(Logistic.Regression, type = "response");diabetes.Probability.Predicted
diabetes.Probability.Predicted[1:10]

max(diabetes.Probability.Predicted)
min(diabetes.Probability.Predicted)
sort(predict(Logistic.Regression, type = "response"),decreasing = TRUE)[1:10] 
sort(predict(Logistic.Regression, type = "response"),decreasing = FALSE)[1:10] 


Predicted.diabetes.Yes_No = rep("No",768)
Predicted.diabetes.Yes_No[diabetes.Probability.Predicted>0.5]='Yes'
Predicted.diabetes.Yes_No

table(Outcome)
table(Predicted.diabetes.Yes_No)

table(Predicted.diabetes.Yes_No,Outcome)
mean(Predicted.diabetes.Yes_No==Outcome)

#######################################################
SPLITTING OF DATASET ON THE BASIS OF AGE > 40
######################################################
summary(Age)
sort(Age, decreasing = TRUE)[1:20]

diabetes.train = (Age < 41)
diabetes.train
diabetes.test = diabetes[!diabetes.train,]
dim(diabetes.test)
Outcome.test = Outcome[!diabetes.train]
Outcome.test

glm.fit <- glm(Outcome~.,
               data = diabetes,family = "binomial",subset = diabetes.train)

glm.pred = predict(glm.fit,diabetes.test,type = "response")

glm.prob <- rep("NO",194)
glm.prob[glm.pred > 0.5]="Yes"


table(glm.prob, Outcome.test)
mean(glm.prob == Outcome.test)

###############################################
LINEAR DISCRIMINANT ANALYSIS
###############################################
library(MASS)

lda.fit = lda(Outcome~ . , data = diabetes , subset = diabetes.train)
lda.fit

lda.pred = predict(lda.fit ,diabetes.test)
names(lda.pred)

lda.class = lda.pred$class
lda.class[1:10]
table(lda.class , Outcome.test)
mean(lda.class == Outcome.test)


###############################################
Quadratic DISCRIMINANT ANALYSIS
###############################################

qda.fit = qda(Outcome~ . , data = diabetes , subset = diabetes.train)
qda.fit
qda.class = predict(qda.fit , diabetes.test)$class
qda.class
table(qda.class , Outcome.test)
mean(qda.class == Outcome.test)


windows(); plot(lda.fit)
par(mfrow = c(1,1))


###############################################
K- NEAREST NEIGHBOUR
###############################################
library(class)
names(diabetes)

diabetes.train = (Age<41)
dim(diabetes.train)
str(diabetes.train)
diabetes.test = diabetes[!diabetes.train,]
dim(diabetes.test)

Outcome.test = Outcome[!diabetes.train]

Outcome.train= Outcome[diabetes.train]

train.X = cbind(Insulin,Glucose, BloodPressure, Age, Pregnancies)[diabetes.train,]
train.X
test.X = cbind(Insulin,Glucose, BloodPressure, Age, Pregnancies)[!diabetes.train,]
test.X


set.seed(123)
knn.pred = knn(train.X, test.X, Outcome.train, k=4)

summary(knn.pred)

table(knn.pred, Outcome.test)
mean(knn.pred == Outcome.test)



