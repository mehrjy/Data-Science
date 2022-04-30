#mehrnaz jalili code 18
setwd("D:/data science")
data_assign<-read.csv("college.csv")
str(data_assign)
class(data_assign)
dim(data_assign)
summary(data_assign)
sum(is.na(data_assign))
View(data_assign)
head(data_assign)
tail(data_assign)
# there isn't any NA
#required libraries---------
library("MASS")
library("caret")
library("glmnet")
library("rpart")
library("rpart.plot")
library("leaps")
library("randomForest")
library("gbm")
library("xgboost")
library("ggplot2")

# explanation about data-------
#Private: A factor with levels No and Yes indicating private or public university
# Apps : Number of applications received
#Accept:Number of applications accepted
#Enroll:Number of new students enrolled
#Top10perc:Pct. new students from top 10% of H.S. class
#Top25perc:Pct. new students from top 25% of H.S. class 
#F.Undergrad: Number of fulltime undergraduates
#P.Undergrad:Number of parttime undergraduates
#Outstate: Out-of-state tuition
#Room.Board: Room and board costs
#Books: Estimated book costs
#Personal: Estimated personal spending
#PhD: Pct. of faculty with Ph.D.'s
#Terminal: Pct. of faculty with terminal degree
#S.F.Ratio: Student/faculty ratio
#perc.alumni: Pct. alumni who donate
#Expend: Instructional expenditure per student
# Grad.Rate: Graduation rate
#exploratory analysis--------
# change variable private to factor
data_assign$Private<-factor(data_assign$Private,levels = c("Yes","No"))
str(data_assign)
#remove first variable
rownames(data_assign)<-data_assign[,1]
data_1<- data_assign[,-1]
str(data_1)
View(data_1)
summary(data_1)
Elite <- rep("No", nrow(data_1))
Elite[data_1$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
data_1 <- data.frame(data_1, Elite)
#how many elite universities there are
summary(data_1$Elite)
plot(data_1$Outstate,data_1$Elite)#logestic
#It is strange that there is a college with more than 100% of percentage in phd
row.names(data_1[data_1$PhD >100,])#"Texas A&M University at Galveston"
row.names(data_1[data_1$Apps>25000, ])#"Rutgers at New Brunswick is an isolated university
row.names(data_1[data_1$Grad.Rate >100,])#"Cazenovia College"
# university with the most students in the top 10% of class
row.names(data_1)[which.max(data_1$Top10perc)]
# lowest acceptance rate
acceptance_rate <- data_1$Accept / data_1$Apps
row.names(data_1)[which.min(acceptance_rate)] 
# highest acceptance rate
row.names(data_1)[which.max(acceptance_rate)]
# High tuition correlates to high graduation rate
plot(data_1$Grad.Rate ~data_1$Outstate) 
# Colleges with low acceptance rate tend to have low S:F ratio.
plot(S.F.Ratio ~ I(Accept/Apps), data = data_1)
# Colleges with the most students from top 10% perc don't necessarily have the highest graduation rate. Also, rate > 100 is erroneous!
plot(Grad.Rate ~ Top10perc, data = data_1)
plot(data_1$Elite, data_1$Outstate, main = "Plot of Outstate vs. Elite", xlab = "Elite", ylab = "Outstate")
par(mfrow = c(2,2))
plot(data_1$Outstate, data_1$Room.Board, xlab = "Outstate", ylab = "Room and board costs")
plot(data_1$Outstate, data_1$Personal, xlab = "Outstate", ylab = "Personal spending")
plot(Elite, data_1$Room.Board, xlab = "Elite", ylab = "Room.Board")
plot(Elite, data_1$Personal, xlab = "Elite", ylab = "Personal")
#Out-of-state tuition fees have a positive relationship with room and board costs. However, personal spending seems comparable across individuals regardless of whether they attend an elite college with higher tuition fees.
par(mfrow = c(2,2))
plot(Elite, data_1$Accept/data_1$Apps, xlab = "Elite", ylab = "Accept/Apps")
plot(Elite, data_1$Grad.Rate, xlab = "Elite", ylab = "Grad.Rate")
plot(Elite, data_1$PhD, xlab = "Elite", ylab = "PhD")
plot(Elite, data_1$S.F.Ratio, xlab = "Elite", ylab = "S.F.Ratio")
#Elite colleges have lower acceptance rates and higher graduation rates than non-elite colleges. Elite colleges also have a higher percentage of faculty with PhDs and a lower student-faculty ratio compared to non-elite colleges.
par(mfrow = c(1,2))
plot(data_1$Top10perc, data_1$Grad.Rate, xlab = "Top10perc", ylab = "Grad.Rate")
hist(data_1$Grad.Rate, main = NULL, xlab = "Grad.Rate")
par(mfrow=c(1,1))
#However, colleges with the highest number of students from the top 10% of their high school class do not necessarily have the highest grduation rate. In addition, a closer inspection of the graduation rate also revealed a rate of more than 100, which is likely to be erroneous.
par(mar=c(2,2,2,2))
par(mfrow=c(4,5))
figure_1<- for(i in 2:18){
  hist(data_1[,i],xlab="", main = paste("Histogram of",names(data_1)[i]))
}
par(mar=c(5, 4, 4, 2) + 0.1)
par(mfrow=c(1,1))
table(data_1$Private)
cor(data_1$Apps, data_1$Accept)#high correlation
cor(data_1$Apps, data_1$Enroll)
cor(data_1$Accept,data_1$Enroll)#high
cor_table<-round(cor(data_1[,2:18]),2)
cor_table
corrplot::corrplot(cor_table)
par(mar=c(2,2,2,2))
par(mfrow=c(4,5))
for(i in 2:18){
  plot(data_1[,i],data_1$Apps,xlab="", main = paste("Apps of",names(data_1)[i]))
}
par(mar=c(5, 4, 4, 2) + 0.1)
par(mfrow=c(1,1))
tapply(data_1$Apps,list(data_1$Private),mean)
#split data set-------
set.seed(1234)
train_case<-sample(1:nrow(data_1),0.8*nrow(data_1))
train<- data_1[train_case,]
test<- data_1[-train_case,]
dim(train)
dim(test)
summary(train)
summary(test)
#model regression-------
model1_reg<-lm(Apps~.,data = train) 
summary(model1_reg)#R^2=0.9304 , R^2 adj=0.9283
#check normality assumption
hist(model1_reg$residuals,probability = TRUE)
lines(density(model1_reg$residuals),col="red")
qqnorm(model1_reg$residuals)
qqline(model1_reg$residuals)
moments::anscombe.test(model1_reg$residuals)
moments::jarque.test(model1_reg$residuals)
#residuals model1_reg are not normally
#plot(model1_reg)
#prediction
pred_model1_reg<-predict(model1_reg,test)
#absolute errors
abs_err_model1_reg <- abs(pred_model1_reg - test$Apps)
mean(abs_err_model1_reg)
median(abs_err_model1_reg)
sd(abs_err_model1_reg)
IQR(abs_err_model1_reg)
min(abs_err_model1_reg)
max(abs_err_model1_reg)
range(abs_err_model1_reg)
mse_model1_reg<-mean((test$Apps-pred_model1_reg )^2)
#plot actual vs predicted
plot(test$Apps, pred_model1_reg, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
#P.Undergrad,Books,Personal,PhD,S.F.Ratio ,perc.alumni,EliteYes: Not significant
model2_reg<-lm(Apps~Private+Accept+Enroll +Top10perc+Top25perc+F.Undergrad+Outstate+Room.Board +Expend+Grad.Rate+Terminal,data = train )
summary(model2_reg)#R^2= 0.9295 , R^2 adj=0.9282
#check normallity assumption
hist(model2_reg$residuals,probability = TRUE)
lines(density(model2_reg$residuals),col="red")
qqnorm(model2_reg$residuals)
qqline(model2_reg$residuals)
moments::anscombe.test(model2_reg$residuals)
moments::jarque.test(model2_reg$residuals)
#residuals model2_reg are not normally
#plot(model1_reg)
#plot(model2_reg)
car::vif(model1_reg) # Enroll, F.Undergrad
car::vif(model2_reg)# Enroll, F.Undergrad
#prediction
pred_model2_reg<-predict(model2_reg,test)
#absolute errors
abs_err_model2_reg <- abs(pred_model2_reg - test$Apps)
mean(abs_err_model2_reg)
median(abs_err_model2_reg)
sd(abs_err_model2_reg)
IQR(abs_err_model2_reg)
min(abs_err_model2_reg)
max(abs_err_model2_reg)
range(abs_err_model2_reg)
mse_model2_reg<-mean((pred_model2_reg - test$Apps )^2)
#plot actual vs predicted
plot(test$Apps, pred_model2_reg, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
hist(abs_err_model2_reg, breaks = 15)
boxplot(abs_err_model2_reg)
#identify outliers
plot(predict(model2_reg),rstudent(model2_reg))
#There are possible outliers as seen in the plot of studentized residuals because there are data with a value greater than 3.
cooks_model2_reg<-cooks.distance(model2_reg)
cooks_model2_reg[cooks_model2_reg>1]#Rutgers at New Brunswick 
tukey_model2_reg<-quantile(data_1$Apps,probs = 0.75)+1.5*IQR(data_1$Apps)
sum(data_1$Apps>tukey_model2_reg)# great than 0.01
#remove variables"Texas A&M University at Galveston","Rutgers at New Brunswick ,"Cazenovia College"
train_trimmed_reg<- train[-which(rownames(train)%in% c("Rutgers at New Brunswick","Texas A&M University at Galveston","Cazenovia College")),]
model1_reg_trimmed<-lm(Apps~.,data = train_trimmed_reg)
summary(model1_reg_trimmed) #R^2=0.9253 , R^2 adj=0.923
model2_reg_trimmed<-lm(Apps~Private+Accept+Enroll +Top10perc+Top25perc+F.Undergrad+Outstate+Room.Board +Expend+Grad.Rate+Terminal,data = train_trimmed_reg)
summary(model2_reg_trimmed) #R^2= 0.9237 , R^2 adj=0.9223 
#check normallity assumption
hist(model2_reg_trimmed$residuals,probability = TRUE)
lines(density(model2_reg_trimmed$residuals),col="red")
qqnorm(model2_reg_trimmed$residuals)
qqline(model2_reg_trimmed$residuals)
moments::anscombe.test(model2_reg_trimmed$residuals)
moments::jarque.test(model2_reg_trimmed$residuals)
#residuals model2_reg_trimmed are not normally
#plot(model2_reg_trimmed)
car::vif(model2_reg_trimmed) # Enroll, F.Undergrad
#prediction
pred_model2_reg_trimmed<-predict(model2_reg_trimmed,test)
#absolute errors
abs_err_model2_reg_trimmed <- abs(pred_model2_reg_trimmed - test$Apps)
mean(abs_err_model2_reg_trimmed)
median(abs_err_model2_reg_trimmed)
sd(abs_err_model2_reg_trimmed)
IQR(abs_err_model2_reg_trimmed)
min(abs_err_model2_reg_trimmed)
max(abs_err_model2_reg_trimmed)
range(abs_err_model2_reg_trimmed)
mse_model2_reg_trimmed<-mean((pred_model2_reg_trimmed - test$Apps)^2)
plot(test$Apps, pred_model2_reg_trimmed, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
coef(model2_reg)
coef(model2_reg_trimmed)
#box_cox transformation
box_result<- boxcox(Apps~.,data=train,lambda = seq(-5,5,0.1))
box_result<-data.frame(box_result$x,box_result$y)
lambda<-box_result[which(box_result$box_result.y==max(box_result$box_result.y)),1]
lambda #0.5
train$boxcox_apps<-((train$Apps^lambda)-1)/lambda
test$boxcox_apps<-((test$Apps^lambda)-1)/lambda
dim(train)
colnames(train)
#model bocox
model3_boxcox_reg<-lm(boxcox_apps~.-Apps,data = train)
summary(model3_boxcox_reg)
model4_boxcox_reg<-lm(boxcox_apps~Private+Accept+Enroll+Top10perc+Outstate +Room.Board+S.F.Ratio +perc.alumni+Expend +Grad.Rate,data = train)
summary(model4_boxcox_reg)
#check normallity assumption
hist(model4_boxcox_reg$residuals,probability = TRUE)
lines(density(model4_boxcox_reg$residuals),col="red")
qqnorm(model4_boxcox_reg$residuals)
qqline(model4_boxcox_reg$residuals)
moments::anscombe.test(model4_boxcox_reg$residuals)
moments::jarque.test(model4_boxcox_reg$residuals)
#plot(model4_boxcox_reg)
car::vif(model4_boxcox_reg)#nothing
#prediction
pred_model4_boxcox_reg<-predict(model4_boxcox_reg,test)
pred_model4_boxcox_reg<-((lambda*pred_model4_boxcox_reg)+1)^(1/lambda)
#absolute errors
abs_err_model4_boxcox_reg <- abs(pred_model4_boxcox_reg - test$Apps)
mean(abs_err_model4_boxcox_reg)
median(abs_err_model4_boxcox_reg)
sd(abs_err_model4_boxcox_reg)
IQR(abs_err_model4_boxcox_reg)
min(abs_err_model4_boxcox_reg)
max(abs_err_model4_boxcox_reg)
range(abs_err_model4_boxcox_reg)
mse_boxcox_reg<-mean((pred_model4_boxcox_reg - test$Apps)^2)
plot(test$Apps,pred_model4_boxcox_reg, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
#boxcox trimmed
train_trimmed_reg$boxcox_apps<-((train_trimmed_reg$Apps^lambda)-1)/lambda
model4_boxcox_reg_trimmed<-lm(boxcox_apps~.-Apps,data = train_trimmed_reg)
summary(model4_boxcox_reg_trimmed)
model5_boxcox_reg_trimmed<-lm(boxcox_apps~Private+Accept+Top10perc+P.Undergrad  +Room.Board+S.F.Ratio +perc.alumni+Expend +Grad.Rate,data = train_trimmed_reg)
summary(model5_boxcox_reg_trimmed)
#check normallity assumption
hist(model5_boxcox_reg_trimmed$residuals,probability = TRUE)
lines(density(model5_boxcox_reg_trimmed$residuals),col="red")
qqnorm(model5_boxcox_reg_trimmed$residuals)
qqline(model5_boxcox_reg_trimmed$residuals)
moments::anscombe.test(model5_boxcox_reg_trimmed$residuals)
moments::jarque.test(model5_boxcox_reg_trimmed$residuals)
#plot(model5_boxcox_reg_trimmed)
car::vif(model5_boxcox_reg_trimmed)#nothing
#prediction
pred_model5_boxcox_reg_trimmed<-predict(model5_boxcox_reg_trimmed,test)
pred_model5_boxcox_reg_trimmed<-((lambda*pred_model5_boxcox_reg_trimmed)+1)^(1/lambda)
#absolute errors
abs_err_model5_boxcox_reg_trimmed <- abs(pred_model5_boxcox_reg_trimmed - test$Apps)
mean(abs_err_model5_boxcox_reg_trimmed)
median(abs_err_model5_boxcox_reg_trimmed)
sd(abs_err_model5_boxcox_reg_trimmed)
IQR(abs_err_model5_boxcox_reg_trimmed)
min(abs_err_model4_boxcox_reg)
max(abs_err_model4_boxcox_reg)
range(abs_err_model5_boxcox_reg_trimmed)
mse_boxcox_reg<-mean((pred_model5_boxcox_reg_trimmed - test$Apps)^2)
plot(test$Apps,pred_model5_boxcox_reg_trimmed, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
#Comparisons of Models--------------------------
df <- data.frame("Model_1" = abs_err_model1_reg,
                 "Model_2" = abs_err_model2_reg, 
                 "Model_3" = abs_err_model2_reg_trimmed, 
                 "Model_4" = abs_err_model4_boxcox_reg,
                 "Model_5"=abs_err_model5_boxcox_reg_trimmed)
models_comp <- data.frame("Mean of AbsErrors"   = apply(df, 2, mean),
                          "Median of AbsErrors" = apply(df, 2, median),
                          "SD of AbsErrors"  = apply(df, 2, sd),
                          "MSE of each model"=apply(df,2,function(x)mean(x^2)),
                          "IQR of AbsErrors" = apply(df, 2, IQR),
                          "Min of AbsErrors" = apply(df, 2, min),
                          "Max of AbsErrors" = apply(df, 2, max)
                          )
                                    
rownames(models_comp) <- c("LM full parameter","LM tune parameter", "LM trimmed", "LM boxcox","Lm boxcox trimmed")                        
View(models_comp)
set.seed(123)
cv_model1_reg<-train(Apps~Private+Accept+Enroll +Top10perc+Top25perc+F.Undergrad+Outstate+Room.Board +Expend+Grad.Rate+Terminal,
                     data=data_1,
                     method="lm",
                     trcontrol=trainControl(method = "cv",number = 10))
cv_model2_reg<-train(Apps~.,
                     data=data_1,
                     method="lm",
                     trcontrol=trainControl(method = "cv",number = 10))
summary(resamples(list("model1"=cv_model1_reg," model2"=cv_model2_reg)))
#RMSE model1 is less than model2
#Boxplot of absolute errors
boxplot(df, main = "Abs. Errors Dist. of Models")
#Forward stepwise selection------
fw_1<-regsubsets(Apps~.-boxcox_apps,nvmax = 18,data = train,method = "forward")
summary(fw_1)
which.max(summary(fw_1)$rsq)
which.max(summary(fw_1)$adjr2)#13
which.min(summary(fw_1)$cp)#13
which.min(summary(fw_1)$bic)#9
coef(fw_1,13)
model_fw_1<-lm(Apps~Private+Accept+ Enroll+ Top10perc+ Top25perc+F.Undergrad+ P.Undergrad+Outstate+Room.Board+ PhD+Expend+Grad.Rate+Elite,data = train)
summary(model_fw_1)
pred_fw_1<-predict(model_fw_1,test)
#absolute errors
abs_err_fw_1 <- abs(pred_fw_1- test$Apps)
models_comp <- rbind(models_comp, "Forward" = c(mean(abs_err_fw_1),
                                                median(abs_err_fw_1),
                                                sd(abs_err_fw_1),
                                                mean(abs_err_fw_1^2),
                                                IQR(abs_err_fw_1),
                                                range(abs_err_fw_1)))
#Forward with train trimmed
fw_2<-regsubsets(Apps~.-boxcox_apps,nvmax = 18,data = train_trimmed_reg,method = "forward")
summary(fw_2)
which.max(summary(fw_2)$rsq)
which.max(summary(fw_2)$adjr2)#14
which.min(summary(fw_2)$cp)#13
which.min(summary(fw_2)$bic)#10
coef(fw_2,14)
model_fw_trimmed<-lm(Apps~Private+Accept+ Enroll+ Top10perc+ Top25perc+F.Undergrad+ P.Undergrad+Outstate+Room.Board+ PhD+ perc.alumni+Expend+Grad.Rate+Elite,data = train_trimmed_reg)
summary(model_fw_trimmed)
pred_fw_trimmed<-predict(model_fw_trimmed,test)
#absolute errors
abs_err_fw_trimmed <- abs(pred_fw_trimmed- test$Apps)
models_comp <- rbind(models_comp, "Forward trimmed" = c(mean(abs_err_fw_trimmed),
                                                median(abs_err_fw_trimmed),
                                                sd(abs_err_fw_trimmed),
                                                mean(abs_err_fw_trimmed^2),
                                                IQR(abs_err_fw_trimmed),
                                                range(abs_err_fw_trimmed)))
#Forward with boxcox
fw_3<-regsubsets(boxcox_apps~.-Apps,nvmax = 18,data = train,method = "forward")
summary(fw_3)
which.max(summary(fw_3)$rsq)
which.max(summary(fw_3)$adjr2)#15
which.min(summary(fw_3)$cp)#12
which.min(summary(fw_3)$bic)#9
coef(fw_3,15)
model_fw_3<-lm(boxcox_apps~Private+Accept+ Enroll+ Top10perc+ P.Undergrad+Outstate+Room.Board+ Books+Personal+ PhD+ S.F.Ratio+perc.alumni+Expend+Grad.Rate+Elite,data = train)
summary(model_fw_3)
pred_fw_3<-predict(model_fw_3,test)
pred_fw_3<-((lambda*pred_fw_3)+1)^(1/lambda)
#absolute errors
abs_err_fw_boxcox <- abs(pred_fw_3- test$Apps)
models_comp <- rbind(models_comp, "Forward boxcox" = c(mean(abs_err_fw_boxcox),
                                                median(abs_err_fw_boxcox),
                                                sd(abs_err_fw_boxcox),
                                                mean(abs_err_fw_boxcox^2),
                                                IQR(abs_err_fw_boxcox),
                                                range(abs_err_fw_boxcox)))
#Forward with boxcox trimmed
fw_4<-regsubsets(boxcox_apps~.-Apps,nvmax = 18,data = train_trimmed_reg,method = "forward")
summary(fw_4)
which.max(summary(fw_4)$rsq)
which.max(summary(fw_4)$adjr2)#15
which.min(summary(fw_4)$cp)#11
which.min(summary(fw_4)$bic)#9
coef(fw_4,15)
model_fw_4<-lm(boxcox_apps~Private+Accept+ Enroll+ Top10perc+ P.Undergrad+Outstate+Room.Board+ Books+Personal+ PhD+ S.F.Ratio+perc.alumni+Expend+Grad.Rate+Elite,data = train_trimmed_reg)
summary(model_fw_4)
pred_fw_4<-predict(model_fw_4,test)
pred_fw_4<-((lambda*pred_fw_4)+1)^(1/lambda)
#absolute errors
abs_err_fw_boxcox_trimmed <- abs(pred_fw_4- test$Apps)
models_comp <- rbind(models_comp, "Forward boxcox trimmed" = c(mean(abs_err_fw_boxcox_trimmed),
                                                       median(abs_err_fw_boxcox_trimmed),
                                                       sd(abs_err_fw_boxcox_trimmed),
                                                       mean(abs_err_fw_boxcox_trimmed^2),
                                                       IQR(abs_err_fw_boxcox_trimmed),
                                                       range(abs_err_fw_boxcox_trimmed)))

# Using K-fold Cross-Validation Approach
k <- 10
set.seed(123)
folds <- sample(1:k, nrow(train), rep = TRUE)
cv_errors_1 <- matrix(NA, k, 18, dimnames = list(NULL , paste(1:18)))
cv_errors_1

#Prediction function
predict_regsubsets_fw <- function(object, newdata, id) {
  reg_formula_fw <- as.formula(object$call[[2]])
  mat_fw    <- model.matrix(reg_formula_fw, newdata)
  coef_i_fw <- coef(object, id = id)
  mat_fw[, names(coef_i_fw)] %*% coef_i_fw
}
#K-fold Cross Validation Forward
set.seed(1234)
for(i in 1:k){
  best_fit_fw <- regsubsets(Apps~.-boxcox_apps, data = train[folds != i,], nvmax = 18, method = "forward")
  for(j in 1:18){
    pred_fw <- predict_regsubsets_fw(best_fit_fw, newdata = train[folds == i,], id = j)
    cv_errors_1[i, j] <- mean((train$Apps[folds == i] - pred_fw) ^ 2)
  }
}

View(cv_errors_1)
mean_cv_erros_1 <- apply(cv_errors_1, 2, mean)
mean_cv_erros_1 
plot(mean_cv_erros_1, type = "b")
which.min(mean_cv_erros_1)
#Coefficients of the backward model
coef(fw_1, 2)
fw_cv_1<-lm(Apps~ Accept+Top10perc,data = train)
summary(fw_cv_1)
#Prediction
pred_fw_cv_1 <- predict(fw_cv_1, test)
#absolute errors
abs_err_fw_cv_1 <- abs(pred_fw_cv_1- test$Apps)
models_comp <- rbind(models_comp, "Forwardward cv" = c(mean(abs_err_fw_cv_1),
                                                       median(abs_err_fw_cv_1),
                                                       sd(abs_err_fw_cv_1),
                                                       mean(abs_err_fw_cv_1^2),
                                                       IQR(abs_err_fw_cv_1),
                                                       range(abs_err_fw_cv_1)))


#K-fold Cross-Validation for forward trimmed
k <- 10
set.seed(123)
folds <- sample(1:k, nrow(train_trimmed_reg), rep = TRUE)
cv_errors_fw <- matrix(NA, k, 18, dimnames = list(NULL , paste(1:18)))
cv_errors_fw

#Prediction function
predict_regsubsets_fw_t <- function(object, newdata, id) {
  reg_formula_fw_t <- as.formula(object$call[[2]])
  mat_fw_t    <- model.matrix(reg_formula_fw_t, newdata)
  coef_i_fw_t<- coef(object, id = id)
  mat_fw_t[, names(coef_i_fw_t)] %*% coef_i_fw_t
}
#K-fold Cross Validation
set.seed(1234)
for(i in 1:k){
  best_fit_fw_t <- regsubsets(Apps~.-boxcox_apps, data = train_trimmed_reg[folds != i,], nvmax = 18, method = "forward")
  for(j in 1:18){
    pred_fw_cv_t <- predict_regsubsets_fw_t(best_fit_fw_t, newdata = train_trimmed_reg[folds == i,], id = j)
    cv_errors_fw[i, j] <- mean((train_trimmed_reg$Apps[folds == i] - pred_fw_cv_t) ^ 2)
  }
}

View(cv_errors_fw)
mean_cv_erros_fw <- apply(cv_errors_fw, 2, mean)
mean_cv_erros_fw 
plot(mean_cv_erros_fw, type = "b")
which.min(mean_cv_erros_fw)
#Coefficients of the backward model
coef(fw_2, 13)
fw_cv_trimmed<-lm(Apps~ Private+ Accept+Enroll+Top10perc+Top25perc+F.Undergrad+ Outstate+ Room.Board+PhD+perc.alumni+ Expend+Grad.Rate+Elite,data = train_trimmed_reg)
summary(fw_cv_trimmed)
#Prediction
pred_fw_cv_trimmed <- predict(fw_cv_trimmed, test)
#absolute errors
abs_err_fw_cv_trimmed <- abs(pred_fw_cv_trimmed- test$Apps)
models_comp <- rbind(models_comp, "Forward cv trimmed" = c(mean(abs_err_fw_cv_trimmed),
                                                            median(abs_err_fw_cv_trimmed),
                                                            sd(abs_err_fw_cv_trimmed),
                                                            mean(abs_err_fw_cv_trimmed^2),
                                                            IQR(abs_err_fw_cv_trimmed),
                                                            range(abs_err_fw_cv_trimmed)))

#Backward stepwise selection------
bw_1<-regsubsets(Apps~.-boxcox_apps,nvmax = 18,data = train,method = "backward")
summary(bw_1)
which.max(summary(bw_1)$rsq)
which.max(summary(bw_1)$adjr2)#13
which.min(summary(bw_1)$cp)#12
which.min(summary(bw_1)$bic)#8
coef(bw_1,13)
model_bw_1<-lm(Apps~Private+Accept+ Enroll+ Top10perc+ Top25perc+F.Undergrad+ P.Undergrad+Outstate+Room.Board+ PhD+Expend+Grad.Rate+Elite,data = train)
summary(model_bw_1)
pred_bw_1<-predict(model_bw_1,test)
#absolute errors
abs_err_bw_1 <- abs(pred_bw_1- test$Apps)
models_comp <- rbind(models_comp, "Backward" = c(mean(abs_err_bw_1),
                                                median(abs_err_bw_1),
                                                sd(abs_err_bw_1),
                                                mean(abs_err_bw_1^2),
                                                IQR(abs_err_bw_1),
                                                range(abs_err_bw_1)))
#backrward with train trimmed
bw_2<-regsubsets(Apps~.-boxcox_apps,nvmax = 18,data = train_trimmed_reg,method = "backward")
summary(bw_2)
which.max(summary(bw_2)$rsq)
which.max(summary(bw_2)$adjr2)#14
which.min(summary(bw_2)$cp)#13
which.min(summary(bw_2)$bic)#10
coef(bw_2,14)
model_bw_trimmed<-lm(Apps~Private+Accept+ Enroll+ Top10perc+ Top25perc+F.Undergrad+ P.Undergrad+Outstate+Room.Board+ PhD+ perc.alumni+Expend+Grad.Rate+Elite,data = train_trimmed_reg)
summary(model_bw_trimmed)
pred_bw_trimmed<-predict(model_bw_trimmed,test)
#absolute errors
abs_err_bw_trimmed <- abs(pred_bw_trimmed- test$Apps)
models_comp <- rbind(models_comp, "Backward trimmed" = c(mean(abs_err_bw_trimmed),
                                                        median(abs_err_bw_trimmed),
                                                        sd(abs_err_bw_trimmed),
                                                        mean(abs_err_bw_trimmed^2),
                                                        IQR(abs_err_bw_trimmed),
                                                        range(abs_err_bw_trimmed)))
#backrward with boxcox
bw_3<-regsubsets(boxcox_apps~.-Apps,nvmax = 18,data = train,method = "backward")
summary(bw_3)
which.max(summary(bw_3)$rsq)
which.max(summary(bw_3)$adjr2)#15
which.min(summary(bw_3)$cp)#12
which.min(summary(bw_3)$bic)#9
coef(bw_3,15)
model_bw_3<-lm(boxcox_apps~Private+Accept+ Enroll+ Top10perc+ P.Undergrad+Outstate+Room.Board+ Books+Personal+ PhD+ S.F.Ratio+perc.alumni+Expend+Grad.Rate+Elite,data = train)
summary(model_bw_3)
pred_bw_3<-predict(model_bw_3,test)
pred_bw_3<-((lambda*pred_bw_3)+1)^(1/lambda)
#absolute errors
abs_err_bw_boxcox <- abs(pred_bw_3- test$Apps)
models_comp <- rbind(models_comp, "Backward boxcox" = c(mean(abs_err_bw_boxcox),
                                                       median(abs_err_bw_boxcox),
                                                       sd(abs_err_bw_boxcox),
                                                       mean(abs_err_bw_boxcox^2),
                                                       IQR(abs_err_bw_boxcox),
                                                       range(abs_err_bw_boxcox)))
#backward with boxcox trimmed
bw_4<-regsubsets(boxcox_apps~.-Apps,nvmax = 18,data = train_trimmed_reg,method = "backward")
summary(bw_4)
which.max(summary(bw_4)$rsq)
which.max(summary(bw_4)$adjr2)#15
which.min(summary(bw_4)$cp)#11
which.min(summary(bw_4)$bic)#9
coef(bw_4,15)
model_bw_4<-lm(boxcox_apps~Private+Accept+ Enroll+ Top10perc+ P.Undergrad+Outstate+Room.Board+ Books+Personal+ PhD+ S.F.Ratio+perc.alumni+Expend+Grad.Rate+Elite,data = train_trimmed_reg)
summary(model_bw_4)
pred_bw_4<-predict(model_bw_4,test)
pred_bw_4<-((lambda*pred_bw_4)+1)^(1/lambda)
#absolute errors
abs_err_bw_boxcox_trimmed <- abs(pred_bw_4- test$Apps)
models_comp <- rbind(models_comp, "Backward boxcox trimmed" = c(mean(abs_err_bw_boxcox_trimmed),
                                                               median(abs_err_bw_boxcox_trimmed),
                                                               sd(abs_err_bw_boxcox_trimmed),
                                                               mean(abs_err_bw_boxcox_trimmed^2),
                                                               IQR(abs_err_bw_boxcox_trimmed),
                                                               range(abs_err_bw_boxcox_trimmed)))
# Using K-fold Cross-Validation Approach
k <- 10
set.seed(123)
folds <- sample(1:k, nrow(train), rep = TRUE)
cv_errors <- matrix(NA, k, 18, dimnames = list(NULL , paste(1:18)))
cv_errors

#Prediction function
predict_regsubsets <- function(object, newdata, id) {
  reg_formula <- as.formula(object$call[[2]])
  mat    <- model.matrix(reg_formula, newdata)
  coef_i <- coef(object, id = id)
  mat[, names(coef_i)] %*% coef_i
}
#K-fold Cross Validation
set.seed(1234)
for(i in 1:k){
  best_fit <- regsubsets(Apps~.-boxcox_apps, data = train[folds != i,], nvmax = 18, method = "backward")
  for(j in 1:18){
    pred <- predict_regsubsets(best_fit, newdata = train[folds == i,], id = j)
    cv_errors[i, j] <- mean((train$Apps[folds == i] - pred) ^ 2)
  }
}

View(cv_errors)
mean_cv_erros <- apply(cv_errors, 2, mean)
mean_cv_erros 
plot(mean_cv_erros, type = "b")
which.min(mean_cv_erros)
#Coefficients of the backward model
coef(bw_1, 2)
bw_cv_1<-lm(Apps~ Accept+Top10perc,data = train)
summary(bw_cv_1)
#Prediction
pred_bw_cv_1 <- predict(bw_cv_1, test)
#absolute errors
abs_err_bw_cv_1 <- abs(pred_bw_cv_1- test$Apps)
models_comp <- rbind(models_comp, "Backward cv" = c(mean(abs_err_bw_cv_1),
                                                                median(abs_err_bw_cv_1),
                                                                sd(abs_err_bw_cv_1),
                                                                mean(abs_err_bw_cv_1^2),
                                                                IQR(abs_err_bw_cv_1),
                                                                range(abs_err_bw_cv_1)))
#K-fold Cross-Validation for backward trimmed
k <- 10
set.seed(123)
folds <- sample(1:k, nrow(train_trimmed_reg), rep = TRUE)
cv_errors_bw <- matrix(NA, k, 18, dimnames = list(NULL , paste(1:18)))
cv_errors_bw

#Prediction function
predict_regsubsets_bw <- function(object, newdata, id) {
  reg_formula_bw <- as.formula(object$call[[2]])
  mat_bw    <- model.matrix(reg_formula_bw, newdata)
  coef_i_bw<- coef(object, id = id)
  mat_bw[, names(coef_i_bw)] %*% coef_i_bw
}
#K-fold Cross Validation
set.seed(1234)
for(i in 1:k){
  best_fit_bw <- regsubsets(Apps~.-boxcox_apps, data = train_trimmed_reg[folds != i,], nvmax = 18, method = "backward")
  for(j in 1:18){
    pred_bw_cv <- predict_regsubsets_bw(best_fit_bw, newdata = train_trimmed_reg[folds == i,], id = j)
    cv_errors_bw[i, j] <- mean((train_trimmed_reg$Apps[folds == i] - pred_bw_cv) ^ 2)
  }
}

View(cv_errors_bw)
mean_cv_erros_bw <- apply(cv_errors_bw, 2, mean)
mean_cv_erros_bw 
plot(mean_cv_erros_bw, type = "b")
which.min(mean_cv_erros_bw)
#Coefficients of the backward model
coef(bw_2, 13)
bw_cv_trimmed<-lm(Apps~ Private+ Accept+Enroll+Top10perc+Top25perc+F.Undergrad+ Outstate+ Room.Board+PhD+perc.alumni+ Expend+Grad.Rate+Elite,data = train_trimmed_reg)
summary(bw_cv_trimmed)
#Prediction
pred_bw_cv_trimmed <- predict(bw_cv_trimmed, test)
#absolute errors
abs_err_bw_cv_trimmed <- abs(pred_bw_cv_trimmed- test$Apps)
models_comp <- rbind(models_comp, "Backward cv trimmed" = c(mean(abs_err_bw_cv_trimmed),
                                                    median(abs_err_bw_cv_trimmed),
                                                    sd(abs_err_bw_cv_trimmed),
                                                    mean(abs_err_bw_cv_trimmed^2),
                                                    IQR(abs_err_bw_cv_trimmed),
                                                    range(abs_err_bw_cv_trimmed)))

#Ridge Regression------
x_ridge_train<-model.matrix(Apps~.-boxcox_apps,data = train)[,-1]
y_ridge_train<-train$Apps
#cross validation to choose the best model
set.seed(1234)
ridge_cv_1<-cv.glmnet(x_ridge_train,y_ridge_train,alpha=0,nfolds = 10)
ridge_cv_1$lambda.min
ridge_reg_1<-glmnet(x_ridge_train,y_ridge_train,alpha = 0,lambda =ridge_cv_1$lambda.min )
coef(ridge_reg_1)
x_ridge_test<-model.matrix(Apps~.-boxcox_apps,data = test)[,-1]
pred_ridge_1<-predict(ridge_reg_1,s=ridge_cv_1$lambda.min,newx = x_ridge_test)
#absolute errors
abs_err_ridge <- abs(pred_ridge_1 - test$Apps)
models_comp <- rbind(models_comp, "RidgeReg" = c(mean(abs_err_ridge),
                                                 median(abs_err_ridge),
                                                 sd(abs_err_ridge),
                                                 mean(abs_err_ridge^2),
                                                 IQR(abs_err_ridge),
                                                 range(abs_err_ridge)))
plot(test$Apps,pred_ridge_1,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#ridge regression for train trimmed 
x_ridge_train_trimmed<-model.matrix(Apps~.-boxcox_apps,data = train_trimmed_reg)[,-1]
y_ridge_train_trimmed<-train_trimmed_reg$Apps
#cross validation to choose the best model
set.seed(1234)
ridge_cv_trimmed<-cv.glmnet(x_ridge_train_trimmed,y_ridge_train_trimmed,alpha=0,nfolds = 10)
ridge_cv_trimmed$lambda.min
ridge_reg_2<-glmnet(x_ridge_train_trimmed,y_ridge_train_trimmed,alpha = 0,lambda =ridge_cv_trimmed$lambda.min )
coef(ridge_reg_2)
pred_ridge_trimmed<-predict(ridge_reg_2,s=ridge_cv_trimmed$lambda.min,newx = x_ridge_test)
#absolute errors
abs_err_ridge_trimmed <- abs(pred_ridge_trimmed - test$Apps)
models_comp <- rbind(models_comp, "RidgeReg trimmed" = c(mean(abs_err_ridge_trimmed ),
                                                 median(abs_err_ridge_trimmed ),
                                                 sd(abs_err_ridge_trimmed ),
                                                 mean(abs_err_ridge_trimmed ^2),
                                                 IQR(abs_err_ridge_trimmed ),
                                                 range(abs_err_ridge_trimmed )))
plot(test$Apps,pred_ridge_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#ridge regression boxcox
x_ridge_train_boxcox<-model.matrix(boxcox_apps~.-Apps,data = train)[,-1]
y_ridge_train_boxcox<-train$boxcox_apps
#cross validation to choose the best model
set.seed(1234)
ridge_cv_boxcox<-cv.glmnet(x_ridge_train_boxcox,y_ridge_train_boxcox,alpha=0,nfolds = 10)
ridge_cv_boxcox$lambda.min
ridge_reg_3<-glmnet(x_ridge_train_boxcox,y_ridge_train_boxcox,alpha = 0,lambda =ridge_cv_boxcox$lambda.min )
coef(ridge_reg_3)
x_ridge_test_boxcox<-model.matrix(boxcox_apps~.-Apps,data = test)[,-1]
pred_ridge_boxcox<-predict(ridge_reg_3,s=ridge_cv_boxcox$lambda.min,newx = x_ridge_test_boxcox)
pred_ridge_boxcox<-((lambda*pred_ridge_boxcox)+1)^(1/lambda)
#absolute errors
abs_err_ridge_bocox <- abs(pred_ridge_boxcox - test$Apps)
models_comp <- rbind(models_comp, "RidgeReg boxcox" = c(mean(abs_err_ridge_bocox),
                                                         median(abs_err_ridge_bocox ),
                                                         sd(abs_err_ridge_bocox),
                                                         mean(abs_err_ridge_bocox^2),
                                                         IQR(abs_err_ridge_bocox),
                                                         range(abs_err_ridge_bocox)))

plot(test$Apps,pred_ridge_boxcox,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#ridge regression boxcox trimmed
x_ridge_train_boxcox_trimmed<-model.matrix(boxcox_apps~.-Apps,data = train_trimmed_reg)[,-1]
y_ridge_train_boxcox_trimmed<-train_trimmed_reg$boxcox_apps
#cross validation to choose the best model
set.seed(1234)
ridge_cv_boxcox_trimmed<-cv.glmnet(x_ridge_train_boxcox_trimmed,y_ridge_train_boxcox_trimmed,alpha=0,nfolds = 10)
ridge_cv_boxcox_trimmed$lambda.min
ridge_reg_4<-glmnet(x_ridge_train_boxcox_trimmed,y_ridge_train_boxcox_trimmed,alpha = 0,lambda =ridge_cv_boxcox_trimmed$lambda.min )
coef(ridge_reg_4)
pred_ridge_boxcox_trimmed<-predict(ridge_reg_4,s=ridge_cv_boxcox_trimmed$lambda.min,newx = x_ridge_test_boxcox)
pred_ridge_boxcox_trimmed<-((lambda*pred_ridge_boxcox_trimmed)+1)^(1/lambda)
#absolute errors
abs_err_ridge_bocox_trimmed <- abs(pred_ridge_boxcox_trimmed - test$Apps)
models_comp <- rbind(models_comp, "RidgeReg boxcox timmed" = c(mean(abs_err_ridge_bocox_trimmed),
                                                        median(abs_err_ridge_bocox_trimmed ),
                                                        sd(abs_err_ridge_bocox_trimmed),
                                                        mean(abs_err_ridge_bocox_trimmed^2),
                                                        IQR(abs_err_ridge_bocox_trimmed),
                                                        range(abs_err_ridge_bocox_trimmed)))

plot(test$Apps,pred_ridge_boxcox,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#Lasso Regression-------
#Cross validation to choose the best model
set.seed(1234)
lasso_cv_1  <- cv.glmnet(x_ridge_train ,y_ridge_train, alpha = 1, nfolds = 10)
lasso_cv_1$lambda.min
lasso_reg_1<-glmnet(x_ridge_train,y_ridge_train,alpha = 1,lambda =lasso_cv_1$lambda.min )
coef(lasso_reg_1)
#test the model
pred_lasso_1 <- predict(lasso_reg_1, s = lasso_cv_1$lambda.min, newx = x_ridge_test)
#absolute errors
abs_err_lasso <- abs(pred_lasso_1 - test$Apps)
models_comp <- rbind(models_comp, "LassoReg" = c(mean(abs_err_lasso),
                                                 median(abs_err_lasso),
                                                 sd(abs_err_lasso),
                                                 mean(abs_err_lasso^2),
                                                 IQR(abs_err_lasso),
                                                 range(abs_err_lasso)))
plot(test$Apps,pred_lasso_1,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#lasso regression for train trimmed
#cross validation to choose the best model
set.seed(1234)
lasso_cv_trimmed<-cv.glmnet(x_ridge_train_trimmed,y_ridge_train_trimmed,alpha=1,nfolds = 10)
lasso_cv_trimmed$lambda.min
lasso_reg_2<-glmnet(x_ridge_train_trimmed,y_ridge_train_trimmed,alpha =1,lambda =lasso_cv_trimmed$lambda.min )
coef(lasso_reg_2)
pred_lasso_trimmed<-predict(lasso_reg_2,s=lasso_cv_trimmed$lambda.min,newx = x_ridge_test)
#absolute errors
abs_err_lasso_trimmed <- abs(pred_lasso_trimmed - test$Apps)
models_comp <- rbind(models_comp, "LassoReg trimmed" = c(mean(abs_err_lasso_trimmed ),
                                                         median(abs_err_lasso_trimmed ),
                                                         sd(abs_err_lasso_trimmed ),
                                                         mean(abs_err_lasso_trimmed ^2),
                                                         IQR(abs_err_lasso_trimmed ),
                                                         range(abs_err_lasso_trimmed )))
plot(test$Apps,pred_lasso_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#Lasso regression boxcox
#cross validation to choose the best model
set.seed(1234)
lasso_cv_boxcox<-cv.glmnet(x_ridge_train_boxcox,y_ridge_train_boxcox,alpha=1,nfolds = 10)
lasso_cv_boxcox$lambda.min
lasso_reg_3<-glmnet(x_ridge_train_boxcox,y_ridge_train_boxcox,alpha =1,lambda =lasso_cv_boxcox$lambda.min )
coef(lasso_reg_3)
pred_lasso_boxcox<-predict(lasso_reg_3,s=lasso_cv_boxcox$lambda.min,newx = x_ridge_test_boxcox)
pred_lasso_boxcox<-((lambda*pred_lasso_boxcox)+1)^(1/lambda)
#absolute errors
abs_err_lasso_bocox <- abs(pred_lasso_boxcox - test$Apps)
models_comp <- rbind(models_comp, "LassoReg boxcox" = c(mean(abs_err_lasso_bocox),
                                                        median(abs_err_lasso_bocox ),
                                                        sd(abs_err_lasso_bocox),
                                                        mean(abs_err_lasso_bocox^2),
                                                        IQR(abs_err_lasso_bocox),
                                                        range(abs_err_lasso_bocox)))

plot(test$Apps,pred_lasso_boxcox,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#lasso regression for boxcox trimmed
#cross validation to choose the best model
set.seed(1234)
lasso_cv_boxcox_trimmed<-cv.glmnet(x_ridge_train_boxcox_trimmed,y_ridge_train_boxcox_trimmed,alpha=1,nfolds = 10)
lasso_cv_boxcox_trimmed$lambda.min
lasso_reg_4<-glmnet(x_ridge_train_boxcox_trimmed,y_ridge_train_boxcox_trimmed,alpha = 1,lambda =lasso_cv_boxcox_trimmed$lambda.min )
coef(lasso_reg_4)
pred_lasso_boxcox_trimmed<-predict(lasso_reg_4,s=lasso_cv_boxcox_trimmed$lambda.min,newx = x_ridge_test_boxcox)
pred_lasso_boxcox_trimmed<-((lambda*pred_lasso_boxcox_trimmed)+1)^(1/lambda)
#absolute errors
abs_err_lasso_bocox_trimmed <- abs(pred_lasso_boxcox_trimmed - test$Apps)
models_comp <- rbind(models_comp, "LassoReg boxcox timmed" = c(mean(abs_err_lasso_bocox_trimmed),
                                                               median(abs_err_lasso_bocox_trimmed ),
                                                               sd(abs_err_lasso_bocox_trimmed),
                                                               mean(abs_err_lasso_bocox_trimmed^2),
                                                               IQR(abs_err_lasso_bocox_trimmed),
                                                               range(abs_err_lasso_bocox_trimmed)))

plot(test$Apps,pred_lasso_boxcox_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#Desicion tree-------
tree_1<-rpart(Apps~.-boxcox_apps,data = train,cp=0.00001,maxdepth=20)
prp(tree_1)
plotcp(tree_1)
tree_1$cptable[which.min(tree_1$cptable[,"xerror"])]
#prune the tree
tree_2<-prune.rpart(tree_1,cp=tree_1$cptable[which.min(tree_1$cptable[,"xerror"])])
prp(tree_2)
#predict the model
pred_tree_2<-predict(tree_2,test)
#absolute errors
abs_err_tree<- abs(pred_tree_2 - test$Apps)
models_comp <- rbind(models_comp, "Desicion Tree" = c(mean(abs_err_tree),
                                                 median(abs_err_tree),
                                                 sd(abs_err_tree),
                                                 mean(abs_err_tree^2),
                                                 IQR(abs_err_tree),
                                                 range(abs_err_tree)))
plot(test$Apps,pred_tree_2,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#desicion tree for train trimmed
tree_3<-rpart(Apps~.-boxcox_apps,data = train_trimmed_reg,cp=0.00001,maxdepth=20)
prp(tree_3)
plotcp(tree_3)
tree_3$cptable[which.min(tree_3$cptable[,"xerror"])]
#prune the tree
tree_4<-prune.rpart(tree_3,cp=tree_3$cptable[which.min(tree_3$cptable[,"xerror"])])
prp(tree_4)
#predict the model
pred_tree__trimmed<-predict(tree_4,test)
#absolute errors
abs_err_tree_trimmed<- abs(pred_tree__trimmed - test$Apps)
models_comp <- rbind(models_comp, "Desicion Tree Trimmed" = c(mean(abs_err_tree_trimmed),
                                                      median(abs_err_tree_trimmed),
                                                      sd(abs_err_tree_trimmed),
                                                      mean(abs_err_tree_trimmed^2),
                                                      IQR(abs_err_tree_trimmed),
                                                      range(abs_err_tree_trimmed)))
plot(test$Apps,pred_tree__trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#desicion tree boxcox
tree_5<-rpart(boxcox_apps~.-Apps,data = train,cp=0.00001,maxdepth=20)
tree_5$cptable[which.min(tree_5$cptable[,"xerror"])]
tree_boxcox<-prune(tree_5,cp=tree_5$cptable[which.min(tree_5$cptable[,"xerror"])])
pred_tree_boxcox<-predict(tree_boxcox,test)
pred_tree_boxcox<- ((lambda*pred_tree_boxcox)+1)^(1/lambda)
#absolute errors
abs_err_tree_boxcox<- abs(pred_tree_boxcox - test$Apps)
models_comp <- rbind(models_comp, "Desicion Tree boxcox" = c(mean(abs_err_tree_boxcox),
                                                              median(abs_err_tree_boxcox),
                                                              sd(abs_err_tree_boxcox),
                                                              mean(abs_err_tree_boxcox^2),
                                                              IQR(abs_err_tree_boxcox),
                                                              range(abs_err_tree_boxcox)))
plot(test$Apps,pred_tree_boxcox,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#desicion tree boxcox trimmed
tree_6<-rpart(boxcox_apps~.-Apps,data = train_trimmed_reg,cp=0.00001,maxdepth=20)
tree_6$cptable[which.min(tree_6$cptable[,"xerror"])]
tree_boxcox_trimmed<-prune(tree_6,cp=tree_6$cptable[which.min(tree_6$cptable[,"xerror"])])
pred_tree_boxcox_trimmed<-predict(tree_boxcox_trimmed,test)
pred_tree_boxcox_trimmed<- ((lambda*pred_tree_boxcox_trimmed)+1)^(1/lambda)
#absolute errors
abs_err_tree_boxcox_trimmed<- abs(pred_tree_boxcox_trimmed - test$Apps)
models_comp <- rbind(models_comp, "Desicion Tree boxcox trimmed" = c(mean(abs_err_tree_boxcox_trimmed),
                                                             median(abs_err_tree_boxcox_trimmed),
                                                             sd(abs_err_tree_boxcox_trimmed),
                                                             mean(abs_err_tree_boxcox_trimmed^2),
                                                             IQR(abs_err_tree_boxcox_trimmed),
                                                             range(abs_err_tree_boxcox_trimmed)))
plot(test$Apps,pred_tree_boxcox_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#Bagging--------
set.seed(1234)
bagging_1<-randomForest(Apps~.-boxcox_apps,mtry=ncol(train)-2,ntree=500,data = train)
#test the model
pred_bagging<- predict(bagging_1,test)
#absolute errors
abs_err_bagging<- abs(pred_bagging - test$Apps)
models_comp <- rbind(models_comp, "Bagging" = c(mean(abs_err_bagging),
                                                      median(abs_err_bagging),
                                                      sd(abs_err_bagging),
                                                      mean(abs_err_bagging^2),
                                                      IQR(abs_err_bagging),
                                                      range(abs_err_bagging)))
plot(test$Apps,pred_bagging,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#bagging for train trimmed
set.seed(1234)
bagging_2<-randomForest(Apps~.-boxcox_apps,mtry=ncol(train_trimmed_reg)-2,ntree=500,data = train_trimmed_reg)
#test the model
pred_bagging_trimmed<- predict(bagging_2,test)
#absolute errors
abs_err_bagging_trimmed<- abs(pred_bagging_trimmed - test$Apps)
models_comp <- rbind(models_comp, "Bagging Trimmed" = c(mean(abs_err_bagging_trimmed),
                                                median(abs_err_bagging_trimmed),
                                                sd(abs_err_bagging_trimmed),
                                                mean(abs_err_bagging_trimmed^2),
                                                IQR(abs_err_bagging_trimmed),
                                                range(abs_err_bagging_trimmed)))
plot(test$Apps,pred_bagging_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#bagging for boxcox
set.seed(1234)
bagging_3<-randomForest(boxcox_apps~.-Apps,mtry=ncol(train)-2,ntree=500,data = train)
#test the model
pred_bagging_boxcox<- predict(bagging_3,test)
pred_bagging_boxcox<-((lambda*pred_bagging_boxcox)+1)^(1/lambda)
#absolute errors
abs_err_bagging_boxcox<- abs(pred_bagging_boxcox - test$Apps)
models_comp <- rbind(models_comp, "Bagging boxcox" = c(mean(abs_err_bagging_boxcox),
                                                median(abs_err_bagging_boxcox),
                                                sd(abs_err_bagging_boxcox),
                                                mean(abs_err_bagging_boxcox^2),
                                                IQR(abs_err_bagging_boxcox),
                                                range(abs_err_bagging_boxcox)))
plot(test$Apps,pred_bagging_boxcox,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#bagging boxcox trimmed
set.seed(1234)
bagging_4<-randomForest(boxcox_apps~.-Apps,mtry=ncol(train_trimmed_reg)-2,ntree=500,data = train_trimmed_reg)
#test the model
pred_bagging_boxcox_trimmed<- predict(bagging_4,test)
pred_bagging_boxcox_trimmed<-((lambda*pred_bagging_boxcox_trimmed)+1)^(1/lambda)
#absolute errors
abs_err_bagging_boxcox_trimmed<- abs(pred_bagging_boxcox_trimmed - test$Apps)
models_comp <- rbind(models_comp, "Bagging boxcox trimmed" = c(mean(abs_err_bagging_boxcox_trimmed),
                                                       median(abs_err_bagging_boxcox_trimmed),
                                                       sd(abs_err_bagging_boxcox_trimmed),
                                                       mean(abs_err_bagging_boxcox_trimmed^2),
                                                       IQR(abs_err_bagging_boxcox_trimmed),
                                                       range(abs_err_bagging_boxcox_trimmed)))
plot(test$Apps,pred_bagging_boxcox_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#random Forest---------
set.seed(1234)
rf_1<-randomForest(Apps~.-boxcox_apps,data = train,ntree=500,
                   importance=TRUE)
set.seed(1234)
rf_cv_1<-rfcv(train[,-c(2,20)],train$Apps,cv.fold = 10,step = 0.5,
              mtry = function(p)max(1,floor(sqrt(p))),
              recursive = FALSE)
class(rf_cv_1)
str(rf_cv_1)
rf_cv_1$n.var
rf_cv_1$error.cv
which.min(rf_cv_1$error.cv)
sort(importance(rf_1)[,1])
floor(sqrt(9))#3
set.seed(1234)
rf_2<-randomForest(Apps~Accept +Enroll+F.Undergrad+Top10perc+Elite +Grad.Rate +Room.Board+perc.alumni+Outstate,data = train,mtry=3,ntree=500)
pred_rf<-predict(rf_2,test)
#absolute errors
abs_err_rf<- abs(pred_rf - test$Apps)
models_comp <- rbind(models_comp, "Random Forest" = c(mean(abs_err_rf),
                                                      median(abs_err_rf),
                                                      sd(abs_err_rf),
                                                      mean(abs_err_rf^2),
                                                      IQR(abs_err_rf),
                                                      range(abs_err_rf)))
plot(test$Apps,pred_rf,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#random forest for train trimmed
set.seed(1234)
rf_3<-randomForest(Apps~.-boxcox_apps,data = train_trimmed_reg,ntree=500,
                   importance=TRUE)
varImpPlot(rf_3)
set.seed(1234)
rf_cv_2<-rfcv(train_trimmed_reg[,-c(2,20)],train_trimmed_reg$Apps,cv.fold = 10,step = 0.5,
              mtry = function(p)max(1,floor(sqrt(p))),
              recursive = FALSE)
class(rf_cv_2)
str(rf_cv_2)
rf_cv_2$n.var
rf_cv_2$error.cv
which.min(rf_cv_2$error.cv)
sort(importance(rf_3)[,1])
floor(sqrt(9))#3
set.seed(1234)
rf_trimmed<-randomForest(Apps~Accept +Enroll+F.Undergrad+Outstate+Top10perc+Top25perc +Grad.Rate +Expend+P.Undergrad,data = train_trimmed_reg,mtry=3,ntree=500)
pred_rf_trimmed<-predict(rf_trimmed,test)
#absolute errors
abs_err_rf_trimmed<- abs(pred_rf_trimmed - test$Apps)
models_comp <- rbind(models_comp, "Random Forest Trimmed" = c(mean(abs_err_rf_trimmed),
                                                      median(abs_err_rf_trimmed),
                                                      sd(abs_err_rf_trimmed),
                                                      mean(abs_err_rf_trimmed^2),
                                                      IQR(abs_err_rf_trimmed),
                                                      range(abs_err_rf_trimmed)))
plot(test$Apps,pred_rf_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#random forest for boxcox
set.seed(1234)
rf_4<-randomForest(boxcox_apps~.-Apps,data = train,ntree=500,
                   importance=TRUE)
varImpPlot(rf_4)
set.seed(1234)
rf_cv_3<-rfcv(train[,-c(2,20)],train$boxcox_apps,cv.fold = 10,step = 0.5,
              mtry = function(p)max(1,floor(sqrt(p))),
              recursive = FALSE)
rf_cv_3$n.var
rf_cv_3$error.cv
which.min(rf_cv_3$error.cv)
sort(importance(rf_4)[,1])
floor(sqrt(9))#3
set.seed(1234)
rf_boxcox<-randomForest(boxcox_apps~Accept +Enroll+F.Undergrad+Outstate+Top10perc+Room.Board + Expend+Top25perc+PhD,data = train,mtry=3,ntree=500)
pred_rf_boxcox<-predict(rf_boxcox,test)
pred_rf_boxcox<-((lambda*pred_rf_boxcox)+1)^(1/lambda)
#absolute errors
abs_err_rf_boxcox<- abs(pred_rf_boxcox - test$Apps)
models_comp <- rbind(models_comp, "Random Forest Boxcox" = c(mean(abs_err_rf_boxcox),
                                                              median(abs_err_rf_boxcox),
                                                              sd(abs_err_rf_boxcox),
                                                              mean(abs_err_rf_boxcox^2),
                                                              IQR(abs_err_rf_boxcox),
                                                              range(abs_err_rf_boxcox)))

plot(test$Apps,pred_rf_boxcox,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#random forest for boxcox trimmed
set.seed(1234)
rf_5<-randomForest(boxcox_apps~.-Apps,data = train_trimmed_reg,ntree=500,
                   importance=TRUE)
varImpPlot(rf_5)
set.seed(1234)
rf_cv_5<-rfcv(train_trimmed_reg[,-c(2,20)],train_trimmed_reg$boxcox_apps,cv.fold = 10,step = 0.5,
              mtry = function(p)max(1,floor(sqrt(p))),
              recursive = FALSE)
rf_cv_5$n.var
rf_cv_5$error.cv
which.min(rf_cv_5$error.cv)
sort(importance(rf_5)[,1])
floor(sqrt(9))#3
set.seed(1234)
rf_boxcox_trimmed<-randomForest(boxcox_apps~Accept +Enroll+F.Undergrad+Outstate+Top25perc+Top10perc+Grad.Rate+Room.Board + Expend,data = train_trimmed_reg,mtry=3,ntree=500)
pred_rf_boxcox_trimmed<-predict(rf_boxcox_trimmed,test)
pred_rf_boxcox_trimmed<-((lambda*pred_rf_boxcox_trimmed)+1)^(1/lambda)
#absolute errors
abs_err_rf_boxcox_trimmed<- abs(pred_rf_boxcox_trimmed - test$Apps)
models_comp <- rbind(models_comp, "Random Forest Boxcox Trimmed" = c(mean(abs_err_rf_boxcox_trimmed),
                                                             median(abs_err_rf_boxcox_trimmed),
                                                             sd(abs_err_rf_boxcox_trimmed),
                                                             mean(abs_err_rf_boxcox_trimmed^2),
                                                             IQR(abs_err_rf_boxcox_trimmed),
                                                             range(abs_err_rf_boxcox_trimmed)))

plot(test$Apps,pred_rf_boxcox_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#Gradient Boost -----------
par_grid <- expand.grid(shrinkage = c(0.01, 0.1, 0.3,0.5),
                        interaction_depth = c(1, 3, 5,7),
                        n_minobsinnode = c(5, 10, 15,18),  
                        bag_fraction = c(0.4,0.5, 0.7, 0.9)  
)
View(par_grid)
nrow(par_grid)
for(i in 1:nrow(par_grid)) {
  set.seed(123)
  gbm_tune <- gbm(formula =Apps~.-boxcox_apps,
                  distribution = "gaussian",
                  data = train,
                  n.trees = 5000,
                  interaction.depth = par_grid$interaction_depth[i],
                  shrinkage = par_grid$shrinkage[i],
                  n.minobsinnode = par_grid$n_minobsinnode[i],
                  bag.fraction = par_grid$bag_fraction[i],
                  train.fraction = 0.8,
                  cv.folds = 0,
                  n.cores = NULL, 
                  verbose = FALSE)  
  par_grid$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  par_grid$min_RMSE[i]      <- sqrt(min(gbm_tune$valid.error))
}

head(par_grid)
View(par_grid)
#modify hyper parameter grid
par_grid_2 <- expand.grid(shrinkage = c(0.01,0.55, 0.1),
                        interaction_depth = c(3, 5,7),
                        n_minobsinnode = c(5, 10, 15),  
                        bag_fraction = c(0.5,0.6, 0.7) ) 

for(i in 1:nrow(par_grid_2)) {
  set.seed(123)
  gbm_tune_2 <- gbm(formula =Apps~.-boxcox_apps,
                  distribution = "gaussian",
                  data = train,
                  n.trees = 600,
                  interaction.depth = par_grid_2$interaction_depth[i],
                  shrinkage = par_grid_2$shrinkage[i],
                  n.minobsinnode = par_grid_2$n_minobsinnode[i],
                  bag.fraction = par_grid_2$bag_fraction[i],
                  train.fraction = 0.8,
                  cv.folds = 0,
                  n.cores = NULL, 
                  verbose = FALSE)  
  par_grid_2$optimal_trees[i] <- which.min(gbm_tune_2$valid.error)
  par_grid_2$min_RMSE[i]      <- sqrt(min(gbm_tune_2$valid.error))
}
head(par_grid_2)
View(par_grid_2)
#modify 
par_grid_3 <- expand.grid(shrinkage = c(0.01,0.55, 0.1),
                          interaction_depth = c(5,6,7),
                          n_minobsinnode = c(5, 10, 15),  
                          bag_fraction = c(0.5,0.55, 0.6) )
for(i in 1:nrow(par_grid_2)) {
  set.seed(123)
  gbm_tune_3 <- gbm(formula =Apps~.-boxcox_apps,
                    distribution = "gaussian",
                    data = train,
                    n.trees = 400,
                    interaction.depth = par_grid_3$interaction_depth[i],
                    shrinkage = par_grid_3$shrinkage[i],
                    n.minobsinnode = par_grid_3$n_minobsinnode[i],
                    bag.fraction = par_grid_3$bag_fraction[i],
                    train.fraction = 0.8,
                    cv.folds = 0,
                    n.cores = NULL, 
                    verbose = FALSE)  
  par_grid_3$optimal_trees[i] <- which.min(gbm_tune_3$valid.error)
  par_grid_3$min_RMSE[i]      <- sqrt(min(gbm_tune_3$valid.error))
}
head(par_grid_3)
View(par_grid_3)
#final model
gbm_final<-gbm(Apps~.-boxcox_apps,data = train,
               distribution = "gaussian",n.trees = 300,
               interaction.depth = 7,shrinkage = 0.01,
               n.minobsinnode = 5,bag.fraction =0.55,
               train.fraction = 0.8,cv.folds = 0,
               n.cores = NULL)
summary(gbm_final)
pred_gbm <- predict(gbm_final,n.trees = 300,newdata = test)
#absolute errors
abs_err_gbm<- abs(pred_gbm - test$Apps)
models_comp <- rbind(models_comp, "GBReg" = c(mean(abs_err_gbm),
                                                      median(abs_err_gbm),
                                                      sd(abs_err_gbm),
                                                      mean(abs_err_gbm^2),
                                                      IQR(abs_err_gbm),
                                                      range(abs_err_gbm)))
plot(test$Apps,pred_gbm,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#gradient boost train trimmed
par_grid_trimmed <- expand.grid(shrinkage = c(0.01,0.55, 0.1),
                          interaction_depth = c(3, 5,7),
                          n_minobsinnode = c(5, 10, 15),  
                          bag_fraction = c(0.5,0.7, 0.9) ) 

for(i in 1:nrow(par_grid_trimmed)) {
  set.seed(123)
  gbm_tune_4 <- gbm(formula =Apps~.-boxcox_apps,
                    distribution = "gaussian",
                    data = train_trimmed_reg,
                    n.trees = 1000,
                    interaction.depth = par_grid_trimmed$interaction_depth[i],
                    shrinkage = par_grid_trimmed$shrinkage[i],
                    n.minobsinnode = par_grid_trimmed$n_minobsinnode[i],
                    bag.fraction = par_grid_trimmed$bag_fraction[i],
                    train.fraction = 0.8,
                    cv.folds = 0,
                    n.cores = NULL, 
                    verbose = FALSE)  
  par_grid_trimmed$optimal_trees[i] <- which.min(gbm_tune_4$valid.error)
  par_grid_trimmed$min_RMSE[i]      <- sqrt(min(gbm_tune_4$valid.error))
}
head(par_grid_trimmed)
View(par_grid_trimmed)
#modify
par_grid_trimmed_2 <- expand.grid(shrinkage = c(0.01,0.08, 0.1),
                                interaction_depth = c( 5,6,7),
                                n_minobsinnode = c(5, 8, 10),  
                                bag_fraction = c(0.5,0.6, 0.7) ) 
for(i in 1:nrow(par_grid_trimmed_2)) {
  set.seed(123)
  gbm_tune_5 <- gbm(formula =Apps~.-boxcox_apps,
                    distribution = "gaussian",
                    data = train_trimmed_reg,
                    n.trees = 500,
                    interaction.depth = par_grid_trimmed_2$interaction_depth[i],
                    shrinkage = par_grid_trimmed_2$shrinkage[i],
                    n.minobsinnode = par_grid_trimmed$n_minobsinnode[i],
                    bag.fraction = par_grid_trimmed_2$bag_fraction[i],
                    train.fraction = 0.8,
                    cv.folds = 0,
                    n.cores = NULL, 
                    verbose = FALSE)  
  par_grid_trimmed_2$optimal_trees[i] <- which.min(gbm_tune_5$valid.error)
  par_grid_trimmed_2$min_RMSE[i]      <- sqrt(min(gbm_tune_5$valid.error))
}
head(par_grid_trimmed_2)
View(par_grid_trimmed_2)
#final model
gbm_final_trimmed<-gbm(Apps~.-boxcox_apps,data = train_trimmed_reg,
               distribution = "gaussian",n.trees = 300,
               interaction.depth = 7,shrinkage = 0.01,
               n.minobsinnode = 5,bag.fraction =0.5,
               train.fraction = 0.8,cv.folds = 0,
               n.cores = NULL)
summary(gbm_final_trimmed)
pred_gbm_trimmed <- predict(gbm_final_trimmed,n.trees = 300,newdata = test)
#absolute errors
abs_err_gbm_trimmed<- abs(pred_gbm_trimmed - test$Apps)
models_comp <- rbind(models_comp, "GBReg Trimmed" = c(mean(abs_err_gbm_trimmed),
                                              median(abs_err_gbm_trimmed),
                                              sd(abs_err_gbm_trimmed),
                                              mean(abs_err_gbm_trimmed^2),
                                              IQR(abs_err_gbm_trimmed),
                                              range(abs_err_gbm_trimmed)))
plot(test$Apps,pred_gbm_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#GB boxcox
par_grid_boxcox <- expand.grid(shrinkage = c(0.01,0.1, 0.55),
                                interaction_depth = c(3, 5,7),
                                n_minobsinnode = c(5, 10, 15),  
                                bag_fraction = c(0.5,0.7, 0.9) ) 

for(i in 1:nrow(par_grid_boxcox)) {
  set.seed(123)
  gbm_tune_boxcox <- gbm(formula =boxcox_apps~.-Apps,
                    distribution = "gaussian",
                    data = train,
                    n.trees = 1000,
                    interaction.depth = par_grid_boxcox$interaction_depth[i],
                    shrinkage = par_grid_boxcox$shrinkage[i],
                    n.minobsinnode = par_grid_boxcox$n_minobsinnode[i],
                    bag.fraction = par_grid_boxcox$bag_fraction[i],
                    train.fraction = 0.8,
                    cv.folds = 0,
                    n.cores = NULL, 
                    verbose = FALSE)  
  par_grid_boxcox$optimal_trees[i] <- which.min(gbm_tune_boxcox$valid.error)
  par_grid_boxcox$min_RMSE[i]      <- sqrt(min(gbm_tune_boxcox$valid.error))
}
head(par_grid_boxcox)
View(par_grid_boxcox)
#modify
par_grid_boxcox_1 <- expand.grid(shrinkage = c(0.01,0.08, 0.1),
                               interaction_depth = c( 5,6,7),
                               n_minobsinnode = c(5,8,10),  
                               bag_fraction = c(0.5,0.6, 0.7) ) 

for(i in 1:nrow(par_grid_boxcox_1)) {
  set.seed(123)
  gbm_tune_boxcox_1 <- gbm(formula =boxcox_apps~.-Apps,
                         distribution = "gaussian",
                         data = train,
                         n.trees = 400,
                         interaction.depth = par_grid_boxcox_1$interaction_depth[i],
                         shrinkage = par_grid_boxcox_1$shrinkage[i],
                         n.minobsinnode = par_grid_boxcox_1$n_minobsinnode[i],
                         bag.fraction = par_grid_boxcox_1$bag_fraction[i],
                         train.fraction = 0.8,
                         cv.folds = 0,
                         n.cores = NULL, 
                         verbose = FALSE)  
  par_grid_boxcox_1$optimal_trees[i] <- which.min(gbm_tune_boxcox_1$valid.error)
  par_grid_boxcox_1$min_RMSE[i]      <- sqrt(min(gbm_tune_boxcox_1$valid.error))
}
head(par_grid_boxcox_1)
View(par_grid_boxcox_1)
#final modelboxcox trimmed
gbm_final_boxcox<-gbm(boxcox_apps~.-Apps,data = train,
                      distribution = "gaussian",n.trees = 100,
                      interaction.depth = 5,shrinkage = 0.08,
                      n.minobsinnode = 5,bag.fraction =0.7,
                      train.fraction = 0.8,cv.folds = 0,
                      n.cores = NULL)
summary(gbm_final_boxcox)
pred_gbm_boxcox <- predict(gbm_final_boxcox,n.trees = 100,newdata = test)
pred_gbm_boxcox<-((lambda*pred_gbm_boxcox)+1)^(1/lambda)
#absolute errors
abs_err_gbm_boxcox<- abs(pred_gbm_boxcox - test$Apps)
models_comp <- rbind(models_comp, "GBReg Boxcox" = c(mean(abs_err_gbm_boxcox),
                                                     median(abs_err_gbm_boxcox),
                                                     sd(abs_err_gbm_boxcox),
                                                     mean(abs_err_gbm_boxcox^2),
                                                     IQR(abs_err_gbm_boxcox),
                                                     range(abs_err_gbm_boxcox)))
plot(test$Apps,pred_gbm_boxcox,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#GB for boxcox trimmed
par_grid_boxcox_trimmed <- expand.grid(shrinkage = c(0.01,0.1, 0.55),
                               interaction_depth = c(3, 5,7),
                               n_minobsinnode = c(5, 10, 15),  
                               bag_fraction = c(0.5,0.7, 0.9) ) 

for(i in 1:nrow(par_grid_boxcox_trimmed)) {
  set.seed(123)
  gbm_tune_boxcox_trimmed <- gbm(formula =boxcox_apps~.-Apps,
                         distribution = "gaussian",
                         data = train_trimmed_reg,
                         n.trees = 1000,
                         interaction.depth = par_grid_boxcox_trimmed$interaction_depth[i],
                         shrinkage = par_grid_boxcox_trimmed$shrinkage[i],
                         n.minobsinnode = par_grid_boxcox_trimmed$n_minobsinnode[i],
                         bag.fraction = par_grid_boxcox_trimmed$bag_fraction[i],
                         train.fraction = 0.8,
                         cv.folds = 0,
                         n.cores = NULL, 
                         verbose = FALSE)  
  par_grid_boxcox_trimmed$optimal_trees[i] <- which.min(gbm_tune_boxcox_trimmed$valid.error)
  par_grid_boxcox_trimmed$min_RMSE[i]      <- sqrt(min(gbm_tune_boxcox_trimmed$valid.error))
}
head(par_grid_boxcox_trimmed)
View(par_grid_boxcox_trimmed)
#modify
par_grid_boxcox_trimmed_1 <- expand.grid(shrinkage = c(0.01,0.07, 0.1),
                                       interaction_depth = c( 5,6,7),
                                       n_minobsinnode = c(5, 8, 10),  
                                       bag_fraction = c(0.5,0.6, 0.7) ) 
for(i in 1:nrow(par_grid_boxcox_trimmed_1)) {
  set.seed(123)
  gbm_tune_boxcox_trimmed_1 <- gbm(formula =boxcox_apps~.-Apps,
                                 distribution = "gaussian",
                                 data = train_trimmed_reg,
                                 n.trees = 1000,
                                 interaction.depth = par_grid_boxcox_trimmed_1$interaction_depth[i],
                                 shrinkage = par_grid_boxcox_trimmed_1$shrinkage[i],
                                 n.minobsinnode = par_grid_boxcox_trimmed_1$n_minobsinnode[i],
                                 bag.fraction = par_grid_boxcox_trimmed_1$bag_fraction[i],
                                 train.fraction = 0.8,
                                 cv.folds = 0,
                                 n.cores = NULL, 
                                 verbose = FALSE)  
  par_grid_boxcox_trimmed_1$optimal_trees[i] <- which.min(gbm_tune_boxcox_trimmed_1$valid.error)
  par_grid_boxcox_trimmed_1$min_RMSE[i]      <- sqrt(min(gbm_tune_boxcox_trimmed_1$valid.error))
}
head(par_grid_boxcox_trimmed_1)
View(par_grid_boxcox_trimmed_1)
#final model boxcox trimmed
gbm_final_boxcox_trimmed<-gbm(boxcox_apps~.-Apps,data = train_trimmed_reg,
                      distribution = "gaussian",n.trees = 500,
                      interaction.depth = 5,shrinkage = 0.1,
                      n.minobsinnode =8,bag.fraction =0.6,
                      train.fraction = 0.8,cv.folds = 0,
                      n.cores = NULL)
summary(gbm_final_boxcox_trimmed)
pred_gbm_boxcox_trimmed<- predict(gbm_final_boxcox_trimmed,n.trees = 500,newdata = test)
pred_gbm_boxcox_trimmed<-((lambda*pred_gbm_boxcox_trimmed)+1)^(1/lambda)
#absolute errors
abs_err_gbm_boxcox_trimmed<- abs(pred_gbm_boxcox_trimmed - test$Apps)
models_comp <- rbind(models_comp, "GBReg Boxcox Trimmed" = c(mean(abs_err_gbm_boxcox_trimmed),
                                                     median(abs_err_gbm_boxcox_trimmed),
                                                     sd(abs_err_gbm_boxcox_trimmed),
                                                     mean(abs_err_gbm_boxcox_trimmed^2),
                                                     IQR(abs_err_gbm_boxcox_trimmed),
                                                     range(abs_err_gbm_boxcox_trimmed)))
plot(test$Apps,pred_gbm_boxcox_trimmed,xlab = "Actual",ylab = "prediction")
abline(a=0,b=1,col="red",lwd=2)
#XGB------------
set.seed(1234)
train_case_xgb<-sample(1:nrow(train),nrow(train)*0.8)
train_xgb<-train[train_case_xgb,]
x_train<-model.matrix(Apps~.-boxcox_apps,data = train_xgb)[,-1]
y_train<-train_xgb$Apps
#validation
validation_xgb <- train[- train_case_xgb,]
dim(validation_xgb)
xvalidation <- model.matrix(Apps ~ + . - boxcox_apps, data = validation_xgb)[, -1] 
yvalidation <- validation_xgb$Apps

#Create hyper-parameter grid
par_grid_xgb <- expand.grid(eta = c(0.01, 0.05, 0.1, 0.3),
                        lambda = c(0, 1, 2, 5),
                        max_depth = c(1, 3, 5, 7),
                        subsample = c(0.65, 0.8, 1), 
                        colsample_bytree = c(0.8, 0.9, 1))

dim(par_grid)

#Grid search 
for(i in 1:nrow(par_grid_xgb )) {
  set.seed(123)
  
  #train model
  xgb_tune <- xgboost(data =  x_train,
                      label = y_train,
                      eta = par_grid_xgb$eta[i],
                      max_depth = par_grid_xgb$max_depth[i],
                      subsample = par_grid_xgb$subsample[i],
                      colsample_bytree = par_grid_xgb$colsample_bytree[i],
                      nrounds = 1000,
                      objective = "reg:squarederror", 
                      verbose = 0,                     
                      early_stopping_rounds = 10      
  )
  
  #prediction on validation data set
  pred_xgb_validation <- predict(xgb_tune, xvalidation)
  rmse <- sqrt(mean((yvalidation - pred_xgb_validation) ^ 2))
  
  #add validation error
  par_grid_xgb$RMSE[i]  <- rmse
}
View(par_grid_xgb)
#final model
set.seed(123)
model_xgb<-xgboost(data = x_ridge_train,label = y_ridge_train,
                   eta=0.1,
                   max_depth=5,
                   lambda=0,
                   nround = 1000,
                   colsample_bytree =0.8,
                   subsample = 1,                 
                   objective = "reg:squarederror", 
                   verbose = 0
                   )
pred_xgb<-predict(model_xgb,x_ridge_test)
abs_err_xgb <- abs(pred_xgb - test$Apps)
models_comp <- rbind(models_comp, "XGBReg" = c(mean(abs_err_xgb),
                                               median(abs_err_xgb),
                                               sd(abs_err_xgb),
                                               mean(abs_err_xgb^2),
                                               IQR(abs_err_xgb),
                                               range(abs_err_xgb)))
plot(test$Apps, pred_xgb, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
#xgb for train trimmed
set.seed(1234)
train_case_xgb_trimmed<-sample(1:nrow(train_trimmed_reg),nrow(train_trimmed_reg)*0.8)
train_xgb_trimmed<-train[train_case_xgb_trimmed,]
x_train_trimmed<-model.matrix(Apps~.-boxcox_apps,data = train_xgb_trimmed)[,-1]
y_train_trimmed<-train_xgb_trimmed$Apps
#validation
validation_xgb_trimmed <- train[- train_case_xgb_trimmed,]
dim(validation_xgb_trimmed)
xvalidation_trimmed <- model.matrix(Apps ~ + . - boxcox_apps, data = validation_xgb_trimmed)[, -1] 
yvalidation_trimmed <- validation_xgb_trimmed$Apps
#Create hyper-parameter grid
par_grid_xgb_trimmed <- expand.grid(eta = c(0.01, 0.05, 0.1, 0.3),
                            lambda = c(0, 1, 2, 5),
                            max_depth = c(1, 3, 5, 7),
                            subsample = c(0.65, 0.8, 1), 
                            colsample_bytree = c(0.8, 0.9, 1))

#Grid search 
for(i in 1:nrow(par_grid_xgb_trimmed )) {
  set.seed(123)

  xgb_tune_trimmed <- xgboost(data =  x_train_trimmed,
                      label = y_train_trimmed,
                      eta = par_grid_xgb_trimmed$eta[i],
                      max_depth = par_grid_xgb_trimmed$max_depth[i],
                      subsample = par_grid_xgb_trimmed$subsample[i],
                      colsample_bytree = par_grid_xgb_trimmed$colsample_bytree[i],
                      nrounds = 1000,
                      objective = "reg:squarederror", 
                      verbose = 0,                     
                      early_stopping_rounds = 10      
  )
  
  #prediction on validation data set
  pred_xgb_validation_trimmed <- predict(xgb_tune_trimmed, xvalidation_trimmed)
  rmse <- sqrt(mean((yvalidation_trimmed - pred_xgb_validation_trimmed) ^ 2))
  
  #add validation error
  par_grid_xgb_trimmed$RMSE[i]  <- rmse
}
View(par_grid_xgb_trimmed)
#final model
set.seed(123)
model_xgb_trimmed<-xgboost(data = x_ridge_train_trimmed,label = y_ridge_train_trimmed,
                   eta=0.3,
                   max_depth=5,
                   lambda=0,
                   nround = 1000,
                   colsample_bytree =1,
                   subsample = 0.65,                 
                   objective = "reg:squarederror", 
                   verbose = 0
)
pred_xgb_trimmed<-predict(model_xgb_trimmed,x_ridge_test)
abs_err_xgb_trimmed <- abs(pred_xgb_trimmed - test$Apps)
models_comp <- rbind(models_comp, "XGBReg Trimmed" = c(mean(abs_err_xgb_trimmed),
                                               median(abs_err_xgb_trimmed),
                                               sd(abs_err_xgb_trimmed),
                                               mean(abs_err_xgb_trimmed^2),
                                               IQR(abs_err_xgb_trimmed),
                                               range(abs_err_xgb_trimmed)))
plot(test$Apps, pred_xgb_trimmed, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
#xgb for boxcox_apps
set.seed(1234)
x_train_boxcox<-model.matrix(boxcox_apps~.-Apps,data = train_xgb)[,-1]
y_train_boxcox<-train_xgb$boxcox_apps
#validation
xvalidation_boxcox <- model.matrix( boxcox_apps~.-Apps, data = validation_xgb)[, -1] 
yvalidation_boxcox <- validation_xgb$boxcox_apps
#Create hyper-parameter grid
par_grid_xgb_boxcox <- expand.grid(eta = c(0.01, 0.05, 0.1, 0.3),
                                    lambda = c(0, 1, 2, 5),
                                    max_depth = c(1, 3, 5, 7),
                                    subsample = c(0.65, 0.8, 1), 
                                    colsample_bytree = c(0.8, 0.9, 1))

#Grid search 
for(i in 1:nrow(par_grid_xgb_boxcox )) {
  set.seed(123)
  
  xgb_tune_boxcox <- xgboost(data =  x_train_boxcox,
                              label = y_train_boxcox,
                              eta = par_grid_xgb_boxcox$eta[i],
                              max_depth = par_grid_xgb_boxcox$max_depth[i],
                              subsample = par_grid_xgb_boxcox$subsample[i],
                              colsample_bytree = par_grid_xgb_boxcox$colsample_bytree[i],
                              nrounds = 1000,
                              objective = "reg:squarederror", 
                              verbose = 0,                     
                              early_stopping_rounds = 10      
  )
  
  #prediction on validation data set
  pred_xgb_validation_boxcox <- predict(xgb_tune_boxcox, xvalidation_boxcox)
  pred_xgb_validation_boxcox<-((lambda*pred_xgb_validation_boxcox)+1)^(1/lambda)
  rmse <- sqrt(mean((yvalidation_boxcox - pred_xgb_validation_boxcox) ^ 2))
  
  #add validation error
  par_grid_xgb_boxcox$RMSE[i]  <- rmse
}
View(par_grid_xgb_boxcox)
#final model
set.seed(123)
model_xgb_boxcox<-xgboost(data = x_ridge_train_boxcox,label = y_ridge_train_boxcox,
                           eta=0.01,
                           max_depth=1,
                           lambda=0,
                           nround = 1000,
                           colsample_bytree =0.8,
                           subsample = 1,                 
                           objective = "reg:squarederror", 
                           verbose = 0
)
pred_xgb_boxcox<-predict(model_xgb_boxcox,x_ridge_test)
pred_xgb_boxcox<-((lambda*pred_xgb_boxcox)+1)^(1/lambda)
abs_err_xgb_boxcox <- abs(pred_xgb_boxcox - test$Apps)
models_comp <- rbind(models_comp, "XGBReg Boxcox" = c(mean(abs_err_xgb_boxcox),
                                                       median(abs_err_xgb_boxcox),
                                                       sd(abs_err_xgb_boxcox),
                                                       mean(abs_err_xgb_boxcox^2),
                                                       IQR(abs_err_xgb_boxcox),
                                                       range(abs_err_xgb_boxcox)))
plot(test$Apps, pred_xgb_boxcox, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
#xgb for boxcox trimmed
x_train_boxcox_trimmed<-model.matrix(boxcox_apps~.-Apps,data = train_xgb_trimmed)[,-1]
y_train_boxcox_trimmed<-train_xgb_trimmed$boxcox_apps
#validation
xvalidation_boxcox_trimmed <- model.matrix(boxcox_apps~.-Apps, data = validation_xgb_trimmed)[, -1] 
yvalidation_boxcox_trimmed <- validation_xgb_trimmed$boxcox_apps
#Create hyper-parameter grid
par_grid_xgb_boxcox_trimmed <- expand.grid(eta = c(0.01, 0.05, 0.1, 0.3),
                                    lambda = c(0, 1, 2, 5),
                                    max_depth = c(1, 3, 5, 7),
                                    subsample = c(0.65, 0.8, 1), 
                                    colsample_bytree = c(0.8, 0.9, 1))

#Grid search 
for(i in 1:nrow(par_grid_xgb_boxcox_trimmed )) {
  set.seed(123)
  
  xgb_tune_boxcox_trimmed <- xgboost(data =  x_train_boxcox_trimmed,
                              label = y_train_boxcox_trimmed,
                              eta = par_grid_xgb_boxcox_trimmed$eta[i],
                              max_depth = par_grid_xgb_boxcox_trimmed$max_depth[i],
                              subsample = par_grid_xgb_boxcox_trimmed$subsample[i],
                              colsample_bytree = par_grid_xgb_boxcox_trimmed$colsample_bytree[i],
                              nrounds = 1000,
                              objective = "reg:squarederror", 
                              verbose = 0,                     
                              early_stopping_rounds = 10      
  )
  
  #prediction on validation data set
  pred_xgb_validation_boxcox_trimmed <- predict(xgb_tune_boxcox_trimmed, xvalidation_boxcox_trimmed)
  pred_xgb_validation_boxcox_trimmed<-((lambda*pred_xgb_validation_boxcox_trimmed)+1)^(1/lambda)
  rmse <- sqrt(mean((yvalidation_boxcox_trimmed - pred_xgb_validation_boxcox_trimmed) ^ 2))
  
  #add validation error
  par_grid_xgb_boxcox_trimmed$RMSE[i]  <- rmse
}
View(par_grid_xgb_boxcox_trimmed)
#final model
set.seed(123)
model_xgb_boxcox_trimmed<-xgboost(data = x_ridge_train_boxcox_trimmed,label = y_ridge_train_boxcox_trimmed,
                           eta=0.01,
                           max_depth=1,
                           lambda=0,
                           nround = 1000,
                           colsample_bytree =0.8,
                           subsample =1,                 
                           objective = "reg:squarederror", 
                           verbose = 0
)
pred_xgb_boxcox_trimmed<-predict(model_xgb_boxcox_trimmed,x_ridge_test)
pred_xgb_boxcox_trimmed<-((lambda*pred_xgb_boxcox_trimmed)+1)^(1/lambda)
abs_err_xgb_boxcox_trimmed <- abs(pred_xgb_boxcox_trimmed - test$Apps)
models_comp <- rbind(models_comp, "XGB Boxcox Trimmed" = c(mean(abs_err_xgb_boxcox_trimmed),
                                                       median(abs_err_xgb_boxcox_trimmed),
                                                       sd(abs_err_xgb_boxcox_trimmed),
                                                       mean(abs_err_xgb_boxcox_trimmed^2),
                                                       IQR(abs_err_xgb_boxcox_trimmed),
                                                       range(abs_err_xgb_boxcox_trimmed)))
plot(test$Apps, pred_xgb_boxcox_trimmed, xlab = "Actual", ylab = "Prediction")
abline(a = 0, b = 1, col = "red", lwd = 2)
#Save the results--------------------------------
save(data_1, train, train_trimmed_reg,train_xgb,train_xgb_trimmed ,test, models_comp, file = "project_college_solve.R")
#Random Forest boxcox Trimmed is the best model------