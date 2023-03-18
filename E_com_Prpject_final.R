#load the data
edata<-read.csv("C:/Users/himan/Downloads/Regression.csv")
View(edata)
#summary to know the na's
summary(edata)
hist(edata$Age)
#place the mean value for na's
edata$Age[is.na(edata$Age)]=39
summary(edata)
head(edata)
#creating dummy variables for char values n coverting them into numeric
edata$Job.Type_employed<-as.numeric(edata$Job.Type=="Employed")
edata$Job.Type_retired<-as.numeric(edata$Job.Type=="Retired")
edata$Job.Type_unemployed<-as.numeric(edata$Job.Type=="Unemployed")
edata$Married_y<-as.numeric(edata$Marital.Status=="Yes")
edata$Education_secondary<-as.numeric(edata$Education=="Secondry")
edata$Education_gra<-as.numeric(edata$Education=="Graduate")
edata$Metro_y<-as.numeric(edata$Metro.City=="Yes")
summary(edata)
View(edata)
#droping the previous char values
final_edata<-edata[-c(2,3,4,5)] ###### not running
final_edata
summary(final_edata)
par(mfrow=c(1,2))
bx=boxplot(final_edata$Age)
#to do capping
quantile(final_edata$Age,.5)
quantile(final_edata$Age,seq(0,1,0.02))
bx$stats
#outliner are removed
final_edata$Age<-ifelse(final_edata$Age>60,57,final_edata$Age)
boxplot(final_edata$Age)
#checking the outliner for othe variable i.e. signed.in
ax<-boxplot(final_edata$Signed.in.since.Days.)
ax$stats
quantile(final_edata$Signed.in.since.Days.,seq(0,1,0.02))
final_edata$Signed.in.since.Days.<-ifelse(final_edata$Signed.in.since.Days.<45,48,final_edata$Signed.in.since.Days.)
boxplot(final_edata$Signed.in.since.Days.)

hist(final_edata$Purchase.made,main='Dependent')
boxplot(final_edata$Purchase.made)

#bi-variate analysis
library(car)
scatterplot(final_edata$Age,final_edata$Purchase.made)
scatterplot(final_edata$Signed.in.since.Days.,final_edata$Purchase.made)
#checking co-relation
#cor(final_edata$Signed.in.since.Days.,final_data$Purchase.made)
cor(final_edata)
#chcecking multi co-linearity
model1<-lm(Purchase.made~.,data = final_edata)
#
summary(model1)
#vif shouldbe less than 5
vif(model1)
#apply step to write the main var in lm
step(model1)
model2<-lm(Purchase.made ~ Age + Signed.in.since.Days. + Job.Type_retired + 
             Job.Type_unemployed + Married_y + Education_gra + Metro_y, 
           data = final_edata)
vif(model2)
summary(model2)
#removing variable whose pr is less than 0.05
model3<-lm(Purchase.made ~  Signed.in.since.Days.  + 
             Job.Type_unemployed + Married_y + Education_gra + Metro_y, 
           data = final_edata)
#keeping the variable unempplloyed coz the diff is 1%
summary(model3)
#evaluating model
library(lmtest)
par(mfrow=c(2,2))
plot(model3)
#model improvement
#capping error for improving model
quantile(final_edata$Purchase.made,seq(0,1,0.02))
#removing 4% from starting and 6% from end
final_data_new=final_edata[(final_edata$Purchase.made>=510 & final_edata$Purchase.made<=13500),]
model4<-lm(Purchase.made ~  Signed.in.since.Days.  + 
             Job.Type_unemployed + Married_y + Education_gra + Metro_y, 
           data = final_edata)
summary(model4)
#droping unemployment variable
model5<-lm(Purchase.made ~  Signed.in.since.Days. 
           + Married_y + Education_gra + Metro_y, 
           data = final_edata)
summary(model5)
par(mfrow=c(2,2))
plot(model5)
#we test if it is more than 2 than it is more co-relative
durbinWatsonTest(model5)
hist(residuals(model5))
model5
summary(model5)
plot(model5)
# Homoscedasticity
plot(final_edata$Purchase.made,residuals(model5))
#checking the cook's distances
#install.packages("predictmeans",dependencies = T)
library(predictmeans)
cooked=CookD(model5)
#install.packages("broom",dependencies = T)
library(broom)
cooksD=predictmeans::CookD(model5)
#we are dropping the rows which are highlighted in thw cooks model
final_edata_1<-final_edata[-c(34,51,220),]
model6<-lm(Purchase.made ~  Signed.in.since.Days. 
           + Married_y + Education_gra + Metro_y, 
           data = final_edata_1)

final_edata_1$Purchase.made<-predict(model6,final_edata_1)


#importing file ----go to import data set than click on excel tham copy file name and cancel and than run the below comman and paste the adress here
#regression2<-read.csv("C://Users//himan//Regression.csv")
final_edata_1$Purchase.made
##finding top 30% from my data 2
modelpercent<-final_edata_1[final_edata_1$Purchase.made>quantile(final_edata_1$Purchase.made,.7),]
modelpercent
write.csv(modelpercent,file = "C:/Users/himan/Downloads/Regression.csv")
quantile(final_edata_1$Purchase.made,.7)

