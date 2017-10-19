install.packages("unbalanced")
library(unbalanced)
rm(df,df3)
df = df12 %>% select(df12$cod,df12$cs.ss.x,df12$)
df = df12[,c(11,16,20,21,22,26,28,30,33,34)]
str(df)
install.packages("caret")
library(caret)
df_sample$cs.ss[df_sample$cs.ss==2] = 1 
df.delivered = df[df$cs.ss==0,]
df.RTO = df[df$cs.ss==1,]
dmy<-dummyVars("~ Tier + cod.rt_each.cl.bins + category.buckets.new + zn.buckets.new + cl.bucket",data=df_sample)
trsf1 = data.frame(predict(dmy,newdata= df_sample))
trsf1 = cbind(df_sample$cs.ss,trsf1)
trsf1 = cbind(df_sample$cod,trsf1)
trsf1 = cbind(df_sample$time_taken,trsf1)
trsf1 = cbind(df_sample$delay,trsf1)
trsf1 = cbind(df_sample$Fraud.ph,trsf1)


library(car)
vif(RTO.Log)


str(trsf1)
names(trsf1)[names(trsf1) == "df_sample$cs.ss"] <- "cs.ss"
names(trsf1)[names(trsf1) == "df_sample$cod"] <- "cod"
names(trsf1)[names(trsf1) == "df_sample$time_taken"] <- "time_taken"
names(trsf1)[names(trsf1) == "df_sample$delay"] <- "delay"
names(trsf1)[names(trsf1) == "df_sample$Fraud.ph"] <- "Fraud.ph"


set.seed(10)
sub <- sample(nrow(trsf), floor(nrow(trsf) * 0.9))
sub1 <- sample(nrow(trsf), floor(nrow(trsf) * 0.1))
training <- trsf [sub, ]
testing <- trsf [-sub, ]
training_sub<- trsf [sub1, ]

n<-ncol(df_sample)
output<- df_sample$cs.ss
output<-as.factor(output)
input<- df_sample[ ,-n]
View(input)

df$cs.ss = as.factor(df$cs.ss)

data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)

balancedData<-cbind(data$X,data$Y)
View(balancedData)

sum(is.na(balancedData))
install.packages("gbm")
library(gbm)
fitcontrol<-trainControl(method="repeatedcv",number=10,repeats=1,verbose=FALSE)
gbmfit<-caret::train(cs.ss ~ time_taken +cod + cod.rt_each.cl.bins.LOW  + cod.rt_each.cl.bins.MEDIUM + category.buckets.new.High_Return + zn.buckets.new.D + zn.buckets.new.Low + cl.bucket.Flipkart + cl.bucket.AMAZON 
                     +cl.bucket.Longtailed +cl.bucket.medium+cl.bucket.Myntra+cl.bucket.Shopclues.Surface + Fraud.ph +delay , data = df_sample.train , method="gbm",trControl = fitcontrol,verbose=FALSE)

score_Y=predict(gbmfit,newdata=df_sample.test,type="prob")[,2]
score_Y=ifelse(score_Y>0.5,1,0)
table(score_Y,df_sample.test$cs.ss)


library(caret)
##Undersampling
d = undersample(classifTask, rate = 0.5, cl = NULL)
library(caret)
classifTask = makeClassifTask(data = df_sample,  target = "cs.ss")
class(df_sample$cs.ss)
View(data)
df_sample.train$cs.ss = as.factor(df_sample.train$cs.ss)
rm(a,a.prop)
