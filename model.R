df1 = merge(df3, df, by.x = "wbn", by.y = "wbn")
str(df)
str(df12)
df12$cs.ss[df12$cs.ss.x == "RTO"] = 1
summary(df12$cs.ss)
table(df12$cs.ss)
df12$cs.ss = as.integer(df12$cs.ss)
df12 = df12[,-34]
library(caTools)
set.seed(89)
split = sample.split(trsf1$cs.ss, SplitRatio = 0.7)
df_sample1 = subset(trsf1,split == TRUE)
split = sample.split(df_sample.train$cs.ss, SplitRatio = 0.1)
df_sample.train1 = subset(df_sample.train, split == TRUE)
df_sample.test1 = subset(trsf1, split == FALSE)
rm(split)
rm(df_sample)

names(df_sample.train)


RTO.Log = glm(cs.ss ~ time_taken +cod + cod.rt_each.cl.bins.LOW  + cod.rt_each.cl.bins.MEDIUM + category.buckets.new.High_Return + zn.buckets.new.D + zn.buckets.new.Low + cl.bucket.Flipkart + cl.bucket.AMAZON 
              +cl.bucket.Myntra+cl.bucket.Longtailed +cl.bucket.Shopclues.Surface , data = df_sample.train, family = binomial)
summary(RTO.Log)
predictTest = predict(RTO.Log, type = "response", newdata = df_sample.test)
table(df_sample.test$cs.ss, predictTest > 0.5)
predictTrain = predict(RTO.Log,type = "response", newdata = df_sample.train)
table(df_sample.test$cs.ss, predictTest > 0.4)
624179/756213
tapply(predictTest, df_sample.test$cs.ss, mean)
sum(is.na(df_sample.train))
summary(df_sample.train)

install.packages("pROC")
library(pROC)
auc = roc(df_sample.test$cs.ss, predictTest)
print(auc)
56585/5401511
959673/5401511
table(df_sample.test$cs.ss)

library(ROCR)
Predict.ROC = prediction(predictTrain, df_sample.train$cs.ss)
ROCRperf = performance(Predict.ROC,"tpr", "fpr")
plot(ROCRperf)
df12 = merge(df, df3[,c(1,12)], by.x = "wbn", by.y = "wbn")
rm(df_train)
a = df12%>%distinct(wbn)
