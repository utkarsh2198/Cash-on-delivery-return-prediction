names(balancedData)[names(balancedData) == 'data$Y'] <- 'cs.ss'
install.packages("randomForest")
library(randomForest)


split = sample.split(df.delivered$cs.ss, SplitRatio = 0.75)
df.delivered = subset(df.delivered,split == FALSE)
df_sample = rbind(df.delivered,df.RTO)
split = sample.split(df_sample$cs.ss, SplitRatio = 0.5)
df_sample = subset(df_sample,split == TRUE)
table(df_sample$cs.ss)

split = sample.split(trsf1$cs.ss, SplitRatio = 0.70)
df_sample.train = subset(trsf1, split == TRUE)
df_sample.test = subset(trsf1, split == FALSE)
rm(split)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
RTO.rf = randomForest(cs.ss ~ time_taken +cod + cod.rt_each.cl.bins.LOW  + cod.rt_each.cl.bins.MEDIUM + category.buckets.new.High_Return + zn.buckets.new.D + zn.buckets.new.Low + cl.bucket.Flipkart + cl.bucket.AMAZON 
                      +cl.bucket.Longtailed +cl.bucket.medium+cl.bucket.Myntra+cl.bucket.Shopclues.Surface + Fraud.ph +delay, data = df_sample.train, nodesize = 20, ntree = 200,trControl = control, tuneGrid=tunegrid)
Predict.forest = predict(RTO.rf, newdata = df_sample.test)
table(df_sample.test$cs.ss, Predict.forest)

Predict.forest = predict(RTO.rf, newdata = df_sample.train)
table(df_sample.train$cs.ss, Predict.forest)

varImpPlot(RTO.rf, sort = T, main="Variable Importance")

library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(7)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- caret::train(cs.ss~time_taken +cod + cod.rt_each.cl.bins.LOW  + cod.rt_each.cl.bins.MEDIUM + category.buckets.new.High_Return + zn.buckets.new.D + zn.buckets.new.Low + cl.bucket.Flipkart + cl.bucket.AMAZON 
                              +cl.bucket.Longtailed +cl.bucket.medium+cl.bucket.Myntra+cl.bucket.Shopclues.Surface + Fraud.ph +delay, data = df_sample.train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
