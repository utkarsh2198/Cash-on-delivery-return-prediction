df.RTO = subset(df, cs.ss == 1)
names(a.prop)[names(a.prop) == '1'] <- 'ph1'

##Clients
a = as.data.frame(round(sort(table(df12$cl),decreasing = TRUE),6))
a$cl.bucket= "medium"
a$cl.bucket[21:1823]= "Longtailed"
a$cl.bucket[1:6]= a$Var1[1:6]
View(a)
a$cl.bucket[1:6]= as.character(a$Var1[1:6])
a$cl.bucket = as.factor(a$cl.bucket)
df3 = merge(df3, a, by.x = "cl", by.y = "Var1")

a = merge(a, a.prop, by.x = "Var1", by.y = "Var1")


##Categories

CrossTable(df12$cat.buckets,df12$cs.ss)
df2 = df2[,-13]
b = as.data.frame(round(sort(prop.table(table(df12$cat.buckets)),decreasing = TRUE),6))
b.rto = as.data.frame(round(sort(table(df.RTO$cat.buckets),decreasing = TRUE),6))


b$cat.buckets = ""
b$cat.buckets[b$Freq<=0.01] = "Others"
b$cat.buckets[1:14]= as.character(b$Var1[1:14])
df2 = df2[,-13]
df12 = merge(df12, b, by.x = "cat.buckets.y", by.y = "Var1")
df2$cat.buckets.y[df2$pseg.cat == "Uncategorized"] = "Others"
b$cat.buckets[b$Var1 == "Uncategorized"] = "Others"
b$cat.buckets[c(2,3,4,8,10)] = "High_Return"
b$cat.buckets[c(1,5,6,7,9,11,12,13,14)] = "Low_Return"
names(b)[names(b) == "cat.buckets"] <- "category.buckets"



##Zones
head(round(sort((table(df2$zn.buckets)),decreasing = TRUE),6),20)
c = as.data.frame(round(sort(prop.table(table(df12$zn)),decreasing = TRUE),6))
CrossTable(df12$zn.buckets.y,df12$cs.ss)
c$zn.buckets = ""
c$zn.buckets[c$Freq<=0.01] = "Others"
c$zn.buckets[1:11]= as.character(c$Var1[1:11])
df12 = merge(df12, c, by.x = "zn", by.y = "Var1")
c$zn.buckets[c(4,5,6,8,9)] = "High"
c$zn.buckets[c(3,7,10,11,12:23)] = "Low"
names(c)[names(c) == "zn.buckets"] <- "zn.buckets.new"


str(df2)
df2 = df2[,-c(19,17,15,13)]



tot = count(subset(df[,c(3,7,17,19)],df$Fraud.ph == TRUE),ph1)
summary(tot)
rto = count(subset(df[,c(3,7,17,19)],cs.ss.x=="RTO" & df$Fraud.ph==TRUE),ph1)


23
###phone no.
library(stringr)
df12 = cbind(df3,str_split_fixed(df3$ph.y, ",|/", 2))
names(df12)[names(df12) == '1'] <- 'ph1'
str(df12)
df12$ph1 = gsub("\\[\"|\"\\]","",df12$ph1)

df12$Fraud.ph = grepl("\\D+",df12$ph1)

summary(df12$Fraud.ph)
df12$Fraud.ph[df12$Fraud.ph == FALSE] =  nchar(as.character(df12$ph1[df12$Fraud.ph == FALSE])) == 21
df = subset(df,df$right==FALSE)
head(round(sort(table(df$ph1),decreasing = TRUE),6),20)
df12$Fraud.ph[df12$Fraud.ph == FALSE] =  nchar(as.character(df12$ph1[df12$Fraud.ph == FALSE])) != 10

ph1 = df[,c(17,19)]
ph1$right =  nchar(as.character(ph1)) == 10
a = grepl("^(\\d)\\1*$",ph1$ph1)
summary(df$right)
a = ph1$ph1 == ""
summary(a)

df12$Fraud.ph[df12$Fraud.ph == FALSE] = grepl("^(\\d)\\1*$",df12$ph1[df12$Fraud.ph == FALSE])
names(df)[names(df) == "right"] <- "Fraud.ph"


###customer.history
str(df)

tot = count(subset(df[,c(3,7,17,19)],df$Fraud.ph == TRUE),ph1)
summary(tot)
rto = count(subset(df[,c(3,7,17,19)],cs.ss.x=="RTO" & df$Fraud.ph==TRUE),ph1)

frac = merge(tot, rto, by.x = "ph1", by.y = "ph1",all = TRUE)
frac$n.y[is.na(frac$n.y)] = 0

frac$customer.return = frac$n.y/frac$n.x

df = merge(df, frac, by.x = "ph1", by.y = "ph1")
summary(frac$customer.return)
ggplot(frac, aes(customer.return)) + geom_histogram() 
quantile(frac$customer.return,prob = seq(0,1,length=11))

str(df)
apply
sum(as.numeric(frac$n))
df1$frac.del_each.cl = df1$n/5596695


####
CrossTable(df12$cod,df12$cs.ss)
df2 = df2[,-13]
b = as.data.frame(round(sort(prop.table(table(df12$cat.buckets)),decreasing = TRUE),6))



quantile(df12$cod,prob = seq(0,1,length=5))
df12$cod.buckets = " Very High"
df12$cod.buckets[df$cod<=6000] = "High"
df12$cod.buckets[df$cod<=2500] = "Medium"
df12$cod.buckets[df$cod<=1000] = "Low"
df12$cod.buckets[df$cod<=300] = "Very Low"
summary(df12$cod.buckets)
df12$cod.buckets = as.factor(df12$cod.buckets)
table(df12$cod.buckets,df12$cs.ss)


#####

quantile(df12$time_taken,prob = seq(0,1,length=5))
df12$time_taken.buckets = "VeryHigh"
df12$time_taken.buckets[df12$time_taken<10] = "High"
df12$time_taken.buckets[df12$time_taken<5] = "Medium"
df12$time_taken.buckets[df12$time_taken<2] = "Low"
df12$time_taken.buckets[df12$time_taken<1] = "VeryLow"
df12$time_taken.buckets = as.factor(df12$time_taken.buckets)
table(df12$time_taken.buckets,df12$cs.ss)
