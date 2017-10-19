df = read.csv("/data/customer/apr_may17_cod.csv", sep = "|", header = T)
df = df%>%
  select(wbn,ph)
str(df)
summary(df)
df = df[,-1]
df_cat = subset(df,select = c(cl,zn,rgn,pseg.cat,cs.ss))
table(df_cat$pseg.cat)
table(df_cat$cs.ss)
table(dfnew$cs.ss)
table(df$cs.ss)
library(dplyr)
dfnew1 = as.data.frame(df[df$cs.ss == c("Delivered","RTO"),])
df1 = df %>% filter(cs.ss %in% c("Delivered", "RTO"))
df1 = df1 %>% filter(cod >= 50)
df1 = df1 %>% filter(cod >= 50 && cod <= 500000)
summary(df1$cod)
rm(dfnew)
rm(dfnew1)
df_cat = subset(df1,select = c(cl,zn,rgn,pseg.cat,cs.ss))
df1 = df %>% filter(cs.ss %in% c("Delivered", "RTO"))
df1 = df1 %>% filter(cod >= 50 && cod <= 500000)
summary(df1$cod)
dfnew = df %>% filter(cs.ss %in% c("Delivered", "RTO"))
dfnew = dfnew %>%
  filter(cod >= 50) %>%
  filter(cod <= 500000)
summary(dfnew$cod)
df_cat = subset(dfnew,select = c(cl,zn,rgn,pseg.cat,cs.ss))
apply(df_cat, 2, function(x){length(unique(x))})
summary(df_cat$cl)
head(sort(table(df_cat$cl), decreasing = FALSE),20)
head(sort(table(df_cat$cl), decreasing = FALSE),50)
head(sort(table(df_cat$cl), decreasing = FALSE),100)
View(dfnew)
tail(summary(df_cat$cl))
tail(sort(summary(df_cat$cl),decreasing = TRUE))
head(sort(summary(df_cat$cl),decreasing = TRUE))
table(df_cat$cl)
summarise_each(funs(sum(.,na.rm=TRUE)),cl)
summarise_each(funs(sum(.,na.rm=TRUE)),cs.ss)
df_cat %>%
  groupby(cl)%>%
  summarise_each(funs(sum(.,na.rm=TRUE)),cs.ss)
df_cat %>%
  group_by(cl)%>%
  summarise_each(funs(sum(.,na.rm=TRUE)),cs.ss)
summarise_each(funs(count(.,na.rm=TRUE)),cs.ss)
df_cat %>%
  group_by(cl)%>%
  summarise_each(funs(count(.,na.rm=TRUE)),cs.ss)
count(df_cat,cl)
sort(count(df_cat,cl),decreasing = FALSE)
count(df_cat,cl<=500)
count(df_cat,cl<=5000)
count(df_cat,cl>=5000)
n = count(df_cat,cl)
summary(n)
str(df_cat)
table(dfnew$cs.ss)
head(sort(summary(df_cat$cl),decreasing = TRUE))
apply(df_cat, 2, function(x){length(unique(x))})
str(df_cat)
summary(df_cat$cs.ss)
apply(df_cat, 2, function(x){length(unique(x))})
df_cat %>%
  group_by(zn)%>%
  summarise_each(funs(count(.,na.rm=TRUE)),cs.ss,cl)
summary(df_cat$zn)
summary(df_cat$rgn)
table(df_cat$rgn)
table(df_cat$zn,df_cat$cs.ss)
table(df_cat$zn,df_cat$cs.ss)
t = table(df_cat$zn,df_cat$cs.ss)
t = as.data.frame(table(df_cat$zn,df_cat$cs.ss))
View(n)
View(t)
table(df_cat$rgn)
str(dfnew)
table(df_cat$pseg.cat)
str(dfnew)
summary(dfnew$ph)
count(dfnew$ph)
count(dfnew,ph)
summary(m)
m = count(dfnew,ph)
summary(m)
summary(m$n)
summary(m$n>=50)
summary(m$n>=5)
summary(m$n==1)
str(dfnew)
date = as.POSIXlt(dfnew$cs.sd)
unclass(date[1])
apr_may17_cod <- read.csv("/data/customer/apr_may17_cod.csv", stringsAsFactors=FALSE)
View(apr_may17_cod)as.POSIXct(dfnew$cs.sd[1], format = "%y-%m-%d %H:%M:%S")
as.POSIXct(dfnew$cs.sd, format = "%Y-%m-%dT%H:%M:%OSZ")

dfnew$cs.sd = as.POSIXlt(dfnew$cs.sd, format = "%Y-%m-%dT%H:%M:%OSZ")
dfnew$cd = as.POSIXlt(dfnew$cd, format = "%Y-%m-%dT%H:%M:%OSZ")
dfnew$pdd = as.POSIXlt(dfnew$pdd, format = "%Y-%m-%dT%H:%M:%OSZ")
dfnew$pd = as.POSIXlt(dfnew$pd, format = "%Y-%m-%dT%H:%M:%OSZ")

str(dfnew)

df_analyze = dfnew%>%
  select(wbn,cl,pin,ph,cn,cd,rs,cod,rgn,cnc,pseg.cat,cs.ss,cs.sd,pdd)

df_analyze$time_taken = df_analyze$cs.sd - df_analyze$cd
df_analyze$time_promised = df_analyze$pdd - df_adf_analyze$cd
df_analyze$time_promised = df_analyze$pdd - df_analyze$cd
df_analyze$delay = ifelse(df_analyze$time_taken<=df_analyze$time_promised ,FALSE,TRUE)

summary(df_analyze$delay)
rm(df)
rm(dfnew)
(df_analyze$time_taken[1])$date
df_analyze$delay = ifelse(df_analyze$time_taken-df_analyze$time_promised>=1, TRUE, FALSE)
summary(df_analyze$delay)
22/35
22/(35+22)

df_analyze%>%
  filter(cs.ss == "Delivered")%>%
  df_analyze$delay = ifelse(df_analyze$cs.sd-df_analyze$pdd>=1, TRUE, FALSE)

df_deli = subset(df1,df1$cs.ss == "Delivered")
df_deli$delay = ifelse(df_deli$dd.fdd-df_deli$pdd>=1, TRUE, FALSE)

summary(df_deli$delay)
12/47
1203871/(1203871+3493039)       

df_deli$delay = ifelse(df_deli$cs.sd<=df_deli$pdd, FALSE, TRUE)
summary(df_deli$delay)
summary(df1$dd.fdd)
n = count(df_analyze,ph)
n = count(df_analyze[,-c(6,13,14,15,16,17)],ph)
summary(n)
rm(df123)

df_delayed = subset(df_deli,df_deli$delay==TRUE)
summary(df_delayed)
0.57/4.5
df = df%>%
  select(zn,wbn)
library(ggplot2)
library(lazyeval)

ggplot(df1 , aes(x = zn)) + geom_histogram(stat = "count")
df1$dd.fdd = as.POSIXlt(df1$dd.fdd, format = "%Y-%m-%dT%H:%M:%OSZ")

str(df1)
summary(df1)
df1$delay = ifelse(df1$dd.fdd-df1$pdd>=1, TRUE, FALSE)
summary(df1$delay)
df2 = na.omit(df1)
summary(df2$delay)
rm(df1)
df2$time_taken = df2$dd.fdd - df2$cd
summary(df2$time_taken)
units(df2$time_taken) <- "days"
df2 = subset(df2, time_taken <= 15)
str(df2)
df2 = df2[,-c(15,16)]
quantile(df3$time_taken,prob = seq(0.9,1,length=11))
