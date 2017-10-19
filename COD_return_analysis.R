install.packages("gmodels")
library(gmodels)
CrossTable(df_analyze1$Tier,df_analyze1$cs.ss.x)
CrossTable(df_analyze1$pseg.cat,df_analyze1$cs.ss.x)
CrossTable(df3$rgn,df3$cs.ss.x)
CrossTable(df3$cod.rt_each.cl.bins,df3$delay)
table(df$cod.rt_each.cl.bins, df$)

CrossTable(df2$,df3$delay)


ggplot(df_analyze1, aes(pseg.cat, fill = cs.ss.x)) + geom_bar() 
ggplot(df, aes(Tier, fill = cs.ss.x)) + geom_bar() 


ggplot(df_analyze1, aes(cod, cod.rt_each.cl)) + geom_point() 
table(df1$zn)
CrossTable(df2$delay,df2$cs.ss.x)
str(df2)
df3 = df2%>%
  select(cl,cod,rgn,pseg.cat,cs.ss.x,Tier,zn,cod.rt_each.cl,frac.del_each.cl,delay,time_taken)
df3 = df3%>%
  filter(zn!="BLBL")%>%
  filter(zn!="CCCC")%>%
  filter(zn!="LKNC")%>%
  filter(zn!="NCHY")
ggplot(df3, aes(zn, time_taken)) + geom_boxplot() 
str(df3)  
summary(df3$pseg.cat)
df3 = df3[,c(1,2,11)] %>%
  filter(pseg.cat!="Home And Kitchen")%>%
  filter(pseg.cat!="Stationery and office Products")

df2 = subset(df2,pseg.cat!="Stationery and office Products")

ggplot(df3, aes(cs.ss.x, cod )) + geom_boxplot() 

ggplot(df3, aes(cs.ss.x, time_taken)) + geom_boxplot() 
str(df3)

ph = as.character(df3$ph.y[1:10])
r = regexpr("(.*?){0,9}",df2$ph)
regmatches(df2$ph[1:10],r)
grep("[\"[^0-9]*\"]",df2$ph,value = TRUE)
r = gsub("\\[\"|\"\\]","",ph[1:10])
df3$ph = gsub("\\D+","",df3$ph.y)
head(sort(table(as.numeric(n$ph)),decreasing = TRUE),20)
ph[1:10]
r= as.data.frame(r)
x = gregexpr(pattern =',', ph)
a=ph[x[1] != -1]



table(df2$ph)
a = subset(df2,df2$ph == "")
str(df3)
summary(df3$cod.rt_each.cl)
df2$cod.rt_each.cl.bins <- cut(df2$cod.rt_each.cl, breaks=c(-Inf,0.13,0.19,1), labels=c("LOW","MEDIUM","HIGH"))
summary(df3$cod.rt_each.cl.bins)
str(df3)
summary(df3$frac.del_each.cl)
ggplot(df3, aes(frac.del_each.cl)) + geom_histogram() 
ggplot(df3, aes(cod.rt_each.cl.bins, fill = delay)) + geom_bar() 
rm(df3)
n = count(df3[,c(22,23)],ph.y)
summary(n)
table(n)
head(sort(table(n),decreasing = TRUE),20)
head(sort(table(as.numeric(df3$ph.x)),decreasing = TRUE),20)
df=df3[,c(1,2,5)]%>%distinct(ph.x)
rm(df)
n = count(df3[,c(1,2,5,24)],ph)
summary(n)

summary(count(df2[,c(1,2)],cl)$n)
head(round(sort(prop.table(table(df2$cl)),decreasing = TRUE),6),25)
df2 = df3 %>% 
  select(wbn,cl,pin,cod,pseg.cat,cs.ss.x,Tier,zn,frac.del_each.cl,delay,cod.rt_each.cl.bins,ph.y)

head(sort(table(df2$pseg.cat),decreasing = TRUE),24)
a$cl.bucket[1:6]= a$Var1[1:6]
summary(a)
