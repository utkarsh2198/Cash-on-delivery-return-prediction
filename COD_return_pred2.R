ggplot(df1, aes(rgn, fill = Tier)) + geom_histogram(stat = "count")
rto = df1 %>%
  select(cl,cs.ss)%>%
  group_by(cl)%>%
  summarise_each(funs(sum(cs.ss == "RTO",na.rm=TRUE)),cs.ss )

tot = count(df1[,-c(7,13,14,15,16)],cl)
summary(tot)
rto = count(df1[,-c(7,13,14,15,16)],cl)

frac = merge(tot, rto, by.x = "cl", by.y = "cl")
frac$cod.rt_each.cl = frac$cs.ss/frac$n
df1 = merge(df1, frac, by.x = "cl", by.y = "cl")
str(df1)
apply
sum(as.numeric(frac$n))
df1$frac.del_each.cl = df1$n/5596695
df1 = df1[,-c(19,20)]
str(df1)
df1$delivery_cost = df1$cod - df1$rs
str(df1)

tot = count(df1[,-c(7,13,14,15,16)],ph)
rto = df1 %>%
  select(ph,cs.ss.x)%>%
  group_by(ph)%>%
  summarise_each(funs(sum(cs.ss.x == "RTO",na.rm=TRUE)),cs.ss.x )

df_analyze1 = df1%>%
  select(cl,wbn,rs,cod,rgn,pseg.cat,cs.ss.x,Tier,cod.rt_each.cl, frac.del_each.cl, delivery_cost)
summary(df_analyze1)
df_date = read.csv("/data/customer/apr_may17_fdd.csv")
rm(df_analyze1)
str(df3)
