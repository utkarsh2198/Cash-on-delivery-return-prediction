df123 = read.csv("/data/customer/apr_may17_pdd.csv")
str(df123)
df12 = merge(df, df123, by.x = "wbn", by.y = "wbn")
dfnew = df12 %>% filter(cs.ss %in% c("Delivered", "RTO"))
a <- subset(dfnew,dfnew$pdd=="")
dfnew = dfnew %>%
  filter(cod >= 50) %>%
  filter(cod <= 500000)%>%
  filter(pdd != "")
df12 = read.csv("/home/shubham/home/testuser/project/df12.csv")
df1 = merge(df_analyze, df1, by.x = "cnc", by.y = "City")

df = df%>%
  select(wbn,zn)

df1 = merge(df1, df, by.x = "wbn", by.y = "wbn")
rm(df)
str(df1)
table(df1$Tier == "Tier 5")
df1 = merge(df1, df_date, by.x = "wbn", by.y = "wbn")
df3 = merge(df2, df, by.x = "wbn", by.y = "wbn")
df=df3[,c(1,2,)]%>%distinct()
b = b[,-2]
