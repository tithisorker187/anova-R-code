#***************************problem-(1)************************************

##(I)

#input the data
data=data.frame(
   Treatment=rep(c("A","B","C","D","E","F"),times=5),
   Yeild=c(39,26,35,26,24,28,
           45,43,28,27,28,36,
           28,32,36,30,31,45,
           32,40,24,25,29,41,
           38,29,31,42,44,25)
)
data
anova=aov(Yeild~Treatment,data=data)
anova
summary(anova)


##(ii)
confint(anova,level=0.95)
