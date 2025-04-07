#Experiment-1

df=data.frame(
Treatment=factor(rep(c("A","B","C","D","E","F"),times=5)),
Yield=c(39,26,35,26,24,28,
45,43,28,27,28,36,
28,32,36,30,31,45,
32,40,24,25,29,41,
38,29,31,42,44,25)
)
df
#(i)
anova=aov(Yield~Treatment,data=df)
anova
summary(anova)
#from anova table
F=0.676
F
Treatment_df=5
Treatment_df
Error_df=24
Error_df

pf(F, Treatment_df, Error_df, lower.tail=FALSE)

#(ii)

t_test_AE=t.test(Yield~Treatment,data=subset(df,Treatment%in%c("A","E")),conf.level=0.95)
t_test_AE
CI=t_test_AE[["conf.int"]]
CI
