# experiment-2
df=data.frame(
Block=factor(rep(1:4,each=5)),
Treatment=factor(c("B","C","A","D","E",
"A","B","D","E","C",
"B","D","C","A","E",
"E","A","B","C","D")),
Yield=c(30,23,20,18,24,
28,26,19,15,19,
22,24,29,37,25,
21,18,23,28,25)
)
df

anova=aov(Yield~Block+Treatment,data=df)
anova
summary(anova)
#from anova table

#(ii)

#fit model for RBD
model_rbd=aov(Yield~Block+Treatment,data=df)
model_rbd
summary(model_rbd)

#model CRD ignoring block

model_crd=aov(Yield~Treatment,data=df)
model_crd
summary(model_crd)

mse_rbd=summary(model_rbd)[[1]]["Residuals","Mean Sq"]
mse_rbd

mse_crd=summary(model_crd)[[1]]["Residuals","Mean Sq"]
mse_crd


RE=mse_crd/mse_rbd
RE

#RE>1 so block factor RBD has been effective sligthly in reducing error variance.


df=data.frame(
Block=factor(rep(1:4,each=5)),
Treatment=factor(c("B","C","A","D","E",
"A","B","D","E","C",
"B","D","C","A","E",
"E","A","B","C","D")),
Yield=c(30,23,20,18,24,
28,26,19,15,19,
22,24,29,NA,25,
21,18,23,28,25)
)
df


miss.index=which(is.na(df$Yield))
mis.index
miss.block=df$Block[miss.index]
mis.block
miss.treatment=df$Treatment[miss.index]
miss.treatment
n.b=4
n.b
n.t=6
n.t
b.sum=sum(df$Yield[df$Block==miss.block],na.rm=TRUE)
b.sum
t.sum=sum(df$Yield[df$Treatment==miss.treatment],na.rm=TRUE)
t.sum
g.sum=sum(df$Yield,na.rm=TRUE)
g.sum

missing.value=((n.b*b.sum)+(n.t*t.sum)-g.sum)/15
missing.value



#find standard error

df$Yield[miss.index]=missing.value
df$Yield[miss.index]

anova.table=aov(Yield~Block+Treatment,data=df)
anova.table
summary(anova.table)

MSE=summary(anova.table)[[1]]["Residuals","Mean Sq"]
MSE

SE=sqrt(2*MSE/4)
SE

























































































