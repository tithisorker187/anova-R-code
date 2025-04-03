#problem-5
install.packages("dplyr")
library(dplyr)
Data=data.frame(
Replicate=rep(1:2,each=8),
B=rep(c("B1","B2"),each=4,times=2),
Treatment=c("dnk40","p46","k52","dnp40","d50","npk38","dpk55","n41","np53","dn40","dnpk43",
"nk43","dk40","pk52","u55","dp50","npk45","d40","p40","dnp52","dpk45","n47","k50",
"dnk35","nk43","dp51","np52","pk58","dk52","dnpk56","u58","dn42"),
Yield=c(40,46,52,49,50,38,55,41,53,40,43,43,40,52,55,50,45,40,40,52,45,47,50,35,
43,51,52,58,52,56,58,42)
)
Data
Data$Replicate=as.factor(Data$Replicate)
Data$Replicate

Data$B=as.factor(Data$B)
Data$B

Data$Treatment=as.factor(Data$Treatment)
Data$Treatment

model=aov(Yield~Replicate*B*Treatment,data=Data)
model

summary(model)