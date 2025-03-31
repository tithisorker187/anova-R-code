Input the data in a simple format

data <- data.frame(
 F = rep(1:3, each = 6),
 V = rep(rep(1:2, each = 3), times = 3),
 Block = rep(1:6, times = 6),
 Yield = c(161, 192, 145, 232, 172, 227, 166, 253, 231, 231, 204, 214, 113, 208, 131, 190, 104, 144, 103, 171, 168, 171, 135, 146, 132, 196, 176, 242, 178, 186, 186, 198, 206, 298, 175, 290) )
data
#Convert F, V, and Block to factors

data$F <- as.factor(data$F)
data$F
data$V <- as.factor(data$V) 
data$V
data$Block <- as.factor(data$Block)
data$Block

Perform ANOVA

anova_model <- aov(Yield ~ F * V + Block, data = data)
anova_model
summary(anova_model)

#Orthogonal contrasts (optional)

contrasts(data$F) <- contr.helmert(3)
 contrasts(data$V) <- contr.helmert(2)
 anova_model_contrasts <- aov(Yield ~ F * V + Block, data = data) 
summary(anova_model_contrasts)