

###(i)
#input data

df=data.frame(
  Block=factor(rep(1:5, each = 5)),
  Treatment = rep(c("A", "B", "C", "D", "E"), times = 5),
  Yield = c(
    18, 20, 20, 21, 21, 
    17, 19, 19, 20, 20, 
    16, 17, 18, 19, 20, 
    16, 16, 17, 18, 18, 
    16, 16, 15, 17, 16)
)
print(df)

# Perform ANOVA
anova_result=aov(Yield ~ Treatment + Block, data = df)
anova_result
# Display ANOVA table
summary(anova_result)

###(II)
confint(anova_result,level=0.90)













