#Expertiment-4

df=data.frame(
Block=factor(rep(1:4,each=8)),
A=factor(c(0,1,1,0,0,1,1,0,
1,0,0,1,1,0,1,0,
0,0,0,1,1,0,1,1,
1,1,0,0,1,0,0,1)),
B=factor(c(0,0,1,1,0,0,1,1,
0,0,1,1,0,0,1,1,
1,1,0,0,1,0,1,0,
1,0,0,1,0,1,0,1)),
C=factor(c(0,0,0,0,1,1,1,1,
1,0,0,1,0,1,0,1,
0,1,1,0,0,0,1,1,
0,1,0,1,0,0,1,1)),
Yield=c(257,232,230,211,210,176,186,175,
267,276,262,220,256,269,285,272,
188,186,160,188,164,214,182,166,
204,206,239,224,254,269,252,301)
)
df

anova=aov(Yield~Block+A*B*C,data=df)
anova
summary(anova)
#(ii)

coefficients=coef(anova)
coefficients


treatment=c("(1)","a","b","ab","c","ac","bc","abc")
total_yield=c(986,930,930,883,901,815,857,889)

yates_function <- function(yields) {
  n <- length(yields)
  mat <- matrix(0, nrow = log2(n) + 1, ncol = n)
  mat[1, ] <- yields
  
  for (i in 1:log2(n)) {
    step <- 2^(i - 1)
    for (j in seq(1, n, by = 2 * step)) {
      for (k in 0:(step - 1)) {
        mat[i + 1, j + k] <- mat[i, j + k] + mat[i, j + k + step]
        mat[i + 1, j + k + step] <- mat[i, j + k] - mat[i, j + k + step]
      }
    }
  }
  return(mat)
}

# Run Yates' function
yates_table <- yates_function(total_yield)
print(yates_table)











coefficients=coef(anova)
coefficients

model=aov(Yield~Block+A*B*C-A:B:C,data=df)
model
summary(model)
















