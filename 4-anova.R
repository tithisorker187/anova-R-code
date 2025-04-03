#problem-4
#(i)

# Load necessary libraries
install.packages("dplyr")
library(dplyr)

# Define the data as given in the image
data <- data.frame(
  Block = rep(1:4, each=8),
  Treatment = rep(c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc"), times=4),
  Yield = c(257, 232, 230, 211, 210, 176, 186, 175,
            267, 276, 262, 220, 256, 269, 285, 272,
            188, 186, 160, 188, 164, 214, 182, 166,
            204, 206, 239, 224, 254, 269, 252, 301)
)
data
# Encode factors
data$A <- ifelse(grepl("a", data$Treatment), 1, -1)
data$A
data$B <- ifelse(grepl("b", data$Treatment), 1, -1)
data$B
data$C <- ifelse(grepl("c", data$Treatment), 1, -1)
data$C

# Compute interaction terms
data$AB <- data$A * data$B
data$AB
data$AC <- data$A * data$C
data$AC
data$BC <- data$B * data$C
data$BC
data$ABC <- data$A * data$B * data$C
data$ABC

# Perform linear regression to estimate effects
model_e <- lm(Yield ~ A + B + C + AB + AC + BC + ABC, data=data)

# Display effects
summary(model_e)

#(ii)
#test of significance
anova_s=aov(Yield~A*B*C,data=data)
anova_s
summary(anova_s)

#III
#ABC confounding
anova_model=aov(Yield~Block+Treatment,data=data)
anova_model






















