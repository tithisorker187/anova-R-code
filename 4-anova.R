#problem-4
#(i)

# Load necessary libraries
install.packages("dplyr")
library(dplyr)

# Load necessary library
library(dplyr)

# Given data (Factorial experiment with blocks)
data <- data.frame(
  A = rep(c(-1, 1), each=4),
  B = rep(c(-1, -1, 1, 1), times=2),
  C = rep(c(-1, 1), times=4),
  Yield = c(986, 930, 930, 883, 901, 815, 857, 889)  # Total yield from the table
)

# Compute main effects and interactions
data <- data %>%
  mutate(AB = A * B, AC = A * C, BC = B * C, ABC = A * B * C)

# Compute means for effects
effects <- colMeans(data[-4]) * 2  # Multiply by 2 as per factorial effect formula

# Display effects
effects

















#*****************************(ii)********************************


model1=aov(yields~treatment+block



















