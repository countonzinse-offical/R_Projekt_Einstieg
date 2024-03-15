
# Your first R project #

rm(list = ls())

# Load the package
#install.packages("pacman")
library(pacman)
p_load(tidyverse)


# Generate a dataset
set.seed(1234)

n <- 10000

# ---- variables ----

# independent
x1 <- rnorm(n, 10, 1)
x2 <- rnorm(n, 1, 0.5)
x3 <- sample(c("m", "w"), replace = T, n)
x4 <- runif(n)
x5 <- rbeta(n, 1, 3)

# betas
b0 <- 1
b1 <- 0.1
b2 <- -1.1
b3 <- 3
b4 <- 2.7
b5 <- 1.7

# dependent
y <- round((b0 + b1*x1 + b2*x2 + b3*ifelse(x3=="m", 0, 1) + b4*x4 + b5*x5)*1000, 2)

# put them in a dataframe
data <- cbind.data.frame(y, x1, x2, x3, x4, x5)
data

data$x3 <- as.factor(data$x3)

# ---- generating missing data ----

# unit-non-response
p1 <- 0.005
miss_data <- data[sample(1:n, replace = F, n-(n*p1)), ]
nrow(miss_data)

# item-non-response
p2 <- 0.01

n_miss <- nrow(miss_data)

# index for the missing rows
idx_row <- sample(rownames(miss_data), p2*n_miss) %>% 
  as.numeric()

# index for the missing columns
idx_col <- sample(1:ncol(miss_data), replace = T, p2*n_miss)

# generating the missings
for(i in 1:length(idx_col)){
  
  miss_data[idx_row[i], idx_col[i]] <- NA
  
}

is.na(miss_data) %>% 
  sum()

# ---- descriptive statistics ----

View(miss_data)
str(miss_data)

# Histogram of the dependent variable
hist(miss_data$x1,
     col = "lightblue",
     main = "Histogram of income")

# Histogram of the dependent variable
hist(miss_data$x1,
     col = "lightblue",
     main = "Histogram of socio-status")

hist(miss_data$x2,
     col = "lightblue",
     main = "Histogram of region of birth")

plot(miss_data$x3,
     col = "lightblue",
     main = "Histogram of sex")

hist(miss_data$x4,
     col = "lightblue",
     main = "Histogram of political opinion")

hist(miss_data$x5,
     col = "lightblue",
     main = "Histogram of factor smart")

# data in describtion
psych::describe(miss_data)




