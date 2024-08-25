install.packages("psych") # A package for multivariate analysis and basic descriptive statistics
install.packages("tidyverse") # A collection of R packages for data science (manipulation, transformation, visualization of data)
install.packages("data.table") # A package for data manipulation
install.packages("olsrr") # For stepwise regression analysis
library(psych) 
library(tidyverse) 
library(data.table)
library(olsrr)
HCP <-read.csv("./heating-cost/HeatingCost.csv")
# Print the first 10 rows (by default, the first 6 rows)
head(HCP, 10)
# Convert all integer variables into numeric ones for futher analysis
library(data.table)
HCP <- as.data.table(lapply(HCP, as.numeric))

psych::describe(HCP)
summary(HCP)
mean(HCP$Heating.Cost)
min(HCP$Heating.Cost)
max(HCP$Heating.Cost)
sd(HCP$Heating.Cost)
sum(HCP$Heating.Cost)
length(HCP$Heating.Cost)

colMeans(HCP[ , c("Heating.Cost", "Minimum.Temperature")])
colMeans(HCP[HCP$Age == 6, c("Heating.Cost", "Minimum.Temperature")])

mean.HCP = aggregate(HCP[, c("Heating.Cost", "Minimum.Temperature")], # to select the specified variables (columns) 
                     by = list(Age = HCP$Age), 
                     FUN = mean)
mean.HCP

CorHCP<-cor(HCP)
CorHCP
upper <- round(CorHCP, 2)
upper[upper.tri(CorHCP)] <- ""
upper <- as.data.frame(upper)
upper

table(HCP$Windows)
prop.table(table(HCP$Windows))
addmargins(prop.table(table(HCP$Windows)))


table(HCP$Windows, HCP$Age, 
      dnn = c("windows", "age"))
# FREQUENCY MARGINALS
freqtable = table(HCP$Windows, HCP$Age, 
                  dnn = c("windows", "age"))

# row marginals - totals for each windows category across age groups
margin.table(freqtable, 1)
# column marginals - totals for each age level across windows categories
margin.table(freqtable, 2)

addmargins(table(HCP$Windows, HCP$Age, 
                 dnn = c("windows", "age")))


model1<-lm(Heating.Cost ~ Minimum.Temperature, data = HCP)
summary(model1)
model1$coefficients
model1$residuals
confint(model1)


model2 <- lm(HCP$Heating.Cost ~ HCP$Minimum.Temperature + HCP$Insulation + HCP$Age + HCP$Windows, data = HCP)
summary(model2)
confint(model2)


ols_step_both_p(model2, pent = 0.1, prem = 0.3, details = FALSE)
