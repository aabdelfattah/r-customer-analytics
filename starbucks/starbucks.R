library(dplyr)

starbucks_data = read.csv("./starbucks/starbucks.csv", sep=";", encoding = "UTF-8")

str(starbucks_data)
psych::describe(starbucks_data)
starbucks_data = starbucks_data[1:140,]
starbucks_data <- starbucks_data %>%
  mutate(across(4:11, as.numeric))
library(data.table)
starbucks_table <- as.data.table(lapply(starbucks_data, as.numeric))

vars <- scale(starbucks_table[1:140,4:11])
cor <- cor(vars)


library(corrplot)
corrplot(cor, 
         method = "number", 
         type = "upper", 
         order = "hclust", # reorder by the size of the correlation coefficients
         tl.cex = 1, # font size of the variable labels
         tl.col = "black", # color of the variable labels
         tl.srt = 45, # rotation angle for the variable labels
         number.cex = 0.8 # font size of the coefficients
)




##########

model <- lm(starbucks_data[,3] ~ ., data = starbucks_data[,4:11])
summary(model)



library(olsrr)
stepwise_model=ols_step_both_p(model, pent = 0.1, prem = 0.3, details = TRUE)
final_model <- stepwise_model$model 
summary(final_model)
