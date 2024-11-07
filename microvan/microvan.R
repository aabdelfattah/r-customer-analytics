microvan_data = read.csv("./microvan/microvan.csv",  sep = ";")
str(microvan_data)
psych::describe(microvan_data)

library(data.table)
microvan_table <- as.data.table(lapply(microvan_data, as.numeric))

vars <- scale(microvan_table[,-1])
outliers <- abs(vars) > 3 

outlier_summary <- colSums(outliers)

# Print the summary of outliers per attribute
print(outlier_summary)

boxplot(vars[, 2:31], main = "Boxplot of Multiple Attributes", las = 2, ylab = "Scores")
hist(vars, breaks = 20, main = "Histogram of Z-Scores", xlab = "Z-Scores")


cor <- cor(vars)
upper<-round(cor,3) # we round the results to the 3d digit after comma
upper[upper.tri(cor)]<-""
upper<-as.data.frame(upper)
upper

library(corrplot)
corrplot(cor, 
         method = "number", 
         type = "upper", 
         order = "hclust", # reorder by the size of the correlation coefficients
         tl.cex = 1, # font size of the variable labels
         tl.col = "black", # color of the variable labels
         tl.srt = 45, # rotation angle for the variable labels
         number.cex = 0.3 # font size of the coefficients
)


cor_matrix_long <- as.data.frame(as.table(cor))

cor_matrix_long <- cor_matrix_long[cor_matrix_long$Var1 != cor_matrix_long$Var2, ]

strong_correlations <- subset(cor_matrix_long, abs(Freq) > 0.7)
sorted_correlations <- strong_correlations[order(strong_correlations$Freq, decreasing = TRUE), ]

print("Correlations > 0.7 or < -0.7:")
print(sorted_correlations)



##########

model <- lm(microvan_data$mvliking ~ ., data = microvan_data[, 3:32])
summary(model)

par(mfrow = c(2, 2))  # Plot in a 2x2 grid
plot(model) 

library(olsrr)
stepwise_model=ols_step_both_p(model, pent = 0.1, prem = 0.3, details = FALSE)
final_model <- stepwise_model$model 
par(mfrow = c(2, 2))  # Plot in a 2x2 grid
plot(final_model) 

##########

#The analyses run in the previous session addressed how well the 30 attribute variables explain the concept
#liking variable. 
# 1. These results may potentially be improved by uncovering the degree of redundancy in the 30
# attribute variables. First, you should explore whether this is indeed the case by using factor analysis. 
# 2. Second,please segment the market using cluster analysis based on the factor scores obtained previously – remember
# to also interpret and discuss the meaning of the clustering solution. 
# 3. Finally, we can relate clusters to demographics to run a “reality check”; please explore the demographic profile of each of the clusters and
# check whether they are consistent with prior analysis insights.
##########

vars <- microvan_table[,3:32]
cor <- cor(vars)
library(psych)
scree(cor, pc = TRUE, factors = FALSE)

EV = eigen(cor)$values
EV
EV/length(EV)
cumsum(EV/length(EV))
# Shares for the cumulative variance explained
plot(cumsum(EV/length(EV)), 
     type = "o", # type of plot: "o" for points and lines 'overplotted'
     col = "darkblue",
     pch = 16, # plot symbol: 16 = filled circle
     cex = 1, # size of plot symbols
     xlab = "Number of factors", # a title for the x axis
     ylab = "Cumulative variance explained", # a title for the y axis
     lwd = 2) # line width
abline(v = 5, lwd = 2, col = "grey") # draw a vertical line at v = 4


#PCA with 5 factors
PCA <- principal(r = cor, 
                 nfactors = 5, 
                 rotate="varimax",
                 scores = TRUE)

print(PCA, 
      digits = 3, # to round numbers to the third digit 
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)

PCA5.scores = factor.scores(vars, unclass(PCA$loadings))$scores
PCA5.scores

#PCA with 3 factors
library(psych) 
PCA <- principal(r = cor, 
                 nfactors = 3, 
                 rotate="varimax",
                 scores = TRUE)

print(PCA, 
      digits = 3, # to round numbers to the third digit 
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)




PCA3.scores = factor.scores(vars, unclass(PCA$loadings))$scores
PCA3.scores


############

# clustering based on original data

# 1. Standardize the variables of interest
vars_scaled <- scale(vars)

# 2. Run a cluster analysis on a distance matrix and using the Ward method
clust_hier <- hclust(dist(vars_scaled), method="ward.D2") 


# Scree plot
par(mfrow = c(1, 1))  
plot(rev(clust_hier$height), # rev is used to plot from low to high values on Y axis
     type = "b",           # to display both the points and lines
     ylab = "Dissimilarity measure",
     xlab = "Number of clusters",
     main = "Scree plot",
     col = "darkblue",
     pch = 16)             # specify the plot symbol: 16 = filled circle
abline(v = 5, lty = 2, col = "darkred") # draw a vertical line at v = 5 

# Dendrogram 
library(dendextend)
plot(set(as.dendrogram(clust_hier),  
         "branches_k_color", # to highlight the cluster solution with a color
         k = 3),
         ylab = "Distance",
         main = "Dendrogram",
         cex = 0.2)             # Size of labels
rect.hclust(clust_hier, k = 2, border = "darkblue")  # draw blue borders around 2 clusters
rect.hclust(clust_hier, k = 3, border = "darkred")  # draw red borders around 3 clusters


# 5 clusters sol
memb5 <- cutree(clust_hier, k = 5)
# 3 clusters sol
memb3 <- cutree(clust_hier, k = 3)


# using PCA3 reduced data
clust_hier_PCA3 <- hclust(dist(PCA3.scores), method="ward.D2") 
plot(set(as.dendrogram(clust_hier_PCA3),  
         "branches_k_color", # to highlight the cluster solution with a color
         k = 3),
     ylab = "Distance",
     main = "Dendrogram",
     cex = 0.2)             # Size of labels
membPCA3 <- cutree(clust_hier_PCA3, k = 3)


# using PCA5 reduced data
clust_hier_PCA5 <- hclust(dist(PCA5.scores), method="ward.D2") 
plot(set(as.dendrogram(clust_hier_PCA5),  
         "branches_k_color", # to highlight the cluster solution with a color
         k = 4),
     ylab = "Distance",
     main = "Dendrogram",
     cex = 0.2)             # Size of labels

membPCA5 <- cutree(clust_hier_PCA5, k = 5)
membPCA5_3 <- cutree(clust_hier_PCA5, k = 3)


#k-means original data ( 5 clusters)

cent <- NULL
for(k in 1:5){
  cent <- rbind(cent, colMeans(vars_scaled[memb5 == k, , drop = FALSE]))
}

round(cent, 3)

set.seed(1)
orig_kmeans <- kmeans(vars_scaled, centers = cent, iter.max = 10)
orig_kmeans
orig_kmeans$size
dist(orig_kmeans$centers)
#vars_scaled <- cbind(vars_scaled, cluster = orig_kmeans$cluster)

library(tidyverse)
vars_scaled_df <- as.data.frame(vars_scaled)

dt.cluster = aggregate(vars_scaled_df,
                       by = list(cluster = orig_kmeans$cluster), 
                       FUN = mean)

dt.cluster %>% 
  gather(carmake, value, -cluster) %>% # to transfrom from wide to long format
  mutate(carmake = fct_rev(factor(carmake))) %>% # to reverse the order of car makes' names on the plot
  ggplot(aes(x = factor(cluster), y = carmake)) +
  geom_tile(aes(fill = round(value, digits = 2))) +
  geom_text(aes(label = round(value, digits = 2)), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Average\n value", low = "lightgrey", high = "darkblue") +
  theme_minimal() +
  labs(title = "Average preferences in each cluster",
       x = "Cluster",
       y = " ") + 
  theme(legend.position="right", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank()) 


# install.packages("factoextra")
library(factoextra)
fviz_cluster(orig_kmeans, data = vars_scaled_df) + 
  theme_bw()



#k-means original data ( 3 clusters)

cent <- NULL
for(k in 1:3){
  cent <- rbind(cent, colMeans(vars_scaled[memb3 == k, , drop = FALSE]))
}

round(cent, 3)

set.seed(1)
orig_kmeans <- kmeans(vars_scaled, centers = cent, iter.max = 10)
orig_kmeans
orig_kmeans$size
dist(orig_kmeans$centers)
#vars_scaled <- cbind(vars_scaled, cluster = orig_kmeans$cluster)

library(tidyverse)
vars_scaled_df <- as.data.frame(vars_scaled)

dt.cluster = aggregate(vars_scaled_df,
                       by = list(cluster = orig_kmeans$cluster), 
                       FUN = mean)

dt.cluster %>% 
  gather(carmake, value, -cluster) %>% # to transfrom from wide to long format
  mutate(carmake = fct_rev(factor(carmake))) %>% # to reverse the order of car makes' names on the plot
  ggplot(aes(x = factor(cluster), y = carmake)) +
  geom_tile(aes(fill = round(value, digits = 2))) +
  geom_text(aes(label = round(value, digits = 2)), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Average\n value", low = "lightgrey", high = "darkblue") +
  theme_minimal() +
  labs(title = "Average preferences in each cluster",
       x = "Cluster",
       y = " ") + 
  theme(legend.position="right", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank()) 


# install.packages("factoextra")
library(factoextra)
fviz_cluster(orig_kmeans, data = vars_scaled_df) + 
  theme_bw()




#k-means reduced data ( 5 clusters)

cent <- NULL
for(k in 1:5){
  cent <- rbind(cent, colMeans(PCA5.scores[membPCA5 == k, , drop = FALSE]))
}

round(cent, 3)

set.seed(1)
red_kmeans <- kmeans(PCA5.scores, centers = cent, iter.max = 10)
red_kmeans
red_kmeans$size
dist(red_kmeans$centers)

library(tidyverse)
PCA5.scores_df <- as.data.frame(PCA5.scores)

dt.cluster = aggregate(PCA5.scores_df,
                       by = list(cluster = red_kmeans$cluster), 
                       FUN = mean)

dt.cluster %>% 
  gather(carmake, value, -cluster) %>% # to transfrom from wide to long format
  mutate(carmake = fct_rev(factor(carmake))) %>% # to reverse the order of car makes' names on the plot
  ggplot(aes(x = factor(cluster), y = carmake)) +
  geom_tile(aes(fill = round(value, digits = 2))) +
  geom_text(aes(label = round(value, digits = 2)), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Average\n value", low = "lightgrey", high = "darkblue") +
  theme_minimal() +
  labs(title = "Average preferences in each cluster",
       x = "Cluster",
       y = " ") + 
  theme(legend.position="right", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank()) 


# install.packages("factoextra")
library(factoextra)
fviz_cluster(red_kmeans, data = PCA5.scores_df) + 
  theme_bw()



#k-means reduced data ( 5 factors, 3 clusters)

cent <- NULL
for(k in 1:3){
  cent <- rbind(cent, colMeans(PCA5.scores[membPCA5_3 == k, , drop = FALSE]))
}

round(cent, 3)

set.seed(1)
red_kmeans <- kmeans(PCA5.scores, centers = cent, iter.max = 10)
red_kmeans
red_kmeans$size
dist(red_kmeans$centers)

library(tidyverse)
PCA5.scores_df <- as.data.frame(PCA5.scores)

dt.cluster = aggregate(PCA5.scores_df,
                       by = list(cluster = red_kmeans$cluster), 
                       FUN = mean)

dt.cluster %>% 
  gather(carmake, value, -cluster) %>% # to transfrom from wide to long format
  mutate(carmake = fct_rev(factor(carmake))) %>% # to reverse the order of car makes' names on the plot
  ggplot(aes(x = factor(cluster), y = carmake)) +
  geom_tile(aes(fill = round(value, digits = 2))) +
  geom_text(aes(label = round(value, digits = 2)), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Average\n value", low = "lightgrey", high = "darkblue") +
  theme_minimal() +
  labs(title = "Average preferences in each cluster",
       x = "Cluster",
       y = " ") + 
  theme(legend.position="right", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank()) 


# install.packages("factoextra")
library(factoextra)
fviz_cluster(red_kmeans, data = PCA5.scores_df) + 
  theme_bw()






#k-means reduced data ( 3 factors, 3 clusters)

cent <- NULL
for(k in 1:3){
  cent <- rbind(cent, colMeans(PCA3.scores[membPCA3 == k, , drop = FALSE]))
}

round(cent, 3)

set.seed(1)
red_kmeans <- kmeans(PCA3.scores, centers = cent, iter.max = 10)
red_kmeans
red_kmeans$size
dist(red_kmeans$centers)

library(tidyverse)
PCA3.scores_df <- as.data.frame(PCA3.scores)

dt.cluster = aggregate(PCA3.scores_df,
                       by = list(cluster = red_kmeans$cluster), 
                       FUN = mean)

dt.cluster %>% 
  gather(carmake, value, -cluster) %>% # to transfrom from wide to long format
  mutate(carmake = fct_rev(factor(carmake))) %>% # to reverse the order of car makes' names on the plot
  ggplot(aes(x = factor(cluster), y = carmake)) +
  geom_tile(aes(fill = round(value, digits = 2))) +
  geom_text(aes(label = round(value, digits = 2)), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Average\n value", low = "lightgrey", high = "darkblue") +
  theme_minimal() +
  labs(title = "Average preferences in each cluster",
       x = "Cluster",
       y = " ") + 
  theme(legend.position="right", 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.ticks = element_blank()) 


# install.packages("factoextra")
library(factoextra)
fviz_cluster(red_kmeans, data = PCA3.scores_df) + 
  theme_bw()



# reality check
microvan_data_real_check3 = cbind(microvan_data[,33:39], PCA3.scores)
microvan_data_real_check3 = cbind(cluster=red_kmeans$cluster, microvan_data_real_check3)

# Load necessary libraries
library(dplyr)

# Summarize demographics by cluster
demographics_summary <- microvan_data_real_check3 %>%
  group_by(cluster) %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),
    avg_income = mean(income, na.rm = TRUE),
    avg_miles = mean(miles, na.rm = TRUE),
    avg_numkids = mean(numkids, na.rm = TRUE),
    avg_female = mean(female, na.rm = TRUE),
    avg_educ = mean(educ, na.rm = TRUE),
    avg_recycle = mean(recycle, na.rm = TRUE)
  )

# View the summarized data
print(demographics_summary)


library(ggplot2)

# Boxplot for age by cluster
ggplot(microvan_data_real_check3, aes(x = factor(cluster), y = age)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Cluster", x = "Cluster", y = "Age")

# Boxplot for income by cluster
ggplot(microvan_data_real_check3, aes(x = factor(cluster), y = income)) +
  geom_boxplot() +
  labs(title = "Income Distribution by Cluster", x = "Cluster", y = "Income")

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Step 1: Create three separate plots for RC1, RC2, and RC3 against the clusters

# Convert 'cluster' to a factor (if it's not already)
microvan_data_real_check3$cluster <- as.factor(microvan_data_real_check3$cluster)

# Plot 1: RC1 vs clusters
p1 <- ggplot(microvan_data_real_check3, aes(x = cluster, y = RC1)) +
  geom_boxplot(aes(fill = cluster)) +
  labs(title = "RC1 vs Clusters", x = "Cluster", y = "RC1") +
  theme_minimal()

# Plot 2: RC2 vs clusters
p2 <- ggplot(microvan_data_real_check3, aes(x = cluster, y = RC2)) +
  geom_boxplot(aes(fill = cluster)) +
  labs(title = "RC2 vs Clusters", x = "Cluster", y = "RC2") +
  theme_minimal()

# Plot 3: RC3 vs clusters
p3 <- ggplot(microvan_data_real_check3, aes(x = cluster, y = RC3)) +
  geom_boxplot(aes(fill = cluster)) +
  labs(title = "RC3 vs Clusters", x = "Cluster", y = "RC3") +
  theme_minimal()

# Step 2: Print each plot individually
print(p1)
print(p2)
print(p3)

