# Specify your path to the file bimodal.csv
bimodal = read.csv("./hierarchical-cluster/bimodal.csv")
bimodal
bimodal <- as.data.frame(scale(bimodal))
psych::describe(bimodal)
dist <- dist(bimodal[,1:2], method = "euclidean")
as.matrix(dist)[1:5, 1:5] # Visualize distances among the first 5 observations
hc.single <- hclust(dist, method="single")
str(hc.single)
plot(as.dendrogram(hc.single), # select the cluster solution
     ylab = "Distance",
     main = "Dendrogram",          # specify the plot title
     cex = 0.3)                    # specify the label size

install.packages("dendextend")
library(dendextend)
plot(set(as.dendrogram(hc.single), # select the cluster solution
         "branches_k_color",       # color the cluster branches for the number of clusters (k = 4)
         k = 4),                   # specify the number of clusters
     ylab = "Distance",
     main = "Dendrogram",          # specify the plot title
     cex = 0.3)                    # specify the label size
rect.hclust(hc.single, k = 4, border = "darkred")  # draw red borders around the cluster

hc.single.cut <- cutree(hc.single, 4)
table(hc.single.cut)

bimodal$cluster <- cutree(hc.single, 4)

# Overall average in the sample
colMeans(bimodal[, c("COL1", "COL2")])
# Average for each cluster 
aggregate(bimodal[, c("COL1", "COL2")],
          by = list(cluster = bimodal$cluster), 
          FUN = mean)

plot(x = bimodal$COL1, 
     y = bimodal$COL2,
     col = bimodal$cluster,  # specify how to color the observations
     xlab = "COL1", 
     ylab = "COL2",
     xlim = c(-2.2, 2.2), 
     ylim = c(-2.2, 2.2), 
     pch = 16, # specify the plot symbol: 16 = filled circle
     cex = 1)  # specify the symbol size
abline(h = 0, col = 1) # add horizontal line at h = 0
abline(v = 0, col = 1) # add vertical line at v = 0
grid(col = "grey")
legend("topleft", # specify the legend position
       col = 1:length(bimodal$cluster), # specify colors
       legend = paste0("Cluster ", 1:4), # specify the legend labels
       pch = 16) # specify the plot symbol: 16 = filled circle