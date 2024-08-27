gsp_share = read.csv("./econ-us/gsp_share.csv")
str(gsp_share)
psych::describe(gsp_share)

vars <- scale(gsp_share[,-1])
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
         number.cex = 0.8 # font size of the coefficients
)

EV = eigen(cor)$values
EV

EV/length(EV)
cumsum(EV/length(EV))

library(psych)
scree(cor, pc = TRUE, factors = FALSE)

# Shares for the cumulative variance explained
plot(cumsum(EV/length(EV)), 
     type = "o", # type of plot: "o" for points and lines 'overplotted'
     col = "darkblue",
     pch = 16, # plot symbol: 16 = filled circle
     cex = 1, # size of plot symbols
     xlab = "Number of factors", # a title for the x axis
     ylab = "Cumulative variance explained", # a title for the y axis
     lwd = 2) # line width
abline(v = 3, lwd = 2, col = "grey") # draw a vertical line at v = 3


library(psych) 
PCA <- principal(r = cor, 
                 nfactors = 3, 
                 rotate="none",
                 scores = TRUE)

print(PCA, 
      digits = 3, # to round numbers to the third digit 
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)

PCA$communality


library(data.table)
L <- as.data.table(unclass(PCA$loadings), keep.rownames = T)




plot(x = L$PC1, y = L$PC2, 
     col ="darkblue",  
     pch = 16,        # plot symbol: 16 = filled circle
     cex = 1,         # size of plot symbols
     xlab = "PC1",    # a title for the x axis
     ylab = "PC2",    # a title for the y axis
     xlim = c(-1,1),  # x axis values from -1 to 1
     ylim = c(-1,1))  # y axis values from -1 to 1

# add point labels
text(L$PC1, L$PC2, 
     labels = L$rn,
     pos = 3, 
     cex = 0.8, 
     col = "darkred")

# add vertical and horizontal lines
abline(h = 0, lwd = 2, col = "grey") # draw a horizontal line at h = 0
abline(v = 0, lwd = 2, col = "grey") # draw a vertical line at v = 0

# extract un-rotated scores of principal components
PCA.scores = factor.scores(vars, unclass(PCA$loadings))$scores
PCA.scores

gsp_share.scores <- cbind(gsp_share, PCA.scores)


plot(x = gsp_share.scores$PC1, 
     y = gsp_share.scores$PC2,
     xlab = "PC1", ylab = "PC2",
     xlim = c(-3, 7), ylim = c(-3, 3), 
     pch = 16, cex = 1, col = "blue")

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")

# add point labels
text(x = gsp_share.scores$PC1, 
     y = gsp_share.scores$PC2, 
     labels = gsp_share.scores$state,
     cex = 1, 
     adj = 1.2, 
     col = "black")


#rotated solution
PCA_rot <- principal(r = cor, 
                 nfactors = 3, 
                 rotate="varimax",
                 scores = TRUE)

print(PCA_rot, 
      digits = 3, # to round numbers to the third digit 
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)

PCA_rot$communality

PCA_rot_4 <- principal(r = cor, 
                     nfactors = 4, 
                     rotate="varimax",
                     scores = TRUE)

print(PCA_rot_4, 
      digits = 3, # to round numbers to the third digit 
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)

PCA_rot_5 <- principal(r = cor, 
                       nfactors = 5, 
                       rotate="varimax",
                       scores = TRUE)

print(PCA_rot_5, 
      digits = 3, # to round numbers to the third digit 
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)
