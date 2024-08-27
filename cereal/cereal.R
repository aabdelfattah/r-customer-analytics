# Specify your path under "/Users/./"" to the file rte_cereal.csv
rte_cereal = read.csv("./cereal/rte_cereal.csv")
psych::describe(rte_cereal)
vars <- scale(rte_cereal[,3:27])
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
         tl.cex = 0.8, # font size of the variable labels
         tl.col = "black", # color of the variable labels
         tl.srt = 45, # rotation angle for the variable labels
         number.cex = 0.5 # font size of the coefficients
)

library(psych)
scree(cor, pc = TRUE, factors = FALSE)

EV = eigen(cor)$values
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
abline(v = 4, lwd = 2, col = "grey") # draw a vertical line at v = 4

EFA1 <- fa(r = cor, 
           nfactors = 4, 
           fm = "pa", 
           rotate = "none") 
print(EFA1, 
      digits = 3, # to round numbers to the third digit 
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)
sort(EFA1$communality)
# Factor loadings
L <- unclass(EFA1$loadings) 
round(L, 3)

EFA2 <- fa(r = cor, 
           nfactors = 4, 
           fm = "pa", 
           rotate = "varimax") 

print(EFA2, 
      digits = 3, # to round numbers to the third digit 
      cut = 0.35, # to show only values > 0.35
      sort = TRUE # to sort rows by loading size
)

EFA2$weights
# extract rotated factor scores 
EFA2.scores = factor.scores(vars, unclass(EFA2$loadings))$scores
head(EFA2.scores) # to show the first 6 observations
rte_cereal.scores <- cbind(rte_cereal, EFA2.scores)
head(rte_cereal.scores)
# Average values for factor scores for each brand 
mean.scores = aggregate(rte_cereal.scores[, c("PA1", "PA2", "PA3", "PA4")],
                        by = list(Brand = rte_cereal.scores$Brand), 
                        FUN = mean)
mean.scores
# ----------------------
# Factor 1 vs. Factor 2
# ----------------------
plot(x = mean.scores$PA1, 
     y = mean.scores$PA2,
     xlab = "Factor 1: Healthful", ylab = "Factor 2: Artificial",
     xlim = c(-2, 2), ylim = c(-2, 2), 
     pch = 16, cex = 1, col = "blue")

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# add point labels
text(x = mean.scores$PA1, 
     y = mean.scores$PA2, 
     labels = mean.scores$Brand,
     cex = 1, 
     adj = 1.2, 
     col = "black")

# ----------------------
# Factor 3 vs. Factor 4
# ----------------------
plot(x = mean.scores$PA3, 
     y = mean.scores$PA4,
     xlab = "Factor 3: Non-Adult", ylab = "Factor 4: Interesting",
     xlim = c(-2, 2), ylim = c(-2, 2), 
     pch = 16, cex = 1, col = "blue")

abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# add point labels
text(x = mean.scores$PA3, 
     y = mean.scores$PA4, 
     labels = mean.scores$Brand,
     cex = 1, 
     adj = 1.2, 
     col = "black")