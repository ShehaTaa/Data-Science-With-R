# install.packages("tidyverse")
install.packages('factoextra')
library(factoextra)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(corrplot)
library(cluster)

# Read the Data
sc_df <- read.csv("Datasets/Shopping_Customers.csv",header=TRUE)
View(sc_df)
str(sc_df)

any(is.na(sc_df))

sc_df$Gender <- ifelse(sc_df$Gender=="Male", 1, 0)
sc_df$Gender <- as.integer(sc_df$Gender)
# Rename columns 4 and 5
names(sc_df)[4] <- 'Annual_Income'
names(sc_df)[5] <- 'Spending_Score'
View(sc_df)

# Check severity of class imbalance
round(prop.table(table(sc_df$Gender)), 2)

# heatmap of attributes
plot_heatmap <- function(df) {
  corrplot(cor(df),        # Correlation matrix
           method = "circle",     # Correlation plot method (method = number, circle, pie, or color)
           type = "full",         # Correlation plot style (also "upper" and "lower")
           diag = TRUE,           # If TRUE (default), adds the diagonal
           tl.col = "black",      # Labels color
           bg = "white",          # Background color
           title = "",            # Main title
           col = NULL,            # Color palette
           tl.cex =0.7,
           cl.ratio =0.2) 
}

plot_heatmap(sc_df[,2:5])
######################### a : K-Means Algorithm ######################### 

set.seed(2021)

# Run k-means on sc_df
km <- kmeans(sc_df[,3:5], 2, nstart = 10)

km$cluster <- factor(km$cluster)
ggplot(sc_df, aes(Annual_Income, Spending_Score)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = km$cluster) + 
  scale_color_manual(values = c('black', 'red'))
x <- sc_df[,2:5]
x <- as.matrix(x)
heatmap(x)

# add cluster label to dataframe 
kmeansRes<-factor(km$cluster)
sc_df$cluster <- km$cluster
sc_df$cluster <- as.integer(sc_df$cluster)
View(sc_df)

# heatmap of attributes with cluster labels
plot_heatmap(sc_df[,2:6])

## Visualizing clusters
km_labels <- km$cluster

fviz_cluster(km, data = sc_df[,2:5])

fviz_cluster(km, data = sc_df[,2:5], palette = c("#FC4E07", "#00AFBB", "#E7B800"), ellipse.type = "euclid", 
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal() )

######################### b : Elbow Curve #########################

elbow <- (nrow(sc_df)-1)*sum(apply(sc_df[,3:5], 2, var))
for (i in 2:20) {
  elbow[i] <- sum(kmeans(sc_df[,3:5],centers=i)$withinss)
}
plot(1:20, elbow, type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

######################### c :  Silhouette #########################
library(purrr)

avg_sil <- function(k) {
  km.res <- kmeans(sc_df[,3:5], centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(sc_df[,3:5]))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

######################### d :  hierarchical clustering #########################
install.packages('dendextend')
suppressPackageStartupMessages(library(dendextend))

SC_Scaled <- scale(sc_df[, 3:5])
head(SC_Scaled)

# dend <- sc_df[, 3:5] %>% scale %>% dist %>% 
#   hclust %>% as.dendrogram %>%
#   set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
#   set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
#   set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
# # plot the dend in usual "base" plotting engine:
# plot(dend)


#use the hclust() function and dist() function (computes and returns the distance matrix)

d <- dist(SC_Scaled, method = "euclidean")
hs <- hclust(d, method = 'single')

#Plot the single dendrogram
com_dend_obj1 <- as.dendrogram(hs)
com_col_dend1 <- color_branches(com_dend_obj1, h = 4)
plot(com_col_dend1)

# plot(hs, hang = -1, cex = 0.6)

hc =  hclust(dist(SC_Scaled), method = 'complete')

#Plot the complete dendrogram
# plot(hc, hang = -1, cex = 0.6)

plot(hc)
com_dend_obj <- as.dendrogram(hc)
com_col_dend <- color_branches(com_dend_obj, h = 4)
plot(com_col_dend)
abline(h = 4, col = 'red')

