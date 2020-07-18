# K-means-heart-diseasr-R
k-means-Heart-disease -Assignment-R
library(tidyverse)

library(ggplot2)

data<-read.csv('datasets_33180_43520_heart.csv')

boxplot(data[,10:14])

ggplot(data, aes(x=target, fill=target)) +   geom_bar() +  xlab("Heart Disease") +ylab("Count")

ggplot(data, aes(cp, fill = target))+  geom_bar(position = "fill")+  ggtitle("cp")

# Comparison of Cholestoral across pain type 

ggplot(data,aes(x=sex,y=chol))+  geom_boxplot(fill="#D55E00")+  xlab("Sex")+ylab("Chol")+  facet_grid(~cp)

cor_heart <- cor(data[,10:14])

cor_heart

library(corrplot)

corrplot(cor_heart, method = "ellipse", type="upper",)

pca <- prcomp(data[,10:14], scale = TRUE)

pca

library(factoextra)

fviz_pca(pca)



uns_df <- scale(data[,10:14])

distance <- get_dist(uns_df)

k2 <- kmeans(uns_df, center = 2,nstart = 25  )

fviz_cluster(k2, data = uns_df)



k3 <- kmeans(uns_df, centers = 3, nstart = 25)

k4 <- kmeans(uns_df, centers = 4, nstart = 25)

k5 <- kmeans(uns_df, centers = 5, nstart = 25)



fviz_cluster(k2, geom = "point", data = uns_df)+

  ggtitle("k = 2")

fviz_cluster(k3, geom = "point", data = uns_df)+

  ggtitle("k = 3")

fviz_cluster(k4, geom = "point", data = uns_df)+

  ggtitle("k = 4")

fviz_cluster(k5, geom = "point", data = uns_df)+

  ggtitle("k = 5")
