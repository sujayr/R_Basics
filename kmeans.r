data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data
# View the firt 3 rows of the data
head(df, n = 3)




#x: numeric matrix, numeric data frame or a numeric vector
#centers: Possible values are the number of clusters (k) or a set of initial (distinct) cluster centers. If a number, a random set of (distinct) rows in x is chosen as the initial centers.
#iter.max: The maximum number of iterations allowed. Default value is 10.
#nstart: The number of random starting partitions when centers is a number. Trying nstart > 1 is often recommended.

kmeans(x, centers, iter.max = 10, nstart = 1)

library(factoextra)
install.packages("factoextra")

library(factoextra)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res)

aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)

# Cluster number for each of the observations
km.res$cluster

head(km.res$cluster, 4)


# Cluster size
km.res$size


# Cluster means
km.res$centers


fviz_cluster(km.res, data = df)





