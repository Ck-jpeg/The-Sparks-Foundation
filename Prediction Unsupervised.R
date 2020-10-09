#importing dataset
df=read.csv("E:/dload/Iris.csv")


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dplyr)


#Viewing the summary of the data
head(df)


#Scaling the variables to a gaussian distribution
rdf <- 
   transmute(df, seplen_scal = scale(SepalLengthCm),
             sepwid_scal = scale(SepalWidthCm),
             petlen_scal = scale(PetalLengthCm),
             petwid_scal = scale(PetalWidthCm),
             species = Species ) 


#Removing the "species" column
data= select(rdf, c(seplen_scal, sepwid_scal, petlen_scal, petwid_scal))
head(data)

# function to compute total within-cluster sum of square 
wss <- function(k) {
   kmeans(data, k, nstart = 10 )$tot.withinss
}


#Using the elbow method to  find the optimum number of clusters
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#other function to look at the optimum number of clusters
set.seed(123)
fviz_nbclust(data , kmeans, method = "wss")



#looking at cluster plots with other values of k

k2= kmeans(data, centers = 2, nstart = 25)
k3 <- kmeans(data, centers = 3, nstart = 25)
k4 <- kmeans(data, centers = 4, nstart = 25)
k5 <- kmeans(data, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)





#plotting the scatter plot with a value of k=3 

set.seed(123)
final <- kmeans(data, 3, nstart = 25)
print(final)





#beautification of the plot.

p2 <- fviz_cluster(final , data , geom = c("point")) +
   scale_color_brewer('Cluster', palette='Set2') + 
   scale_fill_brewer('Cluster', palette='Set2') +
   scale_shape_manual('Cluster', values=c(22,23,24)) + 
   ggtitle(label='') 
p2 + geom_text(data=p2$data, aes(x=x, y=y, label=name, colour=cluster),
               vjust=-1, show.legend = F)    



#THANK YOU
#CODE BY CHITRANG KUREEL