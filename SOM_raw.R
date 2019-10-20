##### Data Import #####

data_raw = read.csv2("data.csv", header = TRUE, stringsAsFactors = F, dec = ".")


##### SOM #####

library(kohonen)

# Preparing data #
data<-unique(data_raw)
raw = read.csv2("data.csv", header = TRUE, stringsAsFactors = F, dec = ".")
data = raw[,3:length(raw)]
rownames(data) = raw[,1]


# For plotting evaluation against colorcode # category (~ classification solution) 
row_label <- as.factor(rownames(data)) 
colors <- c("red", "black", "blue")
colors <- colors[as.numeric(data$Item)]
data_train_matrix <- as.matrix(scale(data))
##### Define the neuronal grid #####
som_grid <- somgrid(xdim = 4, ydim = 4, topo="hexagonal")

##### Train the model #####
som_model <- som(data_train_matrix, grid=som_grid, rlen=1000, alpha=c(0.05,0.01), keep.data=TRUE)

##### Check training progress #####
# As the SOM training iterations progress, the distance from each node's weights to the samples represented by 
# that node is reduced. Ideally, this distance should reach a minimum plateau. This plot option shows the progress 
# over time. If the curve is continually decreasing, more iterations are required. 
plot(som_model, type="changes")

##### Node Counts ##### 
# The Kohonen packages allows us to visualise the count of how many samples are mapped to each node on the map. 
# This metric can be used as a measure of map quality - ideally the sample distribution is relatively uniform. 
# Large values in some map areas suggests that a larger map would be benificial. Empty nodes indicate that your map 
# size is too big for the number of samples. Aim for at least 5-10 samples per node when choosing map size. 
plot(som_model, type = "count")
plot(som_model,type = "mapping",
     col=colors[row_label])
plot(som_model, type ="mapping",
     labels = (rownames(data)),
     col=colors[row_label])

##### U-Matrix / Neighbor Matrix #####
# Often referred to as the "U-Matrix", this visualisation is of the distance between each node and its neighbours. 
# Typically viewed with a grayscale palette, areas of low neighbour distance indicate groups of nodes that are similar. 
# Areas with large distances indicate the nodes are much more dissimilar - and indicate natural boundaries between node 
# clusters. The U-Matrix can be used to identify clusters within the SOM map. 

plot(som_model, type = "property", property = getCodes(som_model, 1)[,2]) # ??? Make a loop over all the variables ?


##### Codes/ Weight vectors #####
# The node weight vectors, or "codes", are made up of normalised values of the original variables used to generate the 
# SOM. Each node's weight vector is representative / similar of the samples mapped to that node. By visualising the 
# weight vectors across the map, we can see patterns in the distribution of samples and variables. The default 
# visualisation of the weight vectors is a "fan diagram", where individual fan representations of the magnitude of each 
# variable in the weight vector is shown for each node. Other represenations are available, see the kohonen plot 
# documentation for details. 

plot(som_model, type="codes")

##### SOM Heatmap #####
# A SOM heatmap allows the visualisation of the distribution of a single variable across the map. Typically, a SOM investigative 
# process involves the creation of multiple heatmaps, and then the comparison of these heatmaps to identify interesting areas on 
# the map. It is important to remember that the individual sample positions do not move from one visualisation to another, the 
# map is simply coloured by different variables.

plot(som_model, type = "property", property = getCodes(som_model), main=names(som_model$data))

##### Clustering #####
# Clustering can be performed on the SOM nodes to isolate groups of samples with similar metrics. Manual identification 
# of clusters is completed by exploring the heatmaps for a number of variables and drawing up a "story" about the different 
# areas on the map. An estimate of the number of clusters that would be suitable can be ascertained using a kmeans algorithm 
# and examing for an "elbow-point" in the plot of "within cluster sum of squares".  The Kohonen package documentation shows 
# how a map can be clustered using hierachical clustering. The results of the clustering can be visualised using the SOM plot function again.
tree <- as.dendrogram(hclust(dist(as.numeric(unlist(som_model$codes))))) 
plot(tree, ylab = "Height (h)")

## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(as.numeric(unlist(som_model$codes)))), h=2)
# plot these results:
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', 
                    '#9467bd', '#8c564b', '#e377c2')
plot(som_model, type="mapping",labels = (rownames(data)),
     bgcol=pretty_palette[som_cluster], col=colors[row_label])
add.cluster.boundaries(som_model,som_cluster)

     