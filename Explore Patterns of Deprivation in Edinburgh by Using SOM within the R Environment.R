setwd("D:\\Explore Patterns of Deprivation in Edinburgh by Using SOM within the R Environment")
library(kohonen)
library(ggplot2)
library(rgdal)
library(gridExtra)
library(grid)
library(readr)
#read in the processed SIMD data for the City of Edinburgh area
data <- read_csv(file="SIMD2020.csv")
#read in the boundary data for the Edinburgh area from Edinburgh.shp. It has been already matched up by row with SIMD2020 data in QGIS.
edinburgh_map <- readOGR(dsn="Edinburgh", layer="Edinburgh")
#check what object types we have already had
class(data)
class(edinburgh_map)
#what are the column names of the dataframe, and of the spatial points dataframe?
names(data)
names(edinburgh_map)
#how many rows do each of the datasets contain?
nrow(data)
nrow(edinburgh_map)
#check the coordinate system of the spatial polygons data frame
proj4string(edinburgh_map)
#plot the spatial polygons data frame
plot(edinburgh_map)
#convert the object into latitude and longitude and for easier use with ggmap later
edinburgh_map <- spTransform(edinburgh_map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#convert spatial polygon to dataframe including columns of spatial information
edinburgh_fort <- fortify(edinburgh_map, region= "DataZone")
#merge the new dataframe with the edinburgh census data using their shared column (LAName)
edinburgh_fort <- merge(edinburgh_fort, data, by="id")
#create a plot of nocentralheat_rate. Thematic maps in this paper were plotted with QGIS though.
ggplot(data=edinburgh_fort, aes(x=long, y=lat, fill=nocentralheat_rate, group=group)) +
  scale_fill_gradientn(colours=c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac"))+
  geom_polygon(colour="black")+
  theme(legend.position="bottom")+
  coord_equal()+
  theme()
#SOM TRAINING
#choose the variables from SIMD2020 data, with which to train the SOM by subsetting the dataframe named data
data_train <- data[, c(6,8,14,35,36)]
#convert to numeric
data_train<-sapply(data_train, as.numeric)
#standardise the data creating z-scores and convert to a matrix
data_train_matrix <- as.matrix(scale(data_train))
#keep the column names of data_train as names in our new matrix
names(data_train_matrix) <- names(data_train)
#define the size and topology of the som grid
som_grid <- somgrid(xdim = 15, ydim=15, topo="hexagonal")
# Train the SOM model
som_model <- som(data_train_matrix,
                 grid=som_grid,
                 rlen=700,
                 alpha=c(1,0.01),
                 keep.data = TRUE )
#SOM VISUALISATION
# Plot of the training progress - how the node distances have stabilised over time.
#mean distance to closes codebook vector during training
plot(som_model, type = "changes")
#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=cm.colors)
#map quality
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=cm.colors)
#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#code spread
plot(som_model, type = "codes")
# Plot the heatmap for a variable at scaled / normalised values
var <- 1
#define the variable to plot
plot(som_model, type = "property", property = getCodes(som_model)[,var],
     main=colnames(getCodes(som_model))[var], palette.name=cm.colors)
#CLUSTERING OF SOM RESULTS.Show the WCSS metric for kmeans for different clustering sizes. It can be used as a "rough" indicator of the ideal number of clusters
mydata <- getCodes(som_model)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
#Plot the data. Generally minimizing the WCSS will maximise the distance between clusters. You may notice a point of diminishing returns as cluster size increases.
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")
# Form clusters on grid and use hierarchical clustering to cluster the codebook vectors.
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6)
# define an appealing colour palette 
pretty_palette <- c('#fbb4ae', '#b3cde3', '#ccebc5', '#decbe4', '#fed9a6', '#ffffcc')
#show the same plot with the codes instead of just colours
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)
#MAPPING OF SMALL AREAS
#BACK TO THE GEOGRAPHY
#adding label set to the SOM - grabs a random subset from data
#extracting Edinburgh intermediate zone names
geog_names <- data$Intermediate_Zone
#Removing duplicates to gain an idea of the expanse across Edinburgh
geog_names[duplicated(geog_names)] <- NA
#searches the index of names which are not NA
naset <- which(!is.na(geog_names))
#randomly picking 10 of the placenames in the data NA
naset <- sample(naset, length(naset)-10)
geog_names[naset] <- NA
#replotting the data with added labels=geog_names
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters", labels=geog_names)
add.cluster.boundaries(som_model, som_cluster)
#create dataframe of the small area id and of the cluster unit
cluster_details <- data.frame(id=data$id, cluster=som_cluster[som_model$unit.classif])
#merge our cluster details onto the fortified spatial polygon dataframe we created earlier
mappoints <- merge(edinburgh_fort, cluster_details, by="id")
# Finally map the areas and colour by cluster
ggplot(data=mappoints, aes(x=long, y=lat, group=group, fill=factor(cluster))) +
  geom_polygon(colour="transparent") +
  coord_equal() +
  scale_fill_manual(values = pretty_palette)
#combine the clustered data with original spatial polygons Edinburgh.shp
edinburgh_map <- merge(edinburgh_map, cluster_details, by.x="DataZone", by.y="id")
#write edinburgh_map as an shapefile and plot it in R
writeOGR(obj=edinburgh_map,
         dsn="edinburgh_map_clustered",
         layer="edinburgh_map_clustered",
         driver="ESRI Shapefile")

