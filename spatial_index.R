# R code associated to paper 'Spatial similarity index for scouting in football'
# by V. Gómez-Rubio, J. Lagos and F. Palmí-Perales (2024).

# Define field

library("spatstat")
campo <- im(matrix(NA, ncol = 20, nrow = 20), xrange = c(0, 100),
  yrange = c(0, 100))


# Define a SpatialGridDataFrame for the field to extract the adjacency matrix
library("sp")
library("spdep")
campo_SPDF <- as(as(campo, "SpatialGridDataFrame"), "SpatialPolygonsDataFrame")
campo_adj <- poly2nb(campo_SPDF)

# Include each pixel as a self-neighbour (recommended by Lee).
campo_adj_self <- include.self(campo_adj)


## Field as an 'sf' object
library("sf")
library("ggsoccer")

campo_sf <- as(campo_SPDF, "sf")


## Display field
library("ggplot2")
ggplot(campo_sf) + 
  annotate_pitch(dimensions = pitch_wyscout) +
  geom_sf(alpha = 0.5)


## Dsiplay field and adjacency matrix
plot(campo_SPDF)
plot(campo_adj, coordinates(campo_SPDF), pch = ".", col = "blue", add = TRUE)


## Spatial weights
sp_weights <- nb2listw(campo_adj_self, style = "B")


## Load players' data
# Raw data are not provided due to commercial reasons.
# Densities are computed in a 20x20 grid (as above).
load("spatial_index_data.RData")

# PLayer's names
pl_names <- names(players)


## Display players' densities
lapply(1:5, function(X) {
    plot(players[[X]]$densidad, main = pl_names[X])
 })


## Extract densities
dens_list <- lapply(1:5, function(X) {
    players[[X]]$densidad
 })
names(dens_list) <- names(players)

dens_list <- as.imlist(dens_list)
plot(dens_list, main = "", equal.ribbon = TRUE)


## -----------------------------------------------------------------------------
mat.p.value <- matrix(NA, ncol = 5, nrow = 5)
colnames(mat.p.value) <- pl_names
row.names(mat.p.value) <- pl_names

mat.statistic <- mat.p.value

for(j1 in 1:5) {
  for(j2 in j1:5) {
    ltest <- lee.test(players[[j1]]$densidad_SPDF$v, players[[j2]]$densidad_SPDF$v, sp_weights, alternative = "greater")
    # Estadístico
    mat.statistic[j1, j2] <-  ltest$statistic
    mat.statistic[j2, j1] <- mat.statistic[j1, j2]
    # P-valor
    mat.p.value[j1, j2] <-  ltest$p.value
    mat.p.value[j2, j1] <- mat.p.value[j1, j2]
  }
}


# Lee's statistics
round(mat.statistic, 3)


# P-values
round(mat.p.value, 3)


## Compute simmilarity matrix (i.e., smaller p-value means more similar players)
dist.mat <- as.dist(mat.p.value)


## Visualize 
library("factoextra")
fviz_dist(dist.mat, order = FALSE, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


## Conduct clustering using the simmilarity matrix
cluster_method <- "complete"
#cluster_method <- "ward.D2"
clust.jug <- hclust(dist.mat, method = cluster_method)

plot(clust.jug)


## Plot hierarchical clustering
plot(hclust(as.dist(mat.statistic), method = cluster_method))


## Cut tree to produce different clusters
cutree(clust.jug, k = 5:1)


