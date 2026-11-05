setwd("/Users/keirajohnson/Library/CloudStorage/OneDrive-UCB-O365/Keira_Johnson_CU/Niwot")
terrain<-read.csv("SNA_TerrainStats.csv")

# Prepare clustering variables
cluster_data <- terrain %>%
  mutate(
    aspect_sin = sin(aspect_degrees * pi / 180),
    aspect_cos = cos(aspect_degrees * pi / 180)
  ) %>%
  select(elevation_m, slope_degrees, aspect_sin, aspect_cos)

# Remove sites with missing values
complete_rows <- complete.cases(cluster_data)
cluster_data <- cluster_data[complete_rows, ]

# Standardize variables
cluster_scaled <- scale(cluster_data)

# Choose number of clusters (e.g., 3)
set.seed(123)
wss <- sapply(1:8, function(k) {
  kmeans(cluster_scaled, centers = k, nstart = 25)$tot.withinss
})

plot(1:8, wss, type = "b", pch = 19,
     xlab = "Number of clusters (k)",
     ylab = "Within-cluster sum of squares")

km <- kmeans(cluster_scaled, centers = 4, nstart = 25)

# Add cluster assignments back to original dataframe
terrain$cluster <- NA_integer_
terrain$cluster[complete_rows] <- km$cluster

# Summarize clusters
p1<-terrain %>%
  ggplot(aes(cluster, elevation_m))+geom_boxplot(aes(group=cluster), outliers = F)+geom_jitter()+
  theme_bw()+theme(text=element_text(size = 20))+labs(x="Cluster", y="Elevation (m)")

# Summarize clusters
p2<-terrain %>%
  ggplot(aes(cluster, slope_degrees))+geom_boxplot(aes(group=cluster), outliers = F)+geom_jitter()+
  theme_bw()+theme(text=element_text(size = 20))+labs(x="Cluster", y="Slope (deg)")

# Summarize clusters
p3<-terrain %>%
  ggplot(aes(cluster, aspect_degrees))+geom_boxplot(aes(group=cluster), outliers = F)+geom_jitter()+
  theme_bw()+theme(text=element_text(size = 20))+labs(x="Cluster", y="Aspect (deg)")

pdf("Cluster_Terrain.pdf", width = 12, height = 4)

ggarrange(p1, p2, p3, nrow = 1)

dev.off()

write.csv(terrain, "SNA_Terrain_Cluster.csv")



