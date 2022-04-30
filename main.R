pacman::p_load(tidyverse, Rtsne, ggdendro,dbscan,factoextra)
set.seed(0)
data <- read_rds("ProyectoMineria/data/beats.rds")%>%as.tibble() 

data2 <- sample(1:nrow(data),2000, replace=FALSE)
data3 <- data[data2,]

data_clean <- data3 %>% 
  select(danceability, energy ,speechiness,acousticness,
         instrumentalness)%>% 
  na.omit() %>%
  unique() %>% 
  as.tibble()

data_tsne <- data_clean %>%
   
  Rtsne() %>% 
  .$Y %>% 
  as.data.frame()

data_tsne %>% summary()

ggplot(data_tsne) +
  geom_point(aes(V1,V2))


abline(h = 40, col="red")
kNNdistplot(data_tsne, k = 4)
abline(h=1.5, col = "red", lty = 2)

modelo_dbscan <- dbscan(data_tsne, eps = 1.5, minPts = 5)

ggplot(data_tsne, aes(V1, V2, col = factor(modelo_dbscan$cluster))) + 
  geom_point(alpha = 0.5) +
  theme(legend.position = "none") 

modelo_dbscan$cluster %>% max()

estad <- data_clean %>% 
  mutate(cluster = modelo_dbscan$cluster) %>% 
  group_by(cluster) %>%
  summarise(mean(danceability),
            mean(energy),
            mean(speechiness),
            mean(acousticness),
            mean(instrumentalness))
