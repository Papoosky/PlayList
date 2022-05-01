pacman::p_load(tidyverse, Rtsne, ggdendro,dbscan,factoextra,mclust,e1071)
set.seed(1)
data <- read_rds("ProyectoMineria/data/beats.rds")%>%as.tibble() 

data2 <- sample(1:nrow(data),2000, replace=FALSE)

data_clean <- data[data2,]  %>%
  select(artist_name,track_name,track_id,track_uri,duration_ms,danceability, energy ,speechiness,acousticness,
         instrumentalness,liveness,loudness,tempo,valence) %>%
  unique()%>%
  na.omit()%>%
  as.tibble()
data_tsne <- data_clean %>%
  select(track_id,danceability, energy ,speechiness,acousticness,
         instrumentalness,liveness,loudness,tempo,valence) %>%
  unique()%>%
  Rtsne() %>% 
  .$Y %>% 
  as.data.frame()


data_tsne %>% summary()

ggplot(data_tsne) +
  geom_point(aes(V1,V2))


kNNdistplot(data_tsne, k = 4)
abline(h=1.8, col = "red", lty = 2)

modelo_dbscan <- dbscan(data_tsne, eps = 1.6, minPts = 6)

ggplot(data_tsne, aes(V1, V2, col = factor(modelo_dbscan$cluster))) + 
  geom_point(alpha = 0.5) +
  theme(legend.position = "none") 

modelo_dbscan$cluster %>% max()

estad <- data_clean %>% 
  mutate(cluster = modelo_dbscan$cluster) %>% 
  group_by(cluster)
  

model_all <- Mclust(data_tsne, G=1:30)
plot(model_all, what = "BIC")

# vemos el resultado del modelo optimo
model_all
ggplot(data_tsne) + 
  aes(x=V1, y=V2, color=factor(model_all$classification)) + 
  geom_point(alpha=0.5) 

clusters <- data_clean %>% 
  unique()%>%
  mutate(cluster = model_all$classification) %>% 
  group_by(cluster)

length(unique(clusters$track_name))
#Le preguntamos al usuario el nombre de la canción que desea utilizar
#Y comprobamos que efectivamente se encuentre en la muestra
#Ejemplo: "Train"
validar=0
i=1
song <- readline(prompt = "Ingrese el nombre de la canción: ")
while(validar!=1){
  for (i in 1:nrow(clusters)) {
    if(song==clusters[i,2]) {
      validar=1
      #Con la variable indice, guardamos la posición de la canción, lo cual servirá para saber su cluster 
      indice=i
      cat("La canción",song,"está en la muestra de la base de datos")
      break
    }
  }
  if(validar==0){
    song <- readline(prompt = "Por favor ingrese una canción que esté en la base: ")
  }
}

#Con la variable cltr guardamos el cluster de la canción ingresada
tiempo=0
j=1
cltr=clusters[indice,15]

#Creamos dataframe con las canciones
lista=data.frame(Autor_Grupo=character(), Cancion=character(),Duracion_Acumulado=numeric())
#Notamos que 10800000 milisegundos son 3 horas, por lo que el while se detendrá
#Una vez haya superado ese tiempo
#Pd: Si la muestra tomada obtiene clusters que no logren satisfacer todo el tiempo,
#Las canciones se comenzarán a repetir hasta cumplir las 3 horas, siendo todas ellas,
#Del mismo cluster
clusters[2]
playlist <- function(clusters,tiempo, j, cltr, lista){
  while(tiempo<=10800000){
    for (j in 1:nrow(clusters)) {
      if(cltr==clusters[j,15]) {
        if (tiempo >=10800000){
          break
        }
        tiempo=tiempo+clusters[j,5]
        cancion=clusters[j,2]
        autor=clusters[j,1]
        vector=c(autor,cancion,tiempo)
        lista=rbind(lista,vector)
      }
      
    }
  }
  return(lista)
}
Base_Gmm<- playlist(clusters,tiempo,j,cltr,lista)

view(Base_Gmm)

sum(Base_Gmm$duration_ms)


