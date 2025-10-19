# Carga y limpieza de los datos
## Carga de las librerias

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(stringr)


##Cargar el dataset

ruta_BD_Netflix <- "C:\\Users\\3340\\OneDrive\\Escritorio\\UCV\\Sem 1\\COMPU 1\\Proyecto\\1111111\\Hidden-Gems\\Data\\netflix_dataset.csv"

netflix<- read_csv(ruta_BD_Netflix)

# Cambios basicos y Presentación

##Mostrar las primeras 6 filas

print(netflix, 6)

spec(netflix)

# Cambiar títulos al español

netflix1 <- netflix %>%
  rename(
    ID = id,
    Titulos_producciones = title,
    Tipos = type,
    Descripcion = description, 
    Año_Lanzamiento = release_year,
    Certificacion_edad = age_certification,
    Tiempo = runtime,
    Generos = genres,
    Paises_produccion = production_countries,
    Temporadas = seasons,
    imdb_ID = imdb_id,
    imdb_votos = imdb_votes,
    tmdb_popularidad = tmdb_popularity,
    tmdb_puntaje = tmdb_score
  ) 
  
print(names(netflix1))

#Visualizar la data

glimpse(netflix)
summary(netflix)

###Chequeo de duplicados 

duplicados_totales <- sum(duplicated(netflix1))
print(paste ("Numero de filas duplicadas:", duplicados_totales))


###Chequeo de dupicados por titulo

duplicados_titulos<- netflix1 %>%
  group_by(Titulos_producciones)%>%
  summarise( Conteo=n(), .groups = "drop")%>%
  filter(Conteo > 1) %>%
  nrow()

print (paste("Titulos duplicados:", duplicados_titulos))

#Manejo de valores faltantes

##Para la clasificación de edad se nombra como "No clasificados" los valores NA
netflix_limpio <-netflix1 %>%
  mutate(
    Certificacion_edad = case_when(
      is.na(Certificacion_edad)~"No Clasificado", 
      TRUE ~ Certificacion_edad)
  )

##Para la descripción cambiar los datos NA como "no disponibles"

netflix_limpio <-netflix_limpio %>%
  mutate(
    Descripcion = case_when(
      is.na(Descripcion)~"No disponible", 
      TRUE ~ Descripcion)
  )

###Se reemplazan con "0" los ID , Score, votos, popularidad de las paginas imdb y tmdb que aparezcan en NA
netflix_limpio <- netflix_limpio %>%
  mutate(
    tmdb_popularidad = ifelse(is.na(tmdb_popularidad), 0, tmdb_popularidad),
    tmdb_puntaje = ifelse(is.na(tmdb_puntaje), 0, tmdb_puntaje) ,
    imdb_score = ifelse(is.na(imdb_score), 0, imdb_score) ,
    imdb_votos = ifelse(is.na(imdb_votos), 0, imdb_votos) ,
    imdb_ID = ifelse(is.na(imdb_ID), "desconocido",imdb_ID)
  )


#Seasons: como los valores Na de esa categoria, significa que es una pelicula, entonces cambiaremos el valor NA por 0
  netflix_limpio <- netflix_limpio %>%
  mutate(
    Temporadas = ifelse(is.na(Temporadas), 0, Temporadas)
  )

#Borramos los titulos faltantes de la data
  
netflix_limpio <- netflix_limpio %>%
    filter(!is.na(Titulos_producciones))

print("NAs después de imputación:")
print(colSums(is.na(netflix_limpio)))
      
#Eliminación de duplicados

netflix_limpio <- netflix_limpio %>%
  distinct(Titulos_producciones, .keep_all = TRUE)  

print(paste("Filas después de eliminar duplicados:", nrow(netflix_limpio)))


#Limpieza de datos 

netflix_limpio <- netflix_limpio %>%
  mutate(
    #Filtrar años inválidos
    Año_Lanzamiento = case_when(
      Año_Lanzamiento < 1700 | Año_Lanzamiento > 2025 ~ NA_real_,
      TRUE ~ Año_Lanzamiento
    ),
    # Filtrar tiempos de reproducción incongruentes 
    Tiempo = case_when(
      Tiempo < 1 | Tiempo > 300 ~ NA_real_, 
      TRUE ~ Tiempo
    ))

#Limpiar String con comas y doble espacios

netflix_limpio <- netflix_limpio %>%
  mutate(
    Generos = str_trim(str_replace_all(Generos, ",\\s+", ", ")),  
         Paises_produccion = str_trim(str_replace_all(Paises_produccion, ",\\s+", ", "))
  )

 
##Crear columnas para los géneros
netflix_limpio <- netflix_limpio %>%
  mutate(
    Generos_Lista = str_split(Generos, ",\\s*") 
  )

netflix_limpio <- netflix_limpio %>%
  separate_rows(Generos, sep = ",\\s*")

netflix_limpio <- netflix_limpio %>%
  mutate(
    Paises_produccion = str_to_lower(Paises_produccion),
    Paises_produccion = str_replace_all(Paises_produccion, "\\$|\\$|'", ""),
    Paises_produccion = str_trim(Paises_produccion)
  ) %>%
  separate_rows(Paises_produccion, sep = ",\\s*")

#prueba

str(netflix_limpio)
glimpse (netflix_limpio)

#Quitar outliers popularidad
##Resumen
print (summary(netflix_limpio$tmdb_popularidad))
#Ver como esta antes del filtrado
p1 <- ggplot(netflix_limpio, aes(y = tmdb_popularidad))+
  geom_boxplot (fill= "#6BAED6", alpha = 0.6)+
  labs(title = "Deteccion de valores atipicos en tmdb_popularidad (ANTES)",
       y = "popularidad (tmdb)")+
  theme_minimal()
print(p1)

#Conteo de outliers
Q1 <- quantile(netflix_limpio$tmdb_popularidad, probs = 0.25, na.rm = TRUE)

Q3 <- quantile(netflix_limpio$tmdb_popularidad, probs = 0.75, na.rm = TRUE)

Rango<-Q3-Q1

##Definir limites

limite_inferior<-Q1-1.5*Rango
limite_superior<-Q3+1.5*Rango

print(Q1)
print(Q3)
print(paste("Rango:", round(Rango, 2)))
print(paste("Límite inferior:", round(limite_inferior, 2)))
print(paste("Límite superior:", round(limite_superior, 2)))

outliers_antes <- sum(netflix_limpio$tmdb_popularidad < limite_inferior| 
                      netflix_limpio$tmdb_popularidad > limite_superior)
print(paste("Número de outliers detectados ANTES:", outliers_antes))
print(paste("Porcentaje de outliers ANTES:", round((outliers_antes / nrow(netflix_limpio)) * 100, 2), "%"))

##Filtrar outliers

netflix_sin_outliers<- netflix_limpio %>%
  filter (tmdb_popularidad >= limite_inferior & tmdb_popularidad <= limite_superior)

print(paste("Filas originales:", nrow(netflix_limpio)))
print(paste("filas sin outliers", nrow(netflix_sin_outliers)))
print(paste("Filas eliminadas:", nrow(netflix_limpio)- nrow(netflix_sin_outliers)))

p2<- ggplot(netflix_sin_outliers, aes(y = tmdb_popularidad))+
  geom_boxplot (fill= "#6BAED6", alpha = 0.6)+
  labs(title = "Deteccion de valores atipicos en tmdb_popularidad",
       y = "popularidad (tmdb)")+
  theme_minimal()

print(p2)


outliers_despues <- sum(netflix_sin_outliers$tmdb_popularidad < limite_inferior | 
                          netflix_sin_outliers$tmdb_popularidad > limite_superior, na.rm = TRUE)
print(paste("Número de outliers detectados despues:", outliers_despues))


#Quitar outliers score
##Resumen
print (summary(netflix_limpio$tmdb_puntaje))

#Conteo de outliers
Q1_score <- quantile(netflix_limpio$tmdb_puntaje, probs = 0.25, na.rm = TRUE)

Q3_score <- quantile(netflix_limpio$tmdb_puntaje, probs = 0.75, na.rm = TRUE)

Rango_score<-Q3_score - Q1_score

##Definir limites

limite_inferior_score<-Q1_score-1.5*Rango_score
limite_superior_score<-Q3_score+1.5*Rango_score

print(Q1_score)
print(Q3_score)
print(paste("Rango:", round(Rango_score, 2)))
print(paste("Límite inferior:", round(limite_inferior_score, 2)))
print(paste("Límite superior:", round(limite_superior_score, 2)))

outliers_antes_score <- sum(netflix_limpio$tmdb_puntaje < limite_inferior_score| 
                            netflix_limpio$tmdb_puntaje > limite_superior_score)
print(paste("Número de outliers detectados ANTES:", outliers_antes_score))
print(paste("Porcentaje de outliers ANTES:", round((outliers_antes_score / nrow(netflix_limpio)) * 100, 2), "%"))

##Filtrar outliers

netflix_sin_outliers<- netflix_limpio %>%
  filter (tmdb_puntaje >= limite_inferior_score & tmdb_puntaje <= limite_superior_score)

print(paste("Filas originales:", nrow(netflix_limpio)))
print(paste("filas sin outliers", nrow(netflix_sin_outliers)))
print(paste("Filas eliminadas:", nrow(netflix_limpio)- nrow(netflix_sin_outliers)))

p2_puntaje<- ggplot(netflix_sin_outliers, aes(y = tmdb_popularidad))+
  geom_boxplot (fill= "#6BAED6", alpha = 0.6)+
  labs(title = "Deteccion de valores atipicos en tmdb_popularidad",
       y = "popularidad (tmdb)")+
  theme_minimal()

print(p2_puntaje)

outliers_despues_puntaje <- sum(netflix_sin_outliers$tmdb_puntaje < limite_inferior_score | 
                                netflix_sin_outliers$tmdb_puntaje > limite_superior_score, na.rm = TRUE)
print(paste("Número de outliers detectados despues:", outliers_despues_puntaje))


#No vamos a quitar los outliers de la data principal que usaremos porque nos parece importante algunos de los datos atipico que salen para futuros analisis. Sin embargo, si usamos estos analisis al momento de evaluar promedios, cuartiles, medias, entre otros. Por eso su creacion.


#Resumen final

glimpse(netflix_limpio)
print("NAs finales:")
print(colSums(is.na(netflix_limpio)))


#Paso 2: Crear variables y organizar la data de la manera necesaria para realizar la investigación
  ##Creamos una varible sobre Decadas 
  netflix_limpio<- netflix_limpio%>%
  mutate(
    decada = case_when(
      Año_Lanzamiento < 1950 ~ "1940´s",
      Año_Lanzamiento < 1960 ~ "1950´s",
      Año_Lanzamiento < 1970 ~ "1960´s",
      Año_Lanzamiento < 1980 ~ "1970´s",
      Año_Lanzamiento < 1990 ~ "1980's",
      Año_Lanzamiento < 2000 ~ "1990's",
      Año_Lanzamiento < 2010 ~ "2000's",
      Año_Lanzamiento < 2020 ~ "2010´s",
      Año_Lanzamiento >= 2020 ~ "2020's"
    )
  )

  
  ##Creamos dos tablas, una con la información de imdb y otra con la información de tmdb

vars_tmdb <- c("ID", "Titulos_producciones", "Tipos", "Año_Lanzamiento", "Temporadas", "Tiempo", "Certificacion_edad", "Generos", "Paises_produccion",
               "tmdb_popularidad", "tmdb_puntaje", "decada")

netflix_tmdb <- netflix_limpio %>%
  select(all_of(vars_tmdb))

netflix_tmdb_pelis <- netflix_tmdb %>%
  filter(Tipos == "MOVIE")

##Filtrar las películas y series de las tablas de tmdb


###Tablas de películas y series para tmdb

netflix_tmdb_pelis <- netflix_tmdb %>%
  filter(Tipos == "MOVIE")

netflix_tmdb_series <- netflix_tmdb %>%
  filter(Tipos == "SHOW")


#Mejoras, por si acaso, antes de realizar las estadisticas
netflix_limpio <- netflix_limpio %>%
  mutate(across(c(Tiempo, tmdb_popularidad, tmdb_puntaje), as.numeric))

  # Estadisticas descriptivas de Año de lanzamiento

resumen_netflix_año_lanzamiento <- netflix_limpio %>%
  summarise(
    media = mean(Año_Lanzamiento, na.rm = TRUE),
    mediana = median(Año_Lanzamiento, na.rm = TRUE),
    desviacion_tipica = sd(Año_Lanzamiento, na.rm = TRUE),
    minimo = min(Año_Lanzamiento, na.rm = TRUE),
    maximo = max(Año_Lanzamiento, na.rm = TRUE),
    cuartil1 = quantile(Año_Lanzamiento, 0.25, na.rm = TRUE),
    cuartil2 = quantile(Año_Lanzamiento, 0.5, na.rm = TRUE),
    cuartil3 = quantile(Año_Lanzamiento, 0.75, na.rm = TRUE)
  )

print(resumen_netflix_año_lanzamiento)

# Estadisticas descriptivas del tiempo

resumen_netflix_tiempo_pelis <- netflix_tmdb_pelis %>%
  summarise(
    media = mean(Tiempo, na.rm = TRUE),
    mediana = median(Tiempo, na.rm = TRUE),
    desviacion_tipica = sd(Tiempo, na.rm = TRUE),
    minimo = min(Tiempo, na.rm = TRUE),
    maximo = max(Tiempo, na.rm = TRUE),
    cuartil1 = quantile(Tiempo, 0.25, na.rm = TRUE),
    cuartil2 = quantile(Tiempo, 0.5, na.rm = TRUE),
    cuartil3 = quantile(Tiempo, 0.75, na.rm = TRUE)
  )

print(resumen_netflix_tiempo_pelis)

#Estadisticas descriptivas del tiempo de las series

resumen_netflix_tiempo_series <- netflix_tmdb_series %>%
  summarise(
    media = mean(Tiempo, na.rm = TRUE),
    mediana = median(Tiempo, na.rm = TRUE),
    desviacion_tipica = sd(Tiempo, na.rm = TRUE),
    minimo = min(Tiempo, na.rm = TRUE),
    maximo = max(Tiempo, na.rm = TRUE),
    cuartil1 = quantile(Tiempo, 0.25, na.rm = TRUE),
    cuartil2 = quantile(Tiempo, 0.5, na.rm = TRUE),
    cuartil3 = quantile(Tiempo, 0.75, na.rm = TRUE)
  )

print(resumen_netflix_tiempo_series)


# Estadisticas descriptivas de tmdb Popularidad

resumen_netflix_tmdb_polularidad <- netflix_sin_outliers %>%
  summarise(
    media = mean(tmdb_popularidad, na.rm = TRUE),
    mediana = median(tmdb_popularidad, na.rm = TRUE),
    desviacion_tipica = sd(tmdb_popularidad, na.rm = TRUE),
    minimo = min(tmdb_popularidad, na.rm = TRUE),
    maximo = max(tmdb_popularidad, na.rm = TRUE),
    cuartil1 = quantile(tmdb_popularidad, 0.25, na.rm = TRUE),
    cuartil2 = quantile(tmdb_popularidad, 0.5, na.rm = TRUE),
    cuartil3 = quantile(tmdb_popularidad, 0.75, na.rm = TRUE)
  )

print(resumen_netflix_tmdb_polularidad)

# Estadisticas descriptivas de tmdb Puntaje

resumen_netflix_tmdb_puntaje <- netflix_sin_outliers %>%
  summarise(
    media = mean(tmdb_puntaje, na.rm = TRUE),
    mediana = median(tmdb_puntaje, na.rm = TRUE),
    desviacion_tipica = sd(tmdb_puntaje, na.rm = TRUE),
    minimo = min(tmdb_puntaje, na.rm = TRUE),
    maximo = max(tmdb_popularidad, na.rm = TRUE),
    cuartil1 = quantile(tmdb_puntaje, 0.25, na.rm = TRUE),
    cuartil2 = quantile(tmdb_puntaje, 0.5, na.rm = TRUE),
    cuartil3 = quantile(tmdb_puntaje, 0.75, na.rm = TRUE)
  )

print(resumen_netflix_tmdb_puntaje)


# Creación de gráfcas para análisis descriptivo

gd <- netflix_limpio 


gd1 <- gd %>% summary(gd)

gd2 <- gd %>% 
  group_by(Generos) %>%
  summarise(cantidad = n())
print(gd2)


# Histograma de tmdb popularidad

grafico_popularidad <- ggplot(netflix_sin_outliers, aes(x = tmdb_popularidad)) +
  geom_histogram(
    binwidth = 30 ,
    fill  = "darkred", 
    color = "black",  
    alpha = 0.7           
  ) +
  coord_flip()+
  labs(
    title = "Histograma de tmdb popularidad",
    x     = "tmdb popularidad",
    y     = "Frecuencia"
  ) +
  theme_minimal()

print(grafico_popularidad)

# Histograma de tmdb puntaje

grafico_puntaje <- ggplot(netflix_sin_outliers, aes(x = tmdb_puntaje)) +
  geom_histogram(
    bins  = 30,           
    fill  = "darkred", 
    color = "black",      
    alpha = 0.7           
  ) +
  labs(
    title = "Histograma de tmdb puntaje",
    x     = "imdb puntaje",
    y     = "Frecuencia"
  ) +
  theme_minimal()

print(grafico_puntaje)

#¿Cómo está compuesto el catálogo de Netflix? 
  
 ## 1-tipo de contenido

grafico_tipos_bar <- ggplot(netflix_limpio, aes(x = Tipos, fill = Tipos)) +
  geom_bar() +
  scale_fill_manual(values = c("MOVIE" = "#931B26", "SHOW" = "#37020D")) +
  labs(
    title = "Cantidad de peliculas y series",
    x = "Tipo",
    y = "Cantidad"
  ) +
  theme_minimal() +  # Llama a theme_minimal() sin argumentos
  theme(  # Aquí van las personalizaciones
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#37020D")
  )

print(grafico_tipos_bar)


### grafico de torta sobre  los tipos para evaluar porcentajes

Tipos_grafic <- netflix_limpio %>%
  group_by(Tipos) %>%
  summarise( Count = n(), .groups = "drop")%>%
  mutate(Porcentaje = round(Count / sum(Count)*100, 2))

grafico_tipos_pie <- ggplot(Tipos_grafic, aes(x ="", y= Count, fill = Tipos)) +
  geom_bar(stat = "identity", width = 1, color = "white")+
  geom_text(aes(label = paste0(Porcentaje, "%")),
            color = "white", size = 5)+
  scale_fill_manual( values = c( "MOVIE" ="#931B26", "SHOW" = "#37020D"))+
  coord_polar(theta = "y")+
  labs(
    title = "Distribucion de peliculas y series",
    fill = "Tipo"
  )+
  theme_minimal()

print(grafico_tipos_pie)

##2-evolución en el tiempo
###distribución de años de lanzamiento. 

#Contar las decadas
distribucion_decadas <- netflix_limpio %>%
    group_by(decada) %>%
    summarise(Conteo = n(), .groups = "drop")

#Crear el grafico

grafico_decadas <- ggplot(distribucion_decadas, aes(x = decada, y = Conteo, group = 1))+
  geom_line(color = "black", size = 1.5, linetype = "solid")+
  geom_point(color = "darkred", size = 3, shape =21, fill = "white")+
  labs(
    title = "Distribucion de producciones por Decadas",
    x = "Decadas",
    y = "Numero de producciones"
  )+
  theme_minimal()

print(grafico_decadas)

#3-Generos
##Conteo de géneros 

netflix_limpio <- netflix_limpio %>%
  mutate(
    Generos = str_to_lower(Generos),                     
    Generos = str_replace_all(Generos, "\\[|\\]|'", ""),
    Generos = str_trim(Generos)                          
  ) %>%
  separate_rows(Generos, sep = ",\\s*")


conteo_generos <- netflix_limpio%>%
  group_by(Generos) %>%
  summarise(Conteo = n(), .groups = "drop") %>%
  mutate(Porcentaje = round(Conteo / sum(Conteo)*100, 2))


grafico_generos <- ggplot(conteo_generos, aes(x = reorder(Generos, Conteo), y = Conteo, fill = Generos)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual( values = c( "action" ="#931B26",
                                 "animation" = "#37020D",
                                 "comedy" = "#7B0003",
                                 "fantasy"= "#290F04", 
                                 "thriller"= "#610013",
                                 "scifi"= "#185ACC", 
                                 "drama" = "#FA030B",
                                 "family"= "#110075", 
                                 "music"= "#1495EA",
                                 "horror"= "#5870F4", 
                                 "crime"= "#60000F",
                                 "european"= "#AB000F",
                                 "documentarion"= "",
                                 "history"="#F2000F",
                                 "reality"="#F2000F",
                                 "romance"= "#6490DE",
                                 "sport"= "#273857",
                                 "war"= "#F57169",
                                 "western"="#8F413D"))+
  
  labs(
    title = "Cantidad de producciones por género",
    x = "Género",
    y = "Número de producciones"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10)
  )
print(grafico_generos)
  


 # 4. Número de temporadas según la mediana de este dato 
  
mediana_temporadas <- median(netflix_tmdb_series$Temporadas, na.rm = TRUE)
print(paste("Mediana de Temporadas en las Series:", round(mediana_temporadas, 2)))


grafico_temp_series <- ggplot(netflix_tmdb_series, aes(x = Temporadas)) +
  geom_histogram(bins = 30, fill = "darkred", color = "white", alpha = 0.7, na.rm = TRUE) +
  geom_vline(xintercept = mediana_temporadas, color = "red", size = 1.2, linetype = "dashed") +
  annotate("text", x = mediana_temporadas, y = Inf, 
           label = paste("Mediana:", round(mediana_temporadas, 2)),
           vjust = 2, color = "red", size = 4) +
  labs(
    title = "Distribución del Número de Temporadas en Series",
    x = "Número de Temporadas",
    y = "Frecuencia"
  ) +
  theme_minimal()

print(grafico_temp_series)



ggplot(netflix_tmdb_series, aes(y = Temporadas)) +
  geom_boxplot(fill = "darkred", color = "red", alpha = 0.6, outlier.color = "red") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -0.5, color = "black", size = 3.5)+
  labs(
    title = "Distribución del Número de Temporadas en Series",
    y = "Número de Temporadas"
  ) +
  theme_minimal(base_size = 13) 


# Análisis de exito según las décadas

## Grafico de barras (peliculas)

ggplot(netflix_tmdb_pelis , aes(x = decada, y = popularidad_promedio, fill = genero)) +
  geom_col(alpha = 0.85, color = "white") +
  scale_fill_manual(values = c("#####", "######", "#9E9AC8", "#74C476", "#FD8D3C", "#FDD49E")) +
  coord_flip() +
  labs(title = "Popularidad promedio por género y década en Netflix",
       subtitle = "Basado en la variable tmdb_popularidad",
       x = "Década",
       y = "Popularidad promedio",
       fill = "Género") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

## Grafico de barras (series)

ggplot(netflix_tmdb_series , aes(x = decada, y = popularidad_promedio, fill = genero)) +
  geom_col(alpha = 0.85, color = "white") +
  scale_fill_manual(values = c("#####", "######", "#9E9AC8", "#74C476", "#FD8D3C", "#FDD49E")) +
  coord_flip() +
  labs(title = "Popularidad promedio por género y década en Netflix",
       subtitle = "Basado en la variable tmdb_popularidad",
       x = "Década",
       y = "Popularidad promedio",
       fill = "Género") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Grafico de lineas

netflix_limpio %>%
  group_by(decada, Tipos) %>%
  summarise(duracion_promedio = mean(Tiempo, na.rm = TRUE)) %>%
  
  ggplot(aes(x = decada, y = duracion_promedio, color = Tipos, group = Tipos)) +
  geom_line(color = "####", size = 1.2) +
  geom_point(color = "lightblue", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_x_continuous() +
  labs(title = "Evolución de la duración promedio por tipo de contenido",
       x = "Década", y = "Duración promedio (minutos)", color = "Tipos") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Grafico de boxplot (geom_boxplot)

ggplot(netflix_tmdb_pelis, aes(x = decada, y = duracion, fill = genero)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Acción" = "#FF6F61",
    "Comedia" = "#6BAED6",
    "Drama" = "#9E9AC8",
    "Documental" = "#74C476",
    "Animación" = "#FD8D3C",
    "Romance" = "#FDD49E"
  )) +
  labs(title = "Distribución de la duración por década y generos del contenido",
       x = "Década", y = "Duración (minutos)") +
  theme_minimal()

ggplot(netflix_tmdb_series, aes(x = decada, y = duracion, fill = genero)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Acción" = "#FF6F61",
    "Comedia" = "#6BAED6",
    "Drama" = "#9E9AC8",
    "Documental" = "#74C476",
    "Animación" = "#FD8D3C",
    "Romance" = "#FDD49E"
  )) +
  labs(title = "Distribución de la duración por década y generos del contenido",
       x = "Década", y = "Duración (minutos)") +
  theme_minimal()


#Mapa de calor

## Calcular la popularidad promedio por país y década

popularidad_pais_decada <- netflix_tmdb_pelis %>%
  group_by(decada, Paises_produccion) %>%
  summarise(popularidad_promedio = mean(tmdb_popularidad, na.rm = TRUE),
            cantidad = n()) %>%
  arrange(decada, desc(popularidad_promedio))


# Gráfico de dispersión con suavizado

ggplot(netflix_tmdb_pelis, aes(x = Tiempo, y = tmdb_puntaje, color = decada)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_color_manual(values = c(
    "1980s" = "#8DD3C7",
    "1990s" = "#FFFFB3",
    "2000s" = "#BEBADA",
    "2010s" = "#FB8072",
    "2020s" = "#80B1D3")) +
  labs(title = "Relación entre duración y score según la década",
          x = "Duración (minutos)", y = "Score") +
      theme_minimal()
    
    ggplot(netflix_tmdb_series, aes(x = Tiempo, y = tmdb_puntaje, color = decada)) +
      geom_point(alpha = 0.5) +
      geom_smooth(se = FALSE, method = "lm") +
      scale_color_manual(values = c(
        "1980s" = "#8DD3C7",
        "1990s" = "#FFFFB3",
        "2000s" = "#BEBADA",
        "2010s" = "#FB8072",
        "2020s" = "#80B1D3")) +
      labs(title = "Relación entre duración y score según la década",
               x = "Duración (minutos)", y = "Score") +
          theme_minimal()
      
        ggplot(netflix_tmdb_series, aes(y = Temporadas)) +
          geom_boxplot(fill = "darkred", color = "red", alpha = 0.6, outlier.color = "red") +
          labs(
            title = "Distribución del Número de Temporadas en Series",
            y = "Número de Temporadas"
          ) +
          theme_minimal(base_size = 13) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
          stat_summary(fun = median, geom = "text", aes(x =1, label = round(after_stat(y),1)), 
                     vjust = -0.5, color = "black", size = 3.5)
        
    
  