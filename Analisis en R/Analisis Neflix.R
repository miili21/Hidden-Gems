# Carga y limpieza de los datos
## Carga de las librerias

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(stringr)

ruta_BD_Netflix <- "C:\\Users\\Usuario\\Documents\\GitHub\\Hidden-Gems\\Data\\netflix_dataset.csv"

##Cargar el dataset

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

###Visualización de valores faltantes
  
columnas_escenciales <- c("tmdb_score", "imdb_votes", "imdb_id", "imdb_score", "age_certification")
df_clean <- netflix %>% filter(if_all(all_of(columnas_esenciales), ~!is.na(.)))


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

###Se reemplazan con "Desconocido" los ID , Score, votos, popularidad de las paginas imdb y tmbd que aparezcan en NA
netflix_limpio <- netflix_limpio %>%
  mutate(
    tmdb_popularidad = ifelse(is.na(tmdb_popularidad), "desconocido", as.character(tmdb_popularidad)),
    tmdb_puntaje = ifelse(is.na(tmdb_puntaje), "desconocido", as.character(tmdb_puntaje)),
    imdb_score = ifelse(is.na(imdb_score), "desconocido", as.character(imdb_score)),
    imdb_votos = ifelse(is.na(imdb_votos), "desconocido", as.character(imdb_votos)),
    imdb_ID = ifelse(is.na(imdb_ID), "desconocido", imdb_ID),  
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
  distinct() 

netflix_limpio <- netflix_limpio %>%
  distinct(Titulos_producciones, .keep_all = TRUE)  

print(paste("Filas después de eliminar duplicados:", nrow(netflix_limpio)))


#Limpieza de datos atipicos

netflix_limpio <- netflix_limpio %>%
  mutate(
    #Filtrar años inválidos
    Año_Lanzamiento = case_when(
      Año_Lanzamiento < 1900 | Año_Lanzamiento > 2025 ~ NA_real_,
      TRUE ~ Año_Lanzamiento
    ),
    # Filtrar tiempos de reproducción incongruentes 
    Tiempo = case_when(
      Tiempo < 1 | Tiempo > 300 ~ NA_real_, 
      TRUE ~ Tiempo
    ),
    #puntajes 
    tmdb_puntaje = pmax(0, pmin(10, tmdb_puntaje))
  ) %>%
  # Convertir a integer donde sea lógico, es decir, Año y Temporadas
  mutate(
    Año_Lanzamiento = as.integer(Año_Lanzamiento),
    Temporadas = as.integer(Temporadas)
  )
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


#Resumen final

glimpse(netflix_limpio)
print("NAs finales:")
print(colSums(is.na(netflix_limpio)))

-------------------------------------------------------------------------------------------------------------------------------
#Paso 2: Crear variables y organizar la data de la manera necesaria para realizar la investigación
  ##Creamos una varible sobre Decadas 
  netflix_limpio<- netflix_limpio%>%
  mutate(
    Decadas = case_when(
      Año_Lanzamiento < 1900 ~ "Decada de los 80's",
      Año_Lanzamiento < 2000 ~ "Decada de los 90's",
      Año_Lanzamiento >2000 ~ "Decada de los 2000"
    )
  )

  ##Creamos dos tablas, una con la información de imdb y otra con la información de tmdb

vars_imdb <- c("ID", "Titulos_producciones", "Tipos", "Año_Lanzamiento", "Temporadas", "Tiempo", "Certificacion_edad", "Generos", "Paises_produccion",
               "imdb_score", "imdb_votos", "imdb_ID")

netflix_imdb <- netflix_limpio %>%
  select(all_of(vars_imdb))

vars_tmdb <- c("ID", "Titulos_producciones", "Tipos", "Año_Lanzamiento", "Temporadas", "Tiempo", "Certificacion_edad", "Generos", "Paises_produccion",
               "tmdb_popularidad", "tmdb_puntaje")

netflix_tmdb <- netflix_limpio %>%
  select(all_of(vars_tmdb))

##Filtrar las películas y series de las tablas de imbd y tmbd

###Tablas de películas y series para imbd

netflix_imbd_pelis <- netflix_imdb %>%
  filter(Tipos == "MOVIE")

netflix_imbd_series <- netflix_imdb %>%
  filter(Tipos == "SHOW")


###Tablas de películas y series para imbd

netflix_tmbd_pelis <- netflix_tmdb %>%
  filter(Tipos == "MOVIE")

netflix_tmbd_series <- netflix_tmdb %>%
  filter(Tipos == "SHOW")