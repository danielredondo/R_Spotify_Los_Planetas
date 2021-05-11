# Manual RSpotify: https://github.com/tiagomendesdantas/Rspotify
# Manual Spotify: https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/
# Spotify developer dashboard: https://developer.spotify.com/dashboard/applications
# En "Edit settings" se ha añadido "http://localhost:1410/" en el apartado de Redirect URIs

# ----- Paquetes -----

# install_github("tiagomendesdantas/Rspotify")
library(Rspotify)
library(dplyr)

# ----- Tokens para la aplicación de Spotify -----

app_name <- "[NOMBRE DE LA APLICACIÓN EN SPOTIFY]"
client_id <- "[CLIENT_ID DE LA APLICACIÓN EN SPOTIFY]"
client_secret <- "[CLIENT_SECRET DE LA APLICACIÓN EN SPOTIFY]"
keys <- spotifyOAuth(app_name, client_id, client_secret)

# ----- Extracción de datos -----

# Encontrar ID del artista
searchArtist("Los Planetas", token = keys)
id <- "0N1TIXCk9Q9JbEPXQDclEL" 

# Extraer discos
discografia <- getAlbums(artist_id = id, token = keys) %>% 
  # Quitar duplicados
  group_by(name) %>% summarise(id = min(id)) %>% 
  # Quitar edición especial y reedición/recopilatorio
  filter(! name %in% c("Zona Temporalmente Autónoma (Edición Especial)",
                       "Canciones para una Orquesta Química",
                       "Principios Basicos De Astronomia"))

discografia

# Poner tildes a algunos discos
discografia$name[6] <- "Una Ópera Egipcia"
discografia$name[7] <- "Una Semana En El Motor De Un Autobús"

for(i in discografia$id){
  # Se extrae el ID para conservarlo
  album_name <- discografia$name[match(i, discografia$id)]
  if(i == discografia$id[1]) canciones_spotify <- cbind(getAlbum(album_id = i, token = keys), album = i, album_name)
  else canciones_spotify <- rbind(canciones_spotify, cbind(getAlbum(album_id = i, token = keys), album = i, album_name))
}

canciones_spotify <- as_tibble(canciones_spotify)

for(cancion in canciones_spotify$id){
  if(cancion == canciones_spotify$id[1]) canciones <- getFeatures(track_id = cancion, token = keys)
  else canciones <- rbind(canciones, getFeatures(track_id = cancion, token = keys))
  
  # Indicador de proceso
  if(match(cancion, canciones_spotify$id) %% 20 == 0 ||
     match(cancion, canciones_spotify$id) == length(canciones_spotify$id))
    print(paste0("Canción ", match(cancion, canciones_spotify$id), "/", length(canciones_spotify$id)))
}

canciones <- as_tibble(canciones)

# ----- Preprocesamiento de datos -----

canciones_spotify$id <- unlist(canciones_spotify$id)
canciones_spotify$name <- unlist(canciones_spotify$name)
canciones <- merge(canciones_spotify, canciones, by = "id") %>%
  as_tibble() %>% 
  select(id, name, album_id = album, album = album_name, duration = duration_ms.y, tempo, valence,
         danceability, energy, acousticness, instrumentalness, liveness, speechiness) %>% 
  # Quitar duplicado de "Desorden" (son dos mezclas de la misma canción, se quita la versión de 4:03)
  # https://es.wikipedia.org/wiki/Super_8_(%C3%A1lbum)#Lista_de_canciones
  filter(id != "2phdCylIBc8WjK7llBajFi")
head(canciones)

# Comprobación de duplicados
length(canciones$name) == length(unique(canciones$name))

# Pasar a porcentajes las características de Spotify
canciones[7:13] <- round(canciones[7:13] * 100, 1)

# Convertir la duración a segundos
canciones$duration <- round(canciones$duration / 1000, 1)

# Redondear tempo a 1 decimal
canciones$tempo <- round(canciones$tempo, 1)

# ----- Exportación de datos -----

save(canciones, file = "canciones.RData")

# ----- Tabla interactiva -----

# Pensado para mostrar algunas características del dataset en web
library(DT)
df <- canciones[c(2, 4:12)] %>% arrange(desc(album))
DT::datatable(df,
              colnames = c("Canción", "Disco", "Duración(s)", "Tempo",
                           "Positiva(%)", "Bailable(%)", "Enérgica(%)",
                           "Acústica(%)", "Instrumental(%)", "Directo(%)")) %>% 
  formatStyle(names(df)[5:length(df)],
              background = styleColorBar(range(df[5:length(df)]), 'lightblue'),
              backgroundSize = '98% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center')
